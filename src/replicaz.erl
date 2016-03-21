%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2016 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% Script to replicate one ZFS filesystem to another in a repeatable fashion.
%%
-module(replicaz).
-export([main/1]).

-define(LOGFILE, "/var/log/replica.log").

main(Args) ->
    OptSpecList = [
        {help,     $h, "help",    boolean, "display usage"},
        {version,  $v, "version", boolean, "display version information"},
        {log_file, $l, "logfile", string,  "path to log file"}
    ],
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            maybe_help(proplists:get_bool(help, Options), OptSpecList, Options, NonOptArgs);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "replicaz", "<source> <dest>")
    end,
    ok.

% Handle the --help optional command-line flag.
maybe_help(true, OptSpecList, _Options, _NonOptArgs) ->
    getopt:usage(OptSpecList, "replicaz", "<source> <dest>");
maybe_help(false, OptSpecList, Options, NonOptArgs) ->
    maybe_version(proplists:get_bool(version, Options), OptSpecList, Options, NonOptArgs).

% Handle the --version optional command-line flag.
maybe_version(true, _OptSpecList, _Options, _NonOptArgs) ->
    ok = application:load(replicaz),
    {ok, Keys} = application:get_all_key(replicaz),
    Version = proplists:get_value(vsn, Keys),
    io:format("replicaz version ~p~n", [Version]);
maybe_version(false, _OptSpecList, Options, NonOptArgs) ->
    % Do the real work of this script.
    if length(NonOptArgs) =/= 2 ->
            io:format("~nMust pass source and target data sets~n~n"),
            exit(badarg);
        true -> ok
    end,
    FromSet = hd(NonOptArgs),
    ToSet = hd(tl(NonOptArgs)),
    LogFile = proplists:get_value(log_file, Options, ?LOGFILE),
    guard_replicate(FromSet, ToSet, LogFile).

% Perform the replication, ensuring that the auto-snapshot property is
% restored to its previous value if an error occurs during replication.
guard_replicate(FromSet, ToSet, LogFile) ->
    % open the log file
    case error_logger:logfile({open, LogFile}) of
        ok -> ok;
        {error, Reason} ->
            io:format("Cannot open log file: ~s~n", [Reason]),
            exit(eaccess)
    end,
    error_logger:tty(false),
    error_logger:info_msg("Replication beginning...~n"),
    % capture the current auto-snapshot settings
    FromSnapVal = get_snapshot_setting(FromSet),
    ToSnapVal = get_snapshot_setting(ToSet),
    % perform the actual replication
    try replicate(FromSet, ToSet) of
        _ -> error_logger:info_msg("replication from ~s to ~s complete~n", [FromSet, ToSet])
    catch
        Type:Error ->
            error_logger:error_msg("replication failure, ~p, ~p~n~p",
                [Type, Error, erlang:get_stacktrace()])
    after
        % ensure snapshot setting is restored on both data sets
        os:cmd(io_lib:format("zfs set com.sun:auto-snapshot=~s ~s", [FromSnapVal, FromSet])),
        os:cmd(io_lib:format("zfs set com.sun:auto-snapshot=~s ~s", [ToSnapVal, ToSet])),
        ok
    end,
    % close the log file to ensure the records are flushed
    ok = error_logger:logfile(close),
    ok.

% Create a snapshot on the source and send it to the destination.
replicate(FromSet, ToSet) ->
    % disable the automatic snapshots to prevent spurious failures
    os:cmd(io_lib:format("zfs set com.sun:auto-snapshot=false ~s", [FromSet])),
    os:cmd(io_lib:format("zfs set com.sun:auto-snapshot=false ~s", [ToSet])),
    create_and_send_snapshot(FromSet, ToSet),
    prune_old_snapshots(FromSet, ToSet),
    ok.

% Retrieve auto-snapshot setting for named dataset.
get_snapshot_setting(DataSet) ->
    Cmd = io_lib:format("zfs get -Ho value com.sun:auto-snapshot ~s", [DataSet]),
    Output = os:cmd(Cmd),
    Value = string:strip(Output, both, $\n),
    error_logger:info_msg("~p com.sun:auto-snapshot=~p~n", [DataSet, Value]),
    Value.

% Creates a snapshot for fsys whose name is today's date and time in the
% ISO 8601 format (YYYY-mm-dd-HH:MM:SS). The time is in UTC.
take_snapshot(DataSet) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:universaltime(),
    Tag = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B",
                        [Year, Month, Day, Hour, Min, Sec]),
    Snapname = io_lib:format("~s@replica:~s", [DataSet, Tag]),
    os:cmd(io_lib:format("zfs snapshot ~s", [Snapname])),
    error_logger:info_msg("created snapshot ~s~n", [Snapname]),
    ok.

% Return a list of the snapshots created by this script.
our_snapshots(DataSet) ->
    % Get our mananged snapshots for the given file system, such that they
    % are named "replica:" followed by a date in the ISO 8601 format (i.e.
    % YYYY-mm-dd-HH:MM:SS).
    error_logger:info_msg("fetching snapshots for ~s~n", [DataSet]),
    SnapshotsOut = os:cmd(io_lib:format("zfs list -t snapshot -Hro name ~s", [DataSet])),
    SplitOutput = re:split(SnapshotsOut, "\n", [{return, list}]),
    Snapshots = lists:filter(fun(Line) -> length(Line) > 0 end, SplitOutput),
    {ok, MP} = re:compile("@replica:\\d{4}-\\d{2}-\\d{2}-\\d{2}:\\d{2}:\\d{2}"),
    KeepOurs = fun(Elem) ->
        case re:run(Elem, MP) of
            {match, _Captured} -> true;
            _ -> false
        end
    end,
    OurSnaps = lists:filter(KeepOurs, Snapshots),
    Snapnames = [hd(tl(re:split(S, "@", [{return, list}]))) || S <- OurSnaps],
    lists:sort(Snapnames).

% Send a replication stream for a single snapshot.
send_snapshot(Src, Dst, Tag) ->
    error_logger:info_msg("sending full snapshot from ~s to ~s~n", [Src, Dst]),

    SendCmd = io_lib:format("zfs send -R ~s@~s", [Src, Tag]),
    RecvCmd = io_lib:format("zfs recv -F ~s", [Dst]),
    % Using ports to pipe the output of one port to the input of another
    % seems to blow up memory, while using os:cmd/1 means we have no idea
    % if the processes had an error or not.
    os:cmd(SendCmd ++ " | " ++ RecvCmd),
    error_logger:info_msg("full snapshot sent from ~s to ~s~n", [Src, Dst]),
    ok.

% Send an incremental replication stream from source to target.
send_incremental(Src, Dst, Tag1, Tag2) ->
    error_logger:info_msg("sending incremental snapshot from ~s to ~s~n", [Src, Dst]),

    SendCmd = io_lib:format("zfs send -R -I ~s ~s@~s", [Tag1, Src, Tag2]),
    RecvCmd = io_lib:format("zfs recv -F ~s", [Dst]),
    % See note above about ports.
    os:cmd(SendCmd ++ " | " ++ RecvCmd),
    error_logger:info_msg("incremental snapshot sent from ~s to ~s~n", [Src, Dst]),
    ok.

% Create a snapshot and send it to the destination.
create_and_send_snapshot(Src, Dst) ->
    % make the new snapshot, get a list of existing snapshots,
    % and decide whether to send a full stream or an incremental
    take_snapshot(Src),
    Snaps = our_snapshots(Src),
    if length(Snaps) == 0 ->
            error_logger:error_msg("Failed to create new snapshot in ~s~n", [Src]),
            throw(snapshot_failed);
        true -> ok
    end,
    DestSnaps = our_snapshots(Dst),
    if length(DestSnaps) > 0 ->
            case lists:member(lists:last(DestSnaps), Snaps) of
                true -> ok;
                false ->
                    Msg = "Destination snapshots out of sync, destroy and try again.~n",
                    error_logger:error_msg(Msg),
                    throw(snapshot_failed)
            end;
        true -> ok
    end,
    if length(Snaps) == 1 ->
            % send the initial snapshot
            send_snapshot(Src, Dst, hd(Snaps));
        length(DestSnaps) == 0 ->
            % send the latest snapshot since the destination has none
            send_snapshot(Src, Dst, lists:last(Snaps));
        true ->
            % destination has matching snapshots, send an incremental
            Recents = lists:nthtail(length(Snaps) - 2, Snaps),
            send_incremental(Src, Dst, hd(Recents), hd(tl(Recents)))
    end,
    ok.

% Prune the old replica snapshots from source and destination.
prune_old_snapshots(Src, Dst) ->
    % prune old snapshots in source file system
    Snaps = our_snapshots(Src),
    Destroy = fun(DataSet, Snap) ->
        Output = os:cmd(io_lib:format("zfs destroy ~s@~s", [DataSet, Snap])),
        if length(Output) > 1 ->
                error_logger:error_msg("zfs destroy output: ~s~n", [Output]);
            true -> ok
        end,
        error_logger:info_msg("deleted old snapshot ~s~n", [Snap])
    end,
    if length(Snaps) > 2 ->
            [Destroy(Src, Snap) || Snap <- lists:sublist(Snaps, length(Snaps) - 2)];
        true -> ok
    end,
    % prune old snapshots in destination file system
    DestSnaps = our_snapshots(Dst),
    if length(DestSnaps) > 2 ->
            [Destroy(Dst, Snap) || Snap <- lists:sublist(DestSnaps, length(DestSnaps) - 2)];
        true -> ok
    end,
    ok.
