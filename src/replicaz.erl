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
%% This script creates a snapshot on the source ZFS file system and sends that in
%% the form of a replication stream to the destination file system. If a previous
%% snapshot created by this script exists then this script will create a new
%% snapshot and send an incremental replication stream to the destination. Older
%% snapshots on both the source and destination will be automatically pruned such
%% that the two most recent are retained.
%%
%% Note that this script uses the -F option for 'zfs recv' such that the
%% destination file system is rolled back before receiving the snapshot(s). This
%% is necessary since otherwise the receive will fail due to the mismatch in
%% existing snapshots. This occurs because simply listing a directory in the
%% destination will modify the access times, which causes a write to the file
%% system. The alternative is to make the destination read-only, but that is an
%% extra step which can be easily avoided.
%%
-module(replicaz).
-export([main/1]).

-define(LOGFILE, "/var/log/replica.log").

main(Args) ->
    OptSpecList = [
        {help,     $h, "help",    boolean, "display usage"},
        {log_file, $l, "logfile", string,  "path to log file"}
    ],
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            case proplists:get_bool(help, Options) of
                true ->
                    getopt:usage(OptSpecList, "replicaz", "<source> <dest>");
                false ->
                    if length(NonOptArgs) =/= 2 ->
                            io:format("~nMust pass source and target data sets~n~n"),
                            exit(badarg);
                        true -> ok
                    end,
                    FromSet = hd(NonOptArgs),
                    ToSet = hd(tl(NonOptArgs)),
                    LogFile = proplists:get_value(log_file, Options, ?LOGFILE),
                    guard_replicate(FromSet, ToSet, LogFile)
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "replicaz", "<source> <dest>")
    end,
    ok.

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
    SendPort = erlang:open_port({spawn, SendCmd}, [exit_status, binary]),

    RecvCmd = io_lib:format("zfs recv -F ~s", [Dst]),
    RecvPort = erlang:open_port({spawn, RecvCmd}, [exit_status, binary]),

    {ok, 0} = pipe_until_exit(SendPort, RecvPort, 0),
    case erlang:port_info(SendPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(SendPort)
    end,
    case erlang:port_info(RecvPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(RecvPort)
    end,
    error_logger:info_msg("full snapshot sent from ~s to ~s~n", [Src, Dst]),
    ok.

% Send an incremental replication stream from source to target.
send_incremental(Src, Dst, Tag1, Tag2) ->
    error_logger:info_msg("sending incremental snapshot from ~s to ~s~n", [Src, Dst]),

    SendCmd = io_lib:format("zfs send -R -I ~s ~s@~s", [Tag1, Src, Tag2]),
    SendPort = erlang:open_port({spawn, SendCmd}, [exit_status, binary]),

    RecvCmd = io_lib:format("zfs recv -F ~s", [Dst]),
    RecvPort = erlang:open_port({spawn, RecvCmd}, [exit_status, binary]),

    {ok, 0} = pipe_until_exit(SendPort, RecvPort, 0),
    case erlang:port_info(SendPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(SendPort)
    end,
    case erlang:port_info(RecvPort) of
        undefined -> ok;
        _         -> true = erlang:port_close(RecvPort)
    end,
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

% Receive data from SendPort and write to RecvPort. Waits for both ports to
% close, returning {ok, ExitStatus} of the last process to finish.
pipe_until_exit(SendPort, RecvPort, ClosedCount) ->
    receive
        {_Port, {exit_status, Status}} ->
            if ClosedCount == 1 -> {ok, Status};
                true -> pipe_until_exit(SendPort, RecvPort, ClosedCount + 1)
            end;
        {SendPort, {data, Data}} ->
            RecvPort ! {self(), {command, Data}},
            pipe_until_exit(SendPort, RecvPort, ClosedCount);
        {RecvPort, {data, Data}} ->
            error_logger:info_msg("received ~p from receiving port~n", [Data]),
            pipe_until_exit(SendPort, RecvPort, ClosedCount);
        {'EXIT', Port, Reason} ->
            error_logger:error_msg("port ~p exited, ~p~n", [Port, Reason])
    end.
