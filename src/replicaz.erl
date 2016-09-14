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

-include_lib("kernel/include/file.hrl").

-define(LOGFILE, "/var/log/replica.log").

main(Args) ->
    OptSpecList = [
        {help,     $h, "help",    boolean, "display usage"},
        {version,  $v, "version", boolean, "display version information"},
        {log_file, $l, "logfile", string,  "path to log file (default " ++ ?LOGFILE ++ ")"},
        {remote,   $r, "remote",  string,  "user@host for SSH"},
        {sudo,     $s, "sudo",    boolean, "use sudo for SSH commands"},
        {label,    $L, "label",   string,  "for the snapshot names (default 'replica')"}
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
maybe_version(false, _OptSpecList, Options, [FromSet, ToSet]) ->
    % Do the real work of this script.
    LogFile = proplists:get_value(log_file, Options, ?LOGFILE),
    Remote = proplists:get_value(remote, Options, false),
    ok = application:set_env(replicaz, remote, Remote),
    UseSudo = proplists:get_value(sudo, Options, false),
    ok = application:set_env(replicaz, use_sudo, UseSudo),
    Label = proplists:get_value(label, Options, "replica"),
    ok = application:set_env(replicaz, label, Label),
    guard_replicate(FromSet, ToSet, LogFile);
maybe_version(false, _OptSpecList, _Options, _Args) ->
    io:format("~nMust pass source and target data sets~n~n").

% Perform the replication, ensuring that the auto-snapshot property is
% restored to its previous value if an error occurs during replication.
guard_replicate(FromSet, ToSet, LogFile) ->
    % open the log file
    case error_logger:logfile({open, LogFile}) of
        ok -> ok;
        {error, Reason} ->
            io:format("Cannot open log file: ~s", [Reason]),
            exit(eaccess)
    end,
    error_logger:tty(false),
    error_logger:info_msg("Replication beginning..."),
    % Out of sheer laziness, start SSH unconditionally.
    ssh:start(),
    % capture the current auto-snapshot settings
    FromSnapVal = get_snapshot_setting(FromSet, fun run_local_cmd/1),
    ToSnapVal = get_snapshot_setting(ToSet, fun run_dest_cmd/1),
    % perform the actual replication
    try replicate(FromSet, ToSet) of
        _ -> error_logger:info_msg("replication from ~s to ~s complete", [FromSet, ToSet])
    catch
        Type:Error ->
            error_logger:error_msg("replication failure, ~p, ~p~n~p",
                [Type, Error, erlang:get_stacktrace()])
    after
        % ensure snapshot setting is restored on both data sets
        run_local_cmd(io_lib:format("zfs set com.sun:auto-snapshot=~s ~s", [FromSnapVal, FromSet])),
        run_dest_cmd(io_lib:format("zfs set com.sun:auto-snapshot=~s ~s", [ToSnapVal, ToSet])),
        ok
    end,
    % close the log file to ensure the records are flushed
    ok = error_logger:logfile(close),
    ok.

% Create a snapshot on the source and send it to the destination.
replicate(FromSet, ToSet) ->
    % disable the automatic snapshots to prevent spurious failures
    run_local_cmd("zfs set com.sun:auto-snapshot=false " ++ FromSet),
    run_dest_cmd("zfs set com.sun:auto-snapshot=false " ++ ToSet),
    create_and_send_snapshot(FromSet, ToSet),
    SourceSnaps = our_snapshots(FromSet, fun run_local_cmd/1),
    prune_old_snapshots(FromSet, SourceSnaps, fun run_local_cmd/1),
    DestSnaps = our_snapshots(ToSet, fun run_dest_cmd/1),
    prune_old_snapshots(ToSet, DestSnaps, fun run_dest_cmd/1),
    ok.

% Retrieve auto-snapshot setting for named dataset, using the given
% function, which handles local versus remote command execution.
get_snapshot_setting(DataSet, CmdRunner) ->
    Output = CmdRunner("zfs get -Ho value com.sun:auto-snapshot " ++ DataSet),
    Value = string:strip(Output, both, $\n),
    error_logger:info_msg("~p com.sun:auto-snapshot=~p", [DataSet, Value]),
    Value.

% Return a list of the snapshots created by this script, using the given
% function, which handles local versus remote command execution.
our_snapshots(DataSet, CmdRunner) ->
    % Get our mananged snapshots for the given file system, such that they
    % contain the given label, followed by a date in the ISO 8601 format
    % (i.e. YYYY-mm-dd-HH:MM:SS).
    %
    % For example: dataset@replica:2016-09-03-04:15:12
    error_logger:info_msg("fetching snapshots for ~s", [DataSet]),
    SnapshotsOut = CmdRunner("zfs list -t snapshot -Hro name " ++ DataSet),
    SplitOutput = re:split(SnapshotsOut, "\n", [{return, list}]),
    Snapshots = lists:filter(fun(Line) -> length(Line) > 0 end, SplitOutput),
    {ok, Label} = application:get_env(replicaz, label),
    {ok, MP} = re:compile("@" ++ Label ++ ":\\d{4}-\\d{2}-\\d{2}-\\d{2}:\\d{2}:\\d{2}"),
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
send_full(Src, Dst, Tag) ->
    error_logger:info_msg("sending full snapshot from ~s to ~s", [Src, Dst]),
    SendCmd = io_lib:format("zfs send -R ~s@~s", [Src, Tag]),
    RecvCmd = build_recv_cmd("zfs recv -F " ++ Dst),
    run_local_cmd(generate_pipe_script(SendCmd, RecvCmd)),
    error_logger:info_msg("full snapshot sent from ~s to ~s", [Src, Dst]),
    ok.

% Send an incremental replication stream from source to target.
send_incremental(Src, Dst, Tag1, Tag2) ->
    error_logger:info_msg("sending incremental snapshot from ~s to ~s", [Src, Dst]),
    SendCmd = io_lib:format("zfs send -R -I ~s ~s@~s", [Tag1, Src, Tag2]),
    RecvCmd = build_recv_cmd("zfs recv -F " ++ Dst),
    run_local_cmd(generate_pipe_script(SendCmd, RecvCmd)),
    error_logger:info_msg("incremental snapshot sent from ~s to ~s", [Src, Dst]),
    ok.

% Create a snapshot and send it to the destination.
create_and_send_snapshot(Src, Dst) ->
    % Make the new snapshot, get a list of existing snapshots, and decide
    % whether to send a full stream or an incremental. Create a snapshot
    % for Src whose name is today's date and time in the ISO 8601 format
    % (YYYY-mm-dd-HH:MM:SS). The time is in UTC.
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:universaltime(),
    Tag = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B",
                        [Year, Month, Day, Hour, Min, Sec]),
    {ok, Label} = application:get_env(replicaz, label),
    Snapname = io_lib:format("~s@~s:~s", [Src, Label, Tag]),
    run_local_cmd("zfs snapshot " ++ Snapname),
    error_logger:info_msg("created snapshot ~s", [Snapname]),
    SrcSnaps = our_snapshots(Src, fun run_local_cmd/1),
    DestSnaps = our_snapshots(Dst, fun run_dest_cmd/1),
    send_snapshot(Src, Dst, SrcSnaps, DestSnaps).

% Send either a full or incremental replication snapshot, depending on the
% state of the existing snapshots on both the source and destination
% datasets.
send_snapshot(Src, _Dst, [], _DestSnaps) ->
    error_logger:error_msg("Failed to create new snapshot in ~s", [Src]),
    throw(snapshot_failed);
send_snapshot(Src, Dst, [SrcSnap], _DestSnaps) ->
    % send the initial snapshot
    send_full(Src, Dst, SrcSnap);
send_snapshot(Src, Dst, SrcSnaps, []) ->
    % send the latest snapshot since the destination has none
    send_full(Src, Dst, lists:last(SrcSnaps));
send_snapshot(Src, Dst, SrcSnaps, DestSnaps) ->
    case lists:member(lists:last(DestSnaps), SrcSnaps) of
        true -> ok;
        false ->
            Msg = "Destination snapshots out of sync, destroy and try again.",
            error_logger:error_msg(Msg),
            throw(snapshot_failed)
    end,
    % destination has matching snapshots, send an incremental
    Recents = lists:nthtail(length(SrcSnaps) - 2, SrcSnaps),
    send_incremental(Src, Dst, hd(Recents), hd(tl(Recents))),
    ok.

% Prune the old replica snapshots from the named Dataset, using the given
% function, which handles local versus remote command execution.
prune_old_snapshots(Dataset, Snapshots, CmdRunner) when length(Snapshots) > 2 ->
    Destroy = fun(Name, Snap) ->
        Output = CmdRunner("zfs destroy " ++ Name ++ "@" ++ Snap),
        if length(Output) > 1 ->
                error_logger:error_msg("zfs destroy output: ~s", [Output]);
            true -> ok
        end,
        error_logger:info_msg("deleted old snapshot ~s", [Snap])
    end,
    [Destroy(Dataset, S) || S <- lists:sublist(Snapshots, length(Snapshots) - 2)],
    ok;
prune_old_snapshots(_Dataset, _Snapshots, _CmdRunner) ->
    % not enough snapshots, nothing to prune
    ok.

% Generate a shell script to execute the first command, piping its output
% to the second command. Returns the path of the generated shell script.
% The exit status will be that of the pipe itself.
generate_pipe_script(FirstCmd, SecondCmd) ->
    % Let the shell do the pipelining for us, as it seems rather difficult
    % to do so in Erlang, without eventually running out of memory. It
    % should use the pipestatus as its own exit code. Note that we return
    % the PIPESTATUS, which requires using bash.
    case os:find_executable("bash", "/bin:/usr/bin:/usr/local/bin") of
        false ->
            error_logger:info_msg("cannot find 'bash' in /bin:/usr/bin:/usr/local/bin"),
            error(missing_bash);
        Bash ->
            Cmds = [
                "#!" ++ Bash,
                FirstCmd ++ " | " ++ SecondCmd,
                "exit ${PIPESTATUS[0]}",
                % ensure the last line ends with a newline
                ""
            ],
            {ok, Cwd} = file:get_cwd(),
            ScriptPath = filename:join(Cwd, "pipe_cmd.sh"),
            {ok, IoDevice} = file:open(ScriptPath, [write]),
            ScriptText = string:join(Cmds, "\n"),
            ok = file:write(IoDevice, list_to_binary(ScriptText)),
            ok = file:close(IoDevice),
            ok = file:write_file_info(ScriptPath, #file_info{mode=8#00755}),
            ScriptPath
    end.

% Run the given command using a port and ensure it exits without error.
% Return the output from the command as a list of bytes.
run_local_cmd(Cmd) ->
    error_logger:info_msg("running local cmd: ~s", [Cmd]),
    ScriptPort = erlang:open_port({spawn, Cmd}, [exit_status]),
    {ok, 0, Output} = wait_for_port(ScriptPort),
    Output.

% Run the given command for the destination dataset, possibly running it
% over an SSH connection, and possibly prefixed with "sudo" (but only if
% remote). Return the output as a list of bytes.
run_dest_cmd(Cmd) ->
    run_dest_cmd(Cmd, application:get_env(replicaz, remote, false)).

% Run the given command using a port and ensure it exits without error.
% Return the output from the command as a list of bytes. If Remote is a
% string (and not 'false') then use SSH to run the command.
run_dest_cmd(Cmd, false) ->
    error_logger:info_msg("running dest cmd locally: ~s", [Cmd]),
    ScriptPort = erlang:open_port({spawn, Cmd}, [exit_status]),
    {ok, 0, Output} = wait_for_port(ScriptPort),
    Output;
run_dest_cmd(Cmd, Remote) ->
    FinalCmd = maybe_add_sudo(Cmd),
    error_logger:info_msg("running dest cmd remotely: ~s", [FinalCmd]),
    {ConnectionRef, ChannelId} = ssh_connect(Remote),
    success = ssh_connection:exec(ConnectionRef, ChannelId, FinalCmd, infinity),
    {ok, 0, Output} = wait_for_closed(ConnectionRef, ChannelId),
    ok = ssh:close(ConnectionRef),
    binary_to_list(Output).

% Construct an SSH connection to the remote system and return the
% connection reference and channel identifier in a tuple.
ssh_connect(Remote) ->
    [User, Host] = re:split(Remote, "@", [{return, list}]),
    SshOpts = [
        {user, User},
        {silently_accept_hosts, true},
        {user_interaction, false},
        {quiet_mode, true}
    ],
    {ok, ConnectionRef} = ssh:connect(Host, 22, SshOpts),
    {ok, ChannelId} =  ssh_connection:session_channel(ConnectionRef, infinity),
    {ConnectionRef, ChannelId}.

% Receive messages from the given SSH connection and channel, and return
% the tuple {ok, ExitStatus, Output} where Output is a binary() and
% ExitStatus is an integer().
wait_for_closed(ConnectionRef, ChannelId) ->
    wait_for_closed(ConnectionRef, ChannelId, <<>>, 0).

% Receive messages from the given SSH connection and channel, and return
% the tuple {ok, ExitStatus, Output} where Output is a binary() and
% ExitStatus is an integer(). The Output parameter is whatever output has
% been accumulated so far, and the Status is whatever was received with the
% 'exit_status' message.
wait_for_closed(ConnectionRef, ChannelId, Output, Status) ->
    receive
        {ssh_cm, ConnectionRef, {data, ChannelId, _DataType, Data}} ->
            % _DataType is 1 for stderr and 0 for stdout but we just merge
            % it all together for our purposes
            wait_for_closed(ConnectionRef, ChannelId, list_to_binary([Output, Data]), Status);
        {ssh_cm, ConnectionRef, {eof, ChannelId}} ->
            wait_for_closed(ConnectionRef, ChannelId, Output, Status);
        {ssh_cm, ConnectionRef, {exit_status, ChannelId, S}} ->
            wait_for_closed(ConnectionRef, ChannelId, Output, S);
        {ssh_cm, ConnectionRef, {closed, ChannelId}} ->
            {ok, Status, Output}
    end.

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}.
wait_for_port(Port) ->
    wait_for_port(Port, false, []).

% Wait for the given Port to complete and return the exit code in the form
% of {ok, Status}. Any output received is written to the log. If the port
% experiences an error, returns {error, Reason}. If Quiet is true, output
% from the port is not logged.
wait_for_port(Port, Quiet, Output) when is_boolean(Quiet) ->
    receive
        {Port, {exit_status, Status}} ->
            ensure_port_closed(Port),
            {ok, Status, Output};
        {Port, {data, Data}} ->
            if Quiet -> error_logger:warning_msg("output from port ignored...");
                true -> error_logger:warning_msg("received output from port: ~s", [Data])
            end,
            wait_for_port(Port, Quiet, Output ++ Data);
        {'EXIT', Port, Reason} ->
            error_logger:info_msg("port ~w exited, ~w", [Port, Reason]),
            {error, Reason}
    end.

% Ensure that the given Port has been properly closed. Does nothing if the
% port is not open.
ensure_port_closed(Port) ->
    case erlang:port_info(Port) of
        undefined -> ok;
        _         -> erlang:port_close(Port)
    end.

% Construct the command for the receiving end of the replication stream. If
% the 'remote' app env setting is not 'false', then prepend the command
% with "ssh" and the value of Remote. If 'remote' is not 'false', and
% 'use_sudo' is true, prepend the command with "sudo".
build_recv_cmd(Cmd) ->
    case application:get_env(replicaz, remote, false) of
        false -> Cmd;
        Remote -> string:join(["ssh", Remote, maybe_add_sudo(Cmd)], " ")
    end.

% If the application is configured to use sudo for remote commands, prepend
% the given command with "sudo".
maybe_add_sudo(Cmd) ->
    case application:get_env(replicaz, use_sudo, false) of
        false -> Cmd;
        true -> "sudo " ++ Cmd
    end.
