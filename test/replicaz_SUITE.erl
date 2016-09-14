%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Nathan Fiedler
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
%% The test suite.
%%
-module(replicaz_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        replicate_test
        % replicate_over_ssh_test
    ].

replicate_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            LogFile = filename:join(PrivDir, "replica.log"),
            % create the datasets for testing
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
            ?assertCmd("sudo zpool create -m /panzer panzer " ++ FSFile),
            ?assertCmd("sudo zfs create panzer/anglerfish"),
            ?assertCmd("sudo zfs create panzer/turtle"),
            ?assertCmd("sudo chmod -R 777 /panzer"),
            % copy everything except the logs, which contain our 64MB files
            Cwd = os:getenv("PWD"),
            ?assertCmd("rsync --exclude=logs -r " ++ Cwd ++ "/* /panzer/anglerfish"),
            %
            % run it once and we should see one set of snapshots
            %
            BinDir = Cwd ++ "/_build/test/bin",
            RelicaCmd = lists:flatten(io_lib:format(
                "sudo ~s/replicaz --logfile ~s --label test panzer/anglerfish panzer/turtle",
                [BinDir, LogFile])),
            ct:log(os:cmd(RelicaCmd)),
            Asnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot panzer/anglerfish"),
            ?assertEqual(1, length(string:tokens(Asnapshots1, "\n"))),
            Tsnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot panzer/turtle"),
            ?assertEqual(1, length(string:tokens(Tsnapshots1, "\n"))),
            %
            % run it again to make sure it produces new snapshots
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            {ok, _BC1} = file:copy(LogFile, "/panzer/anglerfish/replica.log"),
            ct:log(os:cmd(RelicaCmd)),
            Asnapshots2 = ?cmd("sudo zfs list -H -r -t snapshot panzer/anglerfish"),
            ?assertEqual(2, length(string:tokens(Asnapshots2, "\n"))),
            Tsnapshots2 = ?cmd("sudo zfs list -H -r -t snapshot panzer/turtle"),
            ?assertEqual(2, length(string:tokens(Tsnapshots2, "\n"))),
            %
            % run it a third time and ensure only 2 snapshots exist
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            {ok, _BC2} = file:copy(LogFile, "/panzer/anglerfish/replica.log"),
            ct:log(os:cmd(RelicaCmd)),
            Asnapshots3 = ?cmd("sudo zfs list -H -r -t snapshot panzer/anglerfish"),
            ?assertEqual(2, length(string:tokens(Asnapshots3, "\n"))),
            Tsnapshots3 = ?cmd("sudo zfs list -H -r -t snapshot panzer/turtle"),
            ?assertEqual(2, length(string:tokens(Tsnapshots3, "\n"))),
            ?assertCmd("sudo zpool destroy panzer"),
            ok = file:delete(FSFile)
    end.

%
% Difficult to get this working. Requires setting up the public/private SSH
% key pair, the host verification, and the path to the zfs binary. None of
% this is automatic for all systems at this point.
%
% replicate_over_ssh_test(_Config) ->
%     case os:find_executable("zfs") of
%         false ->
%             ct:log("missing 'zfs' in PATH, skipping test..."),
%             ok;
%         _ZfsBin ->
%             PrivDir = ?config(priv_dir, Config),
%             LogFile = filename:join(PrivDir, "replica.log"),
%             % create the datasets for testing
%             FSFile = filename:join(PrivDir, "tank_file"),
%             mkfile(FSFile),
%             % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind
%             ?assertCmd("sudo zpool create -m /panzer panzer " ++ FSFile),
%             ?assertCmd("sudo zfs create panzer/anglerfish"),
%             ?assertCmd("sudo zfs create panzer/turtle"),
%             ?assertCmd("sudo chmod -R 777 /panzer"),
%             % copy everything except the logs, which contain our 64MB files
%             Cwd = os:getenv("PWD"),
%             ?assertCmd("rsync --exclude=logs -r " ++ Cwd ++ "/* /panzer/anglerfish"),
%             %
%             % run it once and we should see one set of snapshots
%             %
%             BinDir = Cwd ++ "/_build/test/bin",
%             UserHost = os:getenv("USER") ++ "@localhost",
%             RelicaCmd = lists:flatten(io_lib:format(
%                 "sudo ~s/replicaz --logfile ~s --remote ~s --sudo panzer/anglerfish panzer/turtle",
%                 [BinDir, LogFile, UserHost])),
%             ct:log(os:cmd(RelicaCmd)),
%             Asnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot panzer/anglerfish"),
%             ?assertEqual(1, length(string:tokens(Asnapshots1, "\n"))),
%             Tsnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot panzer/turtle"),
%             ?assertEqual(1, length(string:tokens(Tsnapshots1, "\n"))),
%             %
%             % clean up
%             %
%             ?assertCmd("sudo zpool destroy panzer"),
%             ok = file:delete(FSFile)
%     end.

% Run the mkfile command (or its Linux equivalent) to create a temporary
% filesytem for ZFS to use as a storage pool.
mkfile(FSFile) ->
    case os:find_executable("mkfile") of
        false ->
            % Hipster Linux doesn't use your grandfather's mkfile...
            case os:find_executable("fallocate") of
                false ->
                    error("need either 'mkfile' or 'fallocate' to run tests");
                FBin ->
                    ?assertCmd(FBin ++ " -l 64M " ++ FSFile)
            end;
        MBin ->
            ?assertCmd(MBin ++ " 64m " ++ FSFile)
    end,
    ok.
