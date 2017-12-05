%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016-2017 Nathan Fiedler
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
-export([init_per_suite/1, end_per_suite/1, all/0, replicate_test/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    % ensure lager is configured for testing
    ok = application:set_env(lager, lager_common_test_backend, debug),
    % Generate a unique ZFS dataset name, ensuring the first character is a
    % letter, which ZFS requires.
    Tank = [$a | string:substr(string:to_lower(ulid:generate_list()), 2)],
    [{tank, Tank} | Config].

end_per_suite(_Config) ->
    case application:stop(replicaz) of
        ok -> ok;
        {error, {not_started, replicaz}} -> ok;
        {error, Reason} -> error(Reason)
    end.

all() ->
    [
        replicate_test
    ].

replicate_test(Config) ->
    case os:find_executable("zfs") of
        false ->
            ct:log("missing 'zfs' in PATH, skipping test..."),
            ok;
        _ZfsBin ->
            PrivDir = ?config(priv_dir, Config),
            % create the datasets for testing
            FSFile = filename:join(PrivDir, "tank_file"),
            mkfile(FSFile),
            % ZFS on Mac and Linux both require sudo access, and FreeBSD doesn't mind.
            % Specify a mount point to avoid unexpected defaults (e.g. ZFS on Mac).
            Tank = ?config(tank, Config),
            AnglerFish = Tank ++ "/anglerfish",
            Turtle = Tank ++ "/turtle",
            ?assertCmd("sudo zpool create -m /" ++ Tank ++ " " ++ Tank ++ " " ++ FSFile),
            ?assertCmd("sudo zfs create " ++ AnglerFish),
            ?assertCmd("sudo zfs create " ++ Turtle),
            ?assertCmd("sudo chmod -R 777 /" ++ Tank),
            % copy everything except the logs, which contain our 64MB files
            Cwd = os:getenv("PWD"),
            ?assertCmd("rsync --exclude=logs -r " ++ Cwd ++ "/* /" ++ AnglerFish),
            %
            % run it once and we should see one set of snapshots
            %
            % load the application so we can override the config params
            ok = application:load(replicaz),
            % set up the application environment to copy our dataset
            DatasetsConf = [{"cmntest", [
                {from_dataset, AnglerFish},
                {to_dataset, Turtle},
                {period, daily},
                {frequency, 1},
                {use_sudo, true},
                {local_sudo, true}  % tests are not run as root, need sudo
            ]}],
            ok = application:set_env(replicaz, datasets, DatasetsConf),
            % fire up the application and wait for it to finish
            {ok, _Started} = application:ensure_all_started(replicaz),
            ok = gen_server:call(replicaz_srv, test_backup),
            Asnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(1, length(string:tokens(Asnapshots1, "\n"))),
            Tsnapshots1 = ?cmd("sudo zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(1, length(string:tokens(Tsnapshots1, "\n"))),
            %
            % run it again to make sure it produces new snapshots
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            writefile("/" ++ AnglerFish ++ "/random1.dat"),
            ok = gen_server:call(replicaz_srv, test_backup),
            Asnapshots2 = ?cmd("sudo zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(2, length(string:tokens(Asnapshots2, "\n"))),
            Tsnapshots2 = ?cmd("sudo zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(2, length(string:tokens(Tsnapshots2, "\n"))),
            %
            % run it a third time and ensure only 2 snapshots exist
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            writefile("/" ++ AnglerFish ++ "/random2.dat"),
            ok = gen_server:call(replicaz_srv, test_backup),
            Asnapshots3 = ?cmd("sudo zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(2, length(string:tokens(Asnapshots3, "\n"))),
            Tsnapshots3 = ?cmd("sudo zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(2, length(string:tokens(Tsnapshots3, "\n"))),
            ?assertCmd("sudo zpool destroy " ++ Tank),
            ok = file:delete(FSFile)
    end.

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

% Write some random data to the given file path.
writefile(Filename) ->
    Bytes = crypto:strong_rand_bytes(1024),
    ok = file:write_file(Filename, Bytes).
