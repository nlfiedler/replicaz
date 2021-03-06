%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016-2018 Nathan Fiedler
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
    % get the name of the zfs pool we are using for testing
    Tank = get_env("RPZ_TEST_POOL"),
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
            %
            % prepare the datasets for testing
            %
            Tank = ?config(tank, Config),
            AnglerFish = Tank ++ "/anglerfish",
            Turtle = Tank ++ "/turtle",
            ?assertCmd("sudo zfs create " ++ AnglerFish),
            ?assertCmd("sudo chmod -R 777 /" ++ Tank),
            %
            % copy everything except the logs, which contain our 64MB files
            %
            Cwd = os:getenv("PWD"),
            ?assertCmd("rsync --exclude=logs -r " ++ Cwd ++ "/* /" ++ AnglerFish),
            %
            % run it once and we should see one set of snapshots
            %
            % load the application so we can override the config params
            ok = application:load(replicaz),
            %
            % Set up the application environment to copy our dataset.
            % Configure two of them to test the supervisor initialization.
            % The second one never actually runs.
            %
            DatasetsConf = [{"cmntest", [
                {from_dataset, AnglerFish},
                {to_dataset, Turtle},
                {period, daily},
                {frequency, 1},
                {use_sudo, true},
                {local_sudo, true}
            ]}, {"nise", [
                {from_dataset, "whatever"},
                {to_dataset, "dontmind"},
                {period, weekly},
                {frequency, 4}
            ]}],
            ok = application:set_env(replicaz, datasets, DatasetsConf),
            %
            % fire up the application and wait for it to finish
            %
            {ok, _Started} = application:ensure_all_started(replicaz),
            ok = gen_server:call(replicaz_srv_cmntest, test_backup, infinity),
            Asnapshots1 = ?cmd("zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(1, length(string:tokens(Asnapshots1, "\n"))),
            Tsnapshots1 = ?cmd("zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(1, length(string:tokens(Tsnapshots1, "\n"))),
            %
            % run it again to make sure it produces new snapshots
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            writefile("/" ++ AnglerFish ++ "/random1.dat"),
            ok = gen_server:call(replicaz_srv_cmntest, test_backup, infinity),
            Asnapshots2 = ?cmd("zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(2, length(string:tokens(Asnapshots2, "\n"))),
            Tsnapshots2 = ?cmd("zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(2, length(string:tokens(Tsnapshots2, "\n"))),
            %
            % run it a third time and ensure only 2 snapshots exist
            % (sleep long enough so the snapshots have distinct names)
            %
            ok = timer:sleep(1100),
            writefile("/" ++ AnglerFish ++ "/random2.dat"),
            ok = gen_server:call(replicaz_srv_cmntest, test_backup, infinity),
            Asnapshots3 = ?cmd("zfs list -H -r -t snapshot " ++ AnglerFish),
            ?assertEqual(2, length(string:tokens(Asnapshots3, "\n"))),
            Tsnapshots3 = ?cmd("zfs list -H -r -t snapshot " ++ Turtle),
            ?assertEqual(2, length(string:tokens(Tsnapshots3, "\n"))),
            %
            % clean up
            %
            ?assertCmd("sudo zfs destroy -r " ++ AnglerFish),
            ?assertCmd("sudo zfs destroy -r " ++ Turtle)
    end.

% Write some random data to the given file path.
writefile(Filename) ->
    Bytes = crypto:strong_rand_bytes(1024),
    ok = file:write_file(Filename, Bytes).

% Retrieve an environment variable, ensuring it is defined.
get_env(Name) ->
    case os:getenv(Name) of
        false ->
            error(lists:flatten(io_lib:format("must define ~p environment variable", [Name])));
        Value -> Value
    end.
