%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Nathan Fiedler
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
-module(replicaz_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Datasets) ->
    supervisor:start_link(?MODULE, Datasets).

init(Datasets) ->
    Children = lists:map(fun(Dataset) ->
        { replicaz_srv
        , {replicaz_srv, start_link, [Dataset]}
        , permanent, 5000, worker, [replicaz_srv]
        }
    end, Datasets),
    {ok, {{one_for_one, 1, 5}, Children}}.
