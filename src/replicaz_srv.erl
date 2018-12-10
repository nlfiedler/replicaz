%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017-2018 Nathan Fiedler
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
%% Primary driver of the backup procedure.
%%
-module(replicaz_srv).
-behavior(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {myname, timer, dataset}).

%%
%% Client API
%%
start_link(MyName, Dataset) ->
    gen_server:start_link({local, MyName}, ?MODULE, {MyName, Dataset}, []).

%%
%% gen_server callbacks
%%
init({MyName, Dataset}) ->
    State = #state{myname=MyName, dataset=Dataset},
    NewState = create_timer(State),
    {ok, NewState}.

handle_call(test_backup, _From, State) ->
    % this is test-only code, so cancel the timer
    WithoutTimer = cancel_timer(State),
    process_dataset(State#state.dataset),
    {reply, ok, WithoutTimer}.

handle_cast(process, State) ->
    process_dataset(State#state.dataset),
    WithTimer = create_timer(State),
    {noreply, WithTimer};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unexpected message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private functions
%%

% Process the dataset.
process_dataset({Key, Values}) ->
    %
    % While often discouraged, using the process dictionary is the easiest way
    % of passing values to the deeply nested functions in the other module.
    %
    erlang:put(rpz_label, Key),
    case proplists:get_value(use_sudo, Values) of
        undefined -> erlang:erase(rpz_use_sudo);
        UseSudo -> erlang:put(rpz_use_sudo, UseSudo)
    end,
    case proplists:get_value(local_sudo, Values) of
        undefined -> erlang:erase(rpz_local_sudo);
        LocalSudo -> erlang:put(rpz_local_sudo, LocalSudo)
    end,
    case proplists:get_value(ssh_host, Values) of
        undefined -> erlang:erase(rpz_ssh_host);
        SshHost -> erlang:put(rpz_ssh_host, SshHost)
    end,
    case proplists:get_value(ssh_user, Values) of
        undefined -> erlang:erase(rpz_ssh_user);
        SshUser -> erlang:put(rpz_ssh_user, SshUser)
    end,
    case proplists:get_value(ssh_user_dir, Values) of
        undefined -> erlang:erase(rpz_ssh_user_dir);
        UserDir -> erlang:put(rpz_ssh_user_dir, UserDir)
    end,
    lager:info("starting replication of dataset ~s", [Key]),
    FromSet = proplists:get_value(from_dataset, Values),
    ToSet = proplists:get_value(to_dataset, Values),
    replicaz:guard_replicate(FromSet, ToSet),
    lager:info("finished replication of dataset ~s", [Key]).

% Cancel the existing timer, if any, and return the updated state.
cancel_timer(State) ->
    case State#state.timer of
        undefined -> ok;
        Timer -> {ok, cancel} = timer:cancel(Timer)
    end,
    State#state{timer=undefined}.

% Create a new timer, returning the updated state.
create_timer(State) ->
    % Start a timer to cast a 'process' message to us at the next backup time
    % for the given dataset.
    M = gen_server,
    F = cast,
    A = [State#state.myname, process],
    {Key, Values} = State#state.dataset,
    Period = proplists:get_value(period, Values),
    Frequency = proplists:get_value(frequency, Values),
    lager:info("setting ~s timer for period ~s, frequency ~w", [Key, Period, Frequency]),
    {ok, Timer} = timer:apply_after(timer_value(Period, Frequency), M, F, A),
    State#state{timer=Timer}.

% Return the milliseconds for the given period and frequency. For instance, a
% period of 'hourly' and frequency of 12 yields 43,200,000 milliseconds.
timer_value(hourly, Frequency) when is_integer(Frequency) ->
    3600 * 1000 * Frequency;
timer_value(daily, Frequency) when is_integer(Frequency) ->
    86400 * 1000 * Frequency;
timer_value(weekly, Frequency) when is_integer(Frequency) ->
    86400 * 1000 * 7 * Frequency.
