%%%-------------------------------------------------------------------
%% @doc aaws public API
%% @end
%%%-------------------------------------------------------------------

-module(aaws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aaws_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
