%%%-------------------------------------------------------------------
%% @doc github_webhook_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(github_webhook_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [
        #{id => submission_queue, start => {submission_queue, start_link, []}},
        #{id => database_service, start => {database_service, start_link, []}},
        #{id => cache_service, start => {cache_service, start_link, []}}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
