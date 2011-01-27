%%
%% @author Marco Yuen <marcoy@cs.princeton.edu>
%% @copyright Marco Yuen 2011
%% @doc Erlang API for Rackspace.
%%

-module(erlrack).
-author("Marco Yuen <marcoy@cs.princeton.edu>").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("rackspace.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, authenticate/2, authenticate/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Startup Function Exports
%% ------------------------------------------------------------------

-export([start/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], 
                          [{debug, [trace,log]}]).

% @doc Authenicates with Rackspace API server.
% @spec authenticate(Username::string(), APIKey::string()) ->
%       term()
authenticate(Username, APIKey) ->
    gen_server:call(?SERVER, {authenticate, Username, APIKey}).

% @doc Authenicates with Rackspace API server.
% @spec authenticate(Username::string(), APIKey::string(), Location::atom()) ->
%       term()
authenticate(Username, APIKey, Location) ->
    gen_server:call(?SERVER, {authenticate, Username, APIKey, Location}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    NewState = #rackspace{},
    {ok, NewState}.

handle_call({authenticate, Username, APIKey}, _From, State) ->
    {RetVal, NewState} = do_authenticate(Username, APIKey, us),
    case RetVal of
        ok ->
            {reply, ok, NewState};
        error ->
            {reply, {error, NewState}, State}
    end;
handle_call({authenticate, Username, APIKey, Location}, _From, State) ->
    {RetVal, NewState} = do_authenticate(Username, APIKey, Location),
    case RetVal of
        ok ->
            {reply, ok, NewState};
        error ->
            {reply, {error, NewState}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% @doc Converts the given location to URL.
% @spec loc2url(Loc::atom()) -> string()
% Loc = us | uk
loc2url(Loc) ->
    case Loc of
        uk -> ?UK_AUTH_URL;
        us -> ?US_AUTH_URL
    end.

% @doc Authenticates with Rackspace API server.
% @spec do_authenticate(Username::string(), APIKey::string(), Location::atom()) ->
%       {ok, #rackspace} | {error, string()}
do_authenticate(Username, APIKey, Location) ->
    Headers = [ {?AUTH_USER_HDR, Username}, {?AUTH_KEY_HDR, APIKey} ],
    AuthURL = loc2url(Location),
    {ok, Status, RespHdrs, _} = ibrowse:send_req(AuthURL,
                                                 Headers,
                                                 get),
    % io:format("Headers: ~p. Status: ~p~n", [RespHdrs, Status]),
    case Status of
        "204" ->
            Token  = proplists:get_value(?AUTH_TOKEN, RespHdrs),
            ManURL = proplists:get_value(?SRV_MAN_URL, RespHdrs),
            io:format("Token: ~s, URL: ~s~n", [Token, ManURL]),
            NewState = #rackspace{username = Username, api_key = APIKey,
                                  auth_token = Token, management_url = ManURL,
                                  auth_url = AuthURL},
            {ok, NewState};
        "401" ->
            {error, "Authentication failed"};
        _ ->
            {error, Status}
    end.

%% ------------------------------------------------------------------
%% Startup Function Definitions
%% ------------------------------------------------------------------

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    application:start(erlrack).

%% ------------------------------------------------------------------
%% Test Function Definitions
%% ------------------------------------------------------------------

