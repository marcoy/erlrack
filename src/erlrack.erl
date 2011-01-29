%%
%% @author Marco Yuen <marcoy@cs.princeton.edu>
%% @copyright 2011 Marco Yuen
%% @doc The frontend module for users. This module contains all of the
%%      API functions.
%%

-module(erlrack).
-author("Marco Yuen <marcoy@cs.princeton.edu>").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("rackspace.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, authenticate/2, authenticate/3, 
         get_flavours/0, get_images/0, create_server/2,
         list_server/0, change_passwd/2]).

-export([create_template/3, template2json/1, create_chpasswd_json/1]).

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

% @doc Authenicates with Rackspace API server. Location defaults to us.
% @spec authenticate(Username::string(), APIKey::string()) ->
%       term()
authenticate(Username, APIKey) ->
    gen_server:call(?SERVER, {authenticate, Username, APIKey}).

% @doc Authenicates with Rackspace API server with location.
% @type location() = us | uk
% @spec authenticate(Username::string(), APIKey::string(), 
%                    Location::location()) -> term()
authenticate(Username, APIKey, Location) ->
    gen_server:call(?SERVER, {authenticate, Username, APIKey, Location}).

% @doc Gets the different types of flavour from Rackspace.
% @spec get_flavours() -> term()
get_flavours() ->
    gen_server:call(?SERVER, flavours).

% @doc Gets the different types of image from Rackspace.
% @spec get_images() -> term()
get_images() ->
    gen_server:call(?SERVER, images).

% @doc Creates a cloud server on Rackspace.
% @spec create_server(ServerTemplate::#server{}, Count::int()) -> Output
%       Output = [term()]
create_server(ServerTemplate, Count) ->
    gen_server:call(?SERVER, {create_server, ServerTemplate, Count}).

% @doc Lists all the Cloud Servers currently running.
% @spec list_server() -> [dictionary()]
list_server() ->
    gen_server:call(?SERVER, list_server).

% @doc Changes the password of a given Cloud Server.
% @spec change_passwd(HostID::string(), NewPass::string()) -> ok
change_passwd(HostID, NewPass) ->
    gen_server:call(?SERVER, {change_passwd, HostID, NewPass}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    NewState = #rackspace{},
    ibrowse:trace_on(),
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
    end;
handle_call(flavours, _From, State) ->
    {ok, RespBody, NewState} = do_get_resource(State, ?FLV_END),
    Reply = er_output:format_flavour_output(RespBody),
    {reply, Reply, NewState};
handle_call(images, _From, State) ->
    {ok, RespBody, NewState} = do_get_resource(State, ?IMG_END),
    Reply = er_output:format_img_output(RespBody),
    {reply, Reply, NewState};
handle_call({ create_server, SrvTemplate, Count }, 
            _From, State) ->
    do_create_server(State, SrvTemplate, Count, []),
    {reply, ok, State};
handle_call(list_server, _From, State) ->
    {ok, RespBody, NewState} = do_get_resource(State, ?LST_SRV_END),
    Reply = er_output:format_list_srv_output(RespBody),
    {reply, Reply, NewState};
handle_call({change_passwd, HostID, NewPass}, _From, State) ->
    {ok, Reply, NewState} = do_change_passwd(State, HostID, NewPass),
    {reply, Reply, NewState}.

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
% @spec do_authenticate(Username::string(), APIKey::string(), 
%                       Location::atom()) -> Reply
%       Reply = {ok, #rackspace{}} | {error, string()}
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
                                  auth_url = AuthURL, location = Location},
            {ok, NewState};
        "401" ->
            {error, "Authentication failed"};
        _ ->
            {error, Status}
    end.

% @doc Create cloud servers in Rackspace.
% @spec do_create_server(State::#rackspace{}, SrvTemplate::#servers{},
%                        Count::int(), Out::list()) -> list()
% @todo Some error checking (Normal Response Code is 202)
do_create_server(_State, _SrvTemplate, Count, Out) when Count =< 0 ->
    Out;
do_create_server(State, SrvTemplate, Count, Out) ->
    ReqBdy = template2json(SrvTemplate),
    ReqURL = State#rackspace.management_url ++ ?SRV_END,
    ReqHdr = [ {?AUTH_TOKEN, State#rackspace.auth_token} ],
    {ok, Status, RespHdrs, RespBody} = ibrowse:send_req(ReqURL,
                                                        ReqHdr,
                                                        post,
                                                        [ReqBdy]),
    io:format("Status: ~p~n", [Status]),
    io:format("RespHdrs: ~p~n", [RespHdrs]),
    io:format("RespBody: ~p~n", [RespBody]),
    case Status of
        "202" -> % Normal
            do_create_server(Status, SrvTemplate, Count-1, [RespBody|Out]);
        "413" -> % over limit (50/day)
            io:format("Over limit"),
            do_create_server(Status, SrvTemplate, 0, Out)
    end.

% @doc Perform a GET request on a given resource.
% @spec do_get_resource(State, ResourceEndPoint::string()) ->
%       Reply
%       State = #rackspace{}
%       Reply = {ok, string(), #rackspace{}} |
%               {error, string(), #rackspace{}}
do_get_resource(State, ResourceEndPoint) ->
    ReqURL = State#rackspace.management_url ++ ResourceEndPoint,
    ReqHdr = [ {?AUTH_TOKEN, State#rackspace.auth_token} ],
    {ok, Status, _RespHdrs, RespBody} = ibrowse:send_req(ReqURL,
                                                         ReqHdr,
                                                         get),
    io:format("RespBody: ~p~n", [RespBody]),
    case Status of
        "200" ->
            {ok, RespBody, State};
        "203" ->
            {ok, RespBody, State};
        "401" ->
            % re-authenticate and retry
            {ok, NewState} = do_authenticate(State#rackspace.username,
                                             State#rackspace.api_key,
                                             State#rackspace.location),
            do_get_resource(NewState, ResourceEndPoint);
        _ ->
            {error, "unknown error", State}
    end.

% @doc Changes the password of a Cloud Server.
% @spec do_change_passwd(State::#server{}, HostID::string(),
%                        NewPass::string()) -> Reply
%       Reply = {ok, ok, #server{}}
do_change_passwd(State, HostID, NewPass) ->
    EndPoint = io_lib:format(?CHG_PAS_END, [HostID]),
    ReqURL = State#rackspace.management_url ++ EndPoint,
    ReqHdr = [ {?AUTH_TOKEN, State#rackspace.auth_token} ],
    ReqBdy = create_chpasswd_json(NewPass),
    {ok, Status, _RespHdrs, _RespBody} = ibrowse:send_req(ReqURL,
                                                          ReqHdr,
                                                          put,
                                                          [ReqBdy]),
    case Status of
        "204" ->
            {ok, ok, State};
        "401" ->
            {ok, NewState} = do_authenticate(State#rackspace.username,
                                             State#rackspace.api_key,
                                             State#rackspace.location),
            do_change_passwd(NewState, HostID, NewPass)
    end.

% @doc Helper for creating server record.
% @spec create_template(Name::string(), ImageID::int(), 
%                       FlavourID::int()) -> #server{}
% ImageID = 55, FlavourID = 1
create_template(Name, ImageID, FlavourID) ->
    #server{image_id  = ImageID, 
            flavor_id = FlavourID,
            name      = Name}.

% @doc Creates the JSON document for 
%      {@link erlrack:change_passwd/2. change_passwd}.
% @spec create_chpasswd_json(NewPass::string()) -> iolist()
create_chpasswd_json(NewPass) ->
    PassJ = { struct, [{<<"adminPass">>,list_to_binary(NewPass)}] },
    Req   = { struct, [{<<"server">>, PassJ}] },
    mochijson2:encode(Req).

% @doc Converts a server record (template) into a JSON document.
% @spec template2json(ServerTemplate::#server{}) -> iolist()
template2json(ServerTemplate) ->
    #server{image_id  = ImageID, 
            flavor_id = FlavourID, 
            name      = Name} = ServerTemplate,
    Details = {struct, [{ <<"name">>, list_to_binary(Name) },
                        { <<"imageId">>, ImageID },
                        { <<"flavorId">>, FlavourID }]},
    Req = {struct, [{ <<"server">>, Details }]},
    mochijson2:encode(Req).

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

