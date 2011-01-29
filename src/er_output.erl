%%
%% @author Marco Yuen <marcoy@cs.princeton.edu>
%% @copyright 2011 Marco Yuen
%% @doc This is the output format module for <em>erlrack</em>.
%%

-module(er_output).

-author("Marco Yuen <marcoy@cs.princeton.edu>").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([format_img_output/1,format_flavour_output/1,
         format_create_srv_output/1, format_list_srv_output/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% @doc Formats the output of {@link erlrack:get_images/0. get_images()}
%      into a proplist. For example, 
%      <code>[{55, "Arch Linux"}, {1, "Ubuntu 10.10"}]</code>.
% @spec format_img_output(Input::string()) -> [term()]
format_img_output(Input) ->
    InputJson = mochijson2:decode(Input),
    {struct, ImageObj} = InputJson,
    Images = proplists:get_value(<<"images">>, ImageObj),
    extractImageDetails(Images, []).

% @doc Formats the output of {@link erlrack:get_flavours/0. get_flavours()}
%      into a proplist. For example, 
%      <code>[{1, "256 server"}, {2, "512 server"}]</code>.
% @spec format_flavour_output(Input::string()) -> [term()]
format_flavour_output(Input) ->
    InputJson = mochijson2:decode(Input),
    {struct, FlvObj} = InputJson,
    Flavours = proplists:get_value(<<"flavors">>, FlvObj),
    extractFlavourDetails(Flavours, []).

% @doc Formats the output of {@link erlrack:create_server/2. create_server}
%      into a dictionary.
% @spec format_create_srv_output(Input::string()) -> dictionary()
format_create_srv_output(Input) ->
    InputJson = mochijson2:decode(Input),
    {struct, SrvObj} = InputJson,
    Server = proplists:get_value(<<"server">>, SrvObj),
    extractServerDetails(Server).

% @doc Formats the output of {@link erlrack:list_server/0. list_server}
%      into a list of dictionaries.
% @spec format_list_srv_output(Input::string()) -> [dictionary()]
format_list_srv_output(Input) ->
    InputJson = mochijson2:decode(Input),
    {struct, ServersObj} = InputJson,
    ServerList = proplists:get_value(<<"servers">>, ServersObj),
    extractServerListDetails(ServerList, []).

%% ------------------------------------------------------------------
%% Helper Function Definitions
%% ------------------------------------------------------------------

extractImageDetails([], PList) ->
    PList;
extractImageDetails([I|Images], PList) ->
    {struct, Details} = I,
    Id = proplists:get_value(<<"id">>, Details),
    Name = binary_to_list(proplists:get_value(<<"name">>, Details)),
    extractImageDetails(Images, [{Id, Name}|PList]).

extractFlavourDetails([], PList) ->
    PList;
extractFlavourDetails([F|Flavours], PList) ->
    {struct, Details} = F,
    Id = proplists:get_value(<<"id">>, Details),
    Name = binary_to_list(proplists:get_value(<<"name">>, Details)),
    extractFlavourDetails(Flavours, [{Id, Name}|PList]).

extractServerDetails(Server) ->
    {struct, Details} = Server,
    {struct, AddrDetails} = proplists:get_value(<<"addresses">>, Details),
    PubIP = lists:map(fun binary_to_list/1, 
                      proplists:get_value(<<"public">>, AddrDetails)),
    PriIP = lists:map(fun binary_to_list/1, 
                      proplists:get_value(<<"private">>, AddrDetails)),
    DictList = [
                {"id", proplists:get_value(<<"id">>, Details)},
                {"imageId", proplists:get_value(<<"imageId">>, Details)},
                {"flavorId", proplists:get_value(<<"flavorId">>, Details)},
                {"adminPass", 
                 binary_to_list(proplists:get_value(<<"adminPass">>, Details, <<"">>))},
                {"name",
                 binary_to_list(proplists:get_value(<<"name">>, Details))},
                {"hostId",
                 binary_to_list(proplists:get_value(<<"hostId">>, Details))},
                {"publicIP", PubIP},
                {"privateIP", PriIP}
               ],
    dict:from_list(DictList).

extractServerListDetails([], DList) ->
    DList;
extractServerListDetails([S|Servers], DList) ->
    SrvDict = extractServerDetails(S),
    extractServerListDetails(Servers, [SrvDict|DList]).

