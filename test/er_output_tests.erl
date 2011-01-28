%%
%% @author Marco Yuen <marcoy@cs.princeton.edu>
%% @copyright 2011 Marco Yuen
%%

-module(er_output_tests).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Test Function Definitions
%% ------------------------------------------------------------------

format_img_output_test() ->
    Input = get_images_output(),
    ImagePropList = er_output:format_img_output(Input),
    Name = proplists:get_value(55, ImagePropList),
    ?assertEqual("Arch 2010.05", Name).

format_flavour_output_test() ->
    Input = get_flavours_output(),
    FlavourPropList = er_output:format_flavour_output(Input),
    Name = proplists:get_value(1, FlavourPropList),
    ?assertEqual("256 server", Name).

format_create_srv_output_test() ->
    Input = create_server_output(),
    ServerDict = er_output:format_create_srv_output(Input),
    ?assertEqual("9bf8231ae00ea9e59f511e07c8c006a5", 
                 dict:fetch("hostId", ServerDict)).

%% ------------------------------------------------------------------
%% Helper Function Definitions
%% ------------------------------------------------------------------

create_server_output() ->
    "{\"server\":{\"progress\":0,\"id\":576418,\"imageId\":55,\"flavorId\":1,\"status\":\"BUILD\",\"adminPass\":\"randpasswd\",\"name\":\"Test\",\"hostId\":\"9bf8231ae00ea9e59f511e07c8c006a5\",\"addresses\":{\"public\":[\"184.106.82.13\"],\"private\":[\"10.180.186.171\"]},\"metadata\":{}}}".

get_flavours_output() ->
    "{\"flavors\":[{\"id\":1,\"ram\":256,\"disk\":10,\"name\":\"256 server\"},{\"id\":2,\"ram\":512,\"disk\":20,\"name\":\"512 server\"},{\"id\":3,\"ram\":1024,\"disk\":40,\"name\":\"1GB server\"},{\"id\":4,\"ram\":2048,\"disk\":80,\"name\":\"2GB server\"},{\"id\":5,\"ram\":4096,\"disk\":160,\"name\":\"4GB server\"},{\"id\":6,\"ram\":8192,\"disk\":320,\"name\":\"8GB server\"},{\"id\":7,\"ram\":15872,\"disk\":620,\"name\":\"15.5GB server\"}]}".
    
get_images_output() ->
    "{\"images\":[{\"id\":58,\"name\":\"Windows Server 2008 R2 x64 - MSSQL2K8R2\"},{\"id\":71,\"name\":\"Fedora 14\"},{\"id\":29,\"name\":\"Windows Server 2003 R2 SP2 x86\"},{\"id\":40,\"name\":\"Oracle EL Server Release 5 Update 4\"},{\"id\":23,\"name\":\"Windows Server 2003 R2 SP2 x64\"},{\"id\":19,\"name\":\"Gentoo 10.1\"},{\"id\":31,\"name\":\"Windows Server 2008 SP2 x86\"},{\"id\":57,\"name\":\"Windows Server 2008 SP2 x64 - MSSQL2K8R2\"},{\"id\":14362,\"name\":\"Ubuntu 9.10 (karmic)\"},{\"id\":49,\"name\":\"Ubuntu 10.04 LTS (lucid)\"},{\"id\":55,\"name\":\"Arch 2010.05\"},{\"id\":41,\"name\":\"Oracle EL JeOS Release 5 Update 3\"},{\"id\":10,\"name\":\"Ubuntu 8.04.2 LTS (hardy)\"},{\"id\":187811,\"name\":\"CentOS 5.4\"},{\"id\":53,\"name\":\"Fedora 13\"},{\"id\":62,\"name\":\"Red Hat Enterprise Linux 5.5\"},{\"id\":24,\"name\":\"Windows Server 2008 SP2 x64\"},{\"id\":51,\"name\":\"CentOS 5.5\"},{\"id\":69,\"name\":\"Ubuntu 10.10 (maverick)\"},{\"id\":28,\"name\":\"Windows Server 2008 R2 x64\"},{\"id\":56,\"name\":\"Windows Server 2008 SP2 x86 - MSSQL2K8R2\"},{\"id\":14,\"name\":\"Red Hat Enterprise Linux 5.4\"},{\"id\":4,\"name\":\"Debian 5.0 (lenny)\"}]}".

