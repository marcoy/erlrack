%%
%% @author Marco Yuen <marcoy@cs.princeton.edu>
%% @copyright Marco Yuen 2011
%%

% URLs
-define(US_AUTH_URL, "https://auth.api.rackspacecloud.com/v1.0").
-define(UK_AUTH_URL, "https://lon.auth.api.rackspacecloud.com/v1.0").

% Headers
-define(AUTH_USER_HDR, "X-Auth-User").
-define(AUTH_KEY_HDR, "X-Auth-Key").
-define(AUTH_TOKEN, "X-Auth-Token").
-define(SRV_MAN_URL, "X-Server-Management-Url").

-record(rackspace, {username, api_key, auth_token, management_url,
                    auth_url=?US_AUTH_URL}).

