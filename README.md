# Introduction
[Rackspace](http://www.rackspace.com) provides a RESTful
[API](http://www.rackspacecloud.com/cloud_hosting_products/servers/api/) for
developers to control Cloud Servers. *erlrack* is an Erlang application for
controlling Cloud Servers in Erlang.

# Basic Usage
Obtain a API key from your [rackspace account](https://manage.rackspacecloud.com/).
Then authenicate by calling `erlrack:authenicate/2` or `erlrack:authenicate/3` using
your *username* and *API key*.

