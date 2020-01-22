erl_proxy
=====

An OTP application

Build
-----

    $ rebar3 as prod tar

Config
-----
edit your config/sys.config:
    `forward` field used for proxy chain;
    auth for server/forward support three auth method:
    1. {16#7c, "password"}          // the recommand method, support encrypt
    2. {2, "username", "password"}  // username+password auth, but no encrypt
    3. {0}                          // no auth and no encrypt

Run
-----

    $ mkdir release
    $ cd release && tar zxvf ../_build/prod/rel/erl_proxy/erl_proxy-0.1.0.tar.gz
or just download the release tarball and untar
    $ cd bin
    $ erl_proxy start
