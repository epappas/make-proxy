{
    application,
    make_proxy_server,
    [
        {description, "Make Proxy Server"},
        {vsn, "2.0.0"},
        {modules, [
            mp_app,
            mp_sup,
            mp_child
        ]},
        {registered, [mp_sup]},
        {applications, [kernel, stdlib]},
        {mod, {mp_app, []}}
    ]
}.
