%% app generated at {2015,6,29} {17,41,22}
{application, lager, [
        {description, "Erlang logging framework"},
        {vsn, "2.0.3"},
        {modules, []},
        {applications, [kernel, stdlib, goldrush]},
        {registered,
            [lager_sup, lager_event, lager_crash_log, lager_handler_watcher_sup]
        },
        {mod, {lager_app, []}},
        {env, [
            {colored, false},
            {colors, [
                {debug, "\e[0;38m"},
                {info, "\e[1;37m"},
                {notice, "\e[1;36m"},
                {warning, "\e[1;33m"},
                {error, "\e[1;31m"},
                {critical, "\e[1;35m"},
                {alert, "\e[1;44m"},
                {emergency, "\e[1;41m"}
            ]},
            {crash_log_msg_size, 65536},
            {crash_log_size, 10485760},
            {crash_log_date, "$D0"},
            {crash_log_count, 5},
            {error_logger_redirect, true},
            {error_logger_hwm, 100},
            {async_threshold, 20},
            {async_threshold_window, 5},
            {crash_log, "../log/gate/crash.log"},
            {handlers, [
                    {lager_console_backend, info},
                    {lager_file_backend, [{file, "../log/gate/error.log"}, {level, error}]},
                    {lager_file_backend, [{file, "../log/gate/console.log"}, {level, info}]}
            ]}
        ]}
    ]
}.