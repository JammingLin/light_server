{application,simple_bridge,
             [{description,"Common Interface to Erlang HTTP Servers"},
              {vsn,"2.0.1"},
              {applications,[kernel,stdlib]},
              {registered,[]},
              {mod,{simple_bridge_app,[]}},
              {modules,[cowboy_request_server,cowboy_simple_bridge,
                        cowboy_simple_bridge_anchor,cowboy_simple_bridge_sup,
                        inets_simple_bridge,inets_simple_bridge_anchor,
                        inets_simple_bridge_sup,mochiweb_simple_bridge,
                        mochiweb_simple_bridge_anchor,
                        mochiweb_simple_bridge_sup,sb_file_upload_handler,
                        sb_uploaded_file,sbw,simple_bridge,simple_bridge_app,
                        simple_bridge_handler,simple_bridge_handler_sample,
                        simple_bridge_multipart,simple_bridge_util,
                        simple_bridge_websocket,webmachine_simple_bridge,
                        webmachine_simple_bridge_anchor,
                        webmachine_simple_bridge_static,
                        webmachine_simple_bridge_sup,yaws_simple_bridge,
                        yaws_simple_bridge_anchor,yaws_simple_bridge_sup]}]}.