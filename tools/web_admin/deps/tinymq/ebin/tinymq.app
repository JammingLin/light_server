{application,tinymq,
             [{description,"TinyMQ: a diminutive message queue"},
              {vsn,"0.8.15"},
              {registered,[tinymq]},
              {modules,[tinymq,tinymq_app,tinymq_channel_controller,
                        tinymq_channel_sup,tinymq_controller,tinymq_sup]},
              {applications,[kernel,stdlib]},
              {env,[{max_age,60}]},
              {mod,{tinymq_app,[]}}]}.
