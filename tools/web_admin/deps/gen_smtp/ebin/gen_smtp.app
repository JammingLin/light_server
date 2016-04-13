{application,gen_smtp,
             [{description,"An erlang SMTP server/client framework"},
              {vsn,"0.1"},
              {modules,[binstr,gen_smtp_application,gen_smtp_client,
                        gen_smtp_server,gen_smtp_server_session,mimemail,
                        smtp_rfc822_parse,smtp_server_example,smtp_util,
                        socket]},
              {applications,[kernel,stdlib,crypto,asn1,public_key,ssl]},
              {registered,[]}]}.
