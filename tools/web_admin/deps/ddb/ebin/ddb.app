{application,ddb,
             [{description,"AWS DynamoDB client"},
              {vsn,"0.1.3"},
              {registered,[ddb_sup]},
              {applications,[kernel,stdlib,crypto,ssl,ibrowse]},
              {env,[]},
              {modules,[ddb,ddb_aws,ddb_iam,ddb_util,ddb_xml]}]}.
