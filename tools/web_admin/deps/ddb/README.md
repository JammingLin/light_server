[![Build Status](https://travis-ci.org/ErlyORM/ddb.svg?branch=master)](https://travis-ci.org/ErlyORM/ddb)

# Accessing Amazon DynamoDB

Authenticating

    inets:start().
    ssl:start().
    lager:start().
    application:start(ibrowse).
    ddb_iam:credentials("access key", "secret").
    {'ok', Key, Secret, Token} = ddb_iam:token(129600).
    ddb:credentials(Key, Secret, Token).


Creating a table with a hash key

    ddb:create_table(<<"foo">>, ddb:key_type(<<"hashkey">>, 'string'), 10, 10).

Creating a table with hash and range keys

    ddb:create_table(<<"bar">>, ddb:key_type(<<"hashkey">>, 'string', <<"rangekey">>, 'number'), 10, 10).

Adding a record to a table with a hash key

    ddb:put(<<"foo">>, [{<<"hashkey">>, <<"hash key value">>, 'string'},
                        {<<"field1">>, <<"string value">>, 'string'}, 
                        {<<"field2">>, <<"100">>, 'number'}]).

Adding a record to a table with hash and range keys

    ddb:put(<<"bar">>, [{<<"hashkey">>, <<"hash key value">>, 'string'},
                        {<<"rangekey">>, <<"1000">>, 'number'},
                        {<<"field1">>, <<"string value">>, 'string'}, 
                        {<<"field2">>, <<"100">>, 'number'}]).
    
    ddb:put(<<"bar">>, [{<<"hashkey">>, <<"hash key value">>, 'string'},
                        {<<"rangekey">>, <<"2000">>, 'number'},
                        {<<"field3">>, <<"string value">>, 'string'}]).

Fetching a record from a table using a hash key

    ddb:get(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string')).

Fetching a record from a table using hash and range keys

    ddb:get(<<"bar">>, ddb:key_value(<<"hash key value">>, 'string', <<"1000">>, 'number')).

Querying a table

    ddb:find(<<"bar">>, {<<"hash key value">>, 'string'}, {'between', 'number', [<<"1000">>, <<"2000">>]}).

Changing value (and type) of one field while deleting another

    ddb:update(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string'), 
                          [{<<"field1">>, <<"1">>, 'number', 'put'},
                           {<<"field2">>, 'delete'}]).

Adding to a string set field and returning pre-update values of updated fields

    ddb:update(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string'), 
                          [{<<"field2">>, [<<"1">>, <<"2">>], ['string'], 'add'}],
                          'updated_old').

Deleting an item from a string set field and returning the values of all fields before the update

    ddb:update(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string'), 
                          [{<<"field2">>, [<<"1">>], ['string'], 'delete'}],
                          'all_old').

Update field1 only when field2 does not exist

    ddb:cond_update(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string'), 
                               [{<<"field1">>, <<"1">>, 'number', 'put'}],
                               {'does_not_exist', <<"field2">>}).

Update field1 only when field2 exists and has a numerical value of 1

    ddb:cond_update(<<"foo">>, ddb:key_value(<<"hash key value">>, 'string'), 
                               [{<<"field1">>, <<"2">>, 'number', 'put'}],
                               {'exists', <<"field2">>, [<<"2">>], ['string']}).

See `src/ddb.erl` for the rest of the API.

Note that dates in Dynamo are represented as seconds since Unix epoch!

All data is stored as strings but you have to specify whether each field is a string or a number.
