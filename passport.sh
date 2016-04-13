#!/bin/sh
[begin
Node = 'dgry_game@ubuntu',
Account = lists:concat(["robot", Index]),
Password = "1",
ID = rpc:call(Node, id_generator, get_id, [passport]),
rpc:call(Node, db, save, [passport, #{account=>Account, password=>Password, player_id=>ID}])
end || Index <- lists:seq(1, 100000)].
