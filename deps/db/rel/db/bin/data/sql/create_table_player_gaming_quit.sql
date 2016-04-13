CREATE TABLE `player_gaming_quit` (
`player_id`  bigint NOT NULL ,
`room_id`   int NOT NULL,
`bean`      int not null,
PRIMARY KEY (`player_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;