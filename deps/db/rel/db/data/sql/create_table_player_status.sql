CREATE TABLE `player_status` (
`player_id`  bigint NOT NULL ,
`last_logon_time`  datetime NOT NULL ,
`last_leave_time`  datetime NOT NULL ,
PRIMARY KEY (`player_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;