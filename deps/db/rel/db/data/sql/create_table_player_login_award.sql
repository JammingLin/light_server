CREATE TABLE `player_login_award` (
`player_id`  bigint NOT NULL,
`login_award_time` datetime NOT NULL,
PRIMARY KEY (`player_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;