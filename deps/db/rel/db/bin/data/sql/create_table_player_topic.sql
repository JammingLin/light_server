CREATE TABLE `player_topic` (
`player_id` bigint NOT NULL,
`topic_id`  bigint NOT NULL,
`wins` int(11) default 0,
`alls` int(11) default 0,
`right_answers` int(11) default 0,
`all_answers` int(11) default 0,
`all_answer_time` int(11) default 0,
`experiment` bigint default 0,
`beans` bigint default 0,
`last_challenge_time` datetime,
PRIMARY KEY (`player_id`,`topic_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;