CREATE TABLE `topic_pool` (
`topic_id`  bigint NOT NULL,
`player_id` bigint NOT NULL,
PRIMARY KEY (`topic_id`,`player_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;