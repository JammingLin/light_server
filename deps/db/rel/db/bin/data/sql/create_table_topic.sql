CREATE TABLE `topic` (
`company_id`  bigint NOT NULL,
`topic` varchar(255) NOT NULL,
`topic_id` bigint NOT NULL,
PRIMARY KEY (`company_id`,`topic`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;