CREATE TABLE `player_company` (
`player_id`  bigint NOT NULL ,
`company_id` bigint NOT NULL,
`company` varchar(255) NOT NULL,
PRIMARY KEY (`player_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;