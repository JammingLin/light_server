CREATE TABLE `player_pk_record` (
`player_id`     bigint   NOT NULL,
`pk_report_id`  bigint   NOT NULL,
`pk_time`       datetime NOT NULL,
`pk_result`     blob     NOT NULL,
`is_read`       int(11)  default 0,
PRIMARY KEY (`player_id`,`pk_report_id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8;