CREATE TABLE IF NOT EXISTS `player_feedback` (
  `feedback_id` bigint(20) not null,
  `player_id` bigint(20) NOT NULL,
  `content` blob DEFAULT NULL,
  `send_time` datetime DEFAULT NULL,
  `image_name` varchar(255) DEFAULT NULL,
  `image` blob DEFAULT NULL,
  `log_file_name` varchar(255) default NULL,
  `log_file_data` blob DEFAULT NULL,
  `is_read` tinyint default 0,
  `remarks`  varchar(1024),
  PRIMARY KEY (`feedback_id`,`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;