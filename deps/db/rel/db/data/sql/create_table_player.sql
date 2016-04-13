CREATE TABLE  IF NOT EXISTS `player` (
  `player_id` bigint NOT NULL,
  `name` varchar(30) NOT NULL,
  `higher_department` varchar(30) DEFAULT NULL,
  `department` varchar(30) DEFAULT NULL,
  `level` int(11) NOT NULL,
  `bean`  int(11) NOT NULL,
  `experience` bigint NOT NULL,
  `area` varchar(50) NOT NULL,
  PRIMARY KEY (`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;