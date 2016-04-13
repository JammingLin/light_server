CREATE TABLE IF NOT EXISTS `passport` (
  `account` varchar(100) NOT NULL,
  `password` varchar(100) NOT NULL,
  `player_id` bigint NOT NULL,
  `create_on`  datetime NOT NULL,
  PRIMARY KEY (`account`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;