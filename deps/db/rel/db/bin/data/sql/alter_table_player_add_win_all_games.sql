ALTER TABLE `player`
ADD COLUMN `wins`  int(11) default 0 AFTER `area`,
ADD COLUMN `all_games` int(11) default 0 AFTER `wins`;