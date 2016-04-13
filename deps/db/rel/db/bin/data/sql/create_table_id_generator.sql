CREATE TABLE IF NOT EXISTS id_generator (
  server_id int not null,
  type varchar(30) NOT NULL,
  count bigint(20) NOT NULL,
  PRIMARY KEY (server_id, type)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;