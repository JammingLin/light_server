CREATE TABLE IF NOT EXISTS question (
  question_topic varchar(255) not null,
  question_id int(11) not null,
  error_report_times int(11) default 0,
  answer_error_times bigint(20) default 0,
  answer_times bigint(20) default 0,
  PRIMARY KEY (question_topic, question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;