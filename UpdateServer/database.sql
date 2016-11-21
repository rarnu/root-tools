create database root_tools character set utf8;

-- at least one photo, and most for 5.
create table feedback(
	id int primary key auto_increment,
	nickname varchar(64) not null,
	comment varchar(1024) not null,
	photo1 varchar(256) null,
	photo2 varchar(256) null,
	photo3 varchar(256) null,
	photo4 varchar(256) null,
	photo5 varchar(256) null,
	commit_date varchar(64) not null,
	status int default 0
) character set utf8;
