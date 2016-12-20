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

create table thanks(
	id int primary key auto_increment,
	name varchar(128) not null,
	description varchar(256) null,
	head_image varchar(256) null
) character set utf8;

create table crash(
	id int primary key auto_increment,
	model varchar(64) not null,
	sdk int default 0,
	appver int default 0,
	commit_date varchar(64) not null,
	data text null,
	status int default 0
) character set utf8;

create table version(
	id int primary key auto_increment,
	versionCode int default 0,
	versionName varchar(64) not null,
	url varchar(256) not null,
	description text not null
) character set utf8;

alter table thanks add column description_en varchar(256) null after description;

alter table version add column description_en text null after description;