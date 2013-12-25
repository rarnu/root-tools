
## jump_mode: jump to url(0) or show text dialog(1)

create table yugioh_recommand (id int primary key, name text not null, jump_mode int not null, jump_url text, jump_text text, image_name text) character set utf8;
