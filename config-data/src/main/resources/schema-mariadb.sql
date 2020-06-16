CREATE TABLE IF NOT EXISTS config_file_content 
	(
	macid varchar(255) not null,
	file_content mediumtext not null,
	primary key(macid)
);