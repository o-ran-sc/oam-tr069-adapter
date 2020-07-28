CREATE TABLE IF NOT EXISTS config_file_content 
	(
	ID int null,
	macid varchar(255) not null,
	file_content mediumtext not null,
	SW_VERSION varchar(64) not null,
	HW_VERSION varchar(64) not null,
	primary key(macid, SW_VERSION, HW_VERSION)
);

CREATE SEQUENCE IF NOT EXISTS hibernate_sequence MINVALUE 1 MAXVALUE 4611686018427387903 CYCLE CACHE 10;