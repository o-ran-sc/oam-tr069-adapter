
CREATE TABLE IF NOT EXISTS ves_device_data 
	(
	ID bigint null,
	DEVICE_ID varchar(30) not null,
	ENODEB_NAME varchar(100),
	OUI varchar(30),
	PRODUCT_CLASS varchar(100),
	ATTR_JSON varchar(4000),
	ATTR_GROUP varchar(256) not null,
	LAST_UPDATED_TIME TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
	UNIQUE(DEVICE_ID,ATTR_GROUP)
);


