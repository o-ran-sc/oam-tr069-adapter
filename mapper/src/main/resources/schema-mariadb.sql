CREATE TABLE IF NOT EXISTS device_operation_details (ID int null,DEVICE_ID varchar(30) not null, SW_VERSION varchar(30) not null, DOWN_LOAD_STATUS int default 0, FIRMWARE_FILE varchar(1024) );