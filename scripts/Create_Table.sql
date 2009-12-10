DROP TABLE ALOG;

CREATE TABLE ALOG (
	level		character varying(10),
	timestamp	timestamp with time zone,
	message		text
);

GRANT ALL PRIVILEGES ON ALOG TO alog;
