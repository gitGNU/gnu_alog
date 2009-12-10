#!/bin/bash
#
# Execute this script as user postgres

psql -f Create_DB_and_User.sql
psql -f Create_Table.sql alog
