#!/bin/sh

DATA_DIR="/var/db/postgres/data10"

pkg install -y postgresql10-server postgresql10-contrib

sysrc postgresql_enable="YES"
sysrc postgresql_class="default"
sysrc postgresql_data="$DATA_DIR"
sysrc postgresql_flags="-w -s -m fast"
sysrc postgresql_initdb_flags="--encoding=utf-8 --lc-collate=C"
sysrc postgresql_profiles=""

service postgresql initdb
service postgresql start

sed -i -e "s|#listen_addresses = 'localhost'|listen_addresses = '*'|" $DATA_DIR/postgresql.conf
echo 'host    all    all    0.0.0.0/0    trust' >> $DATA_DIR/pg_hba.conf

service postgresql restart
service postgresql stop
