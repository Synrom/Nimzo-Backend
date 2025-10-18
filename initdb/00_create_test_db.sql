CREATE DATABASE gambit_test OWNER postgres;
\connect gambit_test;
\i /docker-entrypoint-initdb.d/01_schema_tables.sql
\i /docker-entrypoint-initdb.d/02_update_rank.sql