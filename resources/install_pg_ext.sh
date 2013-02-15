#!/usr/bin/env bash

sudo -u postgres psql < /usr/share/postgresql/9.1/extension/hstore--unpackaged--1.0.sql
sudo -u postgres psql < pgsql/audit.sql
sudo -u postgres psql < pgsql/mashup-server.sql

