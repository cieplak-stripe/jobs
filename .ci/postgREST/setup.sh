#!/bin/sh

PGRST_PKG="https://github.com/PostgREST/postgrest/releases/download/v0.5.0.0/postgrest-v0.5.0.0-freebsd.tar.xz"

#  Install postgREST executable

pkg install -y curl
curl -L $PGRST_PKG > /tmp/postgrest.tgz
tar xf /tmp/postgrest.tgz -C /usr/local/bin/

#  Render posgREST config file

#  Install service script
