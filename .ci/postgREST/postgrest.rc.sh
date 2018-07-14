#!/bin/sh
#
# $FreeBSD$
#

# PROVIDE: postgrest
# REQUIRE: LOGIN
# KEYWORD: shutdown

#
# Add the following line to /etc/rc.conf to enable postgrest:
# postgrest_enable (bool):  Set to "YES" by default.
#                           Set it to "NO" to disable postgrest.

. /etc/rc.subr

name="postgrest"
rcvar=postgrest_enable
load_rc_config $name
: ${postgrest_enable="YES"}
postgrest_user="postgREST"
spidfile="/home/postgREST/${name}_daemon.pid"
cpidfile="/home/postgREST/${name}_child.pid"
logfile="/home/postgREST/${name}_daemon.log"
procname="postgrest /etc/postgrest.conf"
command="/usr/sbin/daemon"
command_args="-P ${spidfile} -p ${cpidfile} -r "  '"'"${procname}"'"'  "-u ${postgrest_user} -t postgrest_service"
pidfile="${cpidfile}"
interpreter="/bin/sh"
check_pidfile $pidfile $procname $interpreter
run_rc_command "$1"
