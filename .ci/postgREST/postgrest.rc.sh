#!/bin/sh
#
# $FreeBSD$
#

# PROVIDE: postgrestd
# REQUIRE: LOGIN
# KEYWORD: shutdown

. /etc/rc.subr

name="postgrestd"
app="/usr/local/bin/postgrestd.sh"
rcvar=postgrestd_enable
load_rc_config $name
command="/usr/sbin/daemon"
daemon_user="user"
command_args="-u ${daemon_user} -o /home/user/postgrestd.log -r ${app}"
run_rc_command "$1"
