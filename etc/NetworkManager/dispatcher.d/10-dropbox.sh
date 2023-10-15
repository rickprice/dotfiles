#!/bin/sh
USER=''{{ user.machine_username }}''
status=$2

case $status in
       up)
		systemctl start dropbox@$USER.service
       ;;
       down)
       		systemctl stop dropbox@$USER.service
       ;;
esac
