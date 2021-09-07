#!/bin/sh
USER=''{{ user.username }}''
status=$2

case $status in
       up)
		systemctl start dropbox@$USER.service
       ;;
       down)
       		systemctl stop dropbox@$USER.service
       ;;
esac
