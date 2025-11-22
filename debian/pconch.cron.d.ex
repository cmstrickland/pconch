#
# Regular cron jobs for the pconch package
#
0 4	* * *	root	[ -x /usr/bin/pconch_maintenance ] && /usr/bin/pconch_maintenance
