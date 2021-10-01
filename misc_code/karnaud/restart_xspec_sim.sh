#!/bin/sh
# Script to restart XSPEC drive_sims.tcl process every hour
# Version 1.0, A. Ptak, 12/21/98
which xspec

while true
do
    # sleep for 1 hour
    echo "Sleeping for 1 hour..."
    sleep 3600
    echo "Killing any existing xspec sessions..."
    # kill pre-existing xspec sessions
    # First see if smart_fit.pid exists
    if test -f sim_model.pid ; then 
	echo "Found sim_model.pid"
	echo "kill `cat sim_model.pid`"
	kill `cat sim_model.pid`
    else
	tempid=`ps ax | grep 'xspec drive' | grep -v grep | awk '{print $1}'`
	tempid=${tempid##[^0-9]*}
	if test "$tempid"; then
	    echo "kill $tempid"
	    kill $tempid
	fi
    fi
    # restart xspec
    echo "Restarting xspec..."
    rm *.qdp
    xspec drive_sims.xcm > /dev/null &
done

