#!/usr/bin/env python

# monitor 3ware raid status (v1.1)
# (C) Jeremy Sanders (2006)
# released under the GNU Public License

# designed to be run under crontab on short intervals
# error handling is minor, so please check it works!
# NO WARRANTY IS PROVIDED

import os
import sys

tw_cli = '/usr/local/sbin/tw_cli'

# get list of controllers
controllers = []
for line in os.popen('%s show' % tw_cli):
    # found a controller on this line
    if len(line) > 1 and line[0] == 'c':
        controllers.append( line.split()[0] )

status = 0

# check each unit on each controller
for cont in controllers:
    for line in os.popen('%s /%s show unitstatus' % (tw_cli, cont)):

        # found a unit on this line
        if len(line) > 1 and line[0] == 'u':
            parts = line.split()
            if parts[2] not in ('OK', 'VERIFYING', 'VERIFY-PAUSED'):
                print >>sys.stderr, ('3ware unit %s on controller %s is '
                                     'NOT okay' % (parts[0], cont))
                print >>sys.stderr, line
                status = 1

# return with correct exit status
sys.exit(status)

                
