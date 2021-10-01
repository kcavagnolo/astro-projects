#!/bin/bash
#
# usage> ./changeroot.sh :ext:cavagnolo@apocalypse.uwaterloo.ca:/usr/local/CVS
#
# Test whether we got an argument for the new CVS Root value - exit if not
# -z <somestring> is a bash conditional experssion that returns true 
# if the <somestring> has a zero length
if [ -z "$1" ]; then 
  echo "

You must supply a new value for CVSRoot
usage: $0 <newcvsrootvalue>

Examples: 
To change to an external repository accessed via ssh:
./changerepository.sh :ext:crb@webdevcvs:/Users/Shared/cvsrep

To change to a local repository:
./changerepository.sh /Users/Shared/cvsrep
"
  exit
fi
# if we were passed a new CVS Root value on the command line, assign it to
# the newRoot variable
newRoot=$1
# walk the current directory (find . ) and if the path matches */CVS/Root, 
# slap the new CVS Root into the files found
find . -path "*/CVS/Root" | while read f; do
  echo $newRoot > $f
done
