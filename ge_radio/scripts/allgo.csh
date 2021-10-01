# clean up old data
rm -rf *.log AP* /mnt/SINISTER/DATA/APOCALYPSE_1/*EY*

# get original data
cp orig/AP* .

# open aips and prep data (zap unwanted bands, remove bands without std cals)
source /usr/local/aips/LOGIN.CSH
aips pr=1 notv
19373
restore 0
default clrmsg;prnumber=-1;clrmsg
version'RUNFIL'
run avlaprep
compress
type'empty catalog ?' checkcat
default prtmsg;outprint'PWD:'!!'AP375_1.fill.log
docrt=-2;prnumber=-1;tput prtmsg;docrt 1
pre_fillm
datain'PWD:AP375_'
gx
indisk pdsk
for j=1:checkcat;error 0;egetname j;datachks;end
for j=1:checkcat;error 0;egetname j;freqchks;end
type'empty catalog ?' checkcat
tget prtmsg;prtmsg;docrt 1
kleenex

# open aips and run pipeline
aips pr=1 notv
19373
restore 0
default clrmsg;prnumber=-1;clrmsg
version'RUNFIL'
run avlaprep
run avlapipe
run nvasdefs
compress
default prtmsg;outprint'PWD:'!!'AP375_1.log
docrt=-3;prnumber=-1;tput prtmsg;docrt 1
procedure expid
  string*20 expnm
  string*75 expmsg
  expnm='AP375_'
  expmsg='PWD:'!!'AP375_1.log
  return expnm
finish
type expid
docrt=1;type expid
run avlapost
avlapipe
type'empty catalog ?' checkcat
flatfov

# check what's in catalog
pcat

# now look for flatn cats with src names
egetname 302
inp fitab
dataout='AIMG:'!!inname
go fitab
kleenex
