#/bin/csh
#NOTE: there is a problem with compatibility with fools and ciao
#to to spectral analysis, heasoft needs to be set last.
# To run this script please type ciao-o to reinstall ciao.
set topdir=`pwd`
cd regfiles
set all = `ls s*.reg | cut -d"." -f1 | cut -c2,3`
cd ../
#echo $all
rm -fr s{$i}/
foreach i ($all)
#echo 'i=' $i
rm -fr s{$i}/
mkdir s{$i}
end
foreach i ($all)
cd s{$i}/
ln -s ../../temp_clean.fits
ln -s ../../pcadf089491523N001_asol1.fits
ln -s ../im_bkg.reg
ln -s ../regfiles/s{$i}.reg
cd ../
end
foreach i ($all)
cd s$i
echo '*******Running psextract for region**********' $i
psextract events="temp_clean.fits[sky=region(s$i.reg)]" bgevents="temp_clean.fits[sky=region(im_bkg.reg)]" bgasol="" root=s{$i} asol=pcadf089491523N001_asol1.fits  gtype=NUM_CTS gspec=1 verbose=2 ptype=pha clobber=no | & tee s{$i}_log.txt
cd ../
end
#echo 'running psextract for region' $i
