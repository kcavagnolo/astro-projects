#!/bin/tcsh
#image generation script + rmf
#
#this script will generate datacubes using a 64 by 64 binning
#and generate a single rmf and arf. 8 different energy bands will be used. 
#will also create exposure maps for each.
#

set version=1.0 #RFT 15/05/06

nice +19

set tmp_sys=`uname -a`
if ( `echo $tmp_sys|grep -c Generic` == "1" ) then
 set tmp_machine=solaris
 alias echo '/bin/echo'
endif
if ( "$tmp_sys" =~ Linux* ) then
 set tmp_machine=linux
endif

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ( $#argv != 9 ) then
cat <<EOF

Use imgen.csh evt1 evt2 evt3 bgevt1 bgevt2 bgevt3 atthk region root
   evt1    -   source events file 1
   evt2    -   source events file 2
   evt3    -   source events file 3
   bgevt1  -   background events file 1
   bgevt2  -   background events file 2
   bgevt3  -   background events file 3
   atthk   -   attitude housekeeping file
   region  -   region file to extract and rmf and arf
   root    -   root name for output files

EOF
exit
endif

set evt1=$1
set evt2=$2
set evt3=$3
set bgevt1=$4
set bgevt2=$5
set bgevt3=$6
set atthk=$7
set srcreg=$8
set root=$9



#check input files exist

foreach input ( $evt1 $evt2 $evt3 $bgevt1 $bgevt2 $bgevt3 $atthk $srcreg)
if ( ! -e $input ) then
 echo "Error: $input does not exist. Exiting...\n"
 exit
endif
end

# set energy slices:
set nbins=22
set emax=8000
set elow=300
set binsize=`echo $emax $elow $nbins | awk '{print ($1-$2)/$3}'`

set elo1=$elow
set ehi1=`echo $elo1 $binsize | awk '{print $1+$2 }'`
set elo2=$ehi1
set ehi2=`echo $elo2 $binsize | awk '{print $1+$2 }'`
set elo3=$ehi2
set ehi3=`echo $elo3 $binsize | awk '{print $1+$2 }'`
set elo4=$ehi3
set ehi4=`echo $elo4 $binsize | awk '{print $1+$2 }'`
set elo5=$ehi4
set ehi5=`echo $elo5 $binsize | awk '{print $1+$2 }'`
set elo6=$ehi5
set ehi6=`echo $elo6 $binsize | awk '{print $1+$2 }'`
set elo7=$ehi6
set ehi7=`echo $elo7 $binsize | awk '{print $1+$2 }'`
set elo8=$ehi7
set ehi8=`echo $elo8 $binsize | awk '{print $1+$2 }'`
set elo9=$ehi8
set ehi9=`echo $elo9 $binsize | awk '{print $1+$2 }'`
set elo10=$ehi9
set ehi10=`echo $elo10 $binsize | awk '{print $1+$2 }'`
set elo11=$ehi10
set ehi11=`echo $elo11 $binsize | awk '{print $1+$2 }'`
set elo12=$ehi11
set ehi12=`echo $elo12 $binsize | awk '{print $1+$2 }'`
set elo13=$ehi12
set ehi13=`echo $elo13 $binsize | awk '{print $1+$2 }'`
set elo14=$ehi13
set ehi14=`echo $elo14 $binsize | awk '{print $1+$2 }'`
set elo15=$ehi14
set ehi15=`echo $elo15 $binsize | awk '{print $1+$2 }'`
set elo16=$ehi15
set ehi16=`echo $elo16 $binsize | awk '{print $1+$2 }'`
set elo17=$ehi16
set ehi17=`echo $elo17 $binsize | awk '{print $1+$2 }'`
set elo18=$ehi17
set ehi18=`echo $elo18 $binsize | awk '{print $1+$2 }'`
set elo19=$ehi18
set ehi19=`echo $elo19 $binsize | awk '{print $1+$2 }'`
set elo20=$ehi19
set ehi20=`echo $elo20 $binsize | awk '{print $1+$2 }'`
set elo21=$ehi20
set ehi21=`echo $elo21 $binsize | awk '{print $1+$2 }'`
set elo22=$ehi21
set ehi22=`echo $elo22 $binsize | awk '{print $1+$2 }'`

set emax=7999
#Create images for source and background

set parimages='xcolumn=X ycolumn=Y ximagebinsize=200 yimagebinsize=200 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=true withimagedatatype=true keepfilteroutput=false'

#goto endimggen
#goto bggen

#goto newarfgen


foreach input ( $evt1 $evt2 $evt3 ) 
 #determine instrument
 fdump $input tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
 set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
 set instr=`echo $instr | tr -d "'"`
 #echo 'Instrument is '$instr "\n"
 if ($instr == EPN ) then
   set spchmax = $emax
   set spbinsize = $binsize
   set pnline = 190
   set flag="#XMMEA_EP" 
   set patexp="PATTERN<=4"
   set inst=pn
 else
   set spchmax = $emax
   set spbinsize  = $binsize
   set pnline = 0
   set flag="#XMMEA_EM"
   set patexp="PATTERN<=12"
   set string=`echo $instr | cut -c5`    
    if ( $string == 1 ) then
     set inst=mos1   
    else
     set inst=mos2
    endif
 endif
  set expr="$flag&&$patexp"

  set parspec="withspectrumset=Y spectralbinsize=$spbinsize energycolumn=PI specchannelmax=$spchmax specchannelmin=$elow withspecranges=Y"

 rm tmpdmp1
  evselect table=$input filteredset=tempev expression="$expr" \
           updateexposure=N writedss=Y withfilteredset=T \
           destruct=Y keepfilteroutput=T
 echo "Creating images and expmaps for $instr in the following energy bands:"
 set i=0
 while ( $i < $nbins ) 
  @ i++
  eval set enlo=`echo \$elo$i`
  eval set enhi=`echo \$ehi$i`
  set enlo=`echo $enlo | awk '{print $1*1}'`
  set enhi=`echo $enhi | awk '{print $1*1}'`
  echo $enlo $enhi eV
  evselect table=tempev imageset=${inst}_img${i}.fits \
           expression='PI>='$enlo'&&PI<='$enhi'' $parimages 
  eexpmap attitudeset=$atthk pimin=$enlo pimax=$enhi \
          imageset=${inst}_img${i}.fits expimageset=${inst}_img${i}_exp.fits \
          eventset=$input usedss=yes withvignetting=yes
 end
goto ignore
 echo "Creating rmf for $instr\n"
 #extract a spectrum in the field 
# set srcreg=ds9.reg
 set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $srcreg | grep expression | awk '{print $4}'`
 evselect table=tempev spectrumset=tempsp $parspec expression=$sourcefilter
 rmfgen spectrumset=tempsp rmfset=${inst}.rmf 

 echo "Creating arf for $instr\n"
 evselect table=tempev:EVENTS destruct=false \
          withimageset=true imageset=${inst}_detmap.fits \
          xcolumn=DETX ycolumn=DETX imagebinning=binSize \
          ximagebinsize=810 yimagebinsize=810 \
          writedss=true updateexposure=true

 arfgen spectrumset=tempsp arfset=${inst}.arf \
        detmaptype=dataset detmaparray=${inst}_detmap.fits: \
        extendedsource=yes withrmfset=true rmfset=${inst}.rmf\
        withbadpixcorr=true badpixlocation=$input setbackscale=Y
ignore:

end 

bggen:

foreach bginput ( $bgevt1 $bgevt2 $bgevt3 ) 
 #determine instrument
 fdump $bginput tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
 set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
 set instr=`echo $instr | tr -d "'"`
 #echo 'Instrument is '$instr "\n"
 if ($instr == EPN ) then
   set spchmax = 20475
   set spbinsize = 5
   set pnline = 190
   set flag="#XMMEA_EP" 
   set patexp="PATTERN<=4"
   set inst=pn
 else
   set spchmax = 11999
   set spbinsize  = 15
   set pnline = 0
   set flag="#XMMEA_EM"
   set patexp="PATTERN<=12"
   set string=`echo $instr | cut -c5`    
    if ( $string == 1 ) then
     set inst=mos1   
    else
     set inst=mos2
    endif
 endif
  set expr="$flag&&$patexp"
 rm tmpdmp1
  evselect table=$bginput filteredset=tempev expression="$expr" \
           updateexposure=N writedss=Y withfilteredset=T \
           destruct=Y keepfilteroutput=T
 echo "Creating bg images for $instr in the following energy bands:"
 set i=0
 while ( $i < $nbins ) 
  @ i++
  eval set enlo=`echo \$elo$i`
  eval set enhi=`echo \$ehi$i`
  set enlo=`echo $enlo | awk '{print $1*1}'`
  set enhi=`echo $enhi | awk '{print $1*1}'`
  echo $enlo $enhi eV
  evselect table=tempev imageset=${inst}_bgimg${i}.fits \
           expression='PI>='$enlo'&&PI<='$enhi'' $parimages
 end

end 
endimggen:
newarfgen:

goto endtemp

#what we need is the following:
#1. and on axis arf
#2. this needs to be at detx, dety=0
#3. This needs to be a very small size i.e. 1pixel by 1pixel to remain on axis
#4. the arf needs to match the spectral bins used previously.
#5. will probably need to generate a corresponding on-axis rmf too. (maybe)
foreach input ( $evt2 )
#foreach input ( $evt1 $evt2 $evt3 ) 
 #determine instrument
 fdump $input tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
 set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
 set instr=`echo $instr | tr -d "'"`
 echo 'Instrument is '$instr "\n"
 if ($instr == EPN ) then
   set spchmax = $emax
   set spbinsize = $binsize
   set pnline = 190
   set flag="#XMMEA_EP" 
   set patexp="PATTERN<=4"
   set inst=pn
 else
   set spchmax = $emax
   set spbinsize  = $binsize
   set pnline = 0
   set flag="#XMMEA_EM"
   set patexp="PATTERN<=12"
   set string=`echo $instr | cut -c5`    
    if ( $string == 1 ) then
     set inst=mos1   
    else
     set inst=mos2
    endif
 endif
  set expr="$flag&&$patexp"
  set parspec="withspectrumset=Y spectralbinsize=$spbinsize energycolumn=PI specchannelmax=$spchmax specchannelmin=$elow withspecranges=Y"
  rm tmpdmp1

set enlo=50
set enhi=12000
 echo "Creating rmf for $instr\n"
 #extract a spectrum in the field 
 set srcreg=ds9_${inst}.reg
 set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $srcreg | grep expression | awk '{print $4}'`

  evselect table=$input filteredset=tempev expression="$expr" \
           updateexposure=N writedss=Y withfilteredset=T \
           destruct=Y keepfilteroutput=T
 evselect table=tempev spectrumset=tempsp $parspec expression=$sourcefilter 
  if ( $instr != EPN ) then
    rmfgen spectrumset=tempsp rmfset=${inst}_oa.rmf \
	withenergybins=T energymin=0.05 energymax=12 nenergybins=2400
   else
    rmfgen spectrumset=tempsp rmfset=${inst}_oa.rmf
  endif
 echo "Creating arf for $instr\n"
 evselect table=tempev:EVENTS destruct=false \
          withimageset=true imageset=${inst}_detmap.fits \
          xcolumn=DETX ycolumn=DETX imagebinning=binSize \
          ximagebinsize=810 yimagebinsize=810 \
          writedss=true updateexposure=true \


 arfgen spectrumset=tempsp arfset=${inst}_oa.arf \
        detmaptype=dataset detmaparray=${inst}_detmap.fits: \
        extendedsource=yes withrmfset=true rmfset=${inst}.rmf\
        withbadpixcorr=true badpixlocation=$input setbackscale=Y




end #end of this whole loop

endtemp:
rm tempev
#rm tempsp

exit








