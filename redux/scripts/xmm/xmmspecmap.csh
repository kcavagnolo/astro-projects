#!/star/local/bin/tcsh
#
# xmmspecmap version 0 last updated 21 Jan 2003 by John Osmond
#
#################################################################################
#################################################################################

# Enter script parameters:

set version=0
set variables=(pnsrctab m1srctab m2srctab pnbkgtab m1bkgtab m2bkgtab attfile grid enlo enhi ra dec rad nxpix nypix mincount nh red dist bkgtx bkgpl1 bkgpl2 bkgscale)
set input=()
set constants=()
set software=()
set envar=(ZHTOOLS FTOOLS SAS_DIR)
set talk=y

#################################################################################
#################################################################################

# Help.

# ??srctab  source events table for each instrument.
# ??bkgtab   background events table for each instrument or text file containing
#           evselect expression describing background region of local dataset.
# attfile   attitude file.
# grid      ds9 grid file (or n for none).
# enlo      lower energy limit for spectral fitting (keV).
# enhi      upper energy limit for spectral fitting (keV).
# ra        right ascension of target in decimal degrees.
# dec       declination of target in decimal degrees.
# rad       radius of extraction region in degrees.
# nxpix     initial number of pixels in x direction (2).
# nypix     initial number of pixels in y direction (2).
# mincount  minimum number of counts/bin required.
# nh        hydrogen column density (xspec units).
# red       redshift.
# bkgscale  scale background by out of field of view counts (y/n).

# The bkgexpmaps must be in detector co-ordinates and trimmed of any black pixel
# borders.

# Contact jpfo@star.sr.bham.ac.uk for further help.

# End of Help.

#################################################################################
#################################################################################

# Echo help if requested:

if ( $1 == help ) then
set startnotes=`grep -n "# Help" $0 | head -1 | cut -d: -f1 | awk '{print $1}'`
set endnotes=`grep -n "# End of Help" $0 | head -1 | cut -d: -f1 | awk '{print ($1-1)}'`
set notetot=`echo $startnotes $endnotes | awk '{print ($2-$1)}'`
head -$endnotes $0 | tail -$notetot | cut -c 2-
exit
endif

# Check variables:

if ( $talk == y ) echo "your input:" $0:t $*

set vartot=`echo "$variables" | wc -w`

if ( $#argv != $vartot ) then
cat <<EOF
real usage: $0:t $variables
for help:   $0:t help
EOF
exit
endif

# Check input files:

set inptot=`echo "$input" | wc -w`
set inpno=0
while ( $inpno < $inptot )
  @ inpno ++
  if ( ! -e "$input[$inpno]" ) then
    echo "$input[$inpno] not found"
    exit
  endif
end

# Start up:
nice +19
set scriptname=$0:t:r
set header=`head -3 $0 | tail -1 | cut -c 3-`
if ( $talk == y ) echo $header
touch ${scriptname}_temp_run

# Set up variables:

set varno=0
while ( $varno < $vartot )
  @ varno ++
  eval set $variables[$varno]='$'$varno
end

# Set up constants:

set contot=`echo $constants | wc -w`
set conno=0
while ( $conno < $contot )
  @ conno ++
  eval set $constants[$conno]=`grep "$constants[$conno] " ${DATA_DIR}/constants.txt | awk '{print $2}'`
end

# Initialise software:

set softtot=`echo $software | wc -w `
set softno=0
while ( $softno < $softtot )
  @ softno ++
  eval $software[$softno]
end

# Check environmental variables:

set envartot=`echo $envar | wc -w`
set envarmis=0
set envarno=0
while ( $envarno < $envartot )
  @ envarno ++
  eval 'if ( ! $?'$envar[$envarno]' ) set envarmis=1'
  if ( $envarmis == 1 ) then
    echo "variable $envar[$envarno] has not been set"
    exit
  endif
end

# Record starting directory:

set startdir=`pwd`

#################################################################################
#################################################################################

# Format energy values as this is important for xspec:

set enlo=`echo $enlo | awk '{printf ("%3.1f",$1)}'`
set enhi=`echo $enhi | awk '{printf ("%3.1f",$1)}'`
set enlochan=`echo $enlo | awk '{printf ($1*1000)}'`
set enhichan=`echo $enhi | awk '{printf ($1*1000)}'`

# Set spectral bin size according to instrument:

set pnspecbin=5
set pnenhistart=20479
set m1specbin=15
set m2specbin=15
set m1enhistart=11999
set m2enhistart=11999

set group=30

#################################################################################

# SCALING CALCULATION

foreach tab (src bkg)

  eval evselect -w 0 table=\$pn${tab}tab withfilteredset=yes filteredset=${scriptname}_temp_list.fits  keepfilteroutput=yes filtertype=expression expression=\"\#XMMEA_16\" destruct=yes writedss=yes
  eval set ${tab}ofov=`fkeyprint infile="${scriptname}_temp_list.fits[EVENTS]" keynam=NAXIS2 exact=yes | grep = | awk '{print ($3)}'`
  eval echo \$${tab}ofov counts found out of field of view in $tab table

end

set srcpbkg = `echo $srcofov $bkgofov | awk '{print ($1/$2)}'`
echo srcpbkg = $srcpbkg

#################################################################################

# Create xspec script:

cat <<EOF >! ${scriptname}_temp_xspec.tcl

# Read in data:

data ${scriptname}_temp_pn_srcspec_grppha.fits
setplot energy
ignore **-${enlo} ${enhi}-**
cpd ${scriptname}_temp_plot.ps/cps
query yes

# Set up the model:

model wabs*vapec & /*

newpar 1 $nh

newpar 2  1
newpar 3  1
newpar 4  =3
newpar 5  =3
newpar 6  =3
newpar 7  =3
newpar 8  =3
newpar 9  =3
newpar 10 1
newpar 11 1
newpar 12 =3
newpar 13 =3
newpar 14 1
newpar 15 =3
newpar 16 $red
newpar 17 1

freeze 1
thaw 3 4 5 6 7 8 9 10 11 12 13 14 15

# Fit model to data:

ignore bad
fit

# Plot data:

plot ldata residuals

error 2 10 14 17

exit
EOF

#if ( -e log ) rm -r log
mkdir log

#################################################################################

set insall=(pn m1 m2)

# Calculate co-ordinates of boresight:

set refra=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFXCRVL | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set refdec=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFYCRVL | grep "=" | cut -d"=" -f2 | awk '{print $1}'`

set ranom=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=ra_nom | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set decnom=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=dec_nom | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set xnom=`echo $ranom $refra $decnom | awk '{print (25921-(($1-$2)*72000*cos($3*3.14159/180)))}'`
set ynom=`echo $decnom $refdec | awk '{print (25921+(($1-$2)*72000))}'`

set xmid=`echo $ra $refra $dec | awk '{print (25921-(($1-$2)*72000*cos($3*3.14159/180)))}'`
set ymid=`echo $dec $refdec | awk '{print (25921+(($1-$2)*72000))}'`
set wid=`echo $rad | awk '{print ($1*2*72000)}'`


set xlo=`echo $xmid $wid | awk '{print ($1-($2/2))}'`
set xhi=`echo $xmid $wid | awk '{print ($1+($2/2))}'`
set ylo=`echo $ymid $wid | awk '{print ($1-($2/2))}'`
set yhi=`echo $ymid $wid | awk '{print ($1+($2/2))}'`
echo "here1"
set rlolim=`echo $xmid $ymid $wid $xnom $ynom | awk '{xoff=((sqrt(($1-$4)^2))-($3/2)); yoff=((sqrt(($2-$5)^2))-($3/2)); xyoff=(sqrt((xoff^2)+(yoff^2))); if (xoff>0 && yoff>0) print xyoff; else if (xoff>0) print xoff; else if (yoff>0) print yoff; else print 0}'`
echo "here1.5"
echo $xmid $ymid $wid $xnom $ynom
set rhilim=`echo $xmid $ymid $wid $xnom $ynom | awk '{xoff=((sqrt(($1-$4)^2))+($3/2)); yoff=((sqrt(($2-$5)^2))+($3/2)); xyoff=(sqrt((xoff^2)+(yoff^2))); print xyoff}'`

# Calculate image co-dinates:
echo "here2"
set nomimg=`echo $xnom $ynom | awk '{print ($1/20),($2/20)}'`

foreach ins ($insall)

  eval evselect -w 0 -V 0 table=\$${ins}srctab imageset=${scriptname}_temp_srcimg.fits \
    xcolumn=X ycolumn=Y ximagebinsize=20 yimagebinsize=20 updateexposure=true \
    imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=true \
    withimagedatatype=true keepfilteroutput=false

  eval eexpmap -w 0 -V 0 attitudeset=$attfile pimin=$enlochan pimax=$enhichan imageset=${scriptname}_temp_srcimg.fits \
    expimageset=${scriptname}_temp_${ins}_expmap.fits eventset=\$${ins}srctab usedss=no > & /dev/null

  farith ${scriptname}_temp_${ins}_expmap.fits ${scriptname}_temp_${ins}_expmap.fits \
    ${scriptname}_temp_${ins}_mask.fits div blank=0 clobber=yes

  set i=0
  set itot=6
  while ( $i < $itot )
    @ i ++

    set rlo=`echo $rlolim $rhilim $i $itot | awk '{printf "%i",($1+(($2-$1)*(($3-1)/$4)))}'`
    set rhi=`echo $rlolim $rhilim $i $itot | awk '{printf "%i",($1+(($2-$1)*($3/$4)))}'`
    set rimg=`echo $rlo $rhi | awk '{print ($1/20),($2/20)}'`

    echo generating response files from an annulus between $rlo and $rhi pixels of the boresight
    echo "here3"
    # Generate rmf and arf:

    echo generating $ins rmf ...

    eval evselect -w 0 table=\$${ins}srctab withfilteredset=yes \
         keepfilteroutput=yes filteredset=${scriptname}_temp_resptab.fits \
         withspectrumset=yes spectrumset=${scriptname}_temp_respspec.fits \
         energycolumn=PI spectralbinsize=\$${ins}specbin withspecranges=true \
         specchannelmin=0 specchannelmax=\$${ins}enhistart writedss=yes \
         expression=\"'(FLAG==0)&&(annulus(${xnom},${ynom},${rlo},${rhi},X,Y))'\"

    rmfgen spectrumset=${scriptname}_temp_respspec.fits \
           rmfset=${scriptname}_temp_${ins}rmf_${rhi}.fits threshold=1e-5 \
           withenergybins=false

    echo generating $ins arf ...

    evselect -w 0 table=${scriptname}_temp_resptab.fits:EVENTS destruct=false \
                  withimageset=true imageset=${scriptname}_temp_detmap.fits \
                  xcolumn=DETX ycolumn=DETX imagebinning=binSize \
                  ximagebinsize=120 yimagebinsize=120 \
                  writedss=true updateexposure=true

    eval arfgen -w 0 spectrumset=${scriptname}_temp_respspec.fits \
                     arfset=${scriptname}_temp_${ins}arf_${rhi}.fits \
                     detmaptype=dataset detmaparray=${scriptname}_temp_detmap.fits: \
                     extendedsource=yes withrmfset=true \
                     rmfset=${scriptname}_temp_${ins}rmf_${rhi}.fits \
                     withbadpixcorr=true badpixlocation=\$${ins}srctab \
                     withsourcepos=yes sourcecoords=pos sourcex=$xmid sourcey=$ymid

    # Set variable corresponding to mean exposure in this region:

    echo "annulus($nomimg[1],$nomimg[2],$rimg[1],$rimg[2])" >! ${scriptname}_temp_region.txt
    eval set ${ins}_${rhi}_expmean=`imexam ${scriptname}_temp_${ins}_expmap.fits mean reg=${scriptname}_temp_region.txt`

  end

   #cat "box($srcimg[1],$srcimg[2],$srcimg[3],$srcimg[3])" >! ${scriptname}_temp_region.txt
   eval set ${ins}_bkg_expmean=\$${ins}_${rhi}_expmean

  # Extract local bacground if required:

  eval set ${ins}bkgform=`eval file \$${ins}bkgtab | cut -d" " -f2`

  eval set bkgform=\$${ins}bkgform
  eval set srctab=\$${ins}srctab

  if ( $bkgform == ASCII ) then

    set exp=`eval head -1 \$${ins}bkgtab`
    eval evselect -w 0 table="${srctab}:EVENTS" withspectrumset=yes \
      spectrumset=${scriptname}_temp_${ins}_bkgspec.fits energycolumn=PI \
      spectralbinsize=\$${ins}specbin withspecranges=true specchannelmin=0 \
      specchannelmax=\$${ins}enhistart writedss=yes expression=\"'$exp'\"

    backscale -w 0 spectrumset=${scriptname}_temp_${ins}_bkgspec.fits \
      withbadpixcorr=yes badpixlocation=$srctab

    eval set ${ins}bkgbackscal=`fkeyprint infile=${scriptname}_temp_${ins}_bkgspec.fits+1 keynam=backscal | grep "=" | cut -d"=" -f2 | awk '{print $1}'`

  else

    eval set ${ins}bkgbackscal=1

  endif

end

#################################################################################

# Set loop variables and begin adaptive fitting:

set nxpixlast=1
set nypixlast=1
set coarseX=$nxpix
set coarseY=$nypix
set fineX=0
set fineY=0
set layers=0
set binsfitted=1

while ( $binsfitted != 0 )

  echo forming $nxpix x $nypix layer

  # Set variables for this layer:

  set xbin=`echo $wid $nxpix | awk 'OFMT = "%.10g" {print $1/$2}'`
  set ybin=`echo $wid $nypix | awk 'OFMT = "%.10g" {print $1/$2}'`

  echo spatial bins are $xbin x $ybin

  foreach par (temp fe si chisq count ent den)

    rm -f ${par}_${nxpix}_${nypix}.txt ${par}_${nxpix}_${nypix}_err.txt

  end

  # Loop over pixels in x direction:

  set binsfitted=0

  set i=0
  while ($i < $nxpix)
  @ i ++

    # Loop over pixels in y direction:

    set j=0
    while ($j < $nypix)
    @ j ++

      set flag = 1
      set x = `echo $i $xbin $xlo | awk '{print ($1*$2-0.5*$2)+$3}'`
      set y = `echo $j $ybin $ylo | awk '{print ($1*$2-0.5*$2)+$3}'`
      echo x = $x , y = $y , i = $i , j = $j

      # Check whether the bin at this location was fitted in the previous step if not then don't bother fitting this time
      # Must be integer arithmetic otherwise the next @ command may fail

      set previousFile = temp_${nxpixlast}_${nypixlast}.txt

      # This needs to be integer arithmetic i.e. 3/2 = 1

      @ line = ( ( $j + 1 ) / 2 ) + ( ( $i - 1 ) / 2 ) * $nypixlast

      if ( -e $previousFile ) then

        # This will be -1 if there were insufficient counts

        set lasttime = `head -n$line $previousFile | tail -1 | awk '{print $3}'`

      else 

        set lasttime = 1

      endif

      if ( $lasttime == -1 ) then

        set flag=0
        echo aborting fitting as bin not fitted last time

      else

        # Select source spectrum:

        echo extracting source spectra

        set count=0
        foreach ins ($insall)

          eval set srctab=\$${ins}srctab
          eval set bkgtab=\$${ins}bkgtab
          eval set bkgbackscal=\$${ins}bkgbackscal
          eval set bkgform=\$${ins}bkgform

          set celloff=`echo $x $y $xnom $ynom | awk '{print (sqrt((($1-$3)^2)+(($2-$4)^2)))}'`
          set resprad=`ls ${scriptname}_temp_${ins}rmf_*.fits | sed s/${scriptname}//g | cut -d_ -f4 | cut -d. -f1 | sort -n | awk '{if ($1>'${celloff}') print $1 }' | head -1`
          set inrad=`ls ${scriptname}_temp_${ins}rmf_*.fits | sed s/${scriptname}//g | cut -d_ -f4 | cut -d. -f1 | sort -n | awk '{if ($1<'${celloff}') print $1 }' | tail -1`
          if ( $inrad == "" ) set inrad=0
           
          echo response radii: $inrad - $resprad

          # Scale normalisation to account for different areas between source and rmf/arf:

          farith ${scriptname}_temp_${ins}_expmap.fits ${scriptname}_temp_${ins}_expmap.fits ${scriptname}_temp_mask.fits div blank=0 clobber=yes
          echo $x $y $xbin $ybin | awk '{print "box("$1/20","$2/20","$3/20","$4/20")"}' >! ${scriptname}_temp.reg
          set srcbackscal=`imexam ${scriptname}_temp_mask.fits sum reg=${scriptname}_temp.reg | awk '{print ($1*(20^2))}'`

          echo $xnom $ynom $inrad $resprad | awk '{print "annulus("$1/20","$2/20","$3/20","$4/20")"}' >! ${scriptname}_temp.reg
          set normscl=`imexam ${scriptname}_temp_mask.fits sum reg=${scriptname}_temp.reg | awk '{print ($1*(20^2))/$srcbackscal}'`

          set srcrange=`eval echo \$${ins}specbin $enlo $enhi | awk '{printf "%i-%i",(($2*1000)/$1),(($3*1000)/$1)}'`
          set bkgrange=`eval echo \$${ins}specbin | awk '{printf "%i-%i",(5000/$1),(10000/$1)}'`

          eval evselect -w 0 table=${srctab} withspectrumset=yes \
            spectrumset=${scriptname}_temp_${ins}_srcspec.fits energycolumn=PI \
            spectralbinsize=\$${ins}specbin withspecranges=true specchannelmin=0 \
            specchannelmax=\$${ins}enhistart writedss=yes \
            expression="'(FLAG==0)&&(box(${x},${y},${xbin},${ybin},0,X,Y))'"
          set srcexp=`fkeyprint infile="${scriptname}_temp_${ins}_srcspec.fits[SPECTRUM]" keynam=exposure | grep = | cut -d= -f2 | awk '{print $1}'`
          set count=`fstatistic infile=${scriptname}_temp_${ins}_srcspec.fits colname=COUNTS rows=$srcrange | grep "The sum" | awk '{printf ("%i",('$count'+$8))}'`

          if ( $srcexp == "" ) set srcexp=0

          if ( $bkgform == FITS ) then

            eval evselect -w 0 table="${bkgtab}:EVENTS" withspectrumset=yes \
              spectrumset=${scriptname}_temp_${ins}_bkgspec.fits energycolumn=PI \
              spectralbinsize=\$${ins}specbin withspecranges=true specchannelmin=0 \
              specchannelmax=\$${ins}enhistart writedss=yes \
              expression="'(FLAG==0)&&(box(${x},${y},${xbin},${ybin},0,X,Y))'"

            if ( $bkgscale == y ) then
              set bkgexp=`echo $srcexp $srcpbkg | awk '{print $1/$2}'`
              fparkey value=$bkgexp fitsfile=${scriptname}_temp_${ins}_bkgspec.fits keyword=exposure add=yes
            else
              set bkgexp=`fkeyprint infile="${scriptname}_temp_${ins}_bkgspec.fits[SPECTRUM]" keynam=exposure | grep = | cut -d= -f2 | awk '{print $1}'`
            endif

            set backscl=1
            set expscl=`echo $srcexp $bkgexp | awk '{if ($2>0) print ($1/$2); else print 1}'`
            set vigscl=1

          else

            fparkey value=$srcbackscal fitsfile=${scriptname}_temp_${ins}_srcspec.fits keyword=backscal add=yes
            set backscl=`echo $srcbackscal $bkgbackscal | awk '{print ($1/$2)}'`
            set expscl=1
            set vigscl=`eval echo \$${ins}_${resprad}_expmean \$${ins}_bkg_expmean | awk '{print ($1/$2)}'`

          endif

          echo "backscl: $backscl ; expscl: $expscl ; vigscl: $vigscl ; $count counts before bgsub"

          set count=`fstatistic infile=${scriptname}_temp_${ins}_bkgspec.fits colname=COUNTS rows=$srcrange | grep "The sum" | awk '{printf ("%i",('$count'-($8*'$backscl'*'$expscl'*'$vigscl')))}'`

          set counterr=0

        end

        echo "$count counts found in bin"

        if ( $count < $mincount ) then

          set flag=0
          echo aborting fitting as less than $mincount counts in bin

        endif

      endif

      if ( $flag == 1 ) then 

        @ binsfitted ++

        foreach ins ($insall)

          # Write backscal keyword to source spectrum:

          #eval backscale -w 0 spectrumset=${scriptname}_temp_${ins}_srcspec.fits withbadpixcorr=yes badpixlocation=\$${ins}srctab
          #set backscal=`echo $xbin $ybin | awk '{print ($1*$2)}'`
          #fparkey value=$backscal fitsfile=${scriptname}_temp_${ins}_srcspec.fits keyword=backscal add=yes
          #fparkey value=$backscal fitsfile=${scriptname}_temp_${ins}_bkgspec.fits keyword=backscal add=yes

          set rmf=${scriptname}_temp_${ins}rmf_${resprad}.fits
          set arf=${scriptname}_temp_${ins}arf_${resprad}.fits

          punlearn grppha
          grppha ${scriptname}_temp_${ins}_srcspec.fits ${scriptname}_temp_${ins}_srcspec_grppha.fits comm="chkey backfile ${scriptname}_temp_${ins}_bkgspec.fits & chkey respfile $rmf & chkey ancrfile $arf & group min $group & exit" clobber=yes >! ${scriptname}_temp_grppha.log

        end

        # Fit the spectrum in xspec

        #alias xspec '/soft/HEAsoft/5.3.1/lheasoft/linux/bin/xspec'

        echo fitting spectrum using xspec
        xspec ${scriptname}_temp_xspec.tcl >! ${scriptname}_temp_xspec.log

        # Copy fit results to seperate directory:

        cp ${scriptname}_temp_xspec.log log/xspec_${nxpix}_${nypix}_${i}_${j}.log
        cp ${scriptname}_temp_plot.ps log/plot_${nxpix}_${nypix}_${i}_${j}.ps

        set temp=`tail +100 ${scriptname}_temp_xspec.log | grep kT | head -1 | awk '{print $7}'`
        set fe=`tail +100 ${scriptname}_temp_xspec.log | grep Fe | head -1 | awk '{print $6}'`
        set si=`tail +100 ${scriptname}_temp_xspec.log | grep Si | head -1 | awk '{print $6}'`
        set su=`tail +100 ${scriptname}_temp_xspec.log | grep " S " | head -1 | awk '{print $6}'`

        set chisq=`grep 'Reduced chi-squared' ${scriptname}_temp_xspec.log | awk '{printf "%f\n",$4}' | sort -n | head -1`
        set chisqerr=0
        if ( $chisq == "" ) set chisq=-1

        # Calculate errors and entropy:

        set temperr=`grep ' ( ' ${scriptname}_temp_xspec.log | grep , | grep ' 2 ' | tail -1 | awk '{printf (($3-$2)/2)}'`
        set sierr=`grep ' ( ' ${scriptname}_temp_xspec.log | grep , | grep ' 10 ' | tail -1 | awk '{print (($3-$2)/2)}'`
        set feerr=`grep ' ( ' ${scriptname}_temp_xspec.log | grep , | grep ' 14 ' | tail -1 | awk '{print (($3-$2)/2)}'`
        set suerr=`grep ' ( ' ${scriptname}_temp_xspec.log | grep , | grep ' 11 ' | tail -1 | awk '{print (($3-$2)/2)}'`
        set normerr=`grep ' ( ' ${scriptname}_temp_xspec.log | grep , | grep ' 17 ' | tail -1 | awk '{print (($3-$2)/2)}'`

        if ( $temperr == "" ) then
          set temperr=`tail +100 ${scriptname}_temp_xspec.log | grep kT | head -1 | awk '{print $9}'`
          set feerr=`tail +100 ${scriptname}_temp_xspec.log | grep Fe | head -1 | awk '{print $8}'`
          set sierr=`tail +100 ${scriptname}_temp_xspec.log | grep Si | head -1 | awk '{print $8}'`
          set suerr=`tail +100 ${scriptname}_temp_xspec.log | grep " S " | head -1 | awk '{print $8}'`
          set normerr=`tail +100 ${scriptname}_temp_xspec.log | grep norm | head -1 | awk '{print $8}'`
        endif

        set vol=`echo $srcbackscal $x $y $xmid $ymid $xbin $ybin $dist | awk '{a=$1; \\
          rmax=sqrt(((sqrt(($2-$4)^2)+($6/2))^2)+((sqrt(($3-$5)^2)+($7/2))^2)); \\
          rmin=sqrt(((sqrt(($2-$4)^2)-($6/2))^2)+((sqrt(($3-$5)^2)-($7/2))^2)); \\
          vol=a*(4/3)*sqrt((rmax^2)-(rmin^2)); \\
          scl=($8*3.09e24*3.14159/(72000*180));volscl=vol*(scl^3); \\
          print volscl}'`

        set norm=`tail +100 ${scriptname}_temp_xspec.log | grep norm | head -1 | awk '{print $6}'`

        if ( $vol != 0 ) then
          set den=`echo $norm $dist $vol | awk '{print 2.08*sqrt($1*4*3.14159*(($2*3.09e24)^2)*(10^14)/$3)}'`
          set denerr=`echo $den $normerr $norm | awk '{print ($1*sqrt((0.5*($2/$3))^2))}'`
          set ent=`echo $temp $den | awk '{if ($2>0) print ($1/(($2*(1.08/(2.08)))^(2/3))); else print 0}'`
          set enterr=`echo $ent $temperr $temp $denerr $den | awk '{print ($1*sqrt((($2/$3)^2)+(((2/3)*($4/$5))^2)))}'`
       else
          set den=0
          set denerr=0
          set ent=0
          set enterr=0
        endif

      else

        foreach par (temp chisq fe si su count ent den)
          eval set $par=-1
          eval set ${par}err=-1
        end

      endif

        foreach par (temp chisq fe si su count ent den)

          eval echo $x \'  \' $y \'  \' \$$par \>\>\! ${par}_${nxpix}_${nypix}.txt
          eval echo $x \'  \' $y \'  \' \$${par}err \>\>\! ${par}_${nxpix}_${nypix}_err.txt

        end

    # End of x loop: 

    end

  # End of y loop:

  end

  echo $binsfitted bins fitted

  if ( $binsfitted != 0 ) @ layers++

  set nxpixlast=$nxpix
  set nypixlast=$nypix

  @ nxpix = ${nxpix} * 2
  @ nypix = ${nypix} * 2

  if ( $nxpix == 128 ) then

    set binsfitted=0

    set nxpixlast=$nxpix
    set nypixlast=$nypix

    @ nxpix = ${nxpix} * 2
    @ nypix = ${nypix} * 2

  endif

end

echo fitted $layers layers

if ( $layers < 2 ) exit

#################################################################################

# Return binsizes to the last value which gave some fitted bins:

@ nxpix = ${nxpix} / 4
@ nypix = ${nypix} / 4

# Overlay the different images:

@ fineX = ${coarseX} * 2
@ fineY = ${coarseY} * 2

foreach par (temp chisq fe si su count ent den)
  cp ${par}_${coarseX}_${coarseY}.txt ${par}_adapt.txt
  cp ${par}_${coarseX}_${coarseY}_err.txt ${par}_adapt_err.txt
end

while ( $fineX <= $nxpix )

  # Replace poor chi-sq fits with previous value:

  #set x=`awk '{print $1}' $file2`
  #set y=`awk '{print $2}' $file2`
  #set temp=`awk '{print $3}' $file2`
  #set chisq=`awk '{print $3}' $file4`
  #set met=`awk '{print $3}' $file6`

  #rm $file2 $file6
  #touch $file2 $file6

  #set bintot=`echo $fineX | awk '{print ($1^2)}'`
  #set binno=0
  #while ( $binno < $bintot )
  #  @ binno ++

  #  echo $x[$binno] $y[$binno] $temp[$binno] $chisq[$binno] | awk '{if ($4<=2) temp=$3; else temp=-1; print $1"    "$2"    "temp}' >> $file2
  #  echo $x[$binno] $y[$binno] $met[$binno]  $chisq[$binno] | awk '{if ($4<=2) met=$3; else met=-1; print $1"    "$2"    "met}' >> $file6

  #end

  foreach par (temp chisq fe si su count ent den)

    echo overlaying images ${par}_${coarseX}_${coarseY}.txt ${par}_${fineX}_${fineY}.txt
    combine.exe $coarseX $fineX $coarseY $fineY ${par}_adapt.txt ${par}_${fineX}_${fineY}.txt
    mv combinedImage.txt ${par}_adapt.txt

    combine.exe $coarseX $fineX $coarseY $fineY ${par}_adapt_err.txt ${par}_${fineX}_${fineY}_err.txt
    mv combinedImage.txt ${par}_adapt_err.txt

  end

  @ fineX = ${fineX} * 2
  @ fineY = ${fineY} * 2
  @ coarseX = ${coarseX} * 2
  @ coarseY = ${coarseY} * 2

end

#################################################################################

# Calculate keywords for fits conversion:

set xref=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFXCRPX | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set yref=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFYCRPX | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set raref=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFXCRVL | grep "=" | cut -d"=" -f2 | awk '{print $1}'`
set decref=`fkeyprint infile="${pnsrctab}[EVENTS]" keynam=REFYCRVL | grep "=" | cut -d"=" -f2 | awk '{print $1}'`

set cdelt1=`echo $xhi $xlo $nxpix | awk '{CONVFMT="%.16g";OFMT="%.16g";print ((($2-$1)/$3)/72000)}'`
set cdelt2=`echo $yhi $ylo $nypix | awk '{CONVFMT="%.16g";OFMT="%.16g";print ((($1-$2)/$3)/72000)}'`

set crpix1=`echo $raref $xlo $xref $cdelt1 $decref | awk '{CONVFMT="%.16g";OFMT="%.16g";print ($1+((($3-($2))/72000)/cos($5*(3.14159/180)))+($4/2))}'`
set crpix2=`echo $decref $ylo $yref $cdelt2 | awk '{CONVFMT="%.16g";OFMT="%.16g";print ($1+(($2-$3)/72000)+($4/2))}'`

set keyname=(EQUINOX CTYPE1 CRPIX1 CRVAL1 CUNIT1 CDELT1 CTYPE2 CRPIX2 CRVAL2 CUNIT2 CDELT2)
set keyval=(2000 RA---TAN 1 $crpix1 deg $cdelt1 DEC--TAN 1 $crpix2 deg $cdelt2)

foreach par (temp chisq fe si su count ent den)

  flst2im infile=${par}_adapt.txt outfile=${par}_adapt.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=$nxpix nybin=$nypix clobber=yes

  set keytot=`echo $keyname | wc -w`
  set keyno=0
  while ( $keyno < $keytot )
    @ keyno ++
    fparkey value=$keyval[$keyno] fitsfile="${par}_adapt.fits[Primary]" keyword=$keyname[$keyno] add=yes
  end

end

# Create quality map and clean spectral maps:

#mkcond ${scriptname}_temp_cleanmask.fits temp_adapt.fits "<" 2
#farith infil1=met_adapt.fits infil2=${scriptname}_temp_cleanmask.fits outfil=met_adapt.fits ops=MUL blank=0 clobber=yes
#farith infil1=temp_adapt.fits infil2=${scriptname}_temp_cleanmask.fits outfil=temp_adapt.fits ops=MUL blank=0 clobber=yes

################################################################################
################################################################################

if ( $grid != n ) then
  set gridopt="-grid -grid load $grid -grid skyformat degrees"
else
  set gridopt="-grid -grid load /base/jpfo/data/ds9_border.grd"
endif

foreach img (temp_adapt.fits fe_adapt.fits)
 
#ds9 -tile $img \
#    -view magnifier no \
#    -cmap sls \
#    -scale linear \
#    -scale mode 99.5 \
#     $gridopt \
#    -zoom to fit \
#    -print destination file \
#    -print filename ${img:r}.ps \
#    -print level 2 \
#    -print interpolate yes \
#    -print resolution 72

end

#################################################################################
#################################################################################

# Return to starting directory:

cd $startdir

# Output results to screen:

if ( -e ${scriptname}.out ) cat ${scriptname}.out

# Update header:

set headername=`echo $header | awk '{print $1}'`
set headerversion=`echo $header | awk '{print $3}'`
set headerdate=`echo $header | awk '{print $6,$7}'`
set update=`ls -l $0 | awk '{print $7,$6}'`

if ( "$headername" != "$scriptname" || $headerversion != $version || "$headerdate" != "$update" ) then

set year=`date +%Y`
set login=`ls -l $0 | awk '{print $3}'`
set author=`finger $login | head -1 | awk '{print $4,$5}'`

head -2 $0 >! ${scriptname}_temp_script.csh
echo "# $0:t:r version $version last updated $update $year by $author" >> ${scriptname}_temp_script.csh
tail +4 $0 >> ${scriptname}_temp_script.csh
cp ${scriptname}_temp_script.csh $0

endif

# Complete and exit:

rm ${scriptname}_temp*
if ( $talk == y ) echo $scriptname "complete"
exit
