pro extract_annuli,xc,yc,mincnts,chipid=chipid,ellip=ellip,$
                   pa=pa,n_to_fit=n_to_fit,cencnts=cencnts,$
                   mkacisrmf=mkacisrmf,spectra_dir=spectra_dir,$
                   re_extract=re_extract,match_dir=match_dir,$
                   specext=specext
;-----------------------------------------------------------------------
;
; Name: EXTRACT_ANNULI
;
; Purpose: Extracts the spectra in circular annuli for spectral fitting  
;          and makes appropriate rmf and arf files
;          
;          
; Inputs:  X and Y center of the cluster (e.g. radio core) in physical
;	   coordinates and minimum number of counts per annular region
;          chipid - the id of the chip of interest, default is 7 (ACIS-S3)
;	   ellip - the ellipticity of the regions to be extracted, 
;		   defined as e=1-b/a, default is 0
;	   pa - position angle of the ellipses in degrees, measured
;		from the positive x-axis, default is 0
;          cencnts - minimum number of counts for central region (optional)
;	   /MKACISRMF - use mkacisrmf to make the weighted rmfs (requires
;                       CIAO 3.2 and above)
;	   spectra_dir - name of the directory to use for spectra
;	   /RE_EXTRACT - re-extracts spectra with different parameters
;			 (i.e. overwrite any old files in spectra_dir)
;	   match_dir - extracts spectra to match a previous extraction or
;		       another observation in the match_dir directory: 
;		       xc and yc are required, but the other inputs are ignored.
;	   /SPECEXT - use specextract instead of acisspec for -110 C obs
;
;         
; Comments: Annulus extraction algorithm taken from Mike Wise's procedure
;           "calc_regions_image.pro"
;           
;           
; Revision history:
;       written by D&L, 2002-11-08
;	added routines required for deconvolution (DR), 2003-3-4
;	changed code to use weighted responses (DR), 2003-3-31
;	updated to use any chip (DR), 2003-06-26
;	updated to CIAO 3.0 (DR), 2004-3-2
;	added elliptical annuli (DR), 2004-6-21
;	
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 3) or (np gt 3)) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'extract_annuli, xc, yc, mincnts [, chipid=chipid, ellip=ellip, pa=pa, cencnts=cencnts, /mkacisrmf, spectra_dir=spectra_dir, /re_extract]'
    return   
endif


;
; Set the defaults
;
if (n_elements(chipid) eq 0) then chipid=7 else chipid=fix(chipid)
if (n_elements(ellip) eq 0) then ellip=0.0
if (n_elements(pa) eq 0) then pa=0.0 else pa=2.0*!PI*pa/360.0
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'


;
; Define root directory for CIAO
;
get_lun,unit
openr,unit,'ciao_info.txt'
ciao_root=' '
readf,unit,ciao_root
cmdroot='source '+ciao_root+'/bin/ciao.csh ; '
close,unit
free_lun,unit


;
; Make a new spectra directory 
;
mincnts_string=strtrim(string(mincnts),2)
;spectra_dir='spectra_'+strtrim(string(mincnts),2)
if not keyword_set(re_extract) then begin
    cmdstring='mkdir '+spectra_dir
    spawn,cmdstring,result
end


;
; Check that required files are present
;
if keyword_set(mkscisrmf) then goto,skip_make_acisspec
search_path=spectra_dir+'/acisspec_mod'
acisspec_file=findfile(search_path,count=num)
if (num eq 0) then begin
    cd,spectra_dir
    make_acisspec_mod
    cd,'..'
endif
skip_make_acisspec:

asol1file=findfile('primary/*asol1*',count=asolnum)


;
; Reset 
;
cd,'primary'

bpixfile=findfile('new_bpix1.fits',count=num)
if (num eq 0) then begin
    bpixfile=findfile('*bpix1.fits',count=num)
    if (num eq 0) then begin
        print,'ERROR:'
        print,'BPIX1 file not found in primary directory!'
        return
    endif
endif
print,'Found BPIX1 file:',bpixfile


cmdstring='punlearn ardlib'
spawn,cmdroot+cmdstring,result

ardlibfile=findfile('acis_set_ardlib',count=num)
if (num eq 0) then begin
    cmdstring='cp '+ciao_root+'/contrib/bin/acis_set_ardlib .'
    spawn,cmdstring,result
endif 

cmdstring='acis_set_ardlib '+bpixfile
spawn,cmdroot+cmdstring,result

cd,'..'


;
; Check if /re_extract is set
;
if keyword_set(re_extract) then begin
    pattern=spectra_dir+'/reg*'
    old_file=findfile(pattern,count=onum)
    if (onum gt 0) then for i=0,onum-1 do rm_file,old_file[i]
    goto,skip_prelim
endif


;
; Create aspect histogram for chip
;
print,'Creating aspect histogram for the chip...'
cmdstring='punlearn asphist'
spawn,cmdroot+cmdstring,result

if (asolnum eq 1) then begin
    cmdstring='asphist infile='+asol1file+' outfile=./'+spectra_dir+'/ccd_asphist.fits evtfile=./reprocessed/evt2_ccd_clean_no_ptsrc.fits dtffile=NONE'
    spawn,cmdroot+cmdstring,result
endif else begin
    cmdstring='asphist infile="@asol1.list" outfile=./'+spectra_dir+'/ccd_asphist.fits evtfile=./reprocessed/evt2_ccd_clean_no_ptsrc.fits dtffile=NONE'
    spawn,cmdroot+cmdstring,result
endelse
print,'...done.'


;
; Make an instrument map
;
print,'Making an instrument map...'
cmdstring='punlearn mkinstmap'
spawn,cmdroot+cmdstring,result

cmdstring='mkinstmap obsfile="./'+spectra_dir+'/ccd_asphist.fits[asphist]" outfile=./'+spectra_dir+'/instmap_ccd.fits det=ACIS-'+strtrim(string(chipid),2)+' pixelgrid="1:1024:#1024,1:1024:#1024" spectrumfile=weights.txt monoenergy=1.0 grating=NONE maskfile=NONE'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Make an exposure map
;
print,'Making an exposure map...'
;cmdstring='rm ~/cxcds_param/get_sky_limits.par'
;spawn,cmdroot+cmdstring,result

cmdstring='punlearn get_sky_limits'
spawn,cmdroot+cmdstring,result

cmdstring='get_sky_limits ./images/ccd_img_clean.fits'
spawn,cmdroot+cmdstring,result

cmdstring='punlearn mkexpmap'
spawn,cmdroot+cmdstring,result

cmdstring='mkexpmap instmapfile=./'+spectra_dir+'/instmap_ccd.fits outfile=./'+spectra_dir+'/expmap_ccd.fits xygrid=")get_sky_limits.xygrid" asphistfile=./'+spectra_dir+'/ccd_asphist.fits useavgaspect=no normalize=no'
spawn,cmdroot+cmdstring,result
print,'...done'


;
; Divide the image of the chip by the exposure map
;
print,'Normalizing by the exposure map...'
cmdstring='punlearn dmimgcalc'
spawn,cmdroot+cmdstring,result

cmdstring='dmimgcalc infile=./images/ccd_img_clean.fits infile2=./'+spectra_dir+'/expmap_ccd.fits outfile=./images/ccd_img_norm.fits operation=div'
spawn,cmdroot+cmdstring,result
print,'...done.'


;
; Make a color image
;
print,'Making a color image...'
cmdstring='color_image -i ./reprocessed/acis_dstrk_evt2.fits -o ./images/color_img.jpg -x '+strtrim(string(xc),2)+' -y '+strtrim(string(yc),2)+' -w 600 -b 1 -S 200:1500 -M 1500:2500 -H 2500:8000'
spawn,cmdroot+cmdstring,result
print,'...done.'


skip_prelim:
;
;-----------------------------------------------------------
;
; The following taken from 'calc_regions_image.pro' with 
; modifications required for elliptical annuli and cencnts
;
; Read in the image
;
im=readfits('./images/ccd_img_clean.fits',hd)
retry_extraction:
print,'Creating regions with a minimum of '+mincnts_string+' counts...'
if (n_elements(cencnts) ne 0) then print,'(with a central region of '+strtrim(string(cencnts),2)+' counts)'


;
; Get pixel scale parameters out of the header
;
CRVAL1P=sxpar(hd,'CRVAL1P','Parameter CRVAL1P not found')
CRPIX1P=sxpar(hd,'CRPIX1P','Parameter CRPIX1P not found')
CDELT1P=sxpar(hd,'CDELT1P','Parameter CDELT1P not found')
CRVAL2P=sxpar(hd,'CRVAL2P','Parameter CRVAL2P not found')
CRPIX2P=sxpar(hd,'CRPIX2P','Parameter CRPIX2P not found')
CDELT2P=sxpar(hd,'CDELT2P','Parameter CDELT2P not found')


;
; A little checking
;
s=size(im)
xsize=s(1)
ysize=s(2)
if (s(0) ne 2) then begin
    print,string(7B),'ERROR: Image must be 2 dimensional'
    return
endif


;
; Calculate the chip X and Y of the image center
;
xcen=(xc-CRVAL1P-CRPIX1P)/CDELT1P
ycen=(yc-CRVAL2P-CRPIX2P)/CDELT2P


;
; Create distance mask
;
mask=fltarr(xsize,ysize)

xdist=findgen(xsize)
xcoord=xdist-float(xcen)
xdist=abs(xdist-float(xcen))
ydist=findgen(ysize)
ycoord=ydist-float(ycen)
ydist=abs(ydist-float(ycen))

for i=0,xsize-1 do begin
    for j=0,ysize-1 do begin
        mask(i,j)=sqrt(xdist(i)*xdist(i)+ydist(j)*ydist(j))
    endfor
endfor
mask(xcen,ycen)=0.01


;
; Determine number of radial bins and define some arrays
;
xybin=1
nrad=fix(max(mask)/xybin)
r1=fltarr(nrad)
r2=fltarr(nrad)
a1=fltarr(nrad)
a2=fltarr(nrad)
b1=fltarr(nrad)
b2=fltarr(nrad)
totcnts=fltarr(nrad) 
area=fltarr(nrad)
ratio=fltarr(nrad)


;
; If ellipses are used, redefine the mask in terms
; of the semimajor axis
;
if (ellip ne 0.0) then begin

    for i=0,xsize-1 do begin
        for j=0,ysize-1 do begin
            x_ell=xcoord(i)*cos(pa)+ycoord(j)*sin(pa) ; change to the ellipse's 
            y_ell=ycoord(j)*cos(pa)-xcoord(i)*sin(pa) ; coordinate system
            
            a_ell=sqrt(x_ell^2.0+y_ell^2.0/(1.0-ellip)^2.0) ; solve for semimajor axis
            
            mask(i,j)=a_ell
        endfor
    endfor

endif
mask(xcen,ycen)=0.01


;
; Loop over bins and calculate the area
;
if (ellip eq 0.0) then begin

    for i=0,nrad-1 do begin
        r1(i)=i*xybin
        r2(i)=(i+1)*xybin
        if (i gt 0) then j=where( (mask gt r1(i)) and (mask le r2(i)) ) $
        else j=where( (mask gt 0.0) and (mask le r2(i)) )
        area(i)=n_elements(j)
        totcnts(i)=total(float(im(j)))
    endfor
    
endif else begin

    for i=0,nrad-1 do begin
        a1(i)=i*xybin
        a2(i)=(i+1)*xybin
        if (i gt 0) then j=where( (mask gt a1(i)) and (mask le a2(i)) ) $
        else j=where( (mask gt 0.0) and (mask le a2(i)) )
        area(i)=n_elements(j)
        totcnts(i)=total(float(im(j)))
    endfor

endelse


;
; Add up individual radial bins
; until the minimum counts per annulus is reached.
;
cumcnts=fltarr(nrad)
cumarea=fltarr(nrad)
for i=0,nrad-1 do begin  
    cumcnts(i)=total(totcnts(0:i))
    cumarea(i)=total(area(0:i))
endfor
if (n_elements(cencnts) ne 0) then begin
    for i=0,nrad-1 do begin
        ratio[i]=fix(float(cumcnts[i])/cencnts)
        if (ratio[i] eq 1) then goto,do_mincnts
    endfor
    do_mincnts:
    for j=i+1,nrad-1 do begin
        ratio[j]=fix(float(cumcnts[j]-cumcnts[i])/mincnts)+1
    endfor
endif else begin
    ratio=fix(float(cumcnts)/mincnts)
endelse
j=uniq(ratio)  

if (ellip eq 0.0) then begin

    nradb=n_elements(j)
    r1b=fltarr(nradb)
    r2b=fltarr(nradb)
    totcntsb=fltarr(nradb) 
    areab=fltarr(nradb)

    r1b(0)=r1(0)
    r2b(0)=r2(j(0))
    totcntsb(0)=total(totcnts(0:j(0)))
    areab(0)=total(area(0:j(0)))

    for i=1,nradb-1 do begin
        k=indgen(j(i)-j(i-1))+j(i-1)+1
        r1b(i)=r2(j(i-1))
        r2b(i)=r2(j(i))
        totcntsb(i)=total(totcnts(k))
        areab(i)=total(area(k))
    endfor

    nrad=nradb
    r1=r1b
    r2=r2b
    totcnts=totcntsb
    area=areab


                                ;
                                ; Write the output file
                                ;
    outfile='./spectra/annuli.txt'
    space='         '
    fmt='$(1x,i6,3x,a9,4(3x,f9.3),2(3x,e13.5))'
    fmt2='$(1x,a6,3x,a9,4(3x,a9),2(3x,a13))'
    get_lun,unit
    openw,unit,outfile
    printf,unit,' '
    printf,unit,'Extracted annular regions file'
    printf,unit,' '
    printf,unit,'Date: ',!stime
    printf,unit,'Input file: ./reprocessed/img8_c7_clean.fits'
    printf,unit,'Sky X     : ',xc
    printf,unit,'Sky Y     : ',yc
    printf,unit,'X Centroid: ',xcen
    printf,unit,'Y Centroid: ',ycen
    printf,unit,'Binsize   : ',xybin
    printf,unit,'Min counts: ',mincnts
    printf,unit,' '                        
    printf,unit,' '                        
    printf,unit,fmt2,'Index ','  Type   ','   X Center  ', $
      '   Y Center  ','   Radius 1  ','   Radius 2  ', $
      '    Area     ','    Counts   '
    for i=0,nrad-1 do begin
        printf,unit,fmt,i,'annulus',xc,yc,r1(i),r2(i),area(i),totcnts(i)
    endfor
    close,unit
    free_lun,unit

endif else begin

    nradb=n_elements(j)
    a1b=fltarr(nradb)
    a2b=fltarr(nradb)
    totcntsb=fltarr(nradb) 
    areab=fltarr(nradb)

    a1b(0)=a1(0)
    a2b(0)=a2(j(0))
    totcntsb(0)=total(totcnts(0:j(0)))
    areab(0)=total(area(0:j(0)))

    for i=1,nradb-1 do begin
        k=indgen(j(i)-j(i-1))+j(i-1)+1
        a1b(i)=a2(j(i-1))
        a2b(i)=a2(j(i))
        totcntsb(i)=total(totcnts(k))
        areab(i)=total(area(k))
    endfor

    nrad=nradb
    a1=a1b
    a2=a2b
    totcnts=totcntsb
    area=areab


                                ;
                                ; Write the output file
                                ;
    outfile='./'+spectra_dir+'/annuli.txt'
    space='         '
    fmt='$(1x,i6,3x,a9,4(3x,f9.3),2(3x,e13.5))'
    fmt2='$(1x,a6,3x,a9,4(3x,a9),2(3x,a13))'
    get_lun,unit
    openw,unit,outfile
    printf,unit,' '
    printf,unit,'Extracted annular regions file'
    printf,unit,' '
    printf,unit,'Date: ',!stime
    printf,unit,'Input file: ./reprocessed/ccd_img_clean.fits'
    printf,unit,'Sky X     : ',xc
    printf,unit,'Sky Y     : ',yc
    printf,unit,'X Centroid: ',xcen
    printf,unit,'Y Centroid: ',ycen
    printf,unit,'Binsize   : ',xybin
    printf,unit,'Min counts: ',mincnts
    printf,unit,'Ellipticity: ',ellip
    printf,unit,'PA        : ',pa
    printf,unit,' '                        
    printf,unit,' '                        
    printf,unit,fmt2,'Index ','  Type   ','   X Center  ', $
      '   Y Center  ','   SMA 1  ','   SMA 2  ', $
      '    Area     ','    Counts   '
    for i=0,nrad-1 do begin
        printf,unit,fmt,i,'annulus',xc,yc,a1(i),a2(i),area(i),totcnts(i)
    endfor
    close,unit
    free_lun,unit

endelse
;
; end of part taken from 'calc_regions_image.pro'
;----------------------------------------------------------------------
;


;
; Write region files in CIAO format for each annulus
;
if (ellip eq 0.0) then begin
    for i=0,nrad-1 do begin
        outfile='./'+spectra_dir+'/reg'+strtrim(string(i+1),2)+'.reg'
        xcstring=strtrim(string(xc),2)
        ycstring=strtrim(string(yc),2)
        r1string=strtrim(string(r1(i)),2)
        r2string=strtrim(string(r2(i)),2)
        get_lun,unit
        openw,unit,outfile
        printf,unit,'# Region file format: CIAO version 3.0'
        printf,unit,'annulus(',xcstring,',',ycstring,',',r1string,',',r2string,')'
        close,unit
        free_lun,unit
    endfor
endif else begin
    pastring=strtrim(string(pa/(2.0*!PI)*360.0),2)
    for i=0,nrad-1 do begin
        outfile='./'+spectra_dir+'/reg'+strtrim(string(i+1),2)+'.reg'
        xcstring=strtrim(string(xc),2)
        ycstring=strtrim(string(yc),2)
        a1string=strtrim(string(a1(i)),2)
        a2string=strtrim(string(a2(i)),2)
        b1string=strtrim(string(a1(i)*(1-ellip)),2)
        b2string=strtrim(string(a2(i)*(1-ellip)),2)

        get_lun,unit
        openw,unit,outfile
        printf,unit,'# Region file format: CIAO version 3.0'
        printf,unit,'+ellipse(',xcstring,',',ycstring,',',a2string,',',b2string,',',pastring,')'
        printf,unit,'-ellipse(',xcstring,',',ycstring,',',a1string,',',b1string,',',pastring,')'
        close,unit
        free_lun,unit
    endfor
endelse


;
; Write region file 'annuli_mincnts.reg' for ds9
;
outfile='./regions/annuli_'+mincnts_string+'.reg'
get_lun,unit
openw,unit,outfile
if (ellip eq 0.0) then begin
    fmt4='$(a8,f8.3,a1,f8.3,a1,f7.3,a1,f8.3,a1)'
    printf,unit,'# Region file format: CIAO version 3.0'
    for i=0,nrad-1 do begin
        printf,unit,fmt4,'annulus(',xc,',',yc,',',r1(i),',',r2(i),')'
    endfor
endif else begin
    fmt4='$(a8,f8.3,a1,f8.3,a1,f8.3,a1,f8.3,a1,f8.3,a1)'
    printf,unit,'# Region file format: CIAO version 3.0'
    for i=0,nrad-1 do begin
        printf,unit,fmt4,'ellipse(',xc,',',yc,',',a2(i),',',a2(i)*(1-ellip),',',pa/(2.0*!PI)*360.0,')'
    endfor
endelse
close,unit
free_lun,unit


;
; Write the file 'radii.dat' with the inner and outer radii
; for use later with isis
;
outfile='./'+spectra_dir+'/radii.dat'
get_lun,unit
openw,unit,outfile
if (ellip eq 0.0) then begin
    fmt4='$(f7.3,a5,f8.3)'
    for i=0,nrad-1 do begin
        printf,unit,fmt4,r1(i),'     ',r2(i)
    endfor
endif else begin
    fmt4='$(f7.3,a5,f8.3)'
    for i=0,nrad-1 do begin
        printf,unit,fmt4,a1(i),'     ',a2(i)
    endfor
endelse
close,unit
free_lun,unit


;
; Before continuing, ask for number of regions to extract
;
if (n_elements(n_to_fit) eq 0) then  begin
    print,' '
    print,strtrim(string(nrad),2)+' regions created with '+mincnts_string+' counts each.'
    print,' '
    print,'Please choose the number of regions, n, to extract.  The'
    print,'innermost n regions will be extracted; the remaining'
    print,'regions will be ignored and therefore will not'
    print,'be available for spectral fitting.  Choosing a smaller'
    print,'number will speed up extraction; additionaly, deprojection'
    print,'works best when there are fewer than 10 regions.  '
    print,' '
    print,'Starting DS9 to check which region files may '
    print,'be safely ignored.  After you decide, close DS9 to'
    print,'continue and enter a value for n.  You may also'
    print,'choose to retry the extraction with a new minimum'
    print,'number of counts. 
    print,' '
    cmdstring=ciao_root+'/ots/saord/ds9 ./images/ccd_img_clean.fits -regionfile ./regions/annuli_'+strtrim(string(mincnts),2)+'.reg'
;   cmdstring='/iraf/extern/ds9/ds9 ./images/ccd_img_clean.fits -regionfile ./regions/annuli_'+strtrim(string(mincnts),2)+'.reg'
    spawn,cmdstring,result
    print,' '
    answer=' '
    read,answer,prompt='Do you want to re-extract annuli with a different MINCNTS (y/n)?:'
    answer_check:
    if ( (answer ne 'y') and (answer ne 'n') ) then begin
        read,answer,prompt='Please type "y" or "n": '
        goto,answer_check
    endif
    if (answer eq 'y') then begin
        print,' '
        read,mincnts,prompt='Enter a new value for MINCNTS: '
        mincnts_string=strtrim(string(mincnts),2)
        goto,retry_extraction
    endif
    read,nreg_to_extract,prompt='Enter value for n (1-'+strtrim(string(nrad),2)+'): '
    reg_check:
    if ( (nreg_to_extract lt 1) or (nreg_to_extract gt nrad) ) then begin
        read,nreg_to_extract,prompt='Please enter a number between 1 and '+strtrim(string(nrad),2)+': '
        goto,reg_check
    endif
endif else begin
    if (n_to_fit le nrad) then nreg_to_extract=n_to_fit else nreg_to_extract=nrad
endelse
nreg_to_extract=fix(nreg_to_extract)


;
; Create pha, wrmf, and warf files
;
; First copy evt2, bg, qe_corr, and GTI files to spectra directory
; (if they're not there already)
;
search_path_evt2=spectra_dir+'/evt2_ccd_clean_no_ptsrc.fits'
evt2file=findfile(search_path_evt2,count=num)
if (num eq 0) then begin
    cmdstring='cp reprocessed/evt2_ccd_clean_no_ptsrc.fits '+spectra_dir+'/.'
    spawn,cmdstring
endif
search_path=spectra_dir+'/bg.fits'
bgfile=findfile(search_path,count=num)
if (num eq 0) then begin
    cmdstring='cp background/bg.fits '+spectra_dir+'/.'
    spawn,cmdstring
endif
search_path=spectra_dir+'/evt2_ccd_bg.gti'
gtifile=findfile(search_path,count=num)
if (num eq 0) then begin
    cmdstring='cp background/evt2_ccd_bg.gti '+spectra_dir+'/.'
    spawn,cmdstring
endif


;
; If FP temperature is -120 C and mkacisrmf keyword is set, use mkacisrmf instead of acisspec_mod
;
; Read in header of evt2 file and get focal plane temperature
;
hd2=headfits(search_path_evt2,exten=1)
fptemp=sxpar(hd2,'FP_TEMP','Parameter FP_TEMP not found')


;
; Check temperature
;
if ( (fptemp gt 152) and (fptemp lt 154) ) then fptempc=-120 else fptempc=0
if ( (fptempc eq -120) and keyword_set(mkacisrmf) ) then begin
    maskfile=findfile('./secondary/*msk1*',count=num)
    if (num eq 0) then begin
        cd,spectra_dir
        for i=1,nreg_to_extract do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of '+strtrim(string(nreg_to_extract),2)
            print,'-------------------------------------------'
            root='reg'+strtrim(string(i),2)
            reg_file='reg'+strtrim(string(i),2)+'.reg'
            wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root
        endfor
    endif else begin
        cd,spectra_dir
        for i=1,nreg_to_extract do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of '+strtrim(string(nreg_to_extract),2)
            print,'-------------------------------------------'
            root='reg'+strtrim(string(i),2)
            reg_file='reg'+strtrim(string(i),2)+'.reg'
            wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root,mask_file=maskfile
        endfor
    endelse

endif else begin

                                ;
                                ; Otherwise, call ACISSPEC_MOD script or SPECEXTRACT to create pha, wrmf, and warf files
                                ;
    cd,spectra_dir
    
    
                                ;
                                ; Use specextract if /SPECEXT is set
                                ;
   if keyword_set(specext) then begin
    for i=1,nreg_to_extract do begin
        print,' '
        print,'Now running SPECEXTRACT on region '+strtrim(string(i),2)+' of '+strtrim(string(nreg_to_extract),2)
        print,'-------------------------------------------'
        root='reg'+strtrim(string(i),2)
        reg_file='reg'+strtrim(string(i),2)+'.reg'
        cmdstring='specextract "evt2_ccd_clean_no_ptsrc.fits[sky=region('+reg_file+')]" bkgfile="bg.fits[sky=region('+reg_file+')]" grouptype=NONE binspec=NONE verbose=1 outroot='+root
        spawn,cmdroot+cmdstring,result
        cmdstring='mv '+root+'_src1.pi '+root+'_sou.pi' 
        spawn,cmdroot+cmdstring,result
        cmdstring='mv '+root+'_bkg1.pi '+root+'_bgd.pi'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit infile='+root+'_sou.pi filelist= operation=add key=BACKFILE value='+root+'_bgd.pi'
        spawn,cmdroot+cmdstring,result
    endfor
   endif else begin
      make_acisspec_mod
      for i=1,nreg_to_extract do begin
         print,' '
         print,'Now running ACISSPEC_MOD on region '+strtrim(string(i),2)+' of '+strtrim(string(nreg_to_extract),2)
         print,'-------------------------------------------'
         root='reg'+strtrim(string(i),2)
         reg_file='reg'+strtrim(string(i),2)+'.reg'
         cmdstring='./acisspec_mod soufile1="evt2_ccd_clean_no_ptsrc.fits[sky=region('+reg_file+')]" bgfile1="bg.fits[sky=region('+reg_file+')]" root='+root
         spawn,cmdroot+cmdstring,result
      endfor
   endelse
endelse

print,' '
print,'Extraction complete.'
print,' '
cd,'..'


;
; Add keywords required by PROJCT to header of each
; pha file:  XFLT001 = semi-major axis in pixels for outer boundary
;	     XFLT002 = semi-minor axis
;	     XFLT003 = position angle
;
if keyword_set(match) then begin ; read in values used previously

    answer='n'
    goto, skip_query
endif 
print,' '
print,' At this point, you may choose to calculate starting and ending angles'
print," for each ellipse.  If you won't use annuli that extend beyond the chip"
print,' boundaries, this step is not necessary.  Otherwise, you need to find'
print,' the appropriate angles to exclude the area that is outside of the chip.'
print,' '
answer=' '
read,answer,prompt='Do you want to calculate angles (y/n)?:'
answer_check2:
if ( (answer ne 'y') and (answer ne 'n') ) then begin
    read,answer,prompt='Please type "y" or "n": '
    goto,answer_check2
endif
skip_query:
if (answer eq 'n') then begin
    print,'Now adding keywords to spectra...'
    if (ellip eq 0.0) then begin
        for i=1,nreg_to_extract do begin
            istring=strtrim(string(i),2)
            phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
            rstring=strtrim(string(r2(i-1)),2)
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+rstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+rstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value=0.0 datatype=float'
            spawn,cmdroot+cmdstring,result
        endfor
    endif else begin
        for i=1,nreg_to_extract do begin
            istring=strtrim(string(i),2)
            phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
            astring=strtrim(string(a2(i-1)),2)
            bstring=strtrim(string(a2(i-1)*(1.0-ellip)),2)
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+astring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+bstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value='+pastring+' datatype=float'
            spawn,cmdroot+cmdstring,result
        endfor
    endelse
endif
if (answer eq 'y') then begin
    print,' '
    print,' To do this, determine the number of partial segments that'
    print,' make up each annulus. Next, find the starting and ending 
    print,' angle (measured in degrees from the +x-axis) for each '
    print,' segment of the annulus from the ds9 display. The start'
    print,' angle should be less than the end angle (e.g., -45,240'
    print,' not the equivalent 315,240).'
    print,' '
    print," Write them down and exit ds9 when you're finnished."
    cmdstring=ciao_root+'/ots/saord/ds9 ./images/ccd_img_clean.fits -regionfile ./regions/annuli_'+strtrim(string(mincnts),2)+'.reg'
;   cmdstring='/iraf/extern/ds9/ds9 ./images/ccd_img_clean.fits -regionfile ./regions/annuli_'+strtrim(string(mincnts),2)+'.reg'
    spawn,cmdstring,result
    print,' '
    print,' Enter the angles in degrees measured from'
    print,' the +x-axis in the following format:
    print,'    start,stop  (e.g. 20,60)'
    print,' '
    angles=dblarr(2)
    xflt0004=dblarr(nreg_to_extract)
    xflt0005=dblarr(nreg_to_extract)
    xflt0006=dblarr(nreg_to_extract)
    xflt0007=dblarr(nreg_to_extract)
    
    
    
    for i=1,nreg_to_extract do begin
        istring=strtrim(string(i),2)
        reenter_num:
        print,'Enter number of partial segments for annulus '+istring
        read,n_seg,prompt='(from 0 - 1; enter 0 if annulus is whole): '
        if (n_seg eq 0) then begin
            xflt0004[i-1]=0
            xflt0005[i-1]=360
;         xflt0006[i-1]=0
;         xflt0007[i-1]=360
        endif
        if (n_seg eq 1) then begin
            read,angles,prompt='Enter angles for annulus '+istring+': '
            xflt0004[i-1]=angles[0]
            xflt0005[i-1]=angles[1]
;         xflt0006[i-1]=0
;         xflt0007[i-1]=360
        endif
;      if (n_seg eq 2) then begin
;         read,angles,prompt='Enter angles for segment 1 of annulus '+istring+': '
;         xflt0004[i-1]=angles[0]
;         xflt0005[i-1]=angles[1]
;         read,angles,prompt='Enter angles for segment 2 of annulus '+istring+': '
;         xflt0006[i-1]=angles[2]
;         xflt0007[i-1]=angles[3]
;      endif
        if ( (n_seg ne 0) and (n_seg ne 1) ) then begin
            print,'Please enter 0 or 1.'
            goto,reenter_num
        endif
    endfor
    
    print,'Now adding keywords to spectra...'
    if (ellip eq 0.0) then begin
        for i=1,nreg_to_extract do begin
            istring=strtrim(string(i),2)
            phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
            rstring=strtrim(string(r2[i-1]),2)
            xflt4string=strtrim(string(xflt0004[i-1]),2)
            xflt5string=strtrim(string(xflt0005[i-1]),2)
;         xflt6string=strtrim(string(xflt0006[i-1]),2)
;         xflt7string=strtrim(string(xflt0007[i-1]),2)
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+rstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+rstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value=0.0 datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0004 value='+xflt4string+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0005 value='+xflt5string+' datatype=float'
            spawn,cmdroot+cmdstring,result
;         cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0006 value='+xflt6string+' datatype=float'
;         spawn,cmdroot+cmdstring,result
;         cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0007 value='+xflt7string+' datatype=float'
;         spawn,cmdroot+cmdstring,result
        endfor
    endif else begin
        for i=1,nreg_to_extract do begin
            istring=strtrim(string(i),2)
            phafile='./'+spectra_dir+'/reg'+istring+'_sou.pi'
            astring=strtrim(string(a2[i-1]),2)
            bstring=strtrim(string(a2[i-1]*(1.0-ellip)),2)
            xflt4string=strtrim(string(xflt0004[i-1]),2)
            xflt5string=strtrim(string(xflt0005[i-1]),2)
;         xflt6string=strtrim(string(xflt0006[i-1]),2)
;         xflt7string=strtrim(string(xflt0007[i-1]),2)
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+astring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+bstring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value='+pastring+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0004 value='+xflt4string+' datatype=float'
            spawn,cmdroot+cmdstring,result
            cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0005 value='+xflt5string+' datatype=float'
            spawn,cmdroot+cmdstring,result
;         cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0006 value='+xflt6string+' datatype=float'
;         spawn,cmdroot+cmdstring,result
;         cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0007 value='+xflt7string+' datatype=float'
;         spawn,cmdroot+cmdstring,result
        endfor
    endelse
endif


;
; Add normalized background exposure time to background files
;
hdbg=headfits('./background/bg.fits',exten=1)
bg_exp=sxpar(hdbg,'EXPOSURE','Parameter EXPOSURE not found')
bgexposure=strtrim(string(bg_exp),2)
for i=1,nreg_to_extract do begin
    istring=strtrim(string(i),2)
    bgfile='./'+spectra_dir+'/reg'+istring+'_bgd.pi'
    cmdstring='dmhedit '+bgfile+' filelist=none operation=add key=EXPOSURE value='+bgexposure+' datatype=float'
    spawn,cmdroot+cmdstring,result
endfor
print,'...done.'


;
; Call Mike Wise's tool calc_sb_image.pro
;
print,'Now calculating surface brightness profile...'
src_regfile=findfile('regions/ccd_src_alt.reg',count=alt_reg)
if (alt_reg eq 0) then regfile='./regions/ccd_src.reg' else regfile='./regions/ccd_src_alt.reg'
if (ellip eq 0.0) then begin
    calc_sb_imagep,'./images/ccd_img_clean.fits', $
      xc=xc,yc=yc,outfile='./'+spectra_dir+'/sb.dat', $
      xybin=10,expfile='./'+spectra_dir+'/expmap_ccd.fits',$
      ccdregfile='./regions/ccd.reg',regfile=regfile,ciao_path=ciao_root
endif else begin
    calc_sb_imagep,'./images/ccd_img_clean.fits', $
      xc=xc,yc=yc,outfile='./'+spectra_dir+'/sb.dat', $
      xybin=10,expfile='./'+spectra_dir+'/expmap_ccd.fits', $
      ellip=ellip,pa=pa*360.0/(2.0*!PI),$
      ccdregfile='./regions/ccd.reg',regfile=regfile,ciao_path=ciao_root
endelse
print,'...done.'


;
;
; Get OBS_ID out of header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')


;
; Update the obs_info file
;
get_lun,unit
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'
openw,unit,outfile,/append
printf,unit,' '
if (n_elements(cencnts) ne 0) then begin
    printf,unit,'Output of EXTRACT_ANNULI,',strtrim(string(xc),2),',',strtrim(string(yc),2),',',strtrim(string(mincnts),2),',chipid=',strtrim(string(chipid),2),',ellip=',strtrim(string(ellip),2),',pa=',strtrim(string(pa/(2.0*!PI)*360.0),2),',cencnts=',strtrim(string(cencnts),2)
endif else begin
    printf,unit,'Output of EXTRACT_ANNULI,',strtrim(string(xc),2),',',strtrim(string(yc),2),',',strtrim(string(mincnts),2),',chipid=',strtrim(string(chipid),2),',ellip=',strtrim(string(ellip),2),',pa=',strtrim(string(pa/(2.0*!PI)*360.0),2)
endelse
printf,unit,!stime
printf,unit,' '
printf,unit,'Aspect Histogram for ccd      : ./'+spectra_dir+'/ccd_asphist.fits'
printf,unit,'Instrument map for ccd        : ./'+spectra_dir+'/instmap_ccd.fits'
printf,unit,'Exposure map for ccd          : ./'+spectra_dir+'/expmap_ccd.fits'
printf,unit,'Normalized (fluxed) image     : ./images/ccd_img_norm.fits'
printf,unit,'Color image                   : ./images/color_img.jpg'
printf,unit,'(red=0.2-1.5 keV, green=1.5-2.5, blue=2.5-8.0)'
printf,unit,' '
printf,unit,'Spectral files                : ./'+spectra_dir+'/reg*_sou.pi, ./'+spectra_dir+'/reg*_bgd.pi'
printf,unit,'Number of regions created     : ',nrad
printf,unit,'Number of regions extracted   : ',fix(nreg_to_extract)
printf,unit,'Minimum counts per region     : ',mincnts
if (n_elements(cencnts) ne 0) then begin
    printf,unit,'Counts in central region      : ',cencnts
endif
printf,unit,'DS9 region file for annuli    : ./regions/annuli_'+mincnts_string+'.reg'
printf,unit,'Summary of region properties  : ./'+spectra_dir+'/annuli.txt'
printf,unit,'Surface brightness data       : ./'+spectra_dir+'/sb.dat'
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,' '
print,'EXTRACT_ANNULI complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
