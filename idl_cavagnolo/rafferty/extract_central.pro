pro extract_central,sma=sma,mincnts=mincnts,chipid=chipid,spectra_dir=spectra_dir,old_pipe=old_pipe,ptsrc=ptsrc
;-----------------------------------------------------------------------
;
; Name: EXTRACT_CENTRAL
;
; Purpose: Extracts the two innermost spectra for deriving central values.
;          
;          
; Inputs:  sma - semi-major axis of innermost region in pixels
;                (limited to > 2 pixels = radius of 95% enclosed energy)
;	   mincnts - counts of innermost region
;	   spectra_dir - the name of the spectra directory
;          chipid - the id of the chip of interest (default is 7)
;	   /OLD_PIPE - if set, uses old pipeline naming conventions
;         
;         
; Comments: Should be run AFTER extract_annuli, as outputs of extract_
;	    annuli are required
;           
;           
; Revision history:
;       written by DR, 2004-7-21
;       updated for ciao 3.3, 2007-4-9
;	
;-----------------------------------------------------------------------
;


;
; Set the defaults
;
if (n_elements(chipid) eq 0) then chipid=7
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
; Reset 
;
cd,'primary'
spawn,'gunzip *bpix*'

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
; Read in annuli properties from annuli.txt
;
fmt = 'A,F'
if keyword_set(old_pipe) then begin
    readcol,'./'+spectra_dir+'/deproj_annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
    xc=annuli_cent[0]
    yc=annuli_cent[1]
    print,' '
    print,'Using x centroid [pixels] : ',xc
    print,'Using y centroid [pixels] : ',yc
    ellip=0.0
    pa=0.0
    print,'Using ellip [1 - b/a]     : ',ellip
    print,'Using position angle [rad]: ',pa
endif else begin
    readcol,'./'+spectra_dir+'/annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
    readcol,'./'+spectra_dir+'/annuli.txt',skipline=11,f=fmt,junk,annuli_prop,delimiter=':',numline=2,/silent
    xc=annuli_cent[0]
    yc=annuli_cent[1]
    ellip=annuli_prop[0]
    pa=annuli_prop[1]
    print,' '
    print,'Using x centroid [pixels] : ',xc
    print,'Using y centroid [pixels] : ',yc
    print,'Using ellip [1 - b/a]     : ',ellip
    print,'Using position angle [rad]: ',pa
    print,' '
endelse


;
; If counts specified, make new region
;
if (n_elements(mincnts) ne 0) then begin


;
;-----------------------------------------------------------
;
; The following taken from 'calc_regions_image.pro' with 
; modifications required for elliptical annuli
;


;
; Make a new image for energies between 0.5-7 keV
;
    if keyword_set(old_pipe) then region_file='./regions/ccd.reg' else region_file='./regions/ccd.reg'
    if keyword_set(old_pipe) then begin
        evt2_file='./spectra/evt2_ccd_clean_no_ptsrc.fits' 
    endif else begin
        evt2_file='./reprocessed/evt2_ccd_clean_no_ptsrc.fits'
    endelse
    if keyword_set(ptsrc) then begin
        evt_file=findfile('spectra/evt2_ccd_clean_no_central_ptsrc.fits',count=num)
        if (num eq 0) then begin
            evt2_file='./spectra/evt2_ccd_no_central_ptsrc.fits'
        endif else begin
            evt2_file='./spectra/evt2_ccd_clean_no_central_ptsrc.fits'
        endelse
    endif
    spawn,'gunzip '+evt2_file+'.gz'
    cmdstring='dmcopy "'+evt2_file+'[energy=500:7000][sky=region('+region_file+')][bin x=::1,y=::1]" ./images/ccd_img_clean_0.5_7.fits'
    spawn,cmdroot+cmdstring,result


;
; Read in the image
;
    print,'Creating inner region with a minimum of '+strtrim(string(mincnts),2)+' counts...'
    im=readfits('./images/ccd_img_clean_0.5_7.fits',hd)


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
    xdist=abs(xdist-float(xcen))
    ydist=findgen(ysize)
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


;
; If ellipses are used, redefine the mask in terms
; of the semimajor axis
;
    if (ellip ne 0.0) then begin

        for i=0,xsize-1 do begin
            for j=0,ysize-1 do begin
                x_ell=xdist(i)*cos(pa)+ydist(j)*sin(pa) ; change to the ellipse's 
                y_ell=ydist(j)*cos(pa)-xdist(i)*sin(pa) ; coordinate system
                
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
    ratio=fix(float(cumcnts)/mincnts)
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
        outfile='./spectra/annuli_central.txt'
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
        outfile='./'+spectra_dir+'/annuli_central.txt'
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
; Set new inner sma
;
    if (ellip eq 0.0) then sma=r2[0] else sma=a2[0]
endif else begin
    if(n_elements(sma) eq 0) then begin
        print,'ERROR:'
        print,'You must enter a value for mincnts or sma!'
        return
    endif
endelse
if (sma lt 2.0 ) then sma=2.0   ; Don't let the extraction radius fall
                                ; below about the radius at 95% encircled
                                ; energy (2 pix at 3 keV), since
                                ; inside that scale, we can no longer
                                ; probe changes in the ICM properties


;
; Read in sma from radii.dat
;
if keyword_set(old_pipe) then begin
    readcol,'./'+spectra_dir+'/deproj_radii.dat',r2,/silent
    s=size(r2)
    rsize=s[1]
    r1=dblarr(rsize)
    r1[0]=0
    for i=1,rsize-1 do begin
        r1[i]=r2[i-1]
    endfor
endif else begin
    readcol,'./'+spectra_dir+'/radii.dat',r1,r2,/silent
    s=size(r2)
    rsize=s[1]
endelse


;
; Make the new regions
;
r_inner_new=dblarr(2)
r_outer_new=dblarr(2)
r_inner_new[0]=0
r_inner_new[1]=sma
r_outer_new[0]=sma
r_outer_new[1]=r2[1]
print,' '
print,'New regions to be extracted:'
print,'   central_reg1: sma from '+strtrim(string(r_inner_new[0]),2)+' to '+strtrim(string(r_outer_new[0]),2)+' pixels'
print,'   central_reg2: sma from '+strtrim(string(r_inner_new[1]),2)+' to '+strtrim(string(r_outer_new[1]),2)+' pixels'


;
; Write region files in CIAO format for the new annuli
;
if (ellip eq 0.0) then begin
    for i=0,1 do begin
        outfile='./'+spectra_dir+'/central_reg'+strtrim(string(i+1),2)+'.reg'
        xcstring=strtrim(string(xc),2)
        ycstring=strtrim(string(yc),2)
        r1string=strtrim(string(r_inner_new(i)),2)
        r2string=strtrim(string(r_outer_new(i)),2)
        get_lun,unit
        openw,unit,outfile
        printf,unit,'# Region file format: CIAO version 3.0'
        printf,unit,'annulus(',xcstring,',',ycstring,',',r1string,',',r2string,')'
        close,unit
        free_lun,unit
    endfor
endif else begin
    pastring=strtrim(string(pa/(2.0*!PI)*360.0),2)
    for i=0,1 do begin
        outfile='./'+spectra_dir+'/central_reg'+strtrim(string(i+1),2)+'.reg'
        xcstring=strtrim(string(xc),2)
        ycstring=strtrim(string(yc),2)
        a1string=strtrim(string(r_inner_new(i)),2)
        a2string=strtrim(string(r_outer_new(i)),2)
        b1string=strtrim(string(r_inner_new(i)*(1-ellip)),2)
        b2string=strtrim(string(r_outer_new(i)*(1-ellip)),2)
        get_lun,unit
        openw,unit,outfile
        printf,unit,'# Region file format: CIAO version 3.1'
        printf,unit,'+ellipse(',xcstring,',',ycstring,',',a2string,',',b2string,',',pastring,')'
        printf,unit,'-ellipse(',xcstring,',',ycstring,',',a1string,',',b1string,',',pastring,')'
        close,unit
        free_lun,unit
    endfor
endelse


;
; Write the file 'radii_central.dat' with the inner and outer radii
;
outfile='./'+spectra_dir+'/radii_central.dat'
get_lun,unit
openw,unit,outfile
r1[0]=r_inner_new[0]
r1[1]=r_inner_new[1]
r2[0]=r_outer_new[0]
r2[1]=r_outer_new[1]
fmt4='$(f7.3,a5,f8.3)'
for i=0,rsize-1 do begin
    printf,unit,fmt4,r1(i),'     ',r2(i)
endfor
close,unit
free_lun,unit


;
; Create pha, wrmf, and warf files
;
; First copy evt2, bg, qe_corr, and GTI files to spectra directory
; (if they're not there already)
;
spawn,'gunzip '+spectra_dir+'/evt2_ccd_clean_no_ptsrc.fits.gz'
search_path_evt2=spectra_dir+'/evt2_ccd_clean_no_ptsrc.fits'
evt2file=findfile(search_path_evt2,count=num)
if (num eq 0) then begin
    spawn,'gunzip reprocessed/evt2_ccd_clean_no_ptsrc.fits.gz'
    cmdstring='cp reprocessed/evt2_ccd_clean_no_ptsrc.fits '+spectra_dir+'/.'
    spawn,cmdstring
endif
spawn,'gunzip '+spectra_dir+'/bg.fits.gz'
search_path=spectra_dir+'/bg.fits'
bgfile=findfile(search_path,count=num)
if (num eq 0) then begin
    spawn,'gunzip background/bg.fits.gz'
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
if ( fptempc eq -120 ) then begin
    spawn,'gunzip secondary/*msk1*.gz'
    maskfile=findfile('./secondary/*msk1*',count=num)
    if (num eq 0) then begin
        cd,spectra_dir
        for i=1,2 do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of 2...'
            print,'-------------------------------------------'
            root='central_reg'+strtrim(string(i),2)
            reg_file='central_reg'+strtrim(string(i),2)+'.reg'
            wextract,reg_file,'evt2_ccd_clean_no_ptsrc.fits','bg.fits',root,ciao_path=ciao_root
        endfor
    endif else begin
        cd,spectra_dir
        for i=1,2 do begin
            print,'Now running WEXTRACT on region '+strtrim(string(i),2)+' of 2...'
            print,'-------------------------------------------'
            root='central_reg'+strtrim(string(i),2)
            reg_file='central_reg'+strtrim(string(i),2)+'.reg'
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
    for i=1,2 do begin
        print,' '
        print,'Now running SPECEXTRACT on region '+strtrim(string(i),2)+' of 2...'
        print,'-------------------------------------------'
        root='central_reg'+strtrim(string(i),2)
        reg_file='central_reg'+strtrim(string(i),2)+'.reg'
        cmdstring='specextract "evt2_ccd_clean_no_ptsrc.fits[sky=region('+reg_file+')]" bkgfile="bg.fits[sky=region('+reg_file+')]" grouptype=NONE binspec=NONE verbose=1 outroot='+root
        spawn,cmdroot+cmdstring,result
        cmdstring='mv '+root+'_src1.pi '+root+'_sou.pi' 
        spawn,cmdroot+cmdstring,result
        cmdstring='mv '+root+'_bkg1.pi '+root+'_bgd.pi'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit infile='+root+'_sou.pi filelist= operation=add key=BACKFILE value='+root+'_bgd.pi'
        spawn,cmdroot+cmdstring,result
    endfor
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
if (ellip eq 0.0) then begin
    for i=1,2 do begin
        istring=strtrim(string(i),2)
        phafile='./'+spectra_dir+'/central_reg'+istring+'_sou.pi'
        rstring=strtrim(string(r_outer_new(i-1)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+rstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value=0.0 datatype=float'
        spawn,cmdroot+cmdstring,result
    endfor
endif else begin
    for i=1,2 do begin
        istring=strtrim(string(i),2)
        phafile='./'+spectra_dir+'/central_reg'+istring+'_sou.pi'
        astring=strtrim(string(r_outer_new(i-1)),2)
        bstring=strtrim(string(r_outer_new(i-1)*(1.0-ellip)),2)
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0001 value='+astring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0002 value='+bstring+' datatype=float'
        spawn,cmdroot+cmdstring,result
        cmdstring='dmhedit '+phafile+' filelist=none operation=add key=XFLT0003 value='+pastring+' datatype=float'
        spawn,cmdroot+cmdstring,result
    endfor
endelse


;
; Add normalized background exposure time to background files
;
hdbg=headfits('./background/bg.fits',exten=1)
bg_exp=sxpar(hdbg,'EXPOSURE','Parameter EXPOSURE not found')
bgexposure=strtrim(string(bg_exp),2)
for i=1,2 do begin
    istring=strtrim(string(i),2)
    bgfile='./'+spectra_dir+'/central_reg'+istring+'_bgd.pi'
    cmdstring='dmhedit '+bgfile+' filelist=none operation=add key=EXPOSURE value='+bgexposure+' datatype=float'
    spawn,cmdroot+cmdstring,result
endfor


;
; Read in header from evt2_ccd_clean file
;
hd=headfits('./reprocessed/evt2_ccd_clean_no_ptsrc.fits',exten=1)


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
printf,unit,'Output of EXTRACT_CENTRAL,'+strtrim(string(sma),2)+' chipid='+strtrim(string(chipid),2)+", spectra_dir='"+spectra_dir+"'"
printf,unit,!stime
printf,unit,' '
printf,unit,'Spectral files created              : ./'+spectra_dir+'/central_reg1_sou.pi'
printf,unit,'                                      ./'+spectra_dir+'/central_reg1_bgd.pi'
printf,unit,'                                      ./'+spectra_dir+'/central_reg2_sou.pi'
printf,unit,'                                      ./'+spectra_dir+'/central_reg2_bgd.pi'
printf,unit,'Region files for new annuli         : ./'+spectra_dir+'/central_reg1.reg'
printf,unit,'                                      ./'+spectra_dir+'/central_reg2.reg'
printf,unit,' '
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit

;
; Print status info to screen
;
print,' '
print,'EXTRACT_CENTRAL complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
