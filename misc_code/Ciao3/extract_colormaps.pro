pro extract_colormaps,eventfile,aspfile,rootname=rootname,bands=bands,      $
                      chip=chip,xcen=xcen,ycen=ycen,xybin=xybin,npix=npix,  $
                      spectfile=spectfile,cmdroot=cmdroot,logfile=logfile,  $
                      qefile=qefile,qeufile=qeufile,bpixfile=bpixfile,      $
                      hetg=hetg
;-----------------------------------------------------------------------
; Name: EXTRACT_COLORMAPS
;
; Purpose: Filter a specified eventfile into energy slices and construct
;	   spectrally weighted exposure maps to go with it.
;          
; Inputs: 
;         
; Comments: Uses DMCOPY to extract images. Uses MKINSTMAP and MKEXPMAP
;	    to construct the exposure maps. Instrument maps are weighted
;	    by the input spectral file or if absent uniform weighting
;	    is assumed across the band. An aspect histogram file is 
;	    assumed to already exist for the observation.
;           
; Revision history:
;
;       written by Michael Wise				11-13-99
;
;	added exposure map code				11-14-99
;
;	modified exposure map code			11-16-99
;	faster sub-image calculation
;
;	modified pointer to EXPMAP			11-19-99
;
;       added line to trim ACIS border			04-13-00
;       returned instr. map to full res.
;
;	added option to use custom BPIX			08-24-00
;	and QEU files. removed distinction
;	between EXPROOT and CMDROOT. Added
;	HETG order weighting code.
;
;	modified to be consistent with CIAO 2.0		08-25-00
;	calling sequences
;
;	fixed QEU bug noted by DSD			09-19-00
;
;	changed default bands to values decided		10-09-00
;	by cluster sub-group
;
;	# Elo  Ehi
;	#-----------
;	0.00  0.29
;	0.29  0.54
;	0.54  1.00
;	1.00  1.57
;	1.57  2.05
;	2.00  3.50
;	3.50  5.00
;	5.00  7.00
;	7.00 10.00
;
;	added option to set the QE file			10-09-00
;
;	slight tweak to remove ARDLIB.PAR		10-25-00
;	added 0.29-7.0 keV band to default set		
;
;	slight tweak to adapt to Linux system		03-07-01
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'pro extract_colormaps,eventfile,aspfile,rootname=rootname,bands=bands,',$
   '                  chip=chip,xcen=xcen,ycen=ycen,xybin=xybin,npix=npix,',$
   '                  spectfile=spectfile,cmdroot=cmdroot,logfile=logfile,',$
   '                  qefile=qefile,qeufile=qeufile,bpixfile=bpixfile,    ',$
   '                  hetg=hetg'
   return   
endif


;
; Handle defaults
;
if (n_elements(rootname) eq 0) then rootname='filtered'
if (n_elements(xcen) eq 0) then xcen=4096.5	; Sky X
if (n_elements(ycen) eq 0) then ycen=4096.5	; Sky Y
if (n_elements(xybin) eq 0) then xybin=1.0
if (n_elements(bands) eq 0) then begin
  bands=fltarr(2,4)
  bands(0,*)=[0.5,1.5,3.0,5.0] ; Low in keV
  bands(1,*)=[1.5,3.0,5.0,9.0] ; Up in keV
endif
if (n_elements(npix) eq 0) then npix=1000	; extracted images will be
						; (2*npix,2*npix)/xybin
						; in size
if (n_elements(logfile) eq 0) then logfile='colormaps.log'
if (n_elements(chip) eq 0) then chip=7


;
; Define location of ASCDS tools 
;
if (n_elements(cmdroot) eq 0) then cmdroot='source /iraf/ciao-2.2/bin/ciao.csh ; '



;
; Define miscellaneous quantities
;
;space='     '
space=''
version='2.0b5'


;
; Handle special case of one energy band
;
s=size(bands)
if (s(0) eq 1) then begin
   bflag=1
   nband=1
endif else begin
   bflag=0
   nband=s(2)
endelse



;
; Open logfile 
;
get_lun,unit
openw,unit,logfile
printf,unit,' '
printf,unit,'Date             : '+!stime
printf,unit,'CXCDS Version    : '+version
printf,unit,' '
printf,unit,'Eventfile        : '+eventfile
printf,unit,'Aspect Histogram : '+aspfile
if (n_elements(bpixfile) ne 0) then begin
   printf,unit,'BPIX file        : '+bpixfile
endif else begin
   printf,unit,'BPIX file        : NONE'
endelse
if (n_elements(qeufile) ne 0) then begin
   printf,unit,'QEU file         : '+qeufile
endif else begin
   printf,unit,'QEU file         : NONE'
endelse
if (n_elements(qefile) ne 0) then begin
   printf,unit,'QE file          : '+qefile
endif else begin
   printf,unit,'QE file          : NONE'
endelse
printf,unit,' '
printf,unit,'Chip ID          : ',chip
printf,unit,' '
for i=0,nband-1 do begin
    if (bflag ne 1) then begin
       printf,unit,'Band '+strtrim(string(i),2)+' : ',bands(0,i),' -- ',$
                 bands(1,i),' keV'
    endif else begin
       printf,unit,'Band '+strtrim(string(i),2)+' : ',bands(0),' -- ',$
                 bands(1),' keV'
    endelse
endfor
printf,unit,' '
printf,unit,' '



;
; If no spectral model file is indicated, construct a spectral 
; file with uniform weighting
;
if (n_elements(spectfile) eq 0) then begin

   spectfile='spect_uniform.dat'
   x=0.1+9.9*findgen(100)/99		; keV from 0.1-10.0
   y=fltarr(100)
   y(*)=1.0/9.9				; normalized weights
   get_lun,unit2
   openw,unit2,spectfile
   for i=0,99 do begin
       printf,unit2,x(i),y(i)
   endfor
   close,unit2
   free_lun,unit2

   printf,unit,' '
   printf,unit,'Creating uniform source spectral weights file ',spectfile
   printf,unit,' '

endif 



;
; Construct spectral weighting files 
;
printf,unit,'Source spectral weighting taken from '+spectfile
construct_weights,spectfile,rootname='weights',bands=bands,files=wfiles
if keyword_set(hetg) then begin
   printf,unit,'Corrections for HETG 0th order applied'
endif
printf,unit,' '
printf,unit,' '



;
; Remove any existing copies of the ARDLIB.PAR file
;
cmdstring='unalias rm; rm -f ~/cxcds_param/ardlib.par '
spawn,cmdstring,result,/sh

printf,unit,space+cmdstring
printf,unit,space+result
printf,unit,' '       
printf,unit,' '       




;
; Set BPIX values in ARDLIB.PAR, if necessary
;
;	assumes extensions are named BADPIXn
; 
if (n_elements(bpixfile) ne 0) then begin

   for i=0,9 do begin

       cs=strtrim(string(i),2)
       cmdstring='pset ardlib.par AXAF_ACIS'+cs+'_BADPIX_FILE='
       cmdstring=cmdstring+'"'+bpixfile+'[BADPIX'+cs+']"'

       spawn,cmdroot+cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result

   endfor
   
   printf,unit,' '       
   printf,unit,' '       

endif



;
; Set QEU values in ARDLIB.PAR, if necessary
;
if (n_elements(qeufile) ne 0) then begin

   for i=0,9 do begin

       cs=strtrim(string(i),2)
       cs2=strtrim(string(i+1),2)
       cmdstring='pset ardlib.par AXAF_ACIS'+cs+'_QEU_FILE='
       cmdstring=cmdstring+'"'+qeufile+'['+cs2+']"'

       spawn,cmdroot+cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result

   endfor
   
   printf,unit,' '       
   printf,unit,' '       

endif



;
; Set QE values in ARDLIB.PAR, if necessary
;
if (n_elements(qefile) ne 0) then begin

   for i=0,9 do begin

       cs=strtrim(string(i),2)
       cs2=strtrim(string(i+1),2)
       cmdstring='pset ardlib.par AXAF_ACIS'+cs+'_QE_FILE='
       cmdstring=cmdstring+'"'+qefile+'['+cs2+']"'

       spawn,cmdroot+cmdstring,result

       printf,unit,space+cmdstring
       printf,unit,space+result

   endfor
   
   printf,unit,' '       
   printf,unit,' '       

endif



;
; Define detector sub-system string
;
ccd=strtrim(string(fix(chip)),2)	; chip designation
if (chip lt 4) then begin
   detsubsys='ACIS-I'+ccd
endif else begin
   detsubsys='ACIS-S'+strtrim(string(fix(chip-4)),2)
endelse
printf,unit,'Detector Sub-system : '+detsubsys
printf,unit,' '
printf,unit,' '


;
; Construct Sky (X,Y) filter strings
;
x1=fix(xcen-npix)        &  xs1=strtrim(string(x1),2)
x2=fix(xcen+npix)        &  xs2=strtrim(string(x2),2)
nx=fix((x2-x1+1)/xybin)  &  nsx=strtrim(string(nx),2)
xfilter=xs1+':'+xs2+':#'+nsx

y1=fix(ycen-npix)        &  ys1=strtrim(string(y1),2)
y2=fix(ycen+npix)        &  ys2=strtrim(string(y2),2)
ny=fix((y2-y1+1)/xybin)  &  nsy=strtrim(string(ny),2)
yfilter=ys1+':'+ys2+':#'+nsy



;
; Loop over number of energy bands
;
for i=0L,nband-1 do begin

;
;   Set limits for this extraction
;
    if (bflag ne 1) then begin
       e1=fix(bands(0,i)*1000.)		; extract in eV
       e2=fix(bands(1,i)*1000.)		; extract in eV
    endif else begin
       e1=fix(bands(0)*1000.)		; extract in eV
       e2=fix(bands(1)*1000.)		; extract in eV
    endelse
    e1=strtrim(string(e1),2)
    e2=strtrim(string(e2),2)

;
;   Define current filenames
;
    curfile=rootname+'_c'+ccd+'_'+e1+'_'+e2+'_img.fits'
    imapfile=rootname+'_c'+ccd+'_'+e1+'_'+e2+'_imap.fits'
    emapfile=rootname+'_c'+ccd+'_'+e1+'_'+e2+'_emap.fits'

;
;   Note in logfile
;
    if (bflag ne 1) then begin
       printf,unit,'Processing energy band '+strtrim(string(i),2)+' : ', $
                 bands(0,i),' -- ',bands(1,i),' keV'
    endif else begin
       printf,unit,'Processing energy band '+strtrim(string(i),2)+' : ', $
                 bands(0),' -- ',bands(1),' keV'
    endelse

    printf,unit,' '
    printf,unit,'Imagefile        : '+curfile
    printf,unit,'Instr. Map file  : '+imapfile
    printf,unit,'Exp. Map file    : '+emapfile
    printf,unit,'Weights file     : '+wfiles(i)
    printf,unit,' '
 

;
;   Create source image using DMCOPY
;
    filter='[EVENTS][ccd_id='+strtrim(string(fix(chip)),2)+    $
           ',energy='+e1+':'+e2+']'+                           $
           '[bin x='+xfilter+',y='+yfilter+']'
           
    cmdstring='dmcopy "'+eventfile+filter+'" '+curfile
    spawn,cmdroot+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '


;
;   Calculate the instrument map for this band
;
    parms='outfile="'+imapfile+'" spectrumfile="'+wfiles(i)+'" '+  $
          'pixelgrid="1:1024:#1024,1:1024:#1024" '+                $
          'maskfile="NONE" monoenergy=1.0 '+                       $
          'detsubsys="'+detsubsys+'" mirror="HRMA" '+              $
          'ardlibparfile="ardlib.par" verbose=2 '+                 $
          'obsfile="'+eventfile+'+1"'

;
;   Set flag for HETG 0th order if indicated
;
    if keyword_set(hetg) then parms=parms+' grating="HETG"'        $
                         else parms=parms+' grating="NONE"'


    cmdstring='mkinstmap '+parms
    spawn,cmdroot+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result
    printf,unit,' '

;
;   Calculate the exposure map for this band
;
    parms='instmapfile="'+imapfile+'" outfile="'+emapfile+'" '+      $
          'xygrid="'+xfilter+','+yfilter+'" '+                       $
          'asphistfile="'+aspfile+'" useavgaspect="no" '+            $
          'verbose=0 '

    cmdstring='mkexpmap '+parms
    spawn,cmdroot+cmdstring,result

    printf,unit,space+cmdstring
    printf,unit,space+result    ; Too many erroneous PIXLIB messages
    printf,unit,' '
    printf,unit,' '
    printf,unit,' '

endfor

;
; Close up logfile
;
printf,unit,'Colormap extraction complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
