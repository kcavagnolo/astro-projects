;;;;;;;;;;;;;;;;;;;;;;;;;;;
; XIM: WRAPPER SCRIPT ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro make_evts,simfile,filename,verbose

;
;
; Sebastian Heinz, 01/01/2009
;
; make_evts writes a virtual observation to a fits file for later processing
; with standard astronomical data reduction tools like ds9 and ftools.
;
; CALLING SEQUENCE:
;
; pro
; make_evts,simspec,xpix,ypix,ra,dec,rdcenter,aimpoint,telescope,instrument,$
;           exptime,channel,egrid,roll,dpixx,dpixy,filename,verbose
;
; input:  
;         simfile: input simfile from spectral simulation (contains all
;                  required data);
;
;         filename: name of output events file
;
;         verbose: print, out progress information
;
; output:
;         all output is written into the events file
; 

restore,simfile

if verbose gt 0 then begin
    print,'Creating events file - this can take a long time.'
    print,'Total number of counts in events file: '+$
      strtrim(string(format='(e15.2)',total(simspec)),2)
endif

; initialize header
fxhmake,head,/extend,/initialize,/date
mjd=systime(0,/JULIAN)-2400000.5
SXADDPAR,head,"HDUNAME","PRIMARY"
SXADDPAR,head,"MJD_OBD",mjd
SXADDPAR,head,"CREATOR","XIM - Version 0.1","tool that created this output"
SXADDPAR,head,"DATE",date_conv(systime(0,/JULIAN),'F'),$
         "Date and time of file creation"
SXADDPAR,head,"DATE-OBS",$
         date_conv(systime(0,/JULIAN)-exptime*1000./3600./24.,'F'),$
         "Observation start date"
SXADDPAR,head,"DATE-END",$
         date_conv(systime(0,/JULIAN),'F'),$
         "Observation end date"
SXADDPAR,head,"TIMESYS","TT      ","Time system"
SXADDPAR,head,"MJDREF",mjd-exptime*1000./3600./24.,"MJD zero point"
SXADDPAR,head,"TIMEZERO",0.0,"Clock correction"
SXADDPAR,head,"TIMEUNIT","s       ","Time unit"
SXADDPAR,head,"CLOCKAPP",LOGICAL_TRUE(1 eq 1),"default"
SXADDPAR,head,"TSTART",0.0,"Observation start time"
SXADDPAR,head,"TSTOP",exptime*1000.,"Observation end time"
SXADDPAR,head,"MISSION",telescope,"Mission"
SXADDPAR,head,"TELESCOP",telescope,"Telescope"
SXADDPAR,head,"INSTRUME",instrument,"Instrument"
SXADDPAR,head,"OBS_ID",0,"Observation id"
SXADDPAR,head,"SEQ_NUM",0,"Sequence number"
fxwrite,filename,head

; initialize binary extension and header
;
; number of rows:
nphot=long(total(long(simspec)))+1l

fxbhmake,head,nphot,'EVENTS','binary extension'
fxbaddcol,1,head,2000.,'TIME','Photon arrival time',TUNIT='s'
fxbaddcol,2,head,max(fix(channel)),$
  'PHA','Pulse Height Analyzer bin number',TUNIT='channel'
fxbaddcol,3,head,xpix[0],'X','projected X position of photon on sky',TUNIT='pixel'
fxbaddcol,4,head,ypix[0],'Y','projected Y position of photon on sky',TUNIT='pixel'
fxbaddcol,5,head,xpix[0],'DETX','Detector X position of photon',TUNIT='pixel'
fxbaddcol,6,head,ypix[0],'DETY','Detector Y position of photon',TUNIT='pixel'
fxbaddcol,7,head,egrid[0],'ENERGY','Photon energy',TUNIT='keV'

; add astrometry information
SXADDPAR,head,"HDUCLASS",'OGIP'
SXADDPAR,head,"INSTRUME",Telescope,"Mission"
SXADDPAR,head,"RADECSYS","KF5","World Coordinate System"
SXADDPAR,head,"EQUINOX",2000,"Equinox for coordinate system"
SXADDPAR,head,"RA_NOM",rdcenter(0),"Nominal RA"
SXADDPAR,head,"DEC_NOM",rdcenter(1),"Nominal DEC"
SXADDPAR,head,"RA_PNT",aimpoint(0)
SXADDPAR,head,"DEC_PNT",aimpoint(1)
SXADDPAR,head,"PA_PNT",roll
SXADDPAR,head,"RA_PNTE",0.000
SXADDPAR,head,"DEC_PNTE",0.000
SXADDPAR,head,"PA_PNTE",0.000
SXADDPAR,head,"TCRPX3",0.0
SXADDPAR,head,"TCRPX4",0.0
SXADDPAR,head,"TCRVL3",rdcenter(0)
SXADDPAR,head,"TCRVL4",rdcenter(1)
SXADDPAR,head,"TCDLT3",-dpixx/3600.
SXADDPAR,head,"TCDLT4",dpixy/3600.
SXADDPAR,head,"TCTYP3","RA---TAN"
SXADDPAR,head,"TCTYP4","DEC--TAN"
SXADDPAR,head,"TCRPC5",0.0
SXADDPAR,head,"TCRPC6",0.0
SXADDPAR,head,"TCRVL5",0.0
SXADDPAR,head,"TCRVL6",0.0
SXADDPAR,head,"TCDLT5",-1.0
SXADDPAR,head,"TCDLT6",1.0

; write data into extension
fxbcreate,lu,filename,head,1
count=0l
c=0l

nni=long(n_elements(simspec(*,0,0)))
nnj=long(n_elements(simspec(0,*,0)))
nnk=long(n_elements(simspec(0,0,*)))

for i=0l,nni-1l do begin
    for j=0l,nnj-1l do begin
        for k=0l,nnk-1l do begin
            if simspec(i,j,k) ge 1. then begin          
                count=long(simspec(i,j,k))
                while count ge 1 do begin
                    count-=1l
                    c+=1l
                    fxbwrite,lu,[randomu(seed)*exptime*1000./3600./24.],1,c
                    fxbwrite,lu,[fix(channel(k))],2,c
                    fxbwrite,lu,[0.5*(xpix(i)+xpix(i+1))],3,c
                    fxbwrite,lu,[0.5*(ypix(j)+ypix(j+1))],4,c
                    fxbwrite,lu,[0.5*(xpix(i)+xpix(i+1))],5,c
                    fxbwrite,lu,[0.5*(ypix(j)+ypix(j+1))],6,c
                    fxbwrite,lu,[0.5*(egrid(k)+egrid(k+1))],7,c
                endwhile
            endif
            if ceil(verbose) ge 3 and i eq 0l and j eq 0l and k eq 0l then $
              print,format='($,"Write events file..... 000.00% complete")'
            if ceil(verbose) ge 3 and (i gt 0l or j gt 0l or k gt 0l) then $
              print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
              float(i*nnj*nnk + j*nnk + k)/float(nni*nnj*nnk)*100.
        endfor
        if ceil(verbose) eq 2 and i eq 0l and j eq 0l then $
          print,format='($,"Write events file..... 000.00% complete")'
        if ceil(verbose) eq 2 and (i gt 0l or j gt 0l) then $
          print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
          float(i*nnj + j)/float(nni*nnj)*100.
    endfor
    if ceil(verbose) eq 1 and i eq 0l then $
      print,format='($,"Write events file..... 000.00% complete")'
    if ceil(verbose) eq 1 and i gt 0l then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(i)/float(n_elements(simspec(*,0,0)))*100.
endfor
if verbose ne 0 then $
   print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),$
   100.0
fxbfinish,lu
free_lun,lu

end
