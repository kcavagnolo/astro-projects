pro make_pi,channel,simspec,chmin,chmax,filename,exptime,$
            telescope,instrument,respf,ancrf,native=native,$
            verbose=verbose,mask=mask

;
;
; Sebastian Heinz, 01/17/2009
;
; make spectral fits file (PI) for later processing with standard astronomical
; data reduction tools like xspec.
;
; CALLING SEQUENCE:
;
; pro
; make_pi,channel,simspec,chmin,chmax,filename,exptime,telescope,$
;         instrument,rmf,arf,native=native
;
; input:  
;         channel: integer vector containing the spectral channel map
;         simspec:  vector or array containing counts in channel (last column
;                  corresponds to channel, first one or two columns correspond
;                  to image dimensions).
;         chmin:   integer containing minimum channel in response matrix
;                  (usually zero) 
;         chmax:   integer containing maximum channel in response matrix
;         filename: string conaining output filename
;         exptime: real scalar exposure time in ksec
;         telescope: string containing telescope name
;         instrument: string containing instrument name
;         respf:   string containing name of response matrix file
;         ancrf:   string containing name of ancillary response file
;
; output:
;         all output is written into the events file
; 
; keywords:
;         native:  if set, spectrum is calculated at native response
;                  resolution
;         verbose: if set, print out progress updates
;         mask:    a 2D array to specify a mask (values between 0 and 1) to
;                  weight different parts of the spectrum (equivalent to
;                  specifying a region file in ds9); same dimensions as
;                  counts(*,*,0)
;         ximfile: specify a xim output file (filename.dat) to read in
;                  (instead of specifying the input variables above.) Filename
;                  should still be specified for the output file.
;

; initialize header
fxhmake,head,/extend,/initialize
mjd=systime(0,/JULIAN)-2400000.5
head2=head
fxwrite,filename,head

; collapse image onto spectral dimension
if (size(simspec))(0) ge 2 then nx=(size(simspec))(1) else nx=1
if (size(simspec))(0) ge 3 then ny=(size(simspec))(2) else ny=1
nz=n_elements(channel)-1
if keyword_set(mask) then mask=mask else mask=replicate(1.d,nx,ny,nz)

simspec=reform(simspec,nx,ny,nz)*mask
if (size(simspec))(0) gt 1 then counts=total(simspec,1) else counts=simspec
if (size(counts))(0) gt 1 then counts=total(counts,1)

if keyword_set(native) then native=native else native=0
if keyword_set(verbose) then verbose=verbose else verbose=0

; re-grid to instrument resolution
if native eq 0 then begin
    chs=chmin+indgen(chmax-chmin+1)
    regrid,counts,channel,octs,chs,1
    channel=chs
endif

; initialize binary extension and header
head=head2
nchs=long(chmax)-long(chmin)+1l

fxbhmake,head,nchs
fxbaddcol,1,head,[1],'CHANNEL','Photon arrival time'
fxbaddcol,2,head,[1.0],'COUNTS','Observed counts in channel',TUNIT='counts'
fxbaddcol,3,head,[1],'GROUPING','Bin pha channels'

SXADDPAR,head,"EXTNAME","SPECTRUM",'Name of binary extension'
SXADDPAR,head,"HDUCLASS",'OGIP','Format conforms to OGIP'
SXADDPAR,head,"HDUCLAS1","SPECTRUM"
SXADDPAR,head,"HDUCLAS2","TOTAL"
SXADDPAR,head,"HDUCLAS3","COUNTS"
SXADDPAR,head,"HDUVERS",'1.2.1'
SXADDPAR,head,"CREATOR","XIM - Version 0.1","tool that created this output"
SXADDPAR,head,"FILENAME",filename
SXADDPAR,head,"DATE",'2009-01-10'
SXADDPAR,head,"TELESCOP",telescope,"Telescope"
SXADDPAR,head,"INSTRUME",instrument,"Instrument"
SXADDPAR,head,"FILTER","none","Filter"
SXADDPAR,head,"EXPOSURE",exptime*1000.,"Exposure time"
SXADDPAR,head,"BACKSCAL",1.0,"Background scale factor"
SXADDPAR,head,"AREASCAL",1.0,"Area scale factor"
SXADDPAR,head,"CORRSCAL",0.0,"Correction scaling factor"
SXADDPAR,head,"OBJECT",filename
SXADDPAR,head,"BACKFILE","NONE","Backgroud file"
SXADDPAR,head,"CORRFILE","none","Correction file"
SXADDPAR,head,"RESPFILE",respf,"Redistribution matrix file"
if ancrf eq "" then $
  SXADDPAR,head,"ANCRFILE","none","Ancillary response file" else $
  SXADDPAR,head,"ANCRFILE",ancrf,"Ancillary response file"
SXADDPAR,head,"CHANTYPE","PI"
SXADDPAR,head,"DETCHANS",long(chmax)-long(chmin)+1l,$
  "Total number of detector channels"
SXADDPAR,head,"TLMIN",chmin,"Lowest possible channel number"
SXADDPAR,head,"TLMAX",chmax,"Largest possible channel number"
SXADDPAR,head,"STAT_ERR",0,"No stat error spcf."
SXADDPAR,head,"POISSERR","T","Poisson errors?"
SXADDPAR,head,"SYS_ERR",0,"No sys. err spcf."
SXADDPAR,head,"QUALITY",0,"No data qual info spcf."

; write data into extension
fxbcreate,lu,filename,head,1
counter=0
c=0l

; loop over all instrument channels to write spectrum
nchs=n_elements(channel)
channel=fix(channel(0l:nchs-2))
for i=chmin,chmax do begin
    c+=1l
    inner=where(channel eq i)
    if inner(0) ne -1 then cts=(total(counts(inner))) else cts=0.
    fxbwrite,lu,[fix(i)],1,c
    fxbwrite,lu,[float(cts)],2,c
    fxbwrite,lu,[1],3,c
    counter+=float(cts)
    if verbose gt 0 and i eq chmin then $
      print,format='($,"Write spectrum file... 000.00% complete")'
    if verbose gt 0 and i gt chmin then $
      print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
      float(i)/float(chmax-chmin+1l)*100.
endfor
if verbose ne 0 then $
   print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),$
   100.0

; clean up
fxbfinish,lu
free_lun,lu,exit_status=ext,/force

end
