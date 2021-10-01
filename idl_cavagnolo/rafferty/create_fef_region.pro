pro create_fef_region,eventfile,ccdtemp=ccdtemp,outfile=outfile
;-----------------------------------------------------------------------
; Name: CREATE_FEF_REGION
;
; Purpose: Computes a table of all relevant FEF files for a given 
;          event file as well as the number of counts in the 
;          individual FEF grid cells.
;
; Inputs:	eventfile -- string containing name of ACIS event file
;		
; Optional inputs:	
;	          ccdtemp -- temperature of CCD (defaults to -110)
;                 outfile -- string containing output file name
;			     (defaults to FEF_TABLE.DAT)
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 1-18-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 1)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'create_fef_region,eventfile,[ccdtemp=ccdtemp,outfile=outfile]'
   return   
endif 


;
; Set defaults
;
if (n_elements(ccdtemp) eq 0) then ccdtemp=-110
if (n_elements(outfile) eq 0) then outfile='fef_table.dat'


;
; Define some miscellaneous stuff
;
nodestring=['a','b','c','d']
datestring='D1999-12-09'


;
; Do some parameter checking
;
if ((ccdtemp ne -90) and (ccdtemp ne -100) and (ccdtemp ne -110)) then begin
   print,string(7B),'ERROR: Value of CCDTEMP must be -90, -100, or -110'
   return
endif


;
; Construst temperature string
;
tstring='FP'+strtrim(string(ccdtemp),2)


;
; Define bin size for each chip
;
case ccdtemp of
     -110: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[32,32,32,32,32,64,32,32,32,32]
           end
     -100: begin
           xbin=[-1,-1,-1,-1,-1,64,-1,32,-1,-1]
           ybin=[-1,-1,-1,-1,-1,64,-1,32,-1,-1]
           end
      -90: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[1024,1024,1024,1024,1024,64,1024,32,1024,1024]
           end
     else: begin
           xbin=[256,256,256,256,256,64,256,32,256,256]
           ybin=[32,32,32,32,32,64,32,32,32,32]
           end
endcase


;
; Extract relevant columns from event file
;
events=mrdfits(eventfile,1)
totcnts=float(n_elements(events))
ccdid=events.ccd_id
nodeid=events.node_id
chipx=events.chipx
chipy=events.chipy


;
; Find number of unique CCDs in event list
;
i=sort(ccdid)
temp=ccdid(i)
chips=temp(uniq(temp))
nchips=n_elements(chips)


;
; Open output file
;
space='                                                  '
space2='--------------------------------------------------'
fmt='$(2x,i3,2x,i4,2x,i3,2x,i3,3x,a50,3x,i8,3x,f8.6)'
fmt2='$(2x,a3,2x,a4,2x,a3,2x,a3,3x,a50,3x,a8,3x,a8)'
get_lun,unit
openw,unit,outfile
printf,unit,fmt2,'CCD','Node','IX','IY','FEF Filename'+space,'Counts', $
            'Fraction'
printf,unit,fmt2,'---','----','--','--',space2,'------','--------'
printf,unit,' '


;
; Loop over the number of chips
;
for ii=0,nchips-1 do begin

;
;   Define bin sizes for this chip
;
    xcbin=xbin(chips(ii))
    ycbin=ybin(chips(ii))

;
;   Identify all events on this chip
;
    i=where(ccdid eq chips(ii))

;
;   Identify the number of nodes on this chip
;
    temp=nodeid(i)
    j=sort(temp)
    temp=temp(j)
    nodes=temp(uniq(temp))
    nnodes=n_elements(nodes)

;
;   Define chip string
;
    cstring=strtrim(string(chips(ii)),2)

;
;   Loop over nodes
;
    for jj=0,nnodes-1 do begin


;
;       Identify all events on this chip and node
;
;           the value-1 is because the FEF cells are
;           numbered from 0
;
        j=where( (ccdid eq chips(ii)) and (nodeid eq nodes(jj)) )

        x=chipx(j)-(256*nodes(jj))-1
        y=chipy(j)-1

;
;       Convert to FEF cell indices
;
        x=fix(x/xcbin)
        y=fix(y/ycbin)

;
;       Define node string
;
        nstring=nodestring(nodes(jj))

;
;       Loop over cells on this node
;
        for kk=min(x),max(x) do begin

;
;           Construct X index string
;
            if (kk lt 10) then xstring='0'+strtrim(string(kk),2)   $
                          else xstring=strtrim(string(kk),2) 


            for ll=min(y),max(y) do begin
;
;               Construct Y index string
;
                if (ll lt 10) then ystring='0'+strtrim(string(ll),2)   $
                              else ystring=strtrim(string(ll),2) 


;
;               Construct FEF filename
;
                file='acis'+cstring+nstring+'_x'+             $
                      xstring+'_y'+ystring+'_'+tstring+'_'+   $
                      datestring+'fef_piN0001.fits'

;
;               Calculate fraction of counts in cell
;
                k=where( (x eq kk) and (y eq ll) )
                fract=n_elements(k)/totcnts

;
;               If there are counts in this cell, print it out
;
                if (k(0) ne -1) then begin
                                printf,unit,fmt,chips(ii),nodes(jj),kk,ll, $
                                            file,n_elements(k),fract
                endif

;
;           Next Y cell
;
            endfor
;
;       Next X cell
;
        endfor

;
;   Next node
;
    endfor

;
; Next chip
;
endfor


;
; Close output file
;
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
