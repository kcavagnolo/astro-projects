pro obs_info
;-----------------------------------------------------------------------
;
; Name: OBS_INFO
;
; Purpose: Reads in evt1 and evt2 FITS files and makes a text file with
;          useful observation info.
;          
;          
; Inputs:  none
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by D&L, 2002-11-01
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 0) or (np gt 0)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'obs_info'
   return   
endif


;
; Read in headers
;
evt1file=findfile('secondary/acisf*evt1.fits')
evt2file=findfile('primary/acisf*evt2.fits')
hd1=headfits(evt1file(0),exten=1)
hd2=headfits(evt2file(0),exten=1)



;
; Get CALDBVER, FP_TEMP, DETNAM, ASCDSVER out of evt1 header
;
caldbver=sxpar(hd1,'CALDBVER','Parameter CALDBVER not found')
fptemp=sxpar(hd1,'FP_TEMP','Parameter FP_TEMP not found')
detnam=sxpar(hd1,'DETNAM','Parameter DETNAM not found')
ascdsver=sxpar(hd1,'ASCDSVER','Parameter ASCDVER not found')


;
; Get info out of evt2 header
;
readmode=sxpar(hd2,'READMODE','Parameter READMODE not found')
datamode=sxpar(hd2,'DATAMODE','Parameter DATAMODE not found')
dateobs=sxpar(hd2,'DATE-OBS','Parameter DATE-OBS not found')
dateend=sxpar(hd2,'DATE-END','Parameter DATE-END not found')
exposure=sxpar(hd2,'EXPOSURE','Parameter EXPOSURE not found')
;gainfile=sxpar(hd2,'GAINFILE','Parameter GAINFILE not found')
obsid=sxpar(hd2,'OBS_ID','Parameter OBS_ID not found')
grating=sxpar(hd2,'GRATING','Parameter GRATING not found')
object=sxpar(hd2,'OBJECT','Parameter OBJECT not found')
observer=sxpar(hd2,'OBSERVER','Parameter OBSERVER not found')
ranom=sxpar(hd2,'RA_NOM','Parameter RA_NOM not found')
decnom=sxpar(hd2,'DEC_NOM','Parameter DEC_NOM not found')
equinox=sxpar(hd2,'EQUINOX','Parameter EQUINOX not found')


;
; Call Mike's tool check_aspect
;
aofffile=findfile('secondary/*aoff1.fits')
aoffoutfile='aspect_qual_obsid'+strtrim(string(obsid),2)+'.ps'
check_aspect,aofffile(0)


;
; Set output file name
;
outfile='obs_info_'+strtrim(string(obsid),2)+'.txt'


;
; Open outfile
;
get_lun,unit
openw,unit,outfile
printf,unit,'Summary for Observation '+strtrim(string(obsid),2)
printf,unit,'------------------------------------------'
printf,unit,'------------------------------------------'
printf,unit,' '
printf,unit,'Observation info:
printf,unit,!stime
printf,unit,' '
printf,unit,'Obs_ID           : ',obsid
printf,unit,'Object Name      : ',object
printf,unit,'Nominal RA       : ',ranom
printf,unit,'Nominal DEC      : ',decnom
printf,unit,'Equinox          : ',equinox
printf,unit,'Observer Name    : ',observer
printf,unit,' '
printf,unit,'Observation Date : ',dateobs
printf,unit,'Observation End  : ',dateend
printf,unit,'Exposure Time    : ',exposure
printf,unit,' '
printf,unit,'Grating          : ',grating
;printf,unit,'Gain File        : ',gainfile
printf,unit,'Read Mode        : ',readmode
printf,unit,'Data Mode        : ',datamode
printf,unit,' '
printf,unit,'CALDB version    : ',caldbver
printf,unit,'Focal Plane Temp : ',fptemp
printf,unit,'Detector Name    : ',detnam
printf,unit,'ASCDS version    : ',ascdsver
printf,unit,' '
printf,unit,'Aspect plot      : ',aoffoutfile
printf,unit,' '


;
; Close logfile
;
printf,unit,'------------------------------------------'
printf,unit,' '
close,unit
free_lun,unit


;
; Print status info to screen
;
print,'OBS_INFO complete.'
print,'Please see '+outfile+' for details.'


;
; Return to IDL
;
return
end
