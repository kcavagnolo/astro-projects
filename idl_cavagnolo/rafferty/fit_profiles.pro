pro fit_profiles,objstring,datadir=datadir,fitrange=fitrange,              $
                 logfile=logfile,texfile

;-----------------------------------------------------------------------
; Name: FIT_PROFILES
;
; Purpose: Fit Beta models to set of surface brightness profiles
;          
; Inputs:  objstring -- template search string
;
; Comments: 
;           
; Revision history:
;       written by Michael Wise, 10-25-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'fit_profiles,objstring,datadir=datadir,fitrange=fitrange,', $
   '             logfile=logfile,texfile'
   return   
endif


;
; Handle defaults
;
if (n_elements(datadir) eq 0) then datadir='.'
if (n_elements(logfile) eq 0) then logfile='fit.log'
if (n_elements(texfile) eq 0) then texfile='fit.tex'
if (n_elements(fitrange) eq 0) then fitrange=1000.0


;
; Define miscellaneous quantities
;
space=''
space1='          '
space5=space1+space1+space1+space1+space1
ciaoversion='2.0b'
progversion='1.0b1'


;
; Open logfile 
;
get_lun,unit
openw,unit,logfile
printf,unit,' '
printf,unit,'Date             : '+!stime
printf,unit,'Program Version  : '+progversion
printf,unit,'CXCDS Version    : '+ciaoversion
printf,unit,' '
printf,unit,'File filter      : ',objstring
printf,unit,'Fit range        : ',fitrange
printf,unit,'Data directory   : ',datadir
printf,unit,'Fit log file     : ',logfile
printf,unit,' '



;
; Locate all images in the DATADIR directory which match OBJSTRING
;
files=findfile(datadir+'/'+objstring)
numfiles=n_elements(files)


;
; Make sure we actually found something
;
if (files(0) eq '') then begin
   print,string(7B),'ERROR: ', $
   'No files matching OBJSTRING were detected'
   return   
endif



;
; Create set of output filenames
;
ofiles=strarr(numfiles)
pfiles=strarr(numfiles)

for i=0,numfiles-1 do begin

    j1=strlen(datadir+'/')
    j2=strlen(files(i))
    curname=strmid(files(i),j1,j2)

    j3=strpos(curname,'sb.dat')

    if (j3 eq -1) then begin
       ofiles(i)=curname+'_fit'
       pfiles(i)=curname+'_ps'
    endif else begin
       ofiles(i)=strmid(curname,0,j3)+'fit.dat'
       pfiles(i)=strmid(curname,0,j3)+'fit.ps'
    endelse

endfor


;
; Record which files will be fit into the logfile
;
printf,unit,' '
printf,unit,'Files to be fit:'
printf,unit,' '
format0='$(a5,2(3x,a40))'
format1='$(i5,2(3x,a40))'
printf,unit,format0,'Index','Input file'+space5,'Output file'+space5
printf,unit,format0,'-----','----------'+space5,'-----------'+space
for i=0,numfiles-1 do begin
    printf,unit,format1,i,files(i)+space5,ofiles(i)+space5
endfor
printf,unit,' '
printf,unit,' '



;
; Create arrays to hold fit values
;
norm=fltarr(numfiles)
enorm=fltarr(numfiles)
rc=fltarr(numfiles)
erc=fltarr(numfiles)
beta=fltarr(numfiles)
ebeta=fltarr(numfiles)
back=fltarr(numfiles)
eback=fltarr(numfiles)
niter=intarr(numfiles)
chisq=fltarr(numfiles)



;
; Fit input profiles using FIT_BETA
;
for i=0,numfiles-1 do begin

    printf,unit,' '
    printf,unit,'Fitting profile: ',files(i)
    printf,unit,' '

    temp=fitrange

    cmdstring='fit_beta,"'+files(i)+'",'+                            $
                          'outfile="'+ofiles(i)+'",'+                $
                          'plotfile="'+pfiles(i)+'",'+               $
                          'fitrange='+strtrim(string(temp),2)

    fit_beta,files(i),outfile=ofiles(i),plotfile=pfiles(i),          $
             fitrange=temp,parms=parms,stats=stats

    printf,unit,cmdstring
    printf,unit,' '


;
;   Save fit values into big array
;
    norm(i)=parms(0,0)
    enorm(i)=parms(0,1)
    rc(i)=parms(1,0)
    erc(i)=parms(1,1)
    beta(i)=parms(2,0)
    ebeta(i)=parms(2,1)
    back(i)=parms(3,0)
    eback(i)=parms(3,1)
    niter(i)=stats(1)
    chisq(i)=stats(0)

endfor



;
; Open output TeX file
;
get_lun,unit1
openw,unit1,texfile


;
; Define format strings
;
fmtl0='$(a45,2(3x,a11),2(3x,a11),2(3x,a11),2(3x,a11),3x,a6,3x,a11)'
fmtl1='$(a45,2(3x,e11.5),2(3x,f11.5),2(3x,f11.5),2(3x,e11.5),3x,i6,3x,f11.5)'

fmtt0='$(a45,2(1x,"&",1x,a11),2(1x,"&",1x,a11),'+                     $
      '2(1x,"&",1x,a11),2(1x,"&",1x,a11),1x,"&",1x,a6,'+              $
      '1x,"&",1x,a11,1x,"\\")'
fmtt1='$(a45,2(1x,"&",1x,e11.5),2(1x,"&",1x,f11.5),'+                     $
      '2(1x,"&",1x,f11.5),2(1x,"&",1x,e11.5),1x,"&",1x,i6,'+              $
      '1x,"&",1x,f11.5,1x,"\\")'


;
; Write table headers to log file
;
printf,unit,' '
printf,unit,'Fit Results'
printf,unit,'-----------'
printf,unit,' '

printf,unit,fmtl0,'File','Norm','Enorm','Core','Ecore','Beta','Ebeta', $
                  'Back','Eback','Niter','Chisq'
printf,unit,fmtl0,'----','----','-----','----','-----','----','-----', $
                  '----','-----','-----','-----'


;
; Write TeX table front material
;
printf,unit1,'%'
printf,unit1,'%'
printf,unit1,'% Surface Brightness Fit results'
printf,unit1,'%'
printf,unit1,'%'
printf,unit1,'% Created: ',!stime
printf,unit1,'%'
printf,unit1,'%'
printf,unit1,'\documentclass[12pt]{article}'
printf,unit1,' '
printf,unit1,'\input symboldef'
printf,unit1,' '
printf,unit1,'\textheight=6.5truein'
printf,unit1,'\textwidth=9.25truein'
;printf,unit1,'\textheight=9.25truein'
;printf,unit1,'\textwidth=6.5truein'
printf,unit1,'\hoffset=-0.5truein'
printf,unit1,'\voffset=-1.0truein'
printf,unit1,' '
printf,unit1,'\begin{document}'
printf,unit1,' '
printf,unit1,'\renewcommand\baselinestretch{1}'
printf,unit1,'\scriptsize'
printf,unit1,'\scriptsize'
printf,unit1,' '
printf,unit1,'\centerline{\Large\bf Surface Brightness Fit Results}'
printf,unit1,'\vspace{0.3truein}'
printf,unit1,' '
printf,unit1,'\begin{center}'
printf,unit1,'\begin{tabular}{lllllllllll}'
printf,unit1,'%\multicolumn{11}{c}{Surface Brightness Fit Results} \\'
printf,unit1,'%\multicolumn{11}{c}{~} \\'
printf,unit1,'\hline \hline'
printf,unit1,' '

printf,unit1,fmtt0,'File','Norm','Enorm','Core','Ecore','Beta','Ebeta', $
                   'Back','Eback','Niter','Chisq'
printf,unit1,'\hline'

for i=0,numfiles-1 do begin

    printf,unit,fmtl1,files(i),norm(i),enorm(i),rc(i),erc(i),              $
                    beta(i),ebeta(i),back(i),eback(i),niter(i),chisq(i)

    printf,unit1,fmtt1,files(i),norm(i),enorm(i),rc(i),erc(i),             $
                    beta(i),ebeta(i),back(i),eback(i),niter(i),chisq(i)

endfor


;
; Write TeX table end material
;
printf,unit1,'\hline\hline'
printf,unit1,'\end{tabular}'
printf,unit1,'\end{center}'
printf,unit1,' '
printf,unit1,' '
printf,unit1,'\end{document}'


;
; Close TeX file
;
close,unit1
free_lun,unit1


;
; Hack to patch any underscore characters which TeX hates
;
spawn,'sed "s%\_%\\\_%g" '+texfile+' > tempfile',result
spawn,'mv -f tempfile '+texfile,/sh


;
; Close logfile
;
printf,unit,' '
printf,unit,' '
printf,unit,' '
printf,unit,'Fitting complete'
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
