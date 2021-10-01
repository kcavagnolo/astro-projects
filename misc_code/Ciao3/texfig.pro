pro texfig,figure,caption,outfile_root,figure2=figure2,eps_scale=eps_scale
;-----------------------------------------------------------------------
;
; Name: TEXFIG
;
; Purpose: Makes a latexed file from a given eps figure and caption
;          
; Inputs: figure - name of EPS file
;	  caption - text of caption
;
; Optional inputs: outfile_root - name of output file root (e.g. 'Fig1';
;				  defaults to root of the figure file)
;		   figure2 - name of second EPS file; figures will plot
;			     side-by-side
;		   eps_scale - scale factor used for plotone
;		 
; Keywords: none
;
; Comments: 
;           
; Revision history:
;       written by DR, 2005-4-14
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np gt 3) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'texfig, figure, caption [, outfile_root, figure2=figure2, eps_scale=eps_scale]'
   return   
endif


;
; Set default outfile name
;
if (n_elements(outfile_root) eq 0) then begin
   pos=strpos(figure,'.eps')   
   root=strmid(figure,0,pos) 
   outfile_root=root
endif 
texfile=outfile_root+'.tex'
dvifile=outfile_root+'.dvi'
psfile=outfile_root+'.ps'


;
; Create header
;
get_lun,unit
openw,unit,texfile
printf,unit,'%'
printf,unit,'% Plot of '+figure
printf,unit,'%'
printf,unit,' '
printf,unit,'\documentclass{aastex}'
printf,unit,'\begin{document}'
printf,unit,' '


;
; Add figure
;
if (n_elements(figure2) eq 0) then begin
   printf,unit,'\begin{figure}'
   if (n_elements(eps_scale) ne 0) then printf,unit,'\epsscale{'+strtrim(string(eps_scale),2)+'}'
   printf,unit,'\plotone{'+figure+'}'
   printf,unit,'\caption{'+caption+'}'
   printf,unit,'\end{figure}'
   printf,unit,' '
endif else begin
   printf,unit,'\begin{figure}'
   printf,unit,'\plottwo{'+figure+'}{'+figure2+'}'
   printf,unit,'\caption{'+caption+'}'
   printf,unit,'\end{figure}'
   printf,unit,' '
endelse


;
; Add ending material to outfile
;
printf,unit,'\end{document}'
close,unit
free_lun,unit 


;
; Call latex
;
cmdstring='/plato2/local/teTeX/bin/latex '+texfile
spawn,cmdstring


;
; Call dvips
;
cmdstring='dvips -t letter -o '+psfile+' '+dvifile
spawn,cmdstring


;
; Summarize
;
print,' '
print,'TEXFIG complete.'
print,'TEX file written to '+texfile
print,'Postscript file is '+psfile


;
; Return
;
return
end