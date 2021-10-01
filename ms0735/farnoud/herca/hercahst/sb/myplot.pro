pro myplot, filename, ps = ps
;**************************************************************************
; This program reads plots useful indexes from the STSDAS data file 
; The keyword (/ps) can be set if a hard copy is desired
; FK; May, 2007
;**************************************************************************

!p.thick=2.5
!P.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5

if (N_params() EQ 0) then begin
	print, 'Syntax - myplot, STSDASdatafilename, backgronderrorvalue '
	print, '       - /ps for hard copy'
	return
endif

; Read in the data
tab_read, filename, tcb, table, header
rad = tab_val(tcb,table,1)

int = tab_val(tcb,table,2)
interr = tab_val(tcb,table,3)
ell = tab_val(tcb,table,6)
ellerr = tab_val(tcb,table,7)
pa = tab_val(tcb,table,8)
paerr = tab_val(tcb,table,9)

x = tab_val(tcb,table,10)
xerr = tab_val(tcb,table,11)
y = tab_val(tcb,table,12)
yerr = tab_val(tcb,table,13)
mag = tab_val(tcb,table,18)
maglerr = tab_val(tcb,table,19)
maguerr = tab_val(tcb,table,20)
tfluxe = tab_val(tcb,table,21)
tmag = tab_val(tcb,table,23)
npix = tab_val(tcb,table,35)

a3 = tab_val(tcb,table,27)
a3err = tab_val(tcb,table,28)
b3 = tab_val(tcb,table,29)
b3err = tab_val(tcb,table,30)
a4 = tab_val(tcb,table,31)
a4err = tab_val(tcb,table,32)
b4 = tab_val(tcb,table,33)
b4err = tab_val(tcb,table,34)

s = size(rad,/dimensions)
	openw,1,'hercafrepmf',/append
	printf,1,'; 1. Radius (px)'
	printf,1,'; 2. Ellipticity'
	printf,1,'; 3. Ellipticity Error'
	printf,1,'; 4. Position Angle'
	printf,1,'; 5. Position Angle Error'
	printf,1,'; 6. Isophotal Surface Brightness (mag/arcsec^2)'
	printf,1,'; 7. Isophotal Surface Brightness Lower Error (mag/arcsec^2)'
	printf,1,'; 8. Isophotal Surface Brightness Upper Error (mag/arcsec^2)'
	printf,1,'; 9. Anulas Flux (counts)'
	printf,1,'; 10. Number of Pixels in Anulus'
	printf,1
for i = 0, (s[0]-1) do begin
	printf,1,rad[i],ell[i],ellerr[i],pa[i],paerr[i],mag[i],maglerr[i],maguerr[i],tfluxe[i], npix[i]
endfor
close,1


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set_plot, 'x'
!p.multi = 0
!fancy = 7

; Decides to either plot to a file or on screen
if keyword_set(ps) then begin
	plotname = ' '
	read, plotname, prompt = 'Enter Name.ps: '
	!p.multi = 0
	set_plot, 'ps'
	device, filename= plotname, /color,/times, xs=60, ys=40, font_size=10,/bold,/encapsulated
	loadct, 13
	!p.multi = 0

; All plots involving Radius as the independent variable
	!p.multi = [12,4,3]
	plot,rad,int,psym=symcat(9),xtitle='Radius (px)',ytitle='Intensity',/xlog,/ylog
	oploterror,rad,int,interr,psym=3,/nohat
	!p.multi = [11,4,3]
	plot,(rad)^0.25,int,psym=symcat(9),xtitle='Radius!E1/4!N (px)',ytitle='Intensity',/ylog
	oploterror,(rad)^0.25,int,interr,psym=3,/nohat
	!p.multi = [10,4,3]
	plot,rad,ell,psym=symcat(16),xtitle='Radius (px)',ytitle='Ellipticity',/xlog
	oploterror,rad,ell,ellerr,psym=3,/nohat
	!p.multi = [9,4,3]
	plot,rad,pa,psym=symcat(16),xtitle='Radius (px)',ytitle='Position Angle',/xlog
	oploterror,rad,pa,paerr,psym=3,/nohat

	!p.multi = [8,4,3]
	plot,rad,tfluxe,psym=symcat(16),xtitle='Radius (px)',ytitle='Total Flux Enclosed by Isophote',/ylog
	!p.multi = [7,4,3]
	ymax = max(mag)
	ymin = min(mag)
	plot,rad,mag,psym=symcat(9),xtitle='Radius (px)',ytitle='!4l!3',yrange=[ymax,ymin],/xlog
	oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
	oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
	!p.multi = [6,4,3]
	plot,rad,x,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='x-center',/xstyle,/ystyle,/xlog
	oploterror,rad,x,xerr,psym=symcat(16),/nohat
	!p.multi = [5,4,3]
	plot,rad,y,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='y-center',/xstyle,/ystyle,/xlog
	oploterror,rad,y,yerr,psym=symcat(16),/nohat
	
	!p.multi = [4,4,3]
	plot,rad,a3,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"A" 3rd Harmonic',/xlog
	oploterror,rad,a3,a3err,psym=symcat(16),/nohat
	!p.multi = [3,4,3]
	plot,rad,b3,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"B" 3rd Harmonic',/xlog
	oploterror,rad,b3,b3err,psym=symcat(16),/nohat
	!p.multi = [2,4,3]
	plot,rad,a4,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"A" 4th Harmonic',/xlog
	oploterror,rad,a4,a4err,psym=symcat(16),/nohat
	!p.multi = [1,4,3]
	plot,rad,b4,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"B" 4th Harmonic',/xlog
	oploterror,rad,b4,b4err,psym=symcat(16),/nohat
	device, /close
endif else begin

; All plots involving Radius as the independent variable
	window, xs=1200, ys=900
	!p.multi = [12,4,3]
	plot,rad,int,psym=symcat(9),xtitle='Radius (px)',ytitle='Intensity',/xlog,/ylog
	oploterror,rad,int,interr,psym=3,/nohat
	!p.multi = [11,4,3]
	plot,(rad)^0.25,int,psym=symcat(9),xtitle='Radius!E1/4!N (px)',ytitle='Intensity',/ylog
	oploterror,(rad)^0.25,int,interr,psym=3,/nohat
	!p.multi = [10,4,3]
	plot,rad,ell,psym=symcat(16),xtitle='Radius (px)',ytitle='Ellipticity',/xlog
	oploterror,rad,ell,ellerr,psym=3,/nohat
	!p.multi = [9,4,3]
	plot,rad,pa,psym=symcat(16),xtitle='Radius (px)',ytitle='Position Angle',/xlog
	oploterror,rad,pa,paerr,psym=3,/nohat

	!p.multi = [8,4,3]
	plot,rad,tfluxe,psym=symcat(16),xtitle='Radius (px)',ytitle='Total Flux Enclosed by Isophote',/ylog
	!p.multi = [7,4,3]
	ymax = max(mag)
	ymin = min(mag)
	plot,rad,mag,psym=symcat(9),xtitle='Radius (px)',ytitle='!4l!3',yrange=[ymax,ymin],/xlog
	oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
	oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
	!p.multi = [6,4,3]
	plot,rad,x,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='x-center',/xstyle,/ystyle,/xlog
	oploterror,rad,x,xerr,psym=symcat(16),/nohat
	!p.multi = [5,4,3]
	plot,rad,y,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='y-center',/xstyle,/ystyle,/xlog
	oploterror,rad,y,yerr,psym=symcat(16),/nohat

	!p.multi = [4,4,3]
	plot,rad,a3,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"A" 3rd Harmonic',/xlog
	oploterror,rad,a3,a3err,psym=symcat(16),/nohat
	!p.multi = [3,4,3]
	plot,rad,b3,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"B" 3rd Harmonic',/xlog
	oploterror,rad,b3,b3err,psym=symcat(16),/nohat
	!p.multi = [2,4,3]
	plot,rad,a4,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"A" 4th Harmonic',/xlog
	oploterror,rad,a4,a4err,psym=symcat(16),/nohat
	!p.multi = [1,4,3]
	plot,rad,b4,psym=2,symsize=0.5,xtitle='Radius (px)',ytitle='"B" 4th Harmonic',/xlog
	oploterror,rad,b4,b4err,psym=symcat(16),/nohat
endelse
set_plot, 'x'

end