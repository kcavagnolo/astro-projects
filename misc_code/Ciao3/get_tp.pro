pro get_tp,radius,ignore=ignore,nearest=nearest,extrap=extrap,last=last,spectra_dir=spectra_dir,log=log
;------------------------------------------------------------------------------
; Name:  GET_TP
;
; Purpose:  Get the temperature, density, abundance, and pressure at the 
;           specified radius. Uses a MPFIT to fit power laws to density
;	    and pressure. Uses a linear interpolation for temperature and
;	    abundance.
;
; Inputs:  radius - the radius or semi-major axis in arcsec
;
;
; Comments:
;
;
; Revision history:
;       written by D&L, 2003-06-25
;	added density and abundance output (DR), 2003-10-01
;	completely rewritten for Ciao 3.2 and new pipeline (DR), 2005-2-20
;
;------------------------------------------------------------------------------   
;
; Check for correct number of parameters
;
np=n_params(0)
if (np ne 1) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'get_tp, radius (arcsec), [, ignore = region number, /nearest,', $
   		 '/extrap, /last, spectra_dir = spectra_dir, /log]'
   return   
endif


;
; Read data from "properties_deproj.dat"
;
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'
readcol,spectra_dir+'/properties_deproj.dat',/silent,skipline=23,ravg,rkpc,kt,ktlo,kthi,n_e,nelo,nehi,p,plo,phi,s,slo,shi,ab,ablo,abhi,tc,tclo,tchi
nT=size(ravg,/n_elements)   ; number of regions fitted
dkTg_avg=(ktlo+kthi)/2.0
dn_eg_avg=(nelo+nehi)/2.0
dAbg_avg=(ablo+abhi)/2.0
dpresg_avg=(plo+phi)/2.0


;
; Ignore last region unless /last is set
;
if not keyword_set(last) then begin
   ravg=ravg[0:nT-2]
   kT=kT[0:nT-2]
   ktlo=kTlo[0:nT-2]
   kthi=kThi[0:nT-2]
   n_e=n_e[0:nT-2]
   nelo=nelo[0:nT-2]
   nehi=nehi[0:nT-2]
   p=p[0:nT-2]
   plo=plo[0:nT-2]
   phi=phi[0:nT-2]
   ab=ab[0:nT-2]
   ablo=ablo[0:nT-2]
   abhi=abhi[0:nT-2]      
endif
nT=n_elements(ravg)	; renormalize


;
; Ignore specified region
;
if (n_elements(ignore) ne 0) then begin
   ignore_reg=fix(ignore)
   if (ignore_reg eq 0) then goto, ignore_none
   if (ignore_reg eq 1) then begin
      ravg=ravg[1:nT-1]
      kT=kT[1:nT-1]
      ktlo=kTlo[1:nT-1]
      kthi=kThi[1:nT-1]
      n_e=n_e[1:nT-1]
      nelo=nelo[1:nT-1]
      nehi=nehi[1:nT-1]
      p=p[1:nT-1]
      plo=plo[1:nT-1]
      phi=phi[1:nT-1]
      ab=ab[1:nT-1]
      ablo=ablo[1:nT-1]
      abhi=abhi[1:nT-1]      
   endif 
   if ( (ignore_reg ne 1) and (ignore_reg ne nT) ) then begin
      ravg=[ravg[0:ignore_reg-2],ravg[ignore_reg:nT-1]]
      kT=[kt[0:ignore_reg-2],kt[ignore_reg:nT-1]]
      kTlo=[ktlo[0:ignore_reg-2],ktlo[ignore_reg:nT-1]]
      kThi=[kthi[0:ignore_reg-2],kthi[ignore_reg:nT-1]]
      n_e=[n_e[0:ignore_reg-2],n_e[ignore_reg:nT-1]]
      nelo=[nelo[0:ignore_reg-2],nelo[ignore_reg:nT-1]]
      nehi=[nehi[0:ignore_reg-2],nehi[ignore_reg:nT-1]]
      p=[p[0:ignore_reg-2],p[ignore_reg:nT-1]]
      plo=[plo[0:ignore_reg-2],plo[ignore_reg:nT-1]]
      phi=[phi[0:ignore_reg-2],phi[ignore_reg:nT-1]]
      ab=[ab[0:ignore_reg-2],ab[ignore_reg:nT-1]]
      ablo=[ablo[0:ignore_reg-2],ablo[ignore_reg:nT-1]]
      abhi=[abhi[0:ignore_reg-2],abhi[ignore_reg:nT-1]]
   endif
endif
ignore_none:
nT=n_elements(ravg)	; renormalize


;
; Setup power law fitting functions for MPFIT
;
n_e_expr='P(0)*X^(P(1))'
pres_expr='P(0)*X^(P(1))'
n_e_start=[n_e[0],-1.0]
pres_start=[p[0],-1.0]


;
; Check to make sure radius is OK
;
if (radius gt max(ravg)) then begin
   print,' '
   print,' WARNING: '
   print,' The radius you entered is larger that the maximum radius ('+strtrim(string(max(ravg)),2)+'")'
   dkTg_int=(kTlo[nT-1]+kthi[nt-1])/2.0
   dn_eg_int=(nelo[nt-1]+nehi[nt-1])/2.0
   dAbg_int=(ablo[nt-1]+abhi[nt-1])/2.0
   dpresg_int=(plo[nt-1]+phi[nt-1])/2.0
   if keyword_set(extrap) then begin
      if keyword_set(nearest) then print,' The properties at this radius will be extrapolated from the fit to the outer two points.' else print,' The properties at this radius will be extrapolated from the fit.' 
      print,' '
   endif else begin
      print,' The properties at this radius will be set to those of the outer region.'
      print,' '
      kTg_int=kt[nt-1]
      n_eg_int=n_e[nt-1]
      Abg_int=ab[nt-1]
      presg_int=p[nt-1]
      goto,output
   endelse
endif
if (radius lt min(ravg)) then begin
   print,' '
   print,' WARNING: '
   print,' The radius you entered is smaller that the minimum radius ('+strtrim(string(min(ravg)),2)+'")'
   dkTg_int=(kTlo[0]+kthi[0])/2.0
   dn_eg_int=(nelo[0]+nehi[0])/2.0
   dAbg_int=(ablo[0]+abhi[0])/2.0
   dpresg_int=(plo[0]+phi[0])/2.0
   if keyword_set(extrap) then begin
      if keyword_set(nearest) then print,' The properties at this radius will be extrapolated from the fit to the inner two points.' else print,' The properties at this radius will be extrapolated from the fit.'
      print,' '
   endif else begin
      print,' The properties at this radius will be set to those of the inner region.'
      print,' '
      kTg_int=kt[0]
      n_eg_int=n_e[0]
      Abg_int=ab[0]
      presg_int=p[0]
      goto,output
   endelse
endif


;
; Next, find the points above and below for error calculation
;
nfit=nT
for i=0,nfit-1 do begin
  if (radius le ravg[i]) then begin
    lower=i-1
    low=lower
    goto,jump1
  endif
    lower=i-1
    low=lower
endfor
jump1:


;
; Interpolate to find the value of kT and P at the specified radius
;
if (lower eq -1) then low=0
if (lower eq nt-1) then low=nt-2
kTg_int=interpol(kt,ravg,radius)
if keyword_set(nearest) then n_e_result=mpfitexpr(n_e_expr,ravg[low:low+1],n_e[low:low+1],(nelo[low:low+1]+nehi[low:low+1])/2.0,n_e_start,/quiet) else n_e_result=mpfitexpr(n_e_expr,ravg,n_e,(nelo+nehi)/2.0,n_e_start,/quiet)
n_eg_int=n_e_result[0]*radius^(n_e_result[1])
Abg_int=interpol(ab,ravg,radius)
if keyword_set(nearest) then pres_result=mpfitexpr(pres_expr,ravg[low:low+1],p[low:low+1],(plo[low:low+1]+phi[low:low+1])/2.0,pres_start,/quiet) else  pres_result=mpfitexpr(pres_expr,ravg,p,(plo+phi)/2.0,pres_start,/quiet)
presg_int=pres_result[0]*radius^(pres_result[1])


;
; Next, find errors
;
if (lower eq -1) then begin
   dkTg_int=dkTg_avg(0)
   dn_eg_int=dn_eg_avg(0)
   dAbg_int=dAbg_avg(0)
   dpresg_int=dpresg_avg(0)
endif else begin
   if (lower eq nt-1) then begin
      dkTg_int=(kTlo[nT-1]+kthi[nt-1])/2.0
      dn_eg_int=(nelo[nt-1]+nehi[nt-1])/2.0
      dAbg_int=(ablo[nt-1]+abhi[nt-1])/2.0
      dpresg_int=(plo[nt-1]+phi[nt-1])/2.0
   endif else begin
      ratio=(radius-ravg(low))/(ravg(low+1)-ravg(low))
      dkTg_int=sqrt((1.0+ratio^2.0)*(dkTg_avg(low))^2.0+ratio^2.0*(dkTg_avg(low+1))^2.0)
      dn_eg_int=sqrt((1.0+ratio^2.0)*(dn_eg_avg(low))^2.0+ratio^2.0*(dn_eg_avg(low+1))^2.0)
      dAbg_int=sqrt((1.0+ratio^2.0)*(dAbg_avg(low))^2.0+ratio^2.0*(dAbg_avg(low+1))^2.0)
      dpresg_int=sqrt((1.0+ratio^2.0)*(dpresg_avg(low))^2.0+ratio^2.0*(dpresg_avg(low+1))^2.0)
   endelse
endelse


output:
print,' '
print,' '
print,'At a radius (semi-major axis) of '+strtrim(string(radius),2)+' arcsec, deprojection gives:'
print,'----------------------------------------------------------------------'
print,' '
print,'    kT = '+strtrim(string(kTg_int),2)+' +/- '+strtrim(string(dkTg_int),2)+' [keV]'
print,'   n_e = '+strtrim(string(n_eg_int),2)+' +/- '+strtrim(string(dn_eg_int),2)+' [cm^-3]'
print,'    Ab = '+strtrim(string(Abg_int),2)+' +/- '+strtrim(string(dAbg_int),2)+' [Solar]'
print,'     P = '+strtrim(string(presg_int),2)+' +/- '+strtrim(string(dpresg_int),2)+' [erg/cm^3]'
print,'----------------------------------------------------------------------'
print,' '
; if /LOG is set, write to obs_info file
if keyword_set(log) then begin
   obsfile=findfile('obs*.txt')
   get_lun,unit
   openw,unit,obsfile,/append
   append_text=' '
   if keyword_set(nearest) then append_text=append_text+', /NEAREST'
   if keyword_set(extrap) then append_text=append_text+', /EXPTRAP'
   if keyword_set(last) then append_text=append_text+', /LAST'
   if (n_elements(ignore) ne 0) then append_text=append_text+' ignore='+strtrim(string(ignore_reg),2)
   printf,unit,'Output of GET_TP, '+strtrim(string(radius),2)+append_text+", spectra_dir='"+spectra_dir+"'"
   printf,unit,!stime
   printf,unit,' '
   printf,unit,'At a radius (semi-major axis) of '+strtrim(string(radius),2)+' arcsec, deprojection gives:'
   printf,unit,'----------------------------------------------------------------------'
   printf,unit,' '
   printf,unit,'    kT = '+strtrim(string(kTg_int),2)+' +/- '+strtrim(string(dkTg_int),2)+' [keV]'
   printf,unit,'   n_e = '+strtrim(string(n_eg_int),2)+' +/- '+strtrim(string(dn_eg_int),2)+' [cm^-3]'
   printf,unit,'    Ab = '+strtrim(string(Abg_int),2)+' +/- '+strtrim(string(dAbg_int),2)+' [Solar]'
   printf,unit,'     P = '+strtrim(string(presg_int),2)+' +/- '+strtrim(string(dpresg_int),2)+' [erg/cm^3]'
   printf,unit,' '
   printf,unit,'------------------------------------------'
   printf,unit,' '
   close,unit
   free_lun,unit
endif


;
; Plot fits to screen
;
loadct,0
if ( (radius ge min(ravg)) or (keyword_set(extrap)) ) then begin
   !p.multi=[0,2,2]
   !x.style=1
   plotsym,0,1
   rint=(indgen(100)+1.0)*max([max(ravg),radius])/99.0
   ploterror,ravg,kt,dkTg_avg,psym=8,title='Temperature',/xlog,COLOR='FFFFFF'x,xtitle='(arcsec)',ytitle='(keV)',$
   	xrange=[min([radius,ravg])*0.5,300]
   oplot,rint,interpol(kt,ravg,rint),psym=-3,COLOR='FFFFFF'x
   oplot,[radius,radius],[kTg_int,kTg_int],psym=2,color='00FF00'x
   oplot,[radius,radius],[0,kTg_int],psym=-3,color='00FF00'x
   oplot,[0.1,radius],[kTg_int,kTg_int],psym=-3,color='00FF00'x

   ploterror,ravg,n_e,dn_eg_avg,psym=8,title='Density',/xlog,/ylog,xtitle='(arcsec)',ytitle='(cm^-3)',xrange=[min([radius,ravg])*0.5,300]
   oplot,rint,(n_e_result[0]*rint^(n_e_result[1])),psym=-3
   oplot,[radius,radius],[n_eg_int,n_eg_int],psym=2,color='00FF00'x
   oplot,[radius,radius],[1E-6,n_eg_int],psym=-3,color='00FF00'x
   oplot,[0.1,radius],[n_eg_int,n_eg_int],psym=-3,color='00FF00'x

   ploterror,ravg,ab,dAbg_avg,psym=8,title='Abundance',/xlog,xtitle='(arcsec)',ytitle='(solar)',xrange=[min([radius,ravg])*0.5,300]
   oplot,rint,interpol(ab,ravg,rint),psym=-3
   oplot,[radius,radius],[Abg_int,Abg_int],psym=2,color='00FF00'x
   oplot,[radius,radius],[-1,Abg_int],psym=-3,color='00FF00'x
   oplot,[0.1,radius],[Abg_int,Abg_int],psym=-3,color='00FF00'x

   ploterror,ravg,p,dpresg_avg,psym=8,title='Pressure',/xlog,/ylog,xtitle='(arcsec)',ytitle='(keV cm^2)',xrange=[min([radius,ravg])*0.5,300]
   oplot,rint,(pres_result[0]*rint^(pres_result[1])),psym=-3
   oplot,[radius,radius],[presg_int,presg_int],psym=2,color='00FF00'x
   oplot,[radius,radius],[1E-14,presg_int],psym=-3,color='00FF00'x
   oplot,[0.1,radius],[presg_int,presg_int],psym=-3,color='00FF00'x
endif


;
; Return to IDL
;
return
end