pro radial_plots_deproj,z,lumdist,angscale,old_pipe=old_pipe,central=central,spectra_dir=spectra_dir
;------------------------------------------------------------------------------
; Name:  RADIAL_PLOTS_DEPROJ
;
; Purpose:  Plot deprojected temperature, density, pressure, entropy, abundance, 
;	    and surface brightness vs. radius for the annuli.
;
;
; Inputs:  z - redshift
;	   lumdist -luminosity distance [Mpc]
;	   angscale - angular scale [kpc/arcsec]
;
;
; Comments:  At the moment, only one partial annulus segment is supported (nseg=1)
;
;
; Revision history:
;       written by D&L, 2003-11-12
;------------------------------------------------------------------------------   
;
; Read data from the output of fit_spectra.pro
;
;goto,test
if (n_elements(spectra_dir) eq 0) then spectra_dir='spectra'
if keyword_set(central) then begin
      readcol, './'+spectra_dir+'/sb.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area
;      readcol, './'+spectra_dir+'/sb.dat',skipline=16,r1,r2,sb,sberr,area      
      readcol, './'+spectra_dir+'/radii_central.dat',rin,rout
endif else begin
   if keyword_set(old_pipe) then begin
      readcol, './'+spectra_dir+'/sb.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area
      readcol, './'+spectra_dir+'/deproj_radii.dat',rout
      s=size(rout)
      rsize=s[1]
      rin=dblarr(rsize)
      rin[0]=0
      for i=1,rsize-1 do begin
         rin[i]=rout[i-1]
      endfor
   endif else begin
      readcol, './'+spectra_dir+'/sb.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area
      readcol, './'+spectra_dir+'/radii.dat',rin,rout
   endelse
endelse


;
; Read in ellipticity from annuli.txt
;
fmt = 'A,F'
if keyword_set(old_pipe) then begin
   readcol,'./'+spectra_dir+'/deproj_annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
   xc=annuli_cent[0]
   yc=annuli_cent[1]
   print,' '
   ellip=0.0
   pa=0.0
   print,'Using ellip [1 - b/a]     : ',ellip
endif else begin
   readcol,'./'+spectra_dir+'/annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
   readcol,'./'+spectra_dir+'/annuli.txt',skipline=11,f=fmt,junk,annuli_prop,delimiter=':',numline=2,/silent
   xc=annuli_cent[0]
   yc=annuli_cent[1]
   ellip=annuli_prop[0]
   pa=annuli_prop[1]
   print,' '
   print,'Using ellip [1 - b/a]     : ',ellip
endelse


;
; Read in model parameters and errors
;
hd=headfits('./'+spectra_dir+'/source_mekal.spectra',exten=1)
nHgal=sxpar(hd,'NH0','Parameter NH0 not found')
nfit=sxpar(hd,'NUM_REG','Parameter NUM_REG not found')
KT=dblarr(nfit)
KT_lo=dblarr(nfit)
KT_hi=dblarr(nfit)
ab=dblarr(nfit)
ab_lo=dblarr(nfit)
ab_hi=dblarr(nfit)
norm=dblarr(nfit)
norm_lo=dblarr(nfit)
norm_hi=dblarr(nfit)
for i=0,nfit-1 do begin
   KT[i]=sxpar(hd,'KT'+strtrim(string(i+1),2))
   KT_lo[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRL')
   KT_hi[i]=sxpar(hd,'KT'+strtrim(string(i+1),2)+'ERRU','Parameter KT'+strtrim(string(i+1),2)+'ERRU not found')
   ab[i]=sxpar(hd,'AB'+strtrim(string(i+1),2),'Parameter AB'+strtrim(string(i+1),2)+' not found')
   ab_lo[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRL','Parameter AB'+strtrim(string(i+1),2)+'ERRL not found')
   ab_hi[i]=sxpar(hd,'AB'+strtrim(string(i+1),2)+'ERRU','Parameter AB'+strtrim(string(i+1),2)+'ERRU not found')
   norm[i]=sxpar(hd,'NORM'+strtrim(string(i+1),2),'Parameter NORM'+strtrim(string(i+1),2)+' not found')
   norm_lo[i]=sxpar(hd,'N'+strtrim(string(i+1),2)+'ERRL','Parameter NORM'+strtrim(string(i+1),2)+'ERRL not found')
   norm_hi[i]=sxpar(hd,'N'+strtrim(string(i+1),2)+'ERRU','Parameter NORM'+strtrim(string(i+1),2)+'ERRU not found')
endfor


;
; Set plot 1 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_deproj_1.ps',/color,landscape=1
!p.multi=[0,2,2]


;
; Calculate the average radius and width for each annulus
;
scale=0.4919		    ; ACIS-S scale [arcsec/pixel]
ravg=(rin+rout)*scale/2    ; central radius of each annulus
rwidth=(rout-rin)*scale/2  ; width of each annulus
rmax=max(rout)
!x.style=1
!x.range=[1,rmax*scale+20]

KTv=KT
KTvlo=KT_lo
KTvhi=KT_hi
Abv=ab			; Set values to nH varied case to make it easy
Abvlo=ab_lo
Abvhi=ab_hi
normv=norm
normvlo=norm_lo
normvhi=norm_hi

KTvloerr=KTv-KTvlo
KTvhierr=KTvhi-KTv
Abvloerr=Abv-Abvlo  ; Values for nH varied
Abvhierr=Abvhi-Abv
normvloerr=normv-normvlo
normvhierr=normvhi-normv

;KTgloerr=KTg-KTglo
;KTghierr=KTghi-KTg  ; Values for nH fixed to galactic
;Abgloerr=Abg-Abglo
;Abghierr=Abghi-Abg
;normgloerr=normg-normglo
;normghierr=normghi-normg
zeroy=intarr(1,nfit)


;
; Plot temperature vs. radius
;
plotsym,3,0.6,/fill
ploterror,ravg,KTv,rwidth,zeroy,psym=8,hatlength=111,/xlog,$
     yrange=[min(KTv)-0.5,max(KTv)+0.5],$
     title='Temperature Profile',$
     xtitle='Radius (arcsec)',ytitle='kT (keV)' 
oploterror,ravg,KTv,rwidth,KTvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,KTv,rwidth,KTvhierr,psym=8,hatlength=180,/HIBAR
;legend,'nH varied',psym=88,position=[1.1,min(KTv)+2.0],charsize=0.6,box=0

;plotsym,0,1
;oploterror,ravg,KTg,rwidth,KTgloerr,psym=8,hatlength=100,/LOBAR
;oploterror,ravg,KTg,rwidth,KTghierr,psym=8,hatlength=100,/HIBAR
;legend,'nH galactic',psym=88,position=[1.1,min(KTv)+1.4],charsize=0.6,box=0


;
; Find the volume of each shell (for ellipses, assume prolate symmetry: V=4*PI*a*b^2/3)
; This is made much more complicated by the possibility of partial segments (defined
; by the angles stored in XFLT0004, XFLT0005, etc.)
;
pixel_conversion=angscale*0.4919*1000*3.085678
answer=' '
answer_pt=' '
print,' '
read,answer_pt,prompt='Did you cut out a central point source (y/n)? '
answer_pt_check:
if ( (answer_pt ne 'y') and (answer_pt ne 'n') ) then begin
   read,answer_pt,prompt='Please type "y" or "n": '
   goto,answer_pt_check
endif
if (answer_pt eq 'y') then begin
   read,inner_a,prompt='Enter semi-major axis of inner boundary [pixels]: '
   read,inner_b,prompt='Enter semi-minor axis of inner boundary [pixels]: '
endif else begin
   inner_a=0.0
   inner_b=0.0
endelse

print,' '
print,'During the extract_annuli stage, did you calculate'
read,answer,prompt='angles of partial annulus segments (y/n)? '
answer_check:
if ( (answer ne 'y') and (answer ne 'n') ) then begin
   read,answer,prompt='Please type "y" or "n": '
   goto,answer_check
endif

if (answer eq 'y') then begin	; Find volume if some annuli are partial 
   ;
   ; First, read in angle keywords from the spectra
   ;
   vol=dblarr(nfit)
   for i=1,nfit do begin
      istring=strtrim(string(i),2)
      phafile='./'+spectra_dir+'/reg'+istring+'_sou_g30.pi'
      hd=headfits(phafile,exten=1)
      xflt0004=sxpar(hd,'XFLT0004','Parameter XFLT0004 not found')
      xflt0005=sxpar(hd,'XFLT0005','Parameter XFLT0005 not found')
;      xflt0006=sxpar(hd,'XFLT0006','Parameter XFLT0006 not found')
;      xflt0007=sxpar(hd,'XFLT0007','Parameter XFLT0007 not found')
      print,' '
      print,'Annulus '+istring+':'
      print,'-----------'
      print,' R inner = ',rin[i-1]
      print,' R outer = ',rout[i-1]
      print,'   Ellip = ',ellip
      print,'      PA = ',pa
      print,'xflt0004 = ',xflt0004
      print,'xflt0005 = ',xflt0005
;      print,'xflt0006 = ',xflt0006
;      print,'xflt0007 = ',xflt0007

;      if ( (xflt0006 eq 0.0) and (xflt0007 eq 360.0) ) then nseg=1 else nseg=2  ; number of segments of partial annulus
      nseg=1    
      
      ;
      ; Now calculate volume of partial segments
      ;
      if (ellip ne 0.0) then begin
         pi=3.141592654
         a1=rin[i-1]
         a2=rout[i-1]
         b1=a1*(1.0-ellip)
         b2=a2*(1.0-ellip)
         if (a1 eq 0.0) then c1=0.0 else c1=sqrt(a1^2.0/(a1^2.0-b1^2.0))
         c2=sqrt(a2^2.0/(a2^2.0-b2^2.0))
         theta1=xflt0004*pi/180.0-pa
         theta2=xflt0005*pi/180.0-pa
         if (theta1 lt 0.0) then theta1=theta1+2.0*pi
         if (theta2 lt 0.0) then theta2=theta2+2.0*pi
         
                  
         ;
         ; Check for 1st and 2nd or 3rd and 4th quadrant problems
         ; due to SIN function
         ;
         if ((theta1 ge 0) and (theta1 le pi/2.0)) then quad1=1
         if ((theta1 gt pi/2.0) and (theta1 le pi)) then quad1=2
         if ((theta1 gt pi) and (theta1 le 3.0*pi/2.0)) then quad1=3
         if ((theta1 gt 3.0*pi/2.0) and (theta1 le 2.0*pi)) then quad1=4
         if ((theta2 ge 0) and (theta2 le pi/2.0)) then quad2=1
         if ((theta2 gt pi/2.0) and (theta2 le pi)) then quad2=2
         if ((theta2 gt pi) and (theta2 le 3.0*pi/2.0)) then quad2=3
         if ((theta2 gt 3.0*pi/2.0) and (theta2 le 2.0*pi)) then quad2=4
         
         if ( ((quad1 eq 1) and (quad2 eq 2)) or ((quad1 eq 1) and (quad2 eq 3)) or $
              ((quad1 eq 4) and (quad2 eq 3)) or ((quad1 eq 4) and (quad2 eq 2)) ) then begin
            if (c1 eq 0.0) then begin
               vol_seg1a=0.0
               vol_seg1b=0.0
            endif else begin
               vol_seg1a=abs(pi/3.0*c1*b1^3.0*(1.0/sqrt(c1^2.0-1.0)-sin(theta1)/sqrt(c1^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg1b=abs(pi/3.0*c1*b1^3.0*(sin(theta2)/sqrt(c1^2.0-(sin(theta2))^2.0)-1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
            endelse
            vol_seg2a=abs(pi/3.0*c2*b2^3.0*(1.0/sqrt(c2^2.0-1.0)-sin(theta1)/sqrt(c2^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol_seg2b=abs(pi/3.0*c2*b2^3.0*(sin(theta2)/sqrt(c2^2.0-(sin(theta2))^2.0)-1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
            vol[i-1]=vol_seg2a+vol_seg2b-vol_seg1a-vol_seg1b  
            goto,seg2_check          
         endif 
         if ( ((quad1 eq 2) and (quad2 eq 1)) or ((quad1 eq 2) and (quad2 eq 4)) or $
              ((quad1 eq 3) and (quad2 eq 4)) or ((quad1 eq 3) and (quad2 eq 1)) ) then begin
            if (c1 eq 0.0) then begin
               vol_seg1a=0.0
               vol_seg1b=0.0
            endif else begin
               vol_seg1a=abs(pi/3.0*c1*b1^3.0*(-1.0/sqrt(c1^2.0-1.0)-sin(theta1)/sqrt(c1^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg1b=abs(pi/3.0*c1*b1^3.0*(sin(theta2)/sqrt(c1^2.0-(sin(theta2))^2.0)+1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
            endelse
            vol_seg2a=abs(pi/3.0*c2*b2^3.0*(-1.0/sqrt(c2^2.0-1.0)-sin(theta1)/sqrt(c2^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol_seg2b=abs(pi/3.0*c2*b2^3.0*(sin(theta2)/sqrt(c2^2.0-(sin(theta2))^2.0)+1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
            vol[i-1]=vol_seg2a+vol_seg2b-vol_seg1a-vol_seg1b  
            goto,seg2_check   
         endif 
         if ( ((quad1 eq 1) and (quad2 eq 4)) or ((quad1 eq 3) and (quad2 eq 2)) ) then begin
            if (c1 eq 0.0) then begin
               vol_seg1a=0.0
               vol_seg1b=0.0
            endif else begin
               vol_seg1a=abs(pi/3.0*c1*b1^3.0*(1.0/sqrt(c1^2.0-1.0)-sin(theta1)/sqrt(c1^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg1b=abs(pi/3.0*c1*b1^3.0*(sin(theta2)/sqrt(c1^2.0-(sin(theta2))^2.0)+1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
            endelse
            vol_seg2a=abs(pi/3.0*c2*b2^3.0*(1.0/sqrt(c2^2.0-1.0)-sin(theta1)/sqrt(c2^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol_seg2b=abs(pi/3.0*c2*b2^3.0*(sin(theta2)/sqrt(c2^2.0-(sin(theta2))^2.0)+1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
            vol[i-1]=vol_seg2a+vol_seg2b+2.0/3.0*pi*a2*b2^2.0-vol_seg1a-vol_seg1b-2.0/3.0*pi*a1*b1^2.0      
            goto,seg2_check   
         endif
         if (theta1 lt theta2) then begin
            if (c1 eq 0.0) then vol_seg1=0.0 else vol_seg1=abs(pi/3.0*c1*b1^3.0*(sin(theta2)/sqrt(c1^2.0-(sin(theta2))^2.0)-sin(theta1)/sqrt(c1^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol_seg2=abs(pi/3.0*c2*b2^3.0*(sin(theta2)/sqrt(c2^2.0-(sin(theta2))^2.0)-sin(theta1)/sqrt(c2^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol[i-1]=vol_seg2-vol_seg1   
         endif else begin
            if (c1 eq 0.0) then vol_seg1=0.0 else vol_seg1=abs(pi/3.0*c1*b1^3.0*(sin(theta2)/sqrt(c1^2.0-(sin(theta2))^2.0)-sin(theta1)/sqrt(c1^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol_seg2=abs(pi/3.0*c2*b2^3.0*(sin(theta2)/sqrt(c2^2.0-(sin(theta2))^2.0)-sin(theta1)/sqrt(c2^2.0-(sin(theta1))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
            vol[i-1]=4.0/3.0*pi*(a2*b2^2.0-a1*b1^2.0)*(pixel_conversion)^3.0-(vol_seg2-vol_seg1)
         endelse

        
         ;
         ; If there are two segments calculate the volume of the second segment
         ;
         seg2_check:
         if (nseg eq 2) then begin
            theta12=(xflt0006-pa)*pi/180.0
            theta22=(xflt0007-pa)*pi/180.0
            if (theta12 lt 0.0) then theta1=theta1+2.0*pi
            if (theta22 lt 0.0) then theta2=theta2+2.0*pi
            
            if ((theta12 ge 0) and (theta12 le pi/2.0)) then quad1=1
            if ((theta12 gt pi/2.0) and (theta12 le pi)) then quad1=2
            if ((theta12 gt pi) and (theta12 le 3.0*pi/2.0)) then quad1=3
            if ((theta12 gt 3.0*pi/2.0) and (theta12 le 2.0*pi)) then quad1=4
            if ((theta22 ge 0) and (theta22 le pi/2.0)) then quad2=1
            if ((theta22 gt pi/2.0) and (theta22 le pi)) then quad2=2
            if ((theta22 gt pi) and (theta22 le 3.0*pi/2.0)) then quad2=3
            if ((theta22 gt 3.0*pi/2.0) and (theta22 le 2.0*pi)) then quad2=4
         
            if ( ((quad1 eq 1) and (quad2 eq 2)) or ((quad1 eq 1) and (quad2 eq 3)) or $
                 ((quad1 eq 3) and (quad2 eq 4)) or ((quad1 eq 4) and (quad2 eq 2)) ) then begin
               if (c1 eq 0.0) then begin
                  vol_seg12a=0.0
                  vol_seg12b=0.0
               endif else begin
                  vol_seg12a=abs(pi/3.0*c1*b1^3.0*(1.0/sqrt(c1^2.0-1.0)-sin(theta12)/sqrt(c1^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
                  vol_seg12b=abs(pi/3.0*c1*b1^3.0*(sin(theta22)/sqrt(c1^2.0-(sin(theta22))^2.0)-1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
               endelse
               vol_seg22a=abs(pi/3.0*c2*b2^3.0*(1.0/sqrt(c2^2.0-1.0)-sin(theta12)/sqrt(c2^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg22b=abs(pi/3.0*c2*b2^3.0*(sin(theta22)/sqrt(c2^2.0-(sin(theta22))^2.0)-1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
               vol[i-1]=vol[i-1]+vol_seg22a+vol_seg22b-vol_seg12a-vol_seg12b  
               goto,density          
            endif 
            if ( ((quad1 eq 2) and (quad2 eq 1)) or ((quad1 eq 2) and (quad2 eq 4)) or $
                 ((quad1 eq 4) and (quad2 eq 3)) or ((quad1 eq 3) and (quad2 eq 1)) ) then begin
               if (c1 eq 0.0) then begin
                  vol_seg12a=0.0
                  vol_seg12b=0.0
               endif else begin
                  vol_seg12a=abs(pi/3.0*c1*b1^3.0*(-1.0/sqrt(c1^2.0-1.0)-sin(theta12)/sqrt(c1^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
                  vol_seg12b=abs(pi/3.0*c1*b1^3.0*(sin(theta22)/sqrt(c1^2.0-(sin(theta22))^2.0)+1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
               endelse
               vol_seg22a=abs(pi/3.0*c2*b2^3.0*(-1.0/sqrt(c2^2.0-1.0)-sin(theta12)/sqrt(c2^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg22b=abs(pi/3.0*c2*b2^3.0*(sin(theta22)/sqrt(c2^2.0-(sin(theta22))^2.0)+1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
               vol[i-1]=vol[i-1]+vol_seg22a+vol_seg22b-vol_seg12a-vol_seg12b     
               goto,density   
            endif 
            if ( ((quad1 eq 1) and (quad2 eq 4)) or ((quad1 eq 3) and (quad2 eq 2)) ) then begin
               if (c1 eq 0.0) then begin
                  vol_seg12a=0.0
                  vol_seg12b=0.0
               endif else begin
                  vol_seg12a=abs(pi/3.0*c1*b1^3.0*(1.0/sqrt(c1^2.0-1.0)-sin(theta12)/sqrt(c1^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
                  vol_seg12b=abs(pi/3.0*c1*b1^3.0*(sin(theta22)/sqrt(c1^2.0-(sin(theta22))^2.0)+1.0/sqrt(c1^2.0-1.0))*(pixel_conversion)^3.0)
               endelse
               vol_seg22a=abs(pi/3.0*c2*b2^3.0*(1.0/sqrt(c2^2.0-1.0)-sin(theta12)/sqrt(c2^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg22b=abs(pi/3.0*c2*b2^3.0*(sin(theta22)/sqrt(c2^2.0-(sin(theta22))^2.0)+1.0/sqrt(c2^2.0-1.0))*(pixel_conversion)^3.0)
               vol[i-1]=vol[i-1]+vol_seg22a+vol_seg22b+2.0/3.0*pi*a2*b2^2.0-vol_seg12a-vol_seg12b-2.0/3.0*pi*a1*b1^2.0      
               goto,density   
            endif
            if (theta1 lt theta2) then begin
               if (c1 eq 0.0) then vol_seg12=0.0 else vol_seg12=abs(pi/3.0*c1*b1^3.0*(sin(theta22)/sqrt(c1^2.0-(sin(theta22))^2.0)-sin(theta12)/sqrt(c1^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg22=abs(pi/3.0*c2*b2^3.0*(sin(theta22)/sqrt(c2^2.0-(sin(theta22))^2.0)-sin(theta12)/sqrt(c2^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol[i-1]=vol[i-1]+vol_seg22-vol_seg12   
            endif else begin
               if (c1 eq 0.0) then vol_seg12=0.0 else vol_seg12=abs(pi/3.0*c1*b1^3.0*(sin(theta22)/sqrt(c1^2.0-(sin(theta22))^2.0)-sin(theta12)/sqrt(c1^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol_seg22=abs(pi/3.0*c2*b2^3.0*(sin(theta22)/sqrt(c2^2.0-(sin(theta22))^2.0)-sin(theta12)/sqrt(c2^2.0-(sin(theta12))^2.0))*(pixel_conversion)^3.0) ;[10^54 cm^3]
               vol[i-1]=vol[i-1]+4.0/3.0*pi*(a2*b2^2.0-a1*b1^2.0)*(pixel_conversion)^3.0-(vol_seg22-vol_seg12)
            endelse 
         endif
      endif else begin		; Find volume for cirular annuli
      
         pi=3.141592654
         rad1=rin[i-1]
         rad2=rout[i-1]
         theta1=(xflt0004)*pi/180.0
         theta2=(xflt0005)*pi/180.0
         if (theta1 lt 0.0) then theta1=theta1+2.0*pi
         if (theta2 lt 0.0) then theta2=theta2+2.0*pi
         
         vol_tot=4.0*pi/3.0*(rad2^3.0-rad1^3.0)*(pixel_conversion)^3.0 ;[10^54 cm^3] Total volume of the shell without cutouts
                  
         if (theta1 lt theta2) then begin
            vol[i-1]=vol_tot*abs((theta2-theta1)/2.0/pi) ;[10^54 cm^3]
         endif else begin
            vol[i-1]=vol_tot*abs(1.0-(theta1-theta2)/2.0/pi) ;[10^54 cm^3]
         endelse
         
        
         ;
         ; If there are two segments calculate the volume of the second segment
         ;
         if (nseg eq 2) then begin
            theta12=xflt0004*pi/180.0
            theta22=xflt0005*pi/180.0
            if (theta12 lt 0.0) then theta12=theta12+2.0*pi
            if (theta22 lt 0.0) then theta22=theta22+2.0*pi
                        
            if (theta12 lt theta22) then begin
               vol[i-1]=vol[i-1]+vol_tot*abs((theta22-theta12)/2.0/pi)	;[10^54 cm^3] 
            endif else begin
               vol[i-1]=vol[i-1]+vol_tot*abs(1.0-(theta12-theta22)/2.0/pi)	;[10^54 cm^3]
            endelse 
         endif         
      endelse
      print,'  Volume = ',vol[i-1],' [10^54 cm^3]'     
   endfor   
endif else begin		; Find volume if all annuli are whole (answer eq 'n')
   if (ellip eq 0.0) then begin
      vol=4.0*3.141592654/3.0*(rout^3.0-rin^3.0)*(pixel_conversion)^3.0 ;[10^54 cm^3]
   endif else begin
      a_in=rin
      b_in=rin*(1.0-ellip)
      a_out=rout
      b_out=rout*(1.0-ellip)
      vol=4.0*3.141592654/3.0*(a_out*b_out^2.0-a_in*b_in^2.0)*(pixel_conversion)^3.0 ;[10^54 cm^3]
   endelse
endelse


;
; If a central point source was removed, subtract its volume
;
vol[0]=vol[0]-4.0*3.141592654/3.0*inner_a*inner_b^2.0*(pixel_conversion)^3.0


;
; Calculate the density from the MEKAL norm
; 
density:
n_ev=sqrt(1.2*normv*4*3.141592654*((lumdist*3.085678E6)^2.0/(1.0+z)^2.0)*1E14/vol/1E18)  ; density for nH varied 
n_evloerr=sqrt((0.5*normvloerr/normv)^2.0)*n_ev  ; add errors in quadrature and find low error of n_ev
n_evhierr=sqrt((0.5*normvhierr/normv)^2.0)*n_ev  ; add errors in quadrature and find high error of n_ev

;n_eg=sqrt(1.2*normg*4*3.141592654*((lumdist*1E6*3.085678)^2.0/(1.0+z)^2.0)*1E14/vol/1E18)  ; density for nH galactic
;n_egloerr=sqrt((0.5*normgloerr/normg)^2.0)*n_eg  ; add errors in quadrature and find low error of n_eg
;n_eghierr=sqrt((0.5*normghierr/normg)^2.0)*n_eg  ; add errors in quadrature and find high error of n_eg


;
; Make the plot of n_e vs. radius
;
plotsym,0,1
ploterror,ravg,n_ev,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(n_ev),max(n_ev)],$
     title='Density Profile',$
     xtitle='Radius (arcsec)',ytitle='n!De!N (cm!E-3!N)' 
oploterror,ravg,n_ev,rwidth,n_evloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,n_ev,rwidth,n_evhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,n_eg,rwidth,n_egloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,n_eg,rwidth,n_eghierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the pressure from the ideal gas law (P=2.0*n_ekT [erg/cm^3])
;
;presg=2.0*n_eg*KTg*1.60219E-9		; includes conversion to from keV to ergs
;presgloerr=sqrt((n_egloerr/n_eg)^2.0+(KTgloerr/KTg)^2.0)*presg
;presghierr=sqrt((n_eghierr/n_eg)^2.0+(KTghierr/KTg)^2.0)*presg

presv=2.0*n_ev*KTv*1.60219E-9
presvloerr=sqrt((n_evloerr/n_ev)^2.0+(KTvloerr/KTv)^2.0)*presv
presvhierr=sqrt((n_evhierr/n_ev)^2.0+(KTvhierr/KTv)^2.0)*presv


;
; Make the plot of pressure vs. raduis
;
plotsym,0,1
ploterror,ravg,presv,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(presv),max(presv)],$
     title='Pressure Profile',$
     xtitle='Radius (arcsec)',ytitle='P (erg cm!E-3!N)' 
oploterror,ravg,presv,rwidth,presvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,presv,rwidth,presvhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,presg,rwidth,presgloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,presg,rwidth,presghierr,psym=8,hatlength=110,/HIBAR


;
; Calculate the entropy as S=T/(n_e)^2/3 [kev cm^2]
;
;sg=KTg/(n_eg)^(0.66667)
;sgloerr=sqrt((KTgloerr/KTg)^2.0+(0.66667*n_egloerr/n_eg)^2.0)*sg
;sghierr=sqrt((KTghierr/KTg)^2.0+(0.66667*n_eghierr/n_eg)^2.0)*sg

sv=KTv/(n_ev)^(0.66667)
svloerr=sqrt((KTvloerr/KTv)^2.0+(0.66667*n_evloerr/n_ev)^2.0)*sv
svhierr=sqrt((KTvhierr/KTv)^2.0+(0.66667*n_evhierr/n_ev)^2.0)*sv


;
; Make the plot of entropy vs. radius
;
plotsym,0,1
ploterror,ravg,sv,rwidth,zeroy,psym=8,hatlength=111,/xlog,/ylog,$
     yrange=[min(sv),max(sv)],$
     title='Entropy Profile',$
     xtitle='Radius (arcsec)',ytitle='S (kev cm!E2!N)' 
oploterror,ravg,sv,rwidth,svloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,sv,rwidth,svhierr,psym=8,hatlength=180,/HIBAR
;plotsym,0,0.6,/fill
;oploterror,ravg,sf,rwidth,sfloerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,sf,rwidth,sfhierr,psym=8,hatlength=110,/HIBAR
device, /close


;
; Set plot 2 parameters
;
!p.font=0
set_plot,'ps'
device,set_font='Times-Roman',file='radial_plots_deproj_2.ps',/color,landscape=1
!p.multi=[0,2,2]


;
; Plot abundance vs. radius
;
plotsym,3,0.6,/fill
ploterror,ravg,Abv,rwidth,zeroy,psym=8,hatlength=111,/xlog,$
     yrange=[min(Abv)-0.3,max(Abv)+0.3],$
     title='Abundance Profile',$
     xtitle='Radius (arcsec)',ytitle='Z (relative to solar)'
oploterror,ravg,Abv,rwidth,Abvloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,Abv,rwidth,Abvhierr,psym=8,hatlength=180,/HIBAR

;plotsym,0,1
;oploterror,ravg,Abg,rwidth,Abgloerr,psym=8,hatlength=100,/LOBAR
;oploterror,ravg,Abg,rwidth,Abghierr,psym=8,hatlength=100,/HIBAR


;
; Plot SB vs. radius
;
; First in units of [cnts/sec/arcsec^2]
;
ravgsb=(r1+r2)*scale/2.0
sb=sb/scale^2				; convert to cnts/s/arcsec^2
sb_elements=where(sb gt 0.0, count)
sb_nonzero=sb[sb_elements]
plotsym,0,0.4
ploterror,ravgsb,sb_nonzero,sberr,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(sb_nonzero),max(sb)*1.1],$
     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N arcsec!E-2!N)'


;
; Then in units of [cnts/sec/cm^2/arcsec^2]
; (usefull since chip effective area drops near the edges)
;
sb_ec=sb_ec/scale^2				; convert to cnts/s/arcsec^2
sb_ec_elements=where(sb_ec gt 0.0, count)
sb_ec_nonzero=sb_ec[sb_ec_elements]
plotsym,0,0.4
ploterror,ravgsb,sb_ec_nonzero,sb_ecerr,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(sb_ec_nonzero),max(sb_ec_nonzero)*1.1],$
     title='Surface Brightness Profile',$
     xtitle='Radius (arcsec)',ytitle='!9S!X (cts sec!E-1!N cm!E-2!N arcsec!E-2!N)'


;
; Find cooling time from the NEWCOOL program
;

cfn_file=findfile('cfn.dat',count=num)
if (num eq 0) then begin
   make_cfn_file
endif
print,' '
print,'Please enter the following values into the NEWCOOL program.'
print,'You can select all of them together with the mouse and paste'
print,'with the middle button.  NEWCOOL will then calculate cooling'
print,'times for each line automatically.'
print,' '
for i=0,nfit-1 do begin
   print,strtrim(string(KTv[i]),2)+' '+strtrim(string(n_ev[i]),2)+' '+strtrim(string(Abv[i]),2)
endfor
print,'0 0 0'
print,' '
spawn,'newcool'
print,' '
y_v=dblarr(nfit)
y_vlo=dblarr(nfit)
y_vhi=dblarr(nfit)
y_vloerr=dblarr(nfit)
y_vhierr=dblarr(nfit)

for i=0,nfit-1 do begin
   read,temp,prompt='Please enter the cooling time for region '+strtrim(string(i+1),2)+': '
   y_v[i]=temp
endfor

print,' '
print,'Please repeat the above procedure for the lower limits.
print,' '
for i=0,nfit-1 do begin
   print,strtrim(string(KTvlo[i]),2)+' '+strtrim(string(n_evhierr[i]+n_ev[i]),2)+' '+strtrim(string(Abvhierr[i]+Abv[i]),2)
endfor
print,'0 0 0'
print,' '
spawn,'newcool'
print,' '
for i=0,nfit-1 do begin
   read,temp,prompt='Please enter the low cooling time for region '+strtrim(string(i+1),2)+': '
   y_vlo[i]=temp
endfor

print,' '
print,'Please repeat the above procedure for the upper limits.
print,' '
for i=0,nfit-1 do begin
   print,strtrim(string(KTvhi[i]),2)+' '+strtrim(string(n_ev[i]-n_evloerr[i]),2)+' '+strtrim(string(Abv[i]-Abvloerr[i]),2)
endfor
print,'0 0 0'
print,' '
spawn,'newcool'
print,' '
for i=0,nfit-1 do begin
   read,temp,prompt='Please enter the high cooling time for region '+strtrim(string(i+1),2)+': '
   y_vhi[i]=temp
endfor
y_vloerr=y_v-y_vlo
y_vhierr=y_vhi-y_v


;
; Make a separate plot of the cooling time for use in 
; finding the cooling radius

plotsym,0,1
ploterror,ravg,y_v,rwidth,zeroy,psym=8,hatlength=100,/xlog,/ylog,$
     yrange=[min(y_v),max(y_v)],$
     title='Cooling Time',$
     xtitle='Radius (arcsec)',ytitle='t!Dcool!N (yrs)'
oploterror,ravg,y_v,rwidth,y_vloerr,psym=8,hatlength=180,/LOBAR
oploterror,ravg,y_v,rwidth,y_vhierr,psym=8,hatlength=180,/HIBAR

;plotsym,0,0.6,/fill
;oploterror,ravg,y_f,rwidth,y_floerr,psym=8,hatlength=110,/LOBAR
;oploterror,ravg,y_f,rwidth,y_fhierr,psym=8,hatlength=110,/HIBAR

oplot,[1,rmax*scale+20.0],[1.4E10,1.4E10],line=2,psym=-3

device, /close


;
; Save all values to a file
;
get_lun,unit
outfile='./'+spectra_dir+'/properties_deproj.dat'
openw,unit,outfile
printf,unit,'# Cluster properties calculated from the deprojected data.'
printf,unit,'#'
printf,unit,'# Columns:'
printf,unit,'# (1) Average radius of annulus [arcsec]'
printf,unit,'# (2) Average radius of annulus [kpc]'
printf,unit,'# (3) kT [keV]'
printf,unit,'# (4) kT low error [keV]'
printf,unit,'# (5) kT high error [keV]'
printf,unit,'# (6) n_e [cm^-3]'
printf,unit,'# (7) n_e low error [cm^-3]'
printf,unit,'# (8) n_e high error [cm^-3]'
printf,unit,'# (9) Pressure [ergs cm^-3]'
printf,unit,'# (10) Pressure low error [ergs cm^-3]'
printf,unit,'# (11) Pressure high error [ergs cm^-3]'
printf,unit,'# (12) Entropy [keV cm^2]'
printf,unit,'# (13) Entropy low error [keV cm^2]'
printf,unit,'# (14) Entropy high error [keV cm^2]'
printf,unit,'# (15) Abundance [solar]'
printf,unit,'# (16) Abundance low error [solar]'
printf,unit,'# (17) Abundance high error [solar]'
printf,unit,'# (18) Cooling time [yr]'
printf,unit,'# (19) Cooling time low error [yr]'
printf,unit,'# (20) Cooling time high error [yr]'
printf,unit,' '
fmt='$(f9.3,3x,f9.3,3(3x,f9.4),6(3x,e13.5),3(3x,f9.3),3(3x,f9.5),3(3x,e13.5))'
for i=0,nfit-1 do begin
   printf,unit,fmt,ravg[i],ravg[i]*angscale,KTv[i],KTvloerr[i],KTvhierr[i],n_ev[i],n_evloerr[i],n_evhierr[i],presv[i],presvloerr[i],presvhierr[i],sv[i],svloerr[i],svhierr[i],Abv[i],Abvloerr[i],Abvhierr[i],y_v[i],y_vloerr[i],y_vhierr[i]
endfor
printf,unit,' '
close,unit
free_lun,unit



;
; Return to IDL
;
return
end
