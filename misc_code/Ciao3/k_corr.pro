function k_corr,z,band,silent=silent
;------------------------------------------------------------------------------
; Name:  K_CORR
;
; Purpose:  Find B-, R-, or K-band k-correction and evolutionary correction
;	    for early-type galaxies using Poggianti (1997) for K-band.  
;	    This program performs a linear interpolation between table entries.
;
; Inputs:  z - the redshift of interest (0 < z < 1.0)
;	   band - the band of interest (B, R, or K)
;
; Comments: Call as follows:
;	        result = k_corr(z, band [, /silent])
;
; Revision history:
;       written by DAR, 2005-08-30
;	
;------------------------------------------------------------------------------   
;
; Check parameters
;
np=n_params(0)
if (np eq 0) then begin
   print,' '
   print,'Syntax: result = k_corr(z, band [, /silent])'
   print,'Returns B-, R-, or K-band k-correction in magnitudes'
   return,[0.,0.]
endif


;
; Check to make sure redshift is OK
;
if (z gt 1.0) then begin
   print,' '
   print,'  ERROR: The redshift you entered is larger that the maximum redshift of 1.0.'
   print,' '
   return,[0.,0.]
endif


;
; Check to make sure band is OK
;
if ( (band ne 'B') and (band ne 'R') and (band ne 'K') ) then begin
   print,' '
   print,"  ERROR: You must enter 'B', 'R', or 'K' for the band."
   print,' '
   return,[0.,0.]
endif


;
; Define tables of redshift, K-correction, and evolutionary correction
;
;	  z		  K		  E
;	-------------------------------------
table_B=[ [0.00,	0.000,		0.000],$
	[0.02,		0.098,		-0.023],$
	[0.04,		0.196,		-0.047],$
	[0.06,		0.297,		-0.071],$
	[0.08,		0.401,		-0.098],$
	[0.10,		0.508,		-0.123],$
	[0.12,		0.614,		-0.149],$
	[0.16,		0.815,		-0.201],$
	[0.20,		0.993,		-0.258],$
	[0.24,		1.151,		-0.318],$
	[0.28,		1.296,		-0.377],$
	[0.32,		1.443,		-0.437],$
	[0.36,		1.586,		-0.497],$
	[0.40,		1.745,		-0.555],$
	[0.44,		1.914,		-0.615],$
	[0.48,		2.085,		-0.681],$
	[0.52,		2.277,		-0.752],$
	[0.60,		2.708,		-0.935],$
	[0.68,		3.167,		-1.200],$
	[0.76,		3.590,		-1.569],$
	[0.84,		3.933,		-2.031],$
	[0.92,		4.222,		-2.534],$
	[1.00,		4.483,		-3.059] ]

table_R=[ [0.00,	0.000,		0.000],$
	[0.02,		0.021,		-0.024],$
	[0.04,		0.040,		-0.049],$
	[0.06,		0.061,		-0.073],$
	[0.08,		0.083,		-0.097],$
	[0.10,		0.106,		-0.120],$
	[0.12,		0.130,		-0.143],$
	[0.16,		0.177,		-0.187],$
	[0.20,		0.226,		-0.232],$
	[0.24,		0.278,		-0.276],$
	[0.28,		0.335,		-0.319],$
	[0.32,		0.400,		-0.363],$
	[0.36,		0.469,		-0.406],$
	[0.40,		0.543,		-0.449],$
	[0.44,		0.625,		-0.491],$
	[0.48,		0.714,		-0.535],$
	[0.52,		0.809,		-0.579],$
	[0.60,		1.016,		-0.679],$
	[0.68,		1.242,		-0.789],$
	[0.76,		1.484,		-0.912],$
	[0.84,		1.724,		-1.049],$
	[0.92,		1.956,		-1.197],$
	[1.00,		2.175,		-1.359] ]

table_K=[ [0.00,	0.000,		0.000],$
	[0.02,		-0.030,		-0.028],$
    	[0.04, 		-0.059,		-0.055],$
     	[0.06,		-0.083,		-0.083],$
    	[0.08,		-0.105,		-0.110],$
    	[0.10,		-0.123,		-0.136],$
  	[0.12,		-0.139,		-0.161],$
        [0.16, 		-0.170,		-0.208],$
	[0.20, 		-0.195,		-0.246],$
	[0.24, 		-0.216,		-0.280],$
	[0.28, 		-0.231,		-0.307],$
	[0.32, 		-0.236,		-0.335],$
	[0.36, 		-0.243,		-0.363],$
	[0.40, 		-0.247,		-0.393],$
	[0.44, 		-0.244,		-0.422],$
	[0.48, 		-0.246,		-0.455],$
	[0.52, 		-0.250,		-0.486],$
	[0.60, 		-0.267,		-0.549],$
	[0.68, 		-0.292,		-0.610],$
	[0.76, 		-0.313,		-0.670],$
	[0.84, 		-0.331,		-0.731],$
	[0.92, 		-0.345,		-0.786],$
	[1.00, 		-0.352,		-0.842] ]	
	
s=size(table_B)
n_elements=s[2]
z_tabl_B=dblarr(n_elements)
K_tabl_B=dblarr(n_elements)
E_tabl_B=dblarr(n_elements)
for i=0,n_elements-1 do begin
   z_tabl_B[i]=table_B[0,i]
   K_tabl_B[i]=table_B[1,i]
   E_tabl_B[i]=table_B[2,i]
endfor
s=size(table_R)
n_elements=s[2]
z_tabl_R=dblarr(n_elements)
K_tabl_R=dblarr(n_elements)
E_tabl_R=dblarr(n_elements)
for i=0,n_elements-1 do begin
   z_tabl_R[i]=table_R[0,i]
   K_tabl_R[i]=table_R[1,i]
   E_tabl_R[i]=table_R[2,i]
endfor
s=size(table_K)
n_elements=s[2]
z_tabl_K=dblarr(n_elements)
K_tabl_K=dblarr(n_elements)
E_tabl_K=dblarr(n_elements)
for i=0,n_elements-1 do begin
   z_tabl_K[i]=table_K[0,i]
   K_tabl_K[i]=table_K[1,i]
   E_tabl_K[i]=table_K[2,i]
endfor


;
; Interpolate to find the value of K and E at the specified redshift in the 
; specified band
;
if (band eq 'B') then begin
   K=interpol(K_tabl_B,z_tabl_B,z)
   E=interpol(E_tabl_B,z_tabl_B,z)
endif
if (band eq 'R') then begin
   K=interpol(K_tabl_R,z_tabl_R,z)
   E=interpol(E_tabl_R,z_tabl_R,z)
endif
if (band eq 'K') then begin
   K=interpol(K_tabl_K,z_tabl_K,z)
   E=interpol(E_tabl_K,z_tabl_K,z)
endif

fmt='$(a,f6.4,a,f7.3,a,f7.3)'
if not keyword_set(silent) then begin
   print,' '
   print,' '
   if (band eq 'B') then print,fmt,'At a z=',z,', the B-band K-correction is ',K,' and E-correction is ',E
   if (band eq 'R') then print,fmt,'At a z=',z,', the R-band K-correction is ',K,' and E-correction is ',E
   if (band eq 'K') then print,fmt,'At a z=',z,', the K-band K-correction is ',K,' and E-correction is ',E
   print,' '
endif


;
; Return to IDL
;
return,[K,E]
end