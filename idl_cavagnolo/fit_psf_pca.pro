;+
; NAME:
;       fit_psf_pca
;
; PURPOSE:
;
;       The purpose of this program is to fit n-th order
;       polynomials to the Principal Component amplitudes of ACS/WFC PSFs
;       and to return the coefficients.
;       
; AUTHOR:
; 
;       M. James Jee, Ph.D. (Johns Hopkins University)
;
; CALLING SEQUENCE:
;
;       coefficients=fit_psf_pca(filename,order,basis=basis,mean_psf=mean_psf)
;
;
; INPUTS:
;       FILENAME - a string variable containing the PCA file
;       
;       ORDER - order of polynomial
;
;      
; KEYWORD PARAMETERS:
;       BASIS - will contain the Princpal Components in the file
;    
;       MEAN_PSF - will contain the mean PSF of the file
;
;
;
function fit_psf_pca,infile,order,basis=basis,mean_psf=mean_psf

mean_psf=mrdfits(infile,0,/silent)
basis=mrdfits(infile,1,/silent)
coeff=mrdfits(infile,2,/silent)
coo=mrdfits(infile,3,/silent)

nc=n_elements(basis[0,*])

fit=dblarr(nc,(order+1)*(order+2)/2)

for i=0,nc-1 do begin
  d=[coo,coeff[i,*]]
  dum=sfit(d,order,/irregular,kx=kx,/max_degree)
  fit[i,*]=kx
endfor

return,fit
end
