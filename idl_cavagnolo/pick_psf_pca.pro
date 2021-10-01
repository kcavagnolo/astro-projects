;+
; NAME:
;       pick_psf_pca
;
; PURPOSE:
;
;       The purpose of this program is to return a ACS/WFC PSF
;       at an arbitrary (x,y) location.
;       
; AUTHOR:
; 
;       M. James Jee, Ph.D. (Johns Hopkins University)
;
; CALLING SEQUENCE:
;
;       psf=pick_psf_pca(fit,basis,x,y,mean_psf)
;
;
; INPUTS:
;       FIT - output from FIT_PSF_PCA. It contains the polynomical coeeficients
;       
;       BASIS - the Principal Components returned by FIT_PSF_PCA
;  
;       x,y - the position of the PSF
; 
;       mean_psf - the mean PSF for the Principal Components
;
;
function pick_psf_pca,fit,basis,x,y,mean_psf

xc=double(x)
yc=double(y)

nc=n_elements(fit[*,0,0])

coeff=dblarr(nc)
order=n_elements(fit[0,*])
order=(-3+sqrt( 9.-4.*(2.-2*order)))/2

for i=0,nc-1 do begin
  o=0
  for j=0,order do for k=0,order-j do begin
    coeff[i]+=fit[i,o] * xc^j * yc^k
    o=o+1
  end
end

s=sqrt(n_elements(basis[*,0]))

psf= coeff ## basis
return,abs(reform(psf,s,s)+reform(mean_psf,s,s))
end
    

