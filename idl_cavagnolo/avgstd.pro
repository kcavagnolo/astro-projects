; avgstd.pro,  started 6-20-98 by Eric Weeks
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl/
;
; I have a C version of this, I want an IDL version....
; ability to handle 2D arrays added 3-2-99 (along with /loop)
; G. Cianci (10Apr05): now prints column number if /loop is selected

pro avgstd,data,result,quiet=quiet,loop=loop,more=more
;    data is evaluated
;    information printed out, and also returned in "result"
;    /quiet supresses printing out info
;    /more to print out skewness, kurtosis (= 0 for gaussian)
;    if given 2D array, can loop i over (i,*) if you do /loop

s=size(data)
if (s(0) ne 2) or (not keyword_set(loop)) then begin
   number=n_elements(data)
   temp=moment(data,sdev=xstd)
   xavg=temp(0) & skew=temp(2) & kurt=temp(3)
   xmin=min(data, max=xmax)
   xmed=median(data)
   if not keyword_set(quiet) then begin
      print,"   average        stdev         min           max          number   median"
      print,xavg,xstd,xmin,xmax,number,xmed
      if (keyword_set(more)) then $
      print,'skewness: ',skew,'   kurtosis[=3*alpha2] = ',kurt
   endif
endif else begin
   nel=n_elements(data(*,0))
   number=fltarr(nel)
   xavg=fltarr(nel) & xstd=fltarr(nel)
   xmin=fltarr(nel) & xmax=fltarr(nel)
   xmed=fltarr(nel) & skew=fltarr(nel) & kurt=fltarr(nel)
   if not keyword_set(quiet) then $
   print,"col     average       stdev         min         max      number      median"
   for i=0,nel-1 do begin
      dat=reform(data(i,*))
      number(i)=n_elements(dat)
      temp=moment(dat,sdev=foo)
      xstd(i)=foo
      xavg(i)=temp(0) & skew(i)=temp(2) & kurt(i)=temp(3)
      xmin(i)=min(dat, max=foo)
      xmax(i)=foo
      xmed(i)=median(dat)
      if not keyword_set(quiet) then BEGIN
         print, format='(i3, " ", 6(g12.5))',$
                i, $
                xavg(i), $
                xstd(i), $
                xmin(i), $
                xmax(i), $
                number(i), $
                xmed(i)
         if (keyword_set(more)) then   $
         print,'skewness: ',skew(i),'   kurtosis[3] = ',kurt(i)
      endif
   endfor
endelse

if (not keyword_set(more)) then begin
   result=transpose([[xavg],[xstd],[xmin],[xmax],[number],[xmed]])
endif else begin
   result=transpose([[xavg],[xstd],[xmin],[xmax],[number],[xmed], $
                     [skew],[kurt]])
endelse

end


