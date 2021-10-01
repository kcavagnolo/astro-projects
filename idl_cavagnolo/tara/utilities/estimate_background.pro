;;; $Id: estimate_background.pro,v 1.1 2007-10-31 04:34:04 cavagnolo Exp $
;;; Estimates a Poisson background (mean) value for a data set contaminated
;;; by positive outliers (e.g. stars).

;;; Test with calls like:
;;;   estimate_background, random(100000, POISSON=0.1), /PLOT
;;; or run the test program at the end of this file.

;;; The algorithm is to find the smallest upper limit of the "good"
;;; data that contains the +3-sigma wing of the good data distribution.

PRO estimate_background, data, background, PLOT=do_plot, VERBOSE=verbose

min_wing_size = 4
wing_sigma    = 3

;; Accept all the data below the median.
num_data = n_elements(data)
max_val = median(data, /EVEN)

repeat begin
  max_val = max_val + 1

  background = mean(/DOUBLE, data[where(data LE max_val, num_good)])
  
  if (num_good EQ num_data) then break

  ; Estimate the extent of the upper wing of the distribution, using the
  ; Gaussian approximation to the Poisson distribution.
  upper_wing_size = min_wing_size > (wing_sigma * sqrt(background))

endrep until (max_val GT (background + upper_wing_size))

if keyword_set(do_plot) then begin
  hist = histogram( data, MIN=0, MAX=ceil(max_val) )
  plot, hist, XSTYLE=3, PSYM=10
  oplot, [background,background], [0, max(hist)], LINE=1
endif

if keyword_set(verbose) then begin
  print, background, 100*(1 - num_good/float(num_data)), max_val, $
         F='(%"background=%f; %5.2f%% of data discarded above %f")'
endif

return
end



PRO test
files = findfile("*.img")

for ii=0,n_elements(files)-1 do begin
  print
  print, files[ii]
  im=readfits(files[ii])
  estimate_background, im
  wait,3
endfor
return
end
