;;; $Id: histogram_percentile.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;;; Patrick Broos, 2004
;;;
;;; Computes the index of a histogram array below which lies the specified 
;;; fraction of the histogram total.
;;;
;;; The histogram can be real-valued, and have negative values (e.g. can be
;;; a background-subtracted spectrum).  
;;;
;;; The real-valued index returned is based on the assumption that the bin
;;; CENTERS are at coordinates 0, 1, 2, ...
;;;
;;; If the histogram total is non-positive zero is returned and ERROR is set.

FUNCTION histogram_percentile, hist, fraction, ERROR=error, SHOW=show

if (fraction LE 0) then message, 'fraction must be > 0'
if (fraction GE 1) then message, 'fraction must be < 1'

hist_total = total( hist, /DOUBLE )
if (hist_total LE 0) then begin
  error=1
  return, 0
endif
error=0

distribution = total( hist, /CUMULATIVE, /DOUBLE ) / hist_total
num_bins = n_elements(distribution)

;; Associate each distribution value with the RIGHT EDGE of that bin.
dist_indexes = findgen(num_bins) + 0.5

;; Estimate the index value where the distribution FIRST exceeds the desired fraction.
first_larger = dist_indexes[ (where(distribution GE fraction))[0] ]

;; Estimate the index value where the distribution LAST falls below the desired fraction.
ind = where(distribution LE fraction, count)
last_smaller = (count GT 0) ? dist_indexes[ ind[count-1] ] : (dist_indexes[0] - 1)

if keyword_set(show) then begin
  function_1d, id1, dist_indexes, distribution
  help, first_larger, last_smaller, 0.5 * (last_smaller + first_larger)
endif

;; In the case of  1 threshold crossing, last_smaller < first_larger.
;; In the case of >1 threshold crossing, last_smaller > first_larger.
;; Either way, our estimate of the bin number is the average of the two.
return, 0.5 * (last_smaller + first_larger)

end
