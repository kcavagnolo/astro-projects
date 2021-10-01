;; $Id: poisson_interval.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;; Patrick Broos, 2004

;; Compute a 1-sided [0..N] confidence interval for a Poisson random variable
;; of the specified mean that encompasses the specified probability.


FUNCTION poisson_interval, poisson_mean, significance

;; For means bigger than 100, use Gaussian approximation since the exp()
;; in the direct Poisson calculation underflows.
if (poisson_mean GE 100) then begin
  upper_limit = -gauss_cvf(significance)
  
  return, sqrt(poisson_mean)*upper_limit + poisson_mean

endif else begin

  prob            = exp(-double(poisson_mean))
  cumulative_prob = prob
  dum = check_math()
  pix_val = 0.0
  while ((cumulative_prob LT significance) AND (check_math(/PRINT) EQ 0)) do begin
    pix_val = pix_val + 1
    prob            = (poisson_mean/pix_val) * prob
    cumulative_prob = cumulative_prob + prob
  endwhile  

  return, pix_val
endelse
end
