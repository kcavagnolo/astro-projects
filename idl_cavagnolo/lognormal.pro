FUNCTION lognormal, x, mu, sigma

ON_ERROR, 2

IF n_params() NE 3 THEN BEGIN
    RETURN, -999
ENDIF

zeroes = where(x EQ 0.0, countz)
nonzero = where(x NE 0.0)
y = dblarr(n_elements(x))
y[nonzero] = (1/(sqrt(2.0*!pi*sigma^2)))*(1.0/abs(x[nonzero]))* $
             exp(-(alog(abs(x[nonzero]))-abs(mu))^2/(2*sigma^2))
IF countz GT 0 THEN BEGIN
    y[zeroes] = 0.0
ENDIF

RETURN, y
END
