FUNCTION WTD_SDOM, in_weights, _EXTRA = extra
ON_ERROR, 0
IF (N_PARAMS() ne 1) THEN MESSAGE, 'bad input'
weights = DOUBLE(in_weights)
good_pts_wts = WHERE(FINITE(weights) EQ 1, count)
IF (count GT 0) THEN BEGIN
   weights = weights[good_pts_wts]
ENDIF
IF (total(weights) EQ 0.0) THEN MESSAGE, 'weights sum to zero'
sdom = 1./SQRT(TOTAL(weights))
RETURN, sdom
END
