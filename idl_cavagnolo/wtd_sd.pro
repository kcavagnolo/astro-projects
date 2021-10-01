FUNCTION WTD_SD, in_values, in_weights, _EXTRA = extra
ON_ERROR, 0
IF (N_PARAMS() NE 2) THEN MESSAGE, 'bad input'
values  = DOUBLE(in_values)
weights = DOUBLE(in_weights)
good_pts_vls = WHERE(FINITE(values) EQ 1, count)
good_pts_wts = WHERE(FINITE(weights) EQ 1, count)
IF (count GT 0) THEN BEGIN
   values  = values[good_pts_vls]
   weights = weights[good_pts_wts]
ENDIF
IF (total(weights) EQ 0.0) THEN MESSAGE, 'weights sum to zero'
sd = sqrt((total(weights*values^2.)*total(weights)-(total(weights*values))^2.)/((total(weights))^2.-total(weights^2.)))
RETURN, sd
END
