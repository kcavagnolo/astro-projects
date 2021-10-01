PRO plaw, x, coeffs, y, pder
y = coeffs[0] * x^(coeffs[1])
IF N_PARAMS() GE 4 THEN $
  pder = [[x^coeffs[1]], [coeffs[0]*x^coeffs[1]*ALOG(x)]]
END
