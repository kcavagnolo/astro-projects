;+
; NAME:
;   metrop
;
; PURPOSE:
;   Metropolis algorithm, from Numerical Recipes 10.9.  Issues a
;   verdict on whether to accept a reconfiguration that leads to a
;   change delta in the objective function.  If delta<0, then return
;   true.  Otherwise, return true with probability exp(delta/t), where
;   t is a temperature determined by the annealing schedule.
;
; CATEGORY:
;   Self Annealing, minimax, Numerical Reconfiguration
;
; CALLING SEQUENCE:
;   metrop(delta,t,/min)
;
; INPUTS:
;   delta     change in the objective function
;   t         temperature-like scaling parameter for prob. function
;
; KEYWORD PARAMETERS:
;   min
;
; OUTPUTS:
;   returns a boolean, true if step should be taken, false if not
;
; CHANGES:
;   $Log: metrop.pro,v $
;   Revision 1.1.1.1  2007-03-23 04:39:51  cavagnolo
;   Power ratio project
;
;   Revision 1.6  2006/10/11 14:03:06  ventimig
;   A comment
;
;   Revision 1.5  2006/08/16 19:29:11  ventimig
;   1. Function now can seek the minimum, as well as the maximum.
;
;-
FUNCTION metrop, delta, t, min = min
  min = n_elements(min) ? min : 0
  dir = min ? -1 : 1            ;direction to seek, down or up, min or max
  returnval = (dir*delta GT 0.0) OR (randomu(systime_seed) LT exp(dir*float(delta)/float(t)))
  IF check_math() GT 0 THEN BEGIN ;bail out of function on math error
    returnval = 0
  END
  return, returnval
END
