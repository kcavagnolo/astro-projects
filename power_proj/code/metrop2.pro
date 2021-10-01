;+
; NAME:
;   metrop2
;
; PURPOSE:
;   Metrop2olis algorithm, from Numerical Recipes 10.9.  Issues a
;   verdict on whether to accept a reconfiguration that leads to a
;   change delta in the objective function.  If delta<0, then return
;   true.  Otherwise, return true with probability exp(delta/t), where
;   t is a temperature determined by the annealing schedule.
;
; CATEGORY:
;   Self Annealing, minimax, Numerical Reconfiguration
;
; CALLING SEQUENCE:
;   metrop2(delta,t,/min)
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
;   $Log: metrop2.pro,v $
;   Revision 1.1.1.1  2007-03-23 04:39:51  cavagnolo
;   Power ratio project
;
;   Revision 1.1  2006/10/15 16:55:58  ventimig
;   Various changes to a number of projects and subprojects.
;
;
;-
FUNCTION metrop2, delta, t, min = min
  min = n_elements(min) ? min : 0
  dir = min ? -1 : 1            ;direction to seek, down or up, min or max
  returnval = (dir*delta GT 0.0)
  IF check_math() GT 0 THEN BEGIN ;bail out of function on math error
    returnval = 0
  END
  return, returnval
END
