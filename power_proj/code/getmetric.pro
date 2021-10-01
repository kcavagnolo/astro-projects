;+
; NAME:
;   getmetric
;
; PURPOSE:
;   Returns the value of the cost function (i.e., the objective
;   function) for a proposed configuration change.
;
; CATEGORY:
;   Simulated Anneal, minimax, Numerical Recipes
;
; CALLING SEQUENCE:
;   getmetric(map, new_x, new_y, func, radius)
;
; INPUTS:
;   map        2-dimensionl array on which to compute metric
;   new_x      abscissa of coord where metric is to be computed
;   new_y      ordinate of coord where metric is to be computed
;   func       string name of a callable real-valued metric function
;   radius     aperture radius on map within which to compute metric
;-
FUNCTION getmetric, map, func, new_x, new_y, radius, order
  return, Call_FUNCTION(func, map, new_x, new_y, radius, order)
END
