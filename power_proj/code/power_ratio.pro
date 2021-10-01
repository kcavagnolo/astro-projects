;+
; NAME:
;   power_ratio
;
; PURPOSE:
;   Computes ratio of power into a particular higher-order
;   moment, over the power into the lowest-order momnent
;   as a measure of substructure, within an aperture.
;
; CATEGORY:
;   clusters, substructure
;
; CALLING SEQUENCE:
;   power_ratio(input_map, xcen, ycen, R_ap, m)
;
; INPUTS:
;   input_map     input array on which to compute ratio
;   xcen      abscissa of center of aperture
;   ycen      ordinate of center of aperture
;   R_ap      radius of region used to evaluate ratio (pixels)
;   m         order of moment in numerator of ratio
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;   returns a float of the power ratio
;
; CHANGES:
;   $Log: power_ratio.pro,v $
;   Revision 1.1.1.1  2007-03-23 04:39:51  cavagnolo
;   Power ratio project
;
;   Revision 1.5  2006/10/24 20:08:39  ventimig
;   Stripped out the apparatus for making this go fast.  In fact, stripped
;   out the calls to the self-annealing alg. completely.  Now, the self-
;   annealing alg. is used outside this procedure, to find the origin
;   for the aperture.  After that, power-ration doesn't know about, or
;   care, how the origin was found.
;
;   Revision 1.4  2006/10/11 14:02:53  ventimig
;   A comment
;
;   Revision 1.3  2006/08/16 19:26:06  ventimig
;   1. Added a comment block.
;   2. Made the moment number m an optional param.
;
;-
FUNCTION power_ratio, input_map, xcen, ycen, R_ap, m
  origin = [xcen, ycen]
  P_0 = moment_power(input_map, origin[0], origin[1], R_ap, 0)
  P_m = moment_power(input_map, origin[0], origin[1], R_ap, m)
  return, P_m/P_0
END
