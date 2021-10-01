
;
; PURPOSE:
;   This algorithm finds the maximum of a generic real single-valued function
;   computed on a 2-dimensional array.  In general it will not be the global
;   maximum that is found, but rather, a reasonable local maximum.
;   With the keyword param /min, it is also capable of beind directed to find
;   a minimum
;
; CATEGORY:
;   self annealing, minimax, Numerical Recipes, minimum, maximum
;
; CALLING SEQUENCE:
;   anneal(map, obj_func, aperture, xcen, ycen, /min)
;
; INPUTS:
;   input_map 2 dimensional array
;   obj_func  objective function to be maximized
;   aperture radius about coords in which obj_func is calced (optional)
;   start_x   abscissa of starting configuration (optional)
;   start_y   ordinate of starting configuration (optional) 
;
; OPTIONAL INPUTS:
;   aperture, start_x, start_y
;
; KEYWORD PARAMETERS:
;   min
;
; OUTPUTS:
;   returns an anonymous struct {
;     map       2 dim array same size as input array, of vals of obj. func
;     run       run of vals. of obj_func vs. temp-like param t
;     funcalls  number of calls (expensive) to obj_func
;   }
;
; CHANGES:
;   $Log: anneal.pro,v $
;   Revision 1.2  2011-01-27 15:48:28  cavagnolo
;   *** empty log message ***
;
;   Revision 1.1.1.1  2007-03-23 04:39:51  cavagnolo
;   Power ratio project
;
;   Revision 1.13  2006/10/24 20:10:30  ventimig
;   Changed the procedure signature so that more of the params that govern
;   how the alg. works, are passed into the procedure, rather than being
;   hard-coded.
;
;   Revision 1.12  2006/10/11 14:03:06  ventimig
;   A comment
;
;   Revision 1.11  2006/08/22 13:46:33  ventimig
;   Minor change.
;
;   Revision 1.10  2006/08/21 17:49:18  ventimig
;   Made the aperture within which new locations may be chosen, 0.10 instead of 0.25.
;
;   Revision 1.9  2006/08/17 17:53:19  ventimig
;   Fixed a bug whereby the code forgot to restore the value of the !except global var, when there is a math error.
;
;   Revision 1.8  2006/08/16 19:27:13  ventimig
;   1. Many of the input params are now optional, and one of them is a keyword.
;   2. Function now can seek the minimum, as well as the maximum.
;
;-
FUNCTION anneal, input_map, obj_func, metrop_func, start_x, start_y, aperture, order, min = min, max_steps, max_attempts, max_successes, scale, window, tfactor

  ;get map dimensions and ascertain smaller dimension
  ncols = (size(input_map, /dimensions))[0]
  nrows = (size(input_map, /dimensions))[1]
  smaller_dim = ncols LE nrows ? ncols : nrows

  ;Use default args if necessary
  start_x = n_elements(start_x) ? start_x : round(ncols/2)
  start_y = n_elements(start_y) ? start_y : round(nrows/2)
  aperture = n_elements(aperture) ? aperture : round(smaller_dim/8)
  min = n_elements(min) ? min : 0
  max_steps = n_elements(max_steps) ? max_steps : 100          ;Maximum decrements of temp
  max_attempts = n_elements(max_attempts) ? max_attempts : 100 ;Maximum configurations to try at any temp 
  scale = n_elements(scale) ? scale : 0.1                      ;Aggressiveness of new configs
  window = n_elements(window) ? window : 0.25                  ;Search window       
  tfactor = n_elements(tfactor) ? tfactor : 0.90               ;Annealing schedule

  ;Reduction of temperature-like scaling parameter t may result in
  ;a floating point underflow error.  Remember the original value of
  ;the !except global variable, then suppress warnings, since we'll
  ;use an underflow as an occasion to bail out of the function
  old_except = !except
  !except = 0
  metric_calls = 0              ;Track how many expensive func calls we make

  ;Generate random configuration changes to determine the range of
  ;values of delta, then choose a starting temp that is considerably
  ;larger than the largest delta encountered
  samples = make_array(100, /float)
  x = start_x
  y = start_y
  old_metric = getmetric(input_map, obj_func, x, y, aperture, order)
  metric_calls++
  FOR i = 0, 9 DO BEGIN
     new_config = getconfig(input_map, x, y, start_x, start_y, window, scale)
     new_x = new_config[0]
     new_y = new_config[1]
     IF new_x GE 0 AND new_y GE 0 THEN BEGIN 
        new_metric = getmetric(input_map, obj_func, new_x, new_y, aperture, order)
        metric_calls++
        samples[i] = new_metric - old_metric
        x = new_x
        y = new_y
        old_metric = new_metric
     END 
  END
  t = max(abs(samples))*2

  ;Make an output array of the same size as the input array
  ncols = (size(input_map, /dimensions))[0]
  nrows = (size(input_map, /dimensions))[1]
  output_map = make_array(ncols, nrows, /float)

  ;Make an output array to cotain the run of metric vs. temp
  output_run = make_array(4, max_steps)

  ;Prime the loop
  x = start_x
  y = start_y
  old_metric = getmetric(input_map, obj_func, x, y, aperture, order)
  metric_calls++
  FOR i = 0L, max_steps-1 DO BEGIN 
     successes = 0              ;Reset # of successes at each temp decrement
     FOR j = 0L, max_attempts DO BEGIN
        new_config = getconfig(input_map, x, y, start_x, start_y, window, scale)
        new_x = new_config[0]
        new_y = new_config[1]
        IF new_x GE 0 AND new_y GE 0 THEN BEGIN 
           new_metric = getmetric(input_map, obj_func, new_x, new_y, aperture, order)
           metric_calls++
           delta = new_metric - old_metric
           IF Call_FUNCTION(metrop_func, delta, t, min = min) THEN BEGIN
;              print, new_x, new_y, delta, t, new_metric
              successes++
              x = new_config[0]
              y = new_config[1]
              output_map[x, y] = new_metric
              old_metric = new_metric
              output_run[0, i] = t
              output_run[1, i] = new_metric
              output_run[2, i] = x
              output_run[3, i] = y
           END
        END
        IF successes GE max_successes THEN BREAK ;skip to next temp decrement if possible
     END
     t = t*tfactor
     IF successes EQ 0 THEN BREAK ;stop trying if a temp step is completely unsucessful
     IF (i EQ (max_steps-1)/4) THEN print, '## STATUS: 25%...'
     IF (i EQ (max_steps-1)/2) THEN print, '## STATUS: 50%...'
     IF (i EQ 3*(max_steps-1)/4) THEN print, '## STATUS: 75%...'
  END
  print, '## STATUS: Complete!'

  ;Restore the remembered value of !except global var, and return the data
  !except = old_except
  RETURN, {map:output_map, run:output_run, funcalls:metric_calls}
END
