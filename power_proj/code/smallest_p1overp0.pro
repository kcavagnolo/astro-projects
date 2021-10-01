FUNCTION smallest_p1overp0, input_map, aperture, max_steps, max_attempts, max_successes, scale, window, tfactor
  start_x = (size(input_map, /dimensions))[0] / 2
  start_y = (size(input_map, /dimensions))[1] / 2
  ncols = (size(input_map, /dimensions))[0]
  nrows = (size(input_map, /dimensions))[1]
  smaller_dim = ncols LE nrows ? ncols : nrows
  aperture = n_elements(aperture) ? aperture : round(smaller_dim/8)

  max_steps = n_elements(max_steps) ? max_steps : 100            ;Maximum decrements of temp
  max_attempts = n_elements(max_attempts) ? max_attempts : 100   ;Maximum configurations to try at any temp 
  max_successes = n_elements(max_successes) ? max_successes : 10 ;Maximum successes per step
  scale = n_elements(scale) ? scale : 0.1                        ;Aggressiveness of new configs
  window = n_elements(window) ? window : 0.25                    ;Search window       
  tfactor = n_elements(tfactor) ? tfactor : 0.90                 ;Annealing schedule

  obj_fun = 'power_ratio'
  metrop_func = 'metrop'
  order = 1

  annealed = $
     anneal(input_map, $
            obj_fun, $
            metrop_func, $
            start_x, $
            start_y, $
            aperture, $
            order, $
            /min, $
            max_steps, $
            max_attempts, $
            max_successes, $
            scale, $
            window, $
            tfactor)

  P_1_grid = annealed.map
  annealed_run = annealed.run[*, where(annealed.run[1, *] GT 0)]
  minval = min(annealed_run[1, *], index)
  origin = [annealed.run[2, index], annealed.run[3, index]]
  return, origin
END
