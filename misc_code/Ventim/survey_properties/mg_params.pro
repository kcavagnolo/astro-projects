;returns a structure of survey parameters.
;uses modified gravity growth function,mg_growth.
function mg_params
  return, {Temp_min:4.66, $
           Temp_max:15.0, $
           tbins:10, $
           z1:0.01, $
           z2:2.00, $
           zbins:10, $
           E1:0.5, $
           E2:2.0, $
           dOmega:2.*!pi, $
           flux_limit:3.3E-14, $
           growth_fn:'mg_growth'}
end


