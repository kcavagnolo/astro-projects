Function interpol_sample, In, X, samples, missing=missing, print=print
;;+
;; This function used as the extension of interpolation, but use the 
;; the closest sample instead of linear interpolation.
;; INPUT:
;;	IN: the input array that is a function of X
;;	X:  the indenpendent array that has same elements as IN
;;	samples: The index that was used to match closet samples
;; KEYWORDS:
;;	print: print out the closest index (of samples  in IN array)
;;	missing: Not use yet, for future use.
;; OUTPUT:
;;	The closest sample of IN.
  ON_ERROR, 2
  NNX = N_elements(in)
  if NNX NE N_elements(X) Then $
    message, "INPUT array has to have same elements with the index array'
  ind = Value_locate(X, samples)
  frac = samples - float(ind)
  k = where(frac GE 0.5, cnt)
  ;help,k
  ;print,ind
  if cnt GT 0 Then $
  ind[k] = ind[k] + 1
  ;;INDEX has to >=0 and <NNX
  
  ind = ind > 0L < (NNX-1)  
  if keyword_set(print) then $
     print,ind
  ;Return, In[ind]
  return, interpol(IN, X, samples)

END
