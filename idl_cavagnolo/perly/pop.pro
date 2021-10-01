; array = [1,2,3]
; print, array
;   1       2       3
; out = pop(array)
; print, out
;   3
; print, array
;   1       2

FUNCTION pop, array
  IF n_elements(array) EQ 0 THEN BEGIN
      print, "# ERROR: nothing to pop"
      value = -1
  ENDIF ELSE BEGIN
      length = n_elements(array)
      value = array[length-1]
      FOR i = 0, length-2 DO BEGIN
          push, newarray, array[i]
      ENDFOR
      array = newarray
  ENDELSE

  RETURN, value

END
