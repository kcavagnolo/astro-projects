FUNCTION temporary, a
return,a
END

PRO void, in
IF n_elements(in) GT 0 THEN void = temporary(in); ELSE print,"ERROR, VOID: nothing to void."
END
