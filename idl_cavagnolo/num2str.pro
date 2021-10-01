FUNCTION num2str, in, sigs

ON_ERROR, 2
if (n_elements(sigs) eq 0) then $
   out = strcompress(in,/remove_all) $
else $
   out = strcompress(sigfig(in,sigs),/remove_all)
return,out
END

