pro make_sb_shock,infile,outfile,background

readcol,infile,rin,rout,sb_norm,sb_norm_err,sb,sb_err

sb=sb - background

get_lun,unit
openw,unit,outfile
printf,unit,'R_in (pixel), R_out (pixel), SB (ct/pix/sec), SB_error'
for i=0,n_elements(rin)-1 do printf,unit,strtrim(string(rin[i]),2)+' '+strtrim(string(rout[i]),2)+' '+strtrim(string(sb[i]),2)+' '+strtrim(string(sb_err[i]),2)
printf,unit,' '
close,unit
free_lun,unit

return
end