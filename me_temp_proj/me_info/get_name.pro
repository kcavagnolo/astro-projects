PRO get_name, ref

a = rd_tfile(ref,/auto)
obs = a(0,*)

sep = string(9B)

openw,1,'new.list'
printf,1,'#Name'+sep+sep+'ObsID'+sep+'Instr'+sep+'ExpTime'+sep+'RA'+sep+'DEC'
FOR i=0,n_elements(obs)-1 DO BEGIN
    obsid = strcompress(obs[i],/remove_all)
    file = '../acis/'+obsid+'/reprocessed/'+obsid+'_evt2.fits'
    file = mrdfits(file,1,hdr)
    push,name,strcompress(sxpar(hdr,'OBJECT'),/remove_all)
    push,instr,strcompress(sxpar(hdr,'DVAL2'),/remove_all)
    push,exp_time,strcompress(sxpar(hdr,'EXPOSURE'),/remove_all)
    push,ra,strcompress(sxpar(hdr,'RA_TARG'),/remove_all)
    push,dec,strcompress(sxpar(hdr,'DEC_TARG'),/remove_all)
ENDFOR

FOR j=0,n_elements(obs)-1 DO BEGIN
    printf,1,name[j],sep,sep,obs[j],sep,instr[j],sep,exp_time[j],sep,sep,ra[j],sep,dec[j]
ENDFOR

close,1

END
