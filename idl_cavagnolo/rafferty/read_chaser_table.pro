pro read_chaser_table,table,outfile

readcol,table,seq,obsid,instr,grat,app_exp,exp_time,target,uptarget,pi,obs,ra1,ra2,ra3,dec1,dec2,dec3,$
        format='a,a,a,a,d,d,a,a,a,a,d,d,d,d,d,d'

s=size(obsid)
nlines=s[1]
        
get_lun,unit
openw,unit,outfile
fmt='$(a5,3x,a6,3x,f7.2,3x,a15,3x,f2,3x,f2,3x,f5.2,3x,f3,3x,f2,3x,f5.2)'
for i=0,nlines-1 do begin
   printf,unit,fmt,obsid[i],instr[i],exp_time[i],uptarget[i],ra1[i],ra2[i],ra3[i],dec1[i],dec2[i],dec3[i]
endfor
close,unit
free_lun,unit


return
end