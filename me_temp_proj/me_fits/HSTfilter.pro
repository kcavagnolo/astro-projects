pro filter, in1

dat = mrdfits(in1,1,hdr)
name = dat.cluster
obsid = dat.obsid
z = dat.z
tx = dat.tx77
tra = dat.ra
tdec = dat.dec
FOR i=0,n_elements(tra)-1 DO BEGIN
    temp = str2arr(tra[i],':')
    push, ra, temp[0]
    temp = str2arr(tdec[i],':')
    push, dec, temp[0]
ENDFOR

cut = where((dat.z GE 0.15) AND $
            (dat.z LE 0.30) AND $
            (dec GE -20) AND $
            (dat.tx77 GE 6.5) $
           )

name = name(cut)
obsid = obsid(cut)
z = z(cut)
tx = tx(cut)
dec = dec(cut)

print, n_elements(name),' clusters found from filtering'

openw,1,'filter.list'
sep = string(9B)
printf,1,'#Name',sep,sep,'Obsid',sep,'z',sep,'Tx',sep,'Dec'
FOR i=0,n_elements(name)-1 DO BEGIN
    printf,1,name[i],sep,sep,obsid[i],sep,z[i],sep,tx[i],sep,dec[i]
ENDFOR
close,1

END

