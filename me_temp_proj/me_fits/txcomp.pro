pro txcomp

dat1 = mrdfits('HSTprop_r2500-70_nhfro_fefree.fits',1)
dat2 = mrdfits('HSTprop_r2500_nhfro_fefree.fits',1)
dat3 = mrdfits('HSTprop_r5000-70_nhfro_fefree.fits',1)
dat4 = mrdfits('HSTprop_r5000_nhfro_fefree.fits',1)
dat5 = mrdfits('HSTprop_rmax-70_nhfro_fefree.fits',1)
dat6 = mrdfits('HSTprop_robs-70_nhfro_fefree.fits',1)

tx1 = dat1.tx77
tx1lo = dat1.tx77-dat1.tx77lo
tx1hi = dat1.tx77hi-dat1.tx77
tx2 = dat2.tx77
tx2lo = dat2.tx77-dat2.tx77lo
tx2hi = dat2.tx77hi-dat2.tx77
tx3 = dat3.tx77
tx3lo = dat3.tx77-dat3.tx77lo
tx3hi = dat3.tx77hi-dat3.tx77
tx4 = dat4.tx77
tx4lo = dat4.tx77-dat4.tx77lo
tx4hi = dat4.tx77hi-dat4.tx77
tx5 = dat5.tx77
tx5lo = dat5.tx77-dat5.tx77lo
tx5hi = dat5.tx77hi-dat5.tx77
tx6 = dat6.tx77
tx6lo = dat6.tx77-dat6.tx77lo
tx6hi = dat6.tx77hi-dat6.tx77

m=0
FOR i=0,n_elements(dat1.cluster)-1 DO BEGIN
    name=dat1.cluster
    obs=dat1.obsid
    ptit = strcompress(name[i])+' '+strcompress(obs[i])
    IF m NE 0 THEN BEGIN
        void,temp
        void,templo
        void,temphi
    ENDIF
    push,temp,tx1[i]
    push,temp,tx2[i]
    push,temp,tx3[i]
    push,temp,tx4[i]
    push,temp,tx5[i]
    push,temp,tx6[i]
    push,templo,tx1lo[i]
    push,templo,tx2lo[i]
    push,templo,tx3lo[i]
    push,templo,tx4lo[i]
    push,templo,tx5lo[i]
    push,templo,tx6lo[i]
    push,temphi,tx1lo[i]
    push,temphi,tx2lo[i]
    push,temphi,tx3lo[i]
    push,temphi,tx4lo[i]
    push,temphi,tx5lo[i]
    push,temphi,tx6lo[i]
    plot,temp,psym=2,$
         /xsty,/ysty,$
         xran=[-1,6],yran=[min(temp-templo),max(temp+temphi)],$
         title=ptit
    oploterror,temp,templo,/lobar,psym=2
    oploterror,temp,temphi,/hibar,psym=2
    yesno = ""
    read, "Advance? (yes=y): ", yesno
    IF (yesno eq "y") THEN print,"advancing"
    m++
ENDFOR

END
