PRO make_chaser

a = mrdfits('reflex_catalog.fits',1)
l = a.LII
b = a.BII
ord = sort(l)
l = l[ord]
b = b[ord]
l = num2str(l,5)
b = num2str(b,5)
counter = 1
file = 1
close,1
openw,1,"chaser_"+num2str(file)+".list"
printf,1,"L2   B2"
FOR i=0,n_elements(l)-1 DO BEGIN
    IF (counter EQ 21) THEN BEGIN
        counter = 1
        file++
        close,1
        openw,1,"chaser_"+num2str(file)+".list"
        printf,1,'L2',STRING(9B),'B2'
    ENDIF
    printf,1,l[i]+STRING(9B)+b[i]
    counter++
ENDFOR
close,1

END
