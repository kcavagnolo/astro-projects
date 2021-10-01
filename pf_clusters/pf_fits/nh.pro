PRO nh

readcol,'comp_nh.dat',FORMAT='A,A,F,F,F,F,F,F',$
        cluster,obsid,c,d,e,f,ratio

tname = 'khfjdhfks'
FOR i=0,n_elements(cluster)-1 DO BEGIN
   IF cluster[i] NE tname THEN BEGIN
      push, arr, ratio[i]
      IF ((ratio[i] LT 0.8) OR (ratio[i] GT 1.2)) THEN BEGIN
         print, format='(A20,F10.2)',cluster[i],ratio[i]
         push, dis, ratio[i]
      ENDIF
   ENDIF
   tname = cluster[i]
ENDFOR

set_plot, 'PS'
device, filename='nhcomp.eps', $
        /encapsulated, $
        /portrait, $
        /helvetica
histoplot, arr, 0.1, $
           xtitle = textoidl('N_{HI}^{DL}/N_{HI}^{LAB}'), $
           ytitle = textoidl('Number of Clusters')
oplot, [1,1], [0,1d5], linestyle=2
oplot, [0.8,0.8], [0,1d5], linestyle=2
oplot, [1.2,1.2], [0,1d5], linestyle=2
device, /close

print, 'More than 20% difference: ',n_elements(dis)
print, 'Total obs: ',n_elements(arr)

END
