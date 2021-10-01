Function sind, degree
;;function to calculate sine values for degree input, instead of radians
;;Call sequence:
;; result=SIND(degree)
;;
   return, SIN(degree*!pi/180.0d)
   
End
