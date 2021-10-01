Function cosd, degree
;;function to calculate cosine values for degree input, instead of radians
;;Call sequence:
;; result=COSD(degree)
;;
   return, COS(degree*!pi/180.0d)
   
End
