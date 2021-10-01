CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Simple check of functios in gfr.c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      implicit none
      real*8 a,b,x,res,fbeta,fincbeta
      print *,'a, b and x?'
      read *,a,b,x
      res=fbeta(a,b)
      print *,'B(a, b):',res
      res=fincbeta(a,b,x)
      print *,'B(a, b, x):',res
      end
