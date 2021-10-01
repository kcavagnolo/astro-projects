C
C Emission measure table
C
      integer ntmax,ntable
      parameter (ntmax=40)
      real kt(ntmax),soft(ntmax),hard(ntmax)
      common /emtab/ ntable,kt,soft,hard
