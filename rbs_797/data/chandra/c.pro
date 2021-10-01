PRO c

readcol, 'nsb.dat', FORMAT='D,D,D', comment='#', $
         nr, nsb, nsberr
readcol, 'ssb.dat', FORMAT='D,D,D', comment='#', $
         sr, ssb, ssberr
readcol, 'ecavsb.dat', FORMAT='D,D,D', comment='#', $
         er, esb, esberr
readcol, 'wcavsb.dat', FORMAT='D,D,D', comment='#', $
         wr, wsb, wsberr

savg = (nsb+ssb)/2.
cavg = (esb+wsb)/2.

plot, nr, savg, /xlog, /ylog
oplot, er, cavg

stop

END
