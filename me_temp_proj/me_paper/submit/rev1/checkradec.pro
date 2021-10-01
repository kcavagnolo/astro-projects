pro junk

readcol, 'crud', FORMAT='A,A,A,A,F,A,A,F,F', $
   name, cds, ra, dec, cdb, cdd, cde, z, lum

stop

prevname = 'fdfdfd'
for i=0,n_elements(name)-1 do begin
   tname = name[i]
   if tname eq prevname then goto,skip
   tz = z[i]
   cosmology,tz,cosmo,/silent
   kpcarc = cosmo[4]
   ord = where(name EQ tname)
   tra = ra[ord]
   tdec = dec[ord]
   void, fra
   void, fdec
   for j=0,n_elements(tra)-1 do begin
      decra = str2arr(tra[j],':')
      decdec = str2arr(tdec[j],':')
      push, fra, (decra[0]+(decra[1]/60.)+(decra[2]/3600.))*(360./24.)
      IF decdec[0] LT 0. THEN term = -1. ELSE term = 1.
      push, fdec, term*(abs(decdec[0])+(decdec[1]/60.)+(decdec[2]/3600.))
   endfor
   radiff = abs(fra-fra[0])
   decdiff = abs(fdec-fdec[0])
   for d=0,n_elements(radiff)-1 do begin
      if radiff[d]*3600*kpcarc gt 1.0 or $
         decdiff[d]*3600*kpcarc gt 1.0 then begin
         print, tname
         print, radiff[d]*3600*kpcarc
         print, decdiff[d]*3600*kpcarc
      endif
   endfor
skip:
   prevname = tname
endfor
end
