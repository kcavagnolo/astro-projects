PRO reproject_event_list, filename, xkey, ykey, targetpix, targetval, delta_gain
; Targetpix & targetval are the coordinates of the fiducial point of the
; target observation, i.e. (TCRPX2,TCRPX3) & (TCRVL2,TCRVL3) from the
; first observation.

t=mrdfits( filename, 1, header )

; Build an astrometry structure for this observation.
crpixX = fxpar( header, xkey.tcrpx )
crpixY = fxpar( header, ykey.tcrpx )

crvalX = fxpar( header, xkey.tcrvl )
crvalY = fxpar( header, ykey.tcrvl )

cdeltX = fxpar( header, xkey.tcdlt )
cdeltY = fxpar( header, ykey.tcdlt )

ctypeX = fxpar( header, xkey.tctyp )
ctypeY = fxpar( header, ykey.tctyp )

make_astr, astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
		 CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

; Compute the coordinates of the reference point in this observations's 
; x/y system.
ad2xy, targetval[0], targetval[1], astr, xindex, yindex
refx=xindex+1
refy=yindex+1

; Adjust the event positions to the reference coordinate system.
delx = (targetpix[0] - refx)
dely = (targetpix[1] - refy)
print, filename, delx, dely
t.x = t.x + delx
t.y = t.y + dely

; Change the fiducial point of this observation to match the reference.
sxaddpar, header, xkey.tcrpx, targetpix[0]
sxaddpar, header, ykey.tcrpx, targetpix[1]
sxaddpar, header, xkey.tcrvl, targetval[0]
sxaddpar, header, ykey.tcrvl, targetval[1]

; Filter spatially.

; Adjust energies.
t.energy = t.energy * delta_gain

; Write out the modified event list.
newfilename = filename + '.reproject'

mkhdr, pheader, '', /EXTEND
writefits, newfilename, 0, pheader

mwrfits, t, newfilename, header
return
end


PRO reproject_event_files, obs_list, DELTA_GAIN=delta_gain

if (NOT keyword_set(xcol)) then XCOL=11
if (NOT keyword_set(ycol)) then YCOL=12
keynames = replicate({tcrpx:'', tcrvl:'', tcdlt:'', tctyp:''},2)
keynames.tcrpx = string([xcol,ycol],F='("TCRPX",I0)')
keynames.tcrvl = string([xcol,ycol],F='("TCRVL",I0)')
keynames.tcdlt = string([xcol,ycol],F='("TCDLT",I0)')
keynames.tctyp = string([xcol,ycol],F='("TCTYP",I0)')
print, keynames
xkey = keynames[0]
ykey = keynames[1]


N = numlines(obs_list)
event_files = strarr(N)
openr,unit,obs_list, /GET_LUN
readf,unit,event_files
close,unit

; Read the first header to define the target fiducial point.
header    = headfits( event_files[0], EXTEN=1 ) 
targetpix = [fxpar( header, xkey.tcrpx ),fxpar( header, ykey.tcrpx )]
targetval = [fxpar( header, xkey.tcrvl ),fxpar( header, ykey.tcrvl )]

if (NOT keyword_set(delta_gain)) then delta_gain = replicate(1.0,N)
for ii=0,N-1 do begin
  reproject_event_list, event_files[ii], xkey, ykey, targetpix, targetval, delta_gain[ii]
endfor

return
end
