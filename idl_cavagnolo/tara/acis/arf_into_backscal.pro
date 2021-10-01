;;; $Id: arf_into_backscal.pro,v 1.2 2008-10-15 19:35:50 cavagnolo Exp $
;;;
;;; Patrick Broos, 2002
;;;
;;; The ARF curve is multiplied by the supplied geometric area and written
;;; to the BACKSCAL column of the supplied spectrum.

PRO arf_into_backscal, spectrum_fn, arf_fn, rmf_fn, area, new_spectrum_fn

;; Read input data & headers.
arf_table = mrdfits(arf_fn, 1)
rmf_table = mrdfits(rmf_fn, 1)

pheader    = headfits(spectrum_fn)
spec_table = mrdfits( spectrum_fn, 1, theader)


;; Remove BACKSCAL keyword & add BACKSCAL column to spectrum table.
sxdelpar, theader, 'BACKSCAL'

num_channels = n_elements(spec_table)
row = create_struct( spec_table[0], 'BACKSCAL', 0.0 )

bin_table = replicate(row, num_channels)
copy_struct, spec_table, bin_table


;; Populate BACKSCAL column by grabbing the ARF value that corresponds to
;; each channel energy.
channel_energy = (rmf_table.E_MIN + rmf_table.E_MAX) / 2.0

for ii=0,num_channels-1 do begin
  ind = (where(channel_energy[ii] GE arf_table.energ_lo))[0]

  bin_table[ii].BACKSCAL = arf_table[ind].SPECRESP
endfor


;; Store area in AREASCAL/
fxaddpar, theader, 'AREASCAL', area


;; Write new spectral file.
get_date, dte
fxaddpar, pheader, 'DATE', dte
fxaddpar, theader, 'DATE', dte
fxaddpar, pheader, 'CREATOR', "$RCSfile: arf_into_backscal.pro,v $, v$Revision: 1.2 $"
fxaddpar, theader, 'CREATOR', "$RCSfile: arf_into_backscal.pro,v $, v$Revision: 1.2 $"

fxaddpar, pheader, "FNFITS", new_spectrum_fn
writefits, new_spectrum_fn, 0, pheader

mwrfits, bin_table, new_spectrum_fn, theader
 
print, 'Wrote ', new_spectrum_fn   
return
end
