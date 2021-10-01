pro junk

; input files array
file = '../me_fits/dat/counts_inner50.dat'
readcol, file, FORMAT='A,F',name, cts

; do all the plotting
set_plot,'PS'
device, filename = "temp.ps", /color
!fancy = 4
!linetype = 0
!p.font = 0
histoplot, cts, 0.5, /log, $
           xtitle = textoidl('Counts Inner 50 kpc'), $
           ytitle = textoidl('Number of Clusters'), $
           charsize=0.8
device, /close
END
