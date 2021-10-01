PRO WVT_TEMPERATUREMAP, evt2, maskfile, ctsfile, signo, binfile, split, evt2root

evtfile  = evt2                        ; Here, you should give the name of your evt2 file
mask     = mrdfits(maskfile,0)         ; mask specifying good (1), bad (0) data
ctsimage = mrdfits(ctsfile,0,header)   ; the counts image
image    = ctsimage                    ; is also our signal 
noise    = sqrt(image)                 ; true for Poissonian data
origdim  = size(image,/dimension)      ; get dimension of image for later
max_area = total(mask)/10.             ; Make sure you get at least 10 bins
file_delete, 'stopmenow',/quiet        ; Avoid stopping at the first iteration

; define output names based on var binfile
abinout   = binfile+'_abinned.fits'
snrout    = binfile+'_abinned_snr.fits'
dataout   = binfile+'_abinned_data.fits'
binnumout = binfile+'_abinned_binnum.fits'
nbinout   = binfile+'_nbins.dat'

targetSN = signo  ; 900 counts -> increase this number if you think this is not
                  ; sufficient to obtain your type of spectral fit? It is 
                  ; recommended to bin spectra to 20 pixels per spectral bin
                  ; before fitting, i.e. 900 cts corresponds to 45 spectral bins

; Adaptively bin your image
wvt_image, image, noise, targetSN, binnedimage, xnode, ynode, weight, $
           snbin=snbin, mask=mask, ctsimage=ctsimage, binnumber=binnumber, $
           binvalue=binvalue, save_all=save_all, max_area=max_area

; Save the output
file_delete, [abinout, snrout, dataout, binnumout],/quiet
mwrfits, binnedimage, abinout, header
snbin=[0,snbin]
mwrfits, mask*(snbin[binnumber]), snrout, header
mwrfits, save_all, dataout
mwrfits, mask*binnumber, binnumout, header

; Save the number of bins in a file called nbins.dat (needed the csh script)
openw, 3, nbinout
printf, 3, max(fix(binnumber))
close, 3
print, "Number of bins created: ", max(fix(binnumber))

; Use the binned image to extract new evt2 files for all bins. The files will
; be named "evt2.fits.XXX" with XXX indicating the bin number.
IF (split EQ 1) THEN WVT_EVTSPLIT, evtfile, binnumber, header, root=evt2root

END
