;;  acis_background_optimize, 'fullfield_4.emap', TARGET_AREA=xxx, evtfn, ENERGY_RANGE=[200, 7000], 'full_band/fullfield_4.gti', 'full_band/fullfield_4.img'      

;;; TARGET_AREA is area of source aperture in units of skypix^2.

@acis_extract

PRO acis_background_optimize, emap_filename, TARGET_AREA=target_area, PROB_NO_SOURCE_THRESHOLD=prob_no_source_threshold, event_filename, ENERGY_RANGE=energy_range, gti_filename, image_filename

;COMMON acis_background_optimize, id1, id2, id3

;; Setup a temp directory.
tempdir = 'temp' + strmid(string(F='(I4.4)',long((systime(1)*10) mod 10000)),3, /REV) +'/'
run_command, /INIT, PARAM_DIR=tempdir

temp_image_fn    = tempdir + 'temp.img'
temp_emap_fn     = tempdir + 'temp.emap'
temp_events_fn1  = tempdir + 'temp1.evt'
temp_events_fn2  = tempdir + 'temp2.evt'
temp_lc_fn       = tempdir + 'temp.lc'

run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes', 'punlearn get_sky_limits']                            


;; Filter the event list by energy; make an image matching the emap scene.
run_command, /QUIET, string(emap_filename, F="(%'get_sky_limits %s verbose=0 precision=2')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', binspec

cmd = string( event_filename, 1000*energy_range, binspec, temp_image_fn, $
    		   F="(%'dmcopy ""%s[energy=%d:%d][bin %s]"" %s')")
run_command, cmd
      
  
;; As an attempt to remove the point sources, examine the flux image to find bright pixels to exclude.
;; Apply mask to the emap and data image and flux image.

;; Form a flux image, masking out low exposure regions.
data_img = readfits(temp_image_fn)
emap     = readfits(emap_filename, emap_header)
flux_img = data_img / emap

good_ind = where(emap GT max(emap)/20., num_samples, COMPLEMENT=mask_ind, NCOMPLEMENT=num_mask )

if (num_mask GT 0) then begin
  flux_img[mask_ind] = !VALUES.F_NAN
  emap    [mask_ind] = !VALUES.F_NAN
end

flux_sample = flux_img[good_ind]
dum = where(flux_sample EQ 0, num_zeros)
frac_zeros = float(num_zeros)/num_samples
if (frac_zeros GT 0.05) then begin
  print, 100*frac_zeros, F="(%'\nWARNING! %d%% of flux samples are zero.  You should consider rebinning the exposure map, e.g. dmcopy ""obs.emap[bin #1=::4,#2=::4]"" obs_bin4.emap ')"
endif

meanclip, flux_sample, mean_flux, sigma_flux
help, mean_flux, sigma_flux
dataset_1d, id1, flux_sample

bad_ind = where( flux_img GT (mean_flux + 1*sigma_flux), count)
if (count GT 0) then begin
  print, 'Masking ', count, ' pixels for light curve computation.'
  emap    [bad_ind] = !VALUES.F_NAN
  data_img[bad_ind] = !VALUES.F_NAN
  flux_img[bad_ind] = !VALUES.F_NAN
endif


;; Write the masked flux image and spawn ds9.
writefits, temp_image_fn, flux_img, emap_header
writefits, temp_emap_fn,  emap,     emap_header

cmd = string(temp_emap_fn, temp_image_fn, F="(%'ds9 -prefs nancolor red %s %s &')")
run_command, cmd


;; Apply the masking recorded by the emap (NOT the flux image) and the energy filter to the event list.
;; Go ahead and sort the events to be ready for the lightcurve tool.
cmd = string(event_filename, temp_emap_fn, temp_events_fn1, F="(%'dmimgpick ""%s[cols time,sky]"" %s %s method=closest clobber=yes')")
run_command, cmd

cmd = string(temp_events_fn1, temp_events_fn2, F="(%'dmsort ""%s[#3>1]"" %s keys=time clobber=yes')")
run_command, cmd
 

;;  Make a light curve; sort the bins by the RATE column.
cmd = string(temp_events_fn2, temp_lc_fn, F="(%'lightcurve infile=""%s[EVENTS]"" bkgfile=NONE outfile=%s nbins=0 binlength=0 counts_per_bin=400')")
run_command, cmd



; The question is whether lightcurve will make one huge bin covering the period between obsids or whether it will
; choke!
; 
; Also, dmmerge does NOT merge the GTI tables!  I tried to fix this using fmodhead in
; /Volumes/Bulk/townsley/NGC3576/data  but now temp_obsid_4496.evt makes dmlist segfault!  Ticket 8214.
; 

;; Scale the light curve to be in units of expected background counts in the specified target aperture.
;;                             
; Find size of image pixels in units of skypixels.
pixel_size = sxpar(emap_header, 'CDELT1P')




;; Find the limiting flux for each possible cumulative exposure and plot.

  ; Find the smallest (integer) number of detected counts, c, for which 
  ; Pr(C>c; B) < prob_no_source_threshold

;; Choose a threshold background RATE.  Plot light curve and theshold.
;; We'll have to deal with the integer quantization noise from above.

;; Build a GTI table that applies that threshold.
;; NOTE we must work around a bug in dmgti!

;; Filter the event list by energy and GTI; make an image matching the emap scene.

;; 


return
end

  
