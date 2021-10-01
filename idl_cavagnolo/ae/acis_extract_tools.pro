;;; $Id: acis_extract_tools.pro,v 1.2 2008-10-15 19:37:03 cavagnolo Exp $
;;; Accessory tools for ACIS Extract. 
;;; Patrick Broos, Penn State University, 2004

@acis_extract

;;; ==========================================================================
;;; This program will "poke" a FITS keyword into the source.stats file for each
;;; source in the srclist.
;;; KEYWORD is a scalar string; VALUE and COMMENT are either scalar string or 
;;; string vectors whos length matches the cataqlog.
PRO   ae_poke_source_property, catalog_or_srclist, KEYWORD=keyword, VALUE=value_p, COMMENT=comment_p

src_stats_basename       = 'source.stats'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

case n_elements(value_p) of
  1: value = replicate(value_p,num_sources)
  num_sources: value = value_p
  else: message, 'Length of VALUE parameter must be either 1 or '+string(num_sources)
endcase

case n_elements(comment_p) of
  1: comment = replicate(comment_p,num_sources)
  num_sources: comment = comment_p
  else: message, 'Length of COMMENT parameter must be either 1 or '+string(num_sources)
endcase

for ii = 0, num_sources-1 do begin
  sourcedir = sourcename[ii] + '/' 
  
  stats_fn  = sourcedir + src_stats_basename
  stats = headfits(stats_fn, ERRMSG=error)
  
  if keyword_set(error) then begin
    print, 'WARNING! Could not read '+stats_fn
    continue
  endif
    
  fxaddpar, stats, keyword, value[ii], comment[ii]
  writefits, stats_fn, 0, stats
endfor ;ii
return
end


;;; ==========================================================================
;;; This tool fits two diffuse sources simultaneously, with one serving as an 
;;; observation of the astrophysical (sky) background.
;;; Each source is assume to have its own background spectrum representing the
;;; local instrumental background (see diffuse recipe).
;;;
;;; The calibration (ARFs) for the diffuse sources is assumed to have been put 
;;; onto a "per arcsec^2" basis by AE.
;;;
;;; Only chi^2 fitting is supported.

PRO ae_fit_diffuse, object_name, sky_name, CHANNEL_RANGE=channel_range, $
                  MODEL_FILENAME=model_filename, MODEL_CHANGES_FILENAME=model_changes_filename, $
                  SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                  INTERACTIVE=interactive
                  
creator_string = "ae_fit_diffuse, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

result = routine_info( 'acis_extract', /SOURCE )
fdecomp, result.PATH, disk, codedir

if NOT keyword_set(model_filename) then begin
  print, 'ERROR:Parameter MODEL_FILENAME not supplied!'
  GOTO, FAILURE
endif

; SNR_RANGE[1] is the user's goal for defining groups; SNR_RANGE[0] is the lower limit allowed before we abort the grouping attempt
if (n_elements(snr_range) EQ 0) then $
  snr_range = [1,3]
if (n_elements(snr_range) NE 2) then begin
  print, 'ERROR: keyword SNR_RANGE should be a 2-element vector giving the range of SNR allowed for each spectral group, e.g. [2.5,5].'
  GOTO, FAILURE      
endif

if (snr_range[1] LT 0) then begin
  print, 'ERROR: minimum SNR value (SNR_RANGE[1]) must be positive'
  GOTO, FAILURE
endif

if (n_elements(num_groups_range) EQ 0) then $
  num_groups_range = [10,250]
if (n_elements(num_groups_range) NE 2) then begin
  print, 'ERROR: keyword NUM_GROUPS_RANGE should be a 2-element vector specifying how many spectral groups are desired, e.g. [10,250].'
  GOTO, FAILURE      
endif

  
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        =         tempdir + 'param/'

run_command, /INIT, PARAM_DIR=param_dir


;; Create directory for this pair of sources.
sourcename = object_name+'_AND_'+sky_name
sourcedir = sourcename+'/'
file_mkdir, sourcedir

;; ------------------------------------------------------------------------
;; Create symlinks to needed object files; group object spectrum
suffix = ['.pi', '_bkg.pi', '.arf', '.rmf']
fn = object_name+suffix
file_delete, sourcedir+fn, /QUIET
file_link, '../'+object_name+'/'+fn, sourcedir

obj_src_spectrum_fn     = sourcedir+fn[0]
obj_bkg_spectrum_fn     = sourcedir+fn[1]
obj_grouped_spectrum_fn = ''

ae_group_spectrum, obj_src_spectrum_fn, obj_bkg_spectrum_fn, obj_grouped_spectrum_fn, $
                   CHANNEL_RANGE=channel_range, $
                   SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                   CREATOR_STRING=creator_string, $
                   this_snr_goal, grp_name, channel_starting_group, num_groups, inband_counts

obj_ignore_spec = string(num_groups, F='(%"1,%d")')

;; ------------------------------------------------------------------------
;; Create symlinks to needed object files; group object spectrum
fn = sky_name+suffix
file_delete, sourcedir+fn, /QUIET
file_link, '../'+sky_name+'/'+fn, sourcedir

sky_src_spectrum_fn     = sourcedir+fn[0]
sky_bkg_spectrum_fn     = sourcedir+fn[1]
sky_grouped_spectrum_fn = ''

ae_group_spectrum, sky_src_spectrum_fn, sky_bkg_spectrum_fn, sky_grouped_spectrum_fn, $
                   CHANNEL_RANGE=channel_range, $
                   SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                   CREATOR_STRING=creator_string, $
                   this_snr_goal, junk, channel_starting_group, num_groups

  
sky_ignore_spec = string(num_groups, F='(%"1,%d")')


;; ------------------------------------------------------------------------
;; Choose energy bins for cplinear model of sky spectrum.
;  We ask AE's grouping algorithm to create exactly 11 groups in order to get the 10 vertices that cplinear needs.
ae_group_spectrum, sky_src_spectrum_fn, sky_bkg_spectrum_fn, '/dev/null', $
                       CHANNEL_RANGE=channel_range, $
                       SNR_RANGE=[0,this_snr_goal], NUM_GROUPS_RANGE=[11,11], $
                       CREATOR_STRING=creator_string, $
                       this_snr_goal, junk, channel_starting_group, num_groups 

if (num_groups NE 11) then begin
  print, 'ERROR: grouping algorithm was not able to choose channels for the cplinear sky model.'
  retall
endif

; Use RMF & ARF to figure out the energy for each spectral channel.
ae_channel_energy_and_arf, sourcedir+fn[3], sourcedir+fn[2], $
  channel_number, channel_lowenergy, channel_highenergy, channel_midenergy

energy_starting_group  = interpol(channel_midenergy, channel_number, channel_starting_group)


;; ------------------------------------------------------------------------
;; Build a name for the model using the basename of MODEL_FILENAME and if supplied
;; appending the basename of MODEL_CHANGES_FILENAME.
modelsubdir              = 'spectral_models/'

fdecomp, model_filename, disk, dir, base_model_name

model_name = base_model_name

if keyword_set(model_changes_filename) then begin
  fdecomp, model_changes_filename, disk, dir, model_changes_name, model_changes_qual

  ; Look for a local file in the source directory to override the specified MODEL_CHANGES_FILENAME.
  custom_model_name = strjoin([model_name,model_changes_name], '_')
  fit_custom_fn     = sourcedir + modelsubdir + custom_model_name + '.' + model_changes_qual[0]
  
  if NOT file_test(fit_custom_fn) then begin
    ; No local override found, so prepare to use the specified files.
    if (n_elements(model_changes_filename) EQ 1) then begin
      fit_custom_fn = model_changes_filename 
    endif else begin
      ; Concatenate all the MODEL_CHANGES files specified.
      fit_custom_fn = tempdir + 'model_changes.xcm'
      cmd = string(strjoin(model_changes_filename, ' '), fit_custom_fn, F='(%"cat %s >! %s")')
      run_command, /UNIX, cmd, /QUIET
    endelse
  endif
  
  if file_test(fit_custom_fn) then begin
    model_name = custom_model_name
    print, 'CUSTOMIZATIONS to xcm script:'
    run_command, /UNIX, 'cat '+fit_custom_fn
    print
  endif else fit_custom_fn = ''
endif else fit_custom_fn = ''

  
;; ------------------------------------------------------------------------
;; Build the fitting script.
file_mkdir, sourcedir + modelsubdir
fit_result_root         = grp_name + '_' + model_name
fit_xcm_fn             = modelsubdir +fit_result_root + '.xcm'

openw,  xcm_unit, sourcedir + fit_xcm_fn, /GET_LUN
printf, xcm_unit, file_basename(obj_grouped_spectrum_fn), F='(%"set obj_spectrum_filename       \"%s\"")'
printf, xcm_unit, file_basename(sky_grouped_spectrum_fn), F='(%"set sky_spectrum_filename       \"%s\"")'
printf, xcm_unit, obj_ignore_spec,                    F='(%"set obj_ignore_spec             \"%s\"")'
printf, xcm_unit, sky_ignore_spec,                    F='(%"set sky_ignore_spec             \"%s\"")'
printf, xcm_unit, fit_result_root,                    F='(%"set model_name              \"%s\"")'
printf, xcm_unit, 0,                                  F='(%"set c_stat_flag               %d")'
printf, xcm_unit, inband_counts,                      F='(%"set src_cnts                  %d")'
printf, xcm_unit, strjoin(string(energy_starting_group[1:10],F='(%"%4.2f")'),' '), $
                                                      F='(%"set cplinear_energies        {%s}")'
printf, xcm_unit, codedir+'xspec_scripts',            F='(%"set model_directory         \"%s\"")'
printf, xcm_unit, keyword_set(interactive),           F='(%"set interactive_flag          %d")'
free_lun, xcm_unit

; Append MODEL_FILENAME to XSPEC script prefix using "sed" to insert any user-supplied customizations to the model.
if (fit_custom_fn EQ '') then begin
  cmd = string(model_filename, sourcedir + fit_xcm_fn, F="(%'cat %s >>! %s')")
endif else begin
  cmd = string(fit_custom_fn, model_filename, sourcedir + fit_xcm_fn, F="(%'sed -e ""/AE CUSTOMIZATIONS/r %s"" %s >>! %s')")
endelse
run_command, /UNIX, cmd

;; ------------------------------------------------------------------------
;; Perform the fit.
ae_perform_fit, sourcedir, fit_result_root, INTERACTIVE=keyword_set(interactive)

FAILURE: 
return
end



;;; ==========================================================================

FUNCTION decode_xspec_flags, errstatus, WAS_FROZEN=was_frozen

null_flag = replicate('.',n_elements(errstatus))
         non_monotonic = null_flag
         upper_search  = null_flag
         lower_search  = null_flag
         bad_minimize  = null_flag
         bad_search    = null_flag
         high_statval  = null_flag

           ; Defintions of flags returned by "tclout error" command in XSPEC:
           ;  1      new minimum found
           ;  2      non-monotonicity detected
           ;  3      minimization may have run into problem
           ;  4      hit hard lower limit
           ;  5      hit hard upper limit
           ;  6      parameter was frozen
           ;  7      search failed in -ve direction
           ;  8      search failed in +ve direction
           ;  9      reduced chi-squared too high
          
           was_frozen = strmatch(errstatus, '?????T*')
           
           ind = where(strmatch(errstatus, '?T*'), count)
           if (count GT 0) then non_monotonic[ind] = 'n'
           
           ind = where(strmatch(errstatus, '???T*' ) OR strmatch(errstatus, '??????T*' ), count)
           if (count GT 0) then lower_search[ind] = 'l'

           ind = where(strmatch(errstatus, '????T*') OR strmatch(errstatus, '???????T*'), count)
           if (count GT 0) then upper_search[ind] = 'u'

           ind = where(strmatch(errstatus, '??T*'), count)
           if (count GT 0) then bad_minimize[ind] = 'm'
                      
           ind = where(strmatch(errstatus, '????????T*'), count)
           if (count GT 0) then high_statval[ind] = 'c'

return, non_monotonic+lower_search+upper_search+bad_minimize+high_statval
end


;;; ==========================================================================
;;; hmsfr_tables
;;; ==========================================================================
;;; The parameter "nominal_effective_area" divides EMAP_TOT for each source
;;; to get an "Effective Exposure" time in ks.
;;;
;;; Recall that the emap(x,y) = ARF(x,y,E) * [sum of GTI for that CCD].
;;; Thus if one adopts a nominal Effective Area (ARF) value corresponding to some
;;; nominal location on the detector, then for any specific emap value for a
;;; source one can find an "Effective Exposure" time, i.e. the observing time 
;;; that would be necessary to produce that emap value (and the number of counts 
;;; actually observed) at the nominal location.
;;; Note that the nominal Effective Area is a property of Chandra not a property of the observation.
;;;
;;; Authors can choose any "nominal" location desired.  Note however two important facts:
;;;  * The largest I-array emap value is NOT at the aimpoint, and is not even on I3.
;;;  * The 1keV emap on S3 is larger than anything on the I-array, so computing
;;;    max(emap) is not a consistent method for defining the nominal location.
;;;
;;; A sensible "nominal" detector location to choose would be near the point of largest 
;;; effective area ON THE I-ARRAY and positioned so that there is no dithering over dead regions.
;;; If your emap covers only the I-array you can define nominal_effective_area = max(emap)/LIVETIME.
;;; If your emap includes S3 then you will need to estimate "max(emap)" for the I-array using ds9.

;;; The largest nominal_effective_area on the I-array @ 1kev @ -120C is 350 cm^2.
;;;
;;; 
;;; The parameter "distance" must be in units of pc.
;;;
;;; The parameter SKY_OFFSET=[deltaX,deltaY] is the optional astrometric offset you wish to apply
;;; to all source positions, in units of CIAO sky pixels.  The RA & x axes have opposite signs
;;; so if you want to increase RA of the sources you supply a negative deltaX value.  
;;; The DEC and y axes have the same signs.
;;;
;;; SRC_SIGNIF_MIN can be supplied as a 2-vector of thresholds against the SRC_SIGNIF column. 
;;; The first value is used to omit sources from the spectroscopy tables.  
;;; The second value is used to suppress reporting of errors on fit parameters

PRO hmsfr_tables, summary_table_filename, template_filename, nominal_effective_area, distance, $
                  NO_SORT=no_sort, SORTED_TABLE_FILENAME=sorted_table_filename, $
                  SKY_OFFSET=sky_offset, QUIET=quiet, $
                  SRC_SIGNIF_MIN=src_signif_min, NET_COUNTS_MIN=net_counts_min

help, nominal_effective_area, distance
creator_string = "hmsfr_tables, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string

;; When log Nh is > this threshold we omit reporting soft fluxes.
logNh_threshold = 22.5

if (n_elements(src_signif_min) EQ 0) then src_signif_min=[0,0]
if (n_elements(src_signif_min) EQ 1) then src_signif_min=[src_signif_min,0]

if (n_elements(net_counts_min) EQ 0) then net_counts_min=0

;; ------------------------------------------------------------------------
;; Read summary table and check source names.
bt = mrdfits( summary_table_filename, 1, theader )
num_sources = n_elements(bt)
seq = 1+indgen(num_sources)

null_col = replicate('\nodata', num_sources)
null_val = replicate(!VALUES.F_NAN, num_sources)


;; Verify catalog is sorted by RA.
sort_ind = keyword_set(no_sort) ? indgen(num_sources) : bsort(bt.RA)
bt       = bt[sort_ind]
dum = where(sort_ind NE indgen(num_sources), count)
if (count GT 0) then print, 'WARNING!  This catalog was not sorted by RA!'


if keyword_set(sky_offset) then begin
  ; THE ADJUSTMENT OF POSITIONS AND NAMES MUST OCCUR AFTER THE SORTING ABOVE
  ; SINCE THE SORTED TABLE IS WRITTEN OVER THE INPUT FILENAME.

  ; Create astrometry conversion between sky & celestial coordinates, then
  ; adjust all the RA & DEC positions as specified & Rename the sources.
  make_astr, astr, CRVAL=[median(bt.RA),median(bt.DEC)], CRPIX=[1,1], DELT=[-0.000136667, 0.000136667]
  ad2xy, bt.RA, bt.DEC, astr, x, y
  xy2ad, x+sky_offset[0], y+sky_offset[1], astr, RA_FINAL, DEC_FINAL
  
  precision  = 1
  OBJECT_FINAL = strcompress(/REMOVE_ALL, adstring(RA_FINAL,DEC_FINAL,precision,/TRUNCATE))
  print, 'SKY_OFFSET=', sky_offset

  forprint, seq, bt.OBJECT, OBJECT_FINAL, F='(%"%4d %s -> %s")'
endif else begin
  RA_FINAL     = bt.RA
  DEC_FINAL    = bt.DEC
  OBJECT_FINAL = bt.OBJECT
endelse


; Use Latex math mode in object name.
CXOU = OBJECT_FINAL
for ii=0,num_sources-1 do begin
  name = strjoin( strsplit(CXOU[ii], /EXTRACT, '+'), '$+$')
  name = strjoin( strsplit(name,     /EXTRACT, '-'), '$-$')
  CXOU[ii] = name
endfor


;; Save the sorted table with extra columns for use in generating other tables, e.g. the counterparts table.
old_bt = bt
bt=replicate(create_struct( old_bt[0], 'RA_FINAL',0D,'DEC_FINAL',0D, 'OBJECT_FINAL','', 'CXOU',''), num_sources)
copy_struct,old_bt,bt
old_bt = 0
bt.RA_FINAL     = RA_FINAL
bt.DEC_FINAL    = DEC_FINAL
bt.OBJECT_FINAL = OBJECT_FINAL
bt.CXOU         = CXOU

if keyword_set(sorted_table_filename) then begin
  writefits,   sorted_table_filename, 0, headfits(summary_table_filename)
  mwrfits, bt, sorted_table_filename, theader
endif else print, 'Use SORTED_TABLE_FILENAME to specify an output file to hold sorted/offset table.' 

;; ------------------------------------------------------------------------
;; Handle some "flag" values from old AE runs that signify "no data".
ind = where(bt.PROB_KS EQ -1, count)
if (count GT 0) then bt[ind].PROB_KS =!VALUES.F_NAN


;; ------------------------------------------------------------------------
;; Verify that the expected photometry energy bands are in the expected place.
;; The AE default energy bands have been set to use the bands desired for HMSFRs
;; 0.5:8;  0.5:2, 2-8;  0.5:1.7, 1.7:2.8  2.8:8

band_full = 0
if max(abs([bt.ENERG_LO[band_full] - 0.5, bt.ENERG_HI[band_full] - 8.0])) GT 0.01 then begin
  print, band_full, F='(%"ERROR: Band %d is not 0.5:8.0 keV")'
  return
endif

band_2000_8000 = 2
if max(abs([bt.ENERG_LO[band_2000_8000] - 2.0, bt.ENERG_HI[band_2000_8000] - 8.0])) GT 0.01 then begin
  print, band_2000_8000, F='(%"ERROR: Band %d is not 2.0:8.0 keV")'
  return
endif

goto, SKIP_HRS

band_500_2000 = 1
if max(abs([bt.ENERG_LO[band_500_2000] - 0.5, bt.ENERG_HI[band_500_2000] - 2.0])) GT 0.01 then begin
  print, band_500_2000, F='(%"ERROR: Band %d is not 0.5:2.0 keV")'
  return
endif

band_500_1700 = 3
if max(abs([bt.ENERG_LO[band_500_1700] - 0.5, bt.ENERG_HI[band_500_1700] - 1.7])) GT 0.01 then begin
  print, band_500_1700, F='(%"ERROR: Band %d is not 0.5:1.7 keV")'
  return
endif

band_1700_2800 = 4
if max(abs([bt.ENERG_LO[band_1700_2800] - 1.7, bt.ENERG_HI[band_1700_2800] - 2.8])) GT 0.01 then begin
  print, band_1700_2800, F='(%"ERROR: Band %d is not 1.7:2.8 keV")'
  return
endif

band_2800_8000 = 5
if max(abs([bt.ENERG_LO[band_2800_8000] - 2.8, bt.ENERG_HI[band_2800_8000] - 8.0])) GT 0.01 then begin
  print, band_2800_8000, F='(%"ERROR: Band %d is not 2.8:8.0 keV")'
  return
endif


;; ------------------------------------------------------------------------
;; Here we use NET_CTS entries in pairs of photometry table rows to make HR = (hard_cnts - soft_cnts)/(hard_cnts + soft_cnts)
;; Compute hardness upper and lower 1-sigma errors using equation 1.31 in 
; "A Practical Guide to Data Analysis for Physical Science Students", L. Lyons, 1991.
hard_band = [band_2000_8000, band_1700_2800, band_2800_8000]
soft_band = [band_500_2000 , band_500_1700 , band_1700_2800]

for ii = 0, n_elements(hard_band)-1 do begin
  print, bt[0].ENERG_LO[soft_band[ii]], bt[0].ENERG_HI[soft_band[ii]], $
         bt[0].ENERG_LO[hard_band[ii]], bt[0].ENERG_HI[hard_band[ii]], $
         F='(%"Hardness Ratio using bands %3.1f:%3.1f and %3.1f:%3.1f")'

  ; Recall that NET_CNTS can be negative.  
  ; To ensure that HRs are bounded by [-1,1] we choose to clip such NET_CNTS entries at zero
  ; and choose to set their lower errors to zero.  
  ; We will later do similar clipping at zero when we offset NET_CNTS downward by NET_CNTS_SIGMA_LOW
  ; during Lyon's error propagation.
  
  hard_cnts = float(bt.NET_CNTS[hard_band[ii]]) > 0
  soft_cnts = float(bt.NET_CNTS[soft_band[ii]]) > 0
  
  hard_sigma_up   = bt.NET_CNTS_SIGMA_UP[hard_band[ii]]
  soft_sigma_up   = bt.NET_CNTS_SIGMA_UP[soft_band[ii]]

  hard_sigma_low  = bt.NET_CNTS_SIGMA_LOW[hard_band[ii]]
  soft_sigma_low  = bt.NET_CNTS_SIGMA_LOW[soft_band[ii]]
  
  ; Compute some metrics & flags to identify various conditions where the HRs and/or their errors are not reliable.
  is_undefined       = ((hard_cnts - hard_sigma_low) LE 0) AND ((soft_cnts - soft_sigma_low) LE 0)
  is_very_hard       = ((hard_cnts - hard_sigma_low) GT 0) AND ((soft_cnts - soft_sigma_low) LE 0)
  is_very_soft       = ((hard_cnts - hard_sigma_low) LE 0) AND ((soft_cnts - soft_sigma_low) GT 0)

  ; The Nx5 arrays "hard" and "soft" below will simplify the 5 evaluations of our function ((h-s)/(h+s)) needed  
  ; to do the Lyons calculations for upper and lower sigmas.
  ; 0th entry is nominal HR 
  ; 1st entry is where hard fluxuates upward,   HR fluxuates upward
  ; 2nd entry is where soft fluxuates upward,   HR fluxuates downward
  ; 3rd entry is where hard fluxuates downward, HR fluxuates downward
  ; 4rd entry is where soft fluxuates downward, HR fluxuates upward
  
  hard_cnts_more = hard_cnts + hard_sigma_up
  soft_cnts_more = soft_cnts + soft_sigma_up

  ; To retain range of HR we clip downward fluxuations in NET_CNTS at zero.
  hard_cnts_less = (hard_cnts - hard_sigma_low) > 0
  soft_cnts_less = (soft_cnts - soft_sigma_low) > 0
  
  hard = [[hard_cnts], [hard_cnts_more], [hard_cnts     ], [hard_cnts_less], [hard_cnts     ]]
  soft = [[soft_cnts], [soft_cnts     ], [soft_cnts_more], [soft_cnts     ], [soft_cnts_less]]
  
  ratios = (hard - soft) / (hard + soft)
  
  hr           = ratios[*,0]

  ; There's a questions here!  Lyon's book seems to say that upper sigma for f is computed using terms (1&2) where the 
  ; input parameters both fluxuate up, and lower sigma for f uses terms (3&4) where parameters fluxuate down.
  ; However it seems more reasonable to estimate the upper limit on f using the terms where f fluxuates upward (1&4)
  ; and estimate the lower limit on f using terms where f fluxuates downward (2&3).
  ; Talk to Niel and Eric about this!!!
  
  hr_sigma_up  = sqrt( (ratios[*,1] - hr)^2 + $
                       (ratios[*,2] - hr)^2 )

  hr_sigma_low = sqrt( (ratios[*,3] - hr)^2 + $
                       (ratios[*,4] - hr)^2 )


  ;; Insert !VALUES.F_NAN any place we want \nodata in the table.
  
  ; Handle ratios whose confidence interval includes +-1 (because confidence interval of hard or soft includes 0).
  ind = where(is_very_hard,count)
  if (count GT 0) then begin
    print, count, ' sources are very hard.'
    hr_sigma_up [ind] = !VALUES.F_NAN
  endif
  
  ind = where(is_very_soft,count)
  if (count GT 0) then begin
    print, count, ' sources are very soft.'
    hr_sigma_low[ind] = !VALUES.F_NAN
  endif

  ; Handle cases where we don't want to report the HR at all.
  ind = where(is_undefined,count)
  if (count GT 0) then begin
    print, count, ' sources have unreliable HR values.'
    hr          [ind] = !VALUES.F_NAN
    hr_sigma_up [ind] = !VALUES.F_NAN
    hr_sigma_low[ind] = !VALUES.F_NAN
  endif

  
  ; Save the results in named variables.
  case ii of
   0: begin
      hr1           = hr
      hr1_sigma_up  = hr_sigma_up
      hr1_sigma_low = hr_sigma_low
  is_undefined1       = is_undefined
  is_very_hard1       = is_very_hard
  is_very_soft1       = is_very_soft 
      end
   1: begin
      hr2           = hr
      hr2_sigma_up  = hr_sigma_up
      hr2_sigma_low = hr_sigma_low
  is_undefined2       = is_undefined
  is_very_hard2       = is_very_hard
  is_very_soft2       = is_very_soft 
      end
   2: begin
      hr3           = hr
      hr3_sigma_up  = hr_sigma_up
      hr3_sigma_low = hr_sigma_low
  is_undefined3       = is_undefined
  is_very_hard3       = is_very_hard
  is_very_soft3       = is_very_soft 
      end
  endcase
endfor ; ii

;; COUP SPECIAL CODE:
save, FILE='hr.sav', seq, OBJECT_FINAL, $
      is_undefined1, is_very_hard1, is_very_soft1, hr1, hr1_sigma_up, hr1_sigma_low, $
      is_undefined2, is_very_hard2, is_very_soft2, hr2, hr2_sigma_up, hr2_sigma_low, $
      is_undefined3, is_very_hard3, is_very_soft3, hr3, hr3_sigma_up, hr3_sigma_low

SKIP_HRS:

;; ------------------------------------------------------------------------
;; Verify the spectral model is as expected.
;; ???????? Look at bt.MODEL

null_flag = replicate('.',num_sources)

;; ------------------------------------------------------------------------
;; Compute columns for spectral tables.
nh_offset     = 22
cm_per_parsec = 3.086D18
dscale        = 4D*!PI*(distance * cm_per_parsec)^2
em_offset     = alog10(1E14 * dscale)
lx_offset     = alog10(dscale)
help, distance, dscale, lx_offset
         
; List of flag values that should invalidate the error estimate.
; We choose to ignore the vague 'm' flags from XSPEC.
bad_uerror_flaglist = '[ouUnc]'
bad_lerror_flaglist = '[olLnc]'

; List of flag values that should invalidate the fit itself.
bad_fit_flaglist    = '[ro]'

         
;; Examine NH results.
if (total(strmatch(tag_names(bt),'NH')) GT 0) then begin
         ; In the fitting scripts the signature of an aborted/failed error computation is that the 
         ; upper and lower limits are both set to zero.  
         ; We convert those to NaN below; thus in this program the signature is two NaNs.
         NH_VAL  = nh_offset + alog10(bt.NH)
         NH_ERRU = nh_offset + alog10(bt.NH_ERRU) + 0./(bt.NH_ERRU NE 0)
         NH_ERRL = nh_offset + alog10(bt.NH_ERRL) + 0./(bt.NH_ERRU NE 0)
         

         ;; ------------------------------------------------------------------------
         ; Look for reasons the fit itself is invalid.
         range_flag = null_flag
         ind = where(((bt.NH LT bt.NH0_MIN) OR (bt.NH GT bt.NH0_MAX)), count)
         if (count GT 0) then begin
           range_flag[ind] = 'r'
           print, count, F='(%"\nThese %d sources have Nh out of range:")'
  
           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, NH_VAL[ind], F='(%"%4d %s %f")'
         endif
             
         ;; ------------------------------------------------------------------------
         ; Look for a variety of reasons the error estimate is invalid.
         
         ; First check to see if the error computation was skipped.  (Fitting script writes zeros which become NaN's above.)
         skipped_flag       = null_flag
         ind = where((finite(NH_ERRL) OR finite(NH_ERRU)) EQ 0, count)
         if (count GT 0) then skipped_flag[ind] = 's'
     
         ; Determine the "error" command anomalies that occurred in XSPEC.
         if (total(strmatch(tag_names(bt),'NH_ERRST')) GT 0) then begin
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=nh_frozen, bt.NH_ERRST)
         endif else begin
           print, 'WARNING!  The flags based on XSPEC "error" command status cannot be computed.'
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=nh_frozen, replicate('',num_sources))
         endelse

         ; Some sources skip the "error" command in XSPEC (to avoid XSPEC bugs) and thus 
         ; the WAS_FROZEN vector computed by decode_xspec_flags() can have missing entries.  
         ; So, we explicitly check the parameter's ranges here to infer whether it _should_ 
         ; have been frozen in the fitting script.
         nh_frozen OR= (bt.NH0_MIN EQ bt.NH0_MAX) 
         if (total(strmatch(tag_names(bt),'NH_FZ')) GT 0) then nh_frozen OR= bt.NH_FZ

         
         ; In cases where the error computation is known to have failed, null out the parameter confidence interval values (which can have junk in them), and THEN compute the bad_ordering_flag.
         ind = where(stregex(/BOOL,xspec_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           NH_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,xspec_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           NH_ERRL[ind] = !VALUES.F_NAN
         endif
         
         bad_ordering_flag  = null_flag
         ind = where( ((NH_ERRU NE 0) AND (NH_ERRU LT NH_VAL)) OR ((NH_ERRL NE 0) AND (NH_ERRL GT NH_VAL)), count)
         if (count GT 0) then bad_ordering_flag[ind] = 'o'

         
         ; Check for confidence intervals that violate the parameter ranges.
         lowerlimit_flag    = null_flag
         ind = where( NH_ERRL LT bt.NH0_MIN, count)
         if (count GT 0) then lowerlimit_flag[ind] = 'L'
         
         upperlimit_flag    = null_flag
         ind = where( NH_ERRU GT bt.NH0_MAX, count)
         if (count GT 0) then upperlimit_flag[ind] = 'U'
         

         ; Invalidate errors where anomalies were found.
         nh_anom_flags = range_flag+bad_ordering_flag+skipped_flag+lowerlimit_flag+upperlimit_flag+xspec_anom_flags

         ind = where( stregex(/BOOL,nh_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           NH_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,nh_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           NH_ERRL[ind] = !VALUES.F_NAN
         endif

         
         ; Hide errors when parameter was frozen.
         ind = where(nh_frozen, count)
         if (count GT 0) then begin
           nh_anom_flags[ind] = 'frozen    '
           NH_ERRU      [ind] = !VALUES.F_NAN 
           NH_ERRL      [ind] = !VALUES.F_NAN 
         endif

         
         ; Hide errors when the source significance is small (since we don't trust them much)
         ind = where(bt.SRC_SIGNIF[band_full] LT src_signif_min[1], count)
         if (count GT 0) then begin
           nh_anom_flags[ind] = 'hidden  '
           NH_ERRU      [ind] = !VALUES.F_NAN 
           NH_ERRL      [ind] = !VALUES.F_NAN 
         endif 

         
         ; Invalidate the fit result itself if certain anomalies were found.
         ind = where((nh_frozen EQ 0) AND stregex(/BOOL,nh_anom_flags,bad_fit_flaglist), count)
         if (count GT 0) then begin
           NH_VAL [ind] = !VALUES.F_NAN
           NH_ERRU[ind] = !VALUES.F_NAN
           NH_ERRL[ind] = !VALUES.F_NAN
         endif 

         ;; ------------------------------------------------------------------------
         ; Build NH strings.
         errl = (NH_VAL  - NH_ERRL)
         erru = (NH_ERRU - NH_VAL)
         par_str      = string( NH_VAL,  F='(%" %0.1f")')
         par_errl_str = string(errl, F='(%"_{-%0.1f}")')
         par_erru_str = string(erru, F='(%"^{+%0.1f}")')
         
         ind = where(nh_frozen, count)
         if (count GT 0) then begin
           par_str[ind] = par_str[ind] + '*'
         endif
         
         ind = where((errl < erru) LT 0.1, count)
         if (count GT 0) then begin
           par_errl_str[ind] = string(errl[ind], F='(%"_{-%0.2f}")')
           par_erru_str[ind] = string(erru[ind], F='(%"^{+%0.2f}")')
         endif
         
         ind = where((finite(errl) EQ 0) AND (finite(erru) EQ 0), count)
         if (count GT 0) then begin
           par_errl_str[ind] = ''
           par_erru_str[ind] = ''
         endif

         ind = where((finite(errl) EQ 0) AND finite(erru), count)
         if (count GT 0) then begin
           par_errl_str[ind] = '_{\cdots}'
         endif
         
         ind = where((finite(erru) EQ 0) AND finite(errl), count)
         if (count GT 0) then begin
           par_erru_str[ind] = '^{\cdots}'
         endif
        nh_range = '$'+par_str+par_errl_str+par_erru_str+'$'
       
        ; We can't leave NaN string inside the math mode above because $\nodata$ will fail.
        ind = where(finite(NH_VAL) EQ 0, count)
        if (count GT 0) then begin
          nh_range[ind] = '\nodata'
        endif
endif


         
;; Examine KT results.
if (total(strmatch(tag_names(bt),'KT')) GT 0) then begin
         ; In the fitting scripts the signature of an aborted/failed error computation is that the 
         ; upper and lower limits are both set to zero.  
         ; We convert those to NaN below; thus in this program the signature is two NaNs.
         KT_VAL  = bt.KT
         KT_ERRU = bt.KT_ERRU + 0./(bt.KT_ERRU NE 0)
         KT_ERRL = bt.KT_ERRL + 0./(bt.KT_ERRL NE 0)
         
         ;; ------------------------------------------------------------------------
         ; Look for reasons the fit itself is invalid.
         range_flag = null_flag
         ind = where(((bt.KT LT bt.KT0_MIN) OR (bt.KT GT bt.KT0_MAX)), count)
         if (count GT 0) then begin
           range_flag[ind] = 'r'
           print, count, F='(%"\nThese %d sources have kT out of range:")'
  
           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, KT_VAL[ind], F='(%"%4d %s %f")'
         endif
         
         ;; ------------------------------------------------------------------------
         ; Look for a variety of reasons the error estimate is invalid.

         ; First check to see if the error computation was skipped.  (Fitting script writes zeros which become NaN's above.)
         skipped_flag       = null_flag
         ind = where((finite(KT_ERRL) OR finite(KT_ERRU)) EQ 0, count)
         if (count GT 0) then skipped_flag[ind] = 's'
  
         ; Determine the "error" command anomalies that occurred in XSPEC.
         if (total(strmatch(tag_names(bt),'KT_ERRST')) GT 0) then begin
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=kt_frozen, bt.KT_ERRST)
         endif else begin
           print, 'WARNING!  The flags based on XSPEC "error" command status cannot be computed.'
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=kt_frozen, replicate('',num_sources))
         endelse

         ; Some sources skip the "error" command in XSPEC (to avoid XSPEC bugs) and thus 
         ; the WAS_FROZEN vector computed by decode_xspec_flags() can have missing entries.  
         ; So, we explicitly check the parameter's ranges here to infer whether it _should_ 
         ; have been frozen in the fitting script.         
         kt_frozen OR= (bt.KT0_MIN EQ bt.KT0_MAX)
         if (total(strmatch(tag_names(bt),'KT_FZ')) GT 0) then kt_frozen OR= bt.KT_FZ


         
         ; In cases where the error computation is known to have failed, null out the parameter confidence interval values (which can have junk in them), and THEN compute the bad_ordering_flag.
         ind = where(stregex(/BOOL,xspec_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           KT_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,xspec_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           KT_ERRL[ind] = !VALUES.F_NAN
         endif
         
         bad_ordering_flag  = null_flag
         ind = where( ((KT_ERRU NE 0) AND (KT_ERRU LT KT_VAL)) OR ((KT_ERRL NE 0) AND (KT_ERRL GT KT_VAL)), count)
         if (count GT 0) then bad_ordering_flag[ind] = 'o'

         
         ; Check for confidence intervals that violate the parameter ranges.
         lowerlimit_flag    = null_flag
         ind = where( KT_ERRL LT bt.KT0_MIN, count)
         if (count GT 0) then lowerlimit_flag[ind] = 'L'
         
         upperlimit_flag    = null_flag
         ind = where( KT_ERRU GT bt.KT0_MAX, count)
         if (count GT 0) then upperlimit_flag[ind] = 'U'


         ; Invalidate errors where anomalies were found.
         kt_anom_flags = range_flag+bad_ordering_flag+skipped_flag+lowerlimit_flag+upperlimit_flag+xspec_anom_flags

         ind = where( stregex(/BOOL,kt_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           KT_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,kt_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           KT_ERRL[ind] = !VALUES.F_NAN
         endif

         
         ; Hide errors when parameter was frozen.
         ind = where(kt_frozen, count)
         if (count GT 0) then begin
           kt_anom_flags[ind] = 'frozen    '
           KT_ERRU      [ind] = !VALUES.F_NAN 
           KT_ERRL      [ind] = !VALUES.F_NAN 
         endif 
  
         
         ; Invalidate the fit result itself if certain anomalies were found.
         ind = where((kt_frozen EQ 0) AND stregex(/BOOL,kt_anom_flags,bad_fit_flaglist), count)
         if (count GT 0) then begin
           KT_VAL [ind] = !VALUES.F_NAN
           KT_ERRU[ind] = !VALUES.F_NAN
           KT_ERRL[ind] = !VALUES.F_NAN
         endif 


         ;; ------------------------------------------------------------------------
         ; Build KT strings.
         errl = (KT_VAL  - KT_ERRL)
         erru = (KT_ERRU - KT_VAL)
         par_str      = string( KT_VAL,  F='(%" %0.1f")')
         par_errl_str = string(errl, F='(%"_{-%0.1f}")')
         par_erru_str = string(erru, F='(%"^{+%0.1f}")')
         
         ind = where(strlen(par_str) EQ 4, count)
         if (count GT 0) then begin
           par_str[ind] = '{\phn}' + par_str[ind]
         endif
         
         ind = where(kt_frozen, count)
         if (count GT 0) then begin
           par_str[ind] = par_str[ind] + '*'
         endif
         
         ind = where((errl < erru) LT 0.1, count)
         if (count GT 0) then begin
           par_errl_str[ind] = string(errl[ind], F='(%"_{-%0.2f}")')
           par_erru_str[ind] = string(erru[ind], F='(%"^{+%0.2f}")')
         endif
         
         ind = where((finite(errl) EQ 0) AND (finite(erru) EQ 0), count)
         if (count GT 0) then begin
           par_errl_str[ind] = ''
           par_erru_str[ind] = ''
         endif

         ind = where((finite(errl) EQ 0) AND finite(erru), count)
         if (count GT 0) then begin
           par_errl_str[ind] = '_{\cdots}'
         endif
         
         ind = where((finite(erru) EQ 0) AND finite(errl), count)
         if (count GT 0) then begin
           par_erru_str[ind] = '^{\cdots}'
         endif
         kt_range = '$'+par_str+par_errl_str+par_erru_str+'$'
       
        ; We can't leave NaN string inside the math mode above because $\nodata$ will fail.
        ind = where(finite(KT_VAL) EQ 0, count)
        if (count GT 0) then begin
          kt_range[ind] = '\nodata'
        endif
endif
  
         
;; Examine Photon Index results.
if (total(strmatch(tag_names(bt),'PH')) GT 0) then begin
         ; In the fitting scripts the signature of an aborted/failed error computation is that the 
         ; upper and lower limits are both set to zero.  
         ; We convert those to NaN below; thus in this program the signature is two NaNs.
         PH_VAL  = bt.PH
         PH_ERRU = bt.PH_ERRU + 0./(bt.PH_ERRU NE 0)
         PH_ERRL = bt.PH_ERRL + 0./(bt.PH_ERRL NE 0)
         
         ;; ------------------------------------------------------------------------
         ; Look for reasons the fit itself is invalid.
         range_flag = null_flag
         ind = where(((bt.PH LT bt.PH0_MIN) OR (bt.PH GT bt.PH0_MAX)), count)
         if (count GT 0) then begin
           range_flag[ind] = 'r'
           print, count, F='(%"\nThese %d sources have Ph out of range:")'
  
           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, PH_VAL[ind], F='(%"%4d %s %f")'
         endif
         
         ;; ------------------------------------------------------------------------
         ; Look for a variety of reasons the error estimate is invalid.

         ; First check to see if the error computation was skipped.  (Fitting script writes zeros which become NaN's above.)
         skipped_flag       = null_flag
         ind = where((finite(PH_ERRL) OR finite(PH_ERRU)) EQ 0, count)
         if (count GT 0) then skipped_flag[ind] = 's'
         
         ; Determine the "error" command anomalies that occurred in XSPEC.
         if (total(strmatch(tag_names(bt),'PH_ERRST')) GT 0) then begin
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=ph_frozen, bt.PH_ERRST)
         endif else begin
           print, 'WARNING!  The flags based on XSPEC "error" command status cannot be computed.'
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=ph_frozen, replicate('',num_sources))
         endelse

         ; Some sources skip the "error" command in XSPEC (to avoid XSPEC bugs) and thus 
         ; the WAS_FROZEN vector computed by decode_xspec_flags() can have missing entries.  
         ; So, we explicitly check the parameter's ranges here to infer whether it _should_ 
         ; have been frozen in the fitting script.
         ph_frozen OR= (bt.PH0_MIN EQ bt.PH0_MAX)
         if (total(strmatch(tag_names(bt),'PH_FZ')) GT 0) then ph_frozen OR= bt.PH_FZ

         
         ; In cases where the error computation is known to have failed, null out the parameter confidence interval values (which can have junk in them), and THEN compute the bad_ordering_flag.
         ind = where(stregex(/BOOL,xspec_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           PH_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,xspec_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           PH_ERRL[ind] = !VALUES.F_NAN
         endif
                  
         bad_ordering_flag  = null_flag
         ind = where( ((PH_ERRU NE 0) AND (PH_ERRU LT PH_VAL)) OR ((PH_ERRL NE 0) AND (PH_ERRL GT PH_VAL)), count)
         if (count GT 0) then bad_ordering_flag[ind] = 'o'


     
         ; Check for confidence intervals that violate the parameter ranges.
         lowerlimit_flag    = null_flag
         ind = where( PH_ERRL LT bt.PH0_MIN, count)
         if (count GT 0) then lowerlimit_flag[ind] = 'L'
         
         upperlimit_flag    = null_flag
         ind = where( PH_ERRU GT bt.PH0_MAX, count)
         if (count GT 0) then upperlimit_flag[ind] = 'U'


         ; Invalidate errors where anomalies were found.
         ph_anom_flags = range_flag+bad_ordering_flag+skipped_flag+lowerlimit_flag+upperlimit_flag+xspec_anom_flags

         ind = where( stregex(/BOOL,ph_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           PH_ERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,ph_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           PH_ERRL[ind] = !VALUES.F_NAN
         endif

         ; Hide errors when parameter was frozen.
         ind = where(ph_frozen, count)
         if (count GT 0) then begin
           ph_anom_flags[ind] = 'frozen    '
           PH_ERRU      [ind] = !VALUES.F_NAN 
           PH_ERRL      [ind] = !VALUES.F_NAN 
         endif 
         
         ; Invalidate the fit result itself if anomalies were found.
         ind = where((ph_frozen EQ 0) AND stregex(/BOOL,ph_anom_flags,bad_fit_flaglist), count)
         if (count GT 0) then begin
           PH_VAL [ind] = !VALUES.F_NAN
           PH_ERRU[ind] = !VALUES.F_NAN
           PH_ERRL[ind] = !VALUES.F_NAN
         endif 

         
         ;; ------------------------------------------------------------------------
         ; Build PH strings.
         errl = (PH_VAL  - PH_ERRL)
         erru = (PH_ERRU - PH_VAL)
         par_str      = string( PH_VAL,  F='(%" %0.1f")')
         par_errl_str = string(errl, F='(%"_{-%0.1f}")')
         par_erru_str = string(erru, F='(%"^{+%0.1f}")')
         
         ind = where(ph_frozen, count)
         if (count GT 0) then begin
           par_str[ind] = par_str[ind] + '*'
         endif
         
         ind = where((errl < erru) LT 0.1, count)
         if (count GT 0) then begin
           par_errl_str[ind] = string(errl[ind], F='(%"_{-%0.2f}")')
           par_erru_str[ind] = string(erru[ind], F='(%"^{+%0.2f}")')
         endif
         
         ind = where((finite(errl) EQ 0) AND (finite(erru) EQ 0), count)
         if (count GT 0) then begin
           par_errl_str[ind] = ''
           par_erru_str[ind] = ''
         endif

         ind = where((finite(errl) EQ 0) AND finite(erru), count)
         if (count GT 0) then begin
           par_errl_str[ind] = '_{\cdots}'
         endif
         
         ind = where((finite(erru) EQ 0) AND finite(errl), count)
         if (count GT 0) then begin
           par_erru_str[ind] = '^{\cdots}'
         endif
         ph_range = '$'+par_str+par_errl_str+par_erru_str+'$'
       
        ; We can't leave NaN string inside the math mode above because $\nodata$ will fail.
        ind = where(finite(PH_VAL) EQ 0, count)
        if (count GT 0) then begin
          ph_range[ind] = '\nodata'
        endif
endif
  

;; Examine NORM results.
if (total(strmatch(tag_names(bt),'NORM')) GT 0) then begin
         ; In the fitting scripts the signature of an aborted/failed error computation is that the 
         ; upper and lower limits are both set to zero.  
         ; We convert those to NaN below; thus in this program the signature is two NaNs.
         NORM     = alog10(bt.NORM)
         NORMERRU = alog10(bt.NORMERRU) + 0./(bt.NORMERRU NE 0)
         NORMERRL = alog10(bt.NORMERRL) + 0./(bt.NORMERRL NE 0)

         ;; ------------------------------------------------------------------------
         ; Look for reasons the fit itself is invalid.
         range_flag = null_flag

         
         ;; ------------------------------------------------------------------------
         ; Look for a variety of reasons the error estimate is invalid.

         ; First check to see if the error computation was skipped.  (Fitting script writes zeros which become NaN's above.)
         skipped_flag       = null_flag
         ind = where((finite(NORMERRL) OR finite(NORMERRU)) EQ 0, count)
         if (count GT 0) then skipped_flag[ind] = 's'
         
         ; Determine the "error" command anomalies that occurred in XSPEC.
         if (total(strmatch(tag_names(bt),'NORMERRS')) GT 0) then begin
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=norm_frozen, bt.NORMERRS)
         endif else begin
           print, 'WARNING!  The flags based on XSPEC "error" command status cannot be computed.'
           xspec_anom_flags = decode_xspec_flags(WAS_FROZEN=norm_frozen, replicate('',num_sources))
         endelse


         ; In cases where the error computation is known to have failed, null out the parameter confidence interval values (which can have junk in them), and THEN compute the bad_ordering_flag.
         ind = where(stregex(/BOOL,xspec_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           NORMERRU[ind] = !VALUES.F_NAN
         endif
         
         ind = where( stregex(/BOOL,xspec_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           NORMERRL[ind] = !VALUES.F_NAN
         endif
                  
         bad_ordering_flag  = null_flag
         ind = where( ((NORMERRU NE 0) AND (NORMERRU LT NORM)) OR ((NORMERRL NE 0) AND (NORMERRL GT NORM)), count)
         if (count GT 0) then bad_ordering_flag[ind] = 'o'


         ; Invalidate errors where anomalies were found.
         norm_anom_flags = range_flag+bad_ordering_flag+skipped_flag+lowerlimit_flag+upperlimit_flag+xspec_anom_flags

         ind = where( stregex(/BOOL,norm_anom_flags,bad_uerror_flaglist), count )
         if (count GT 0) then begin
           NORMERRU[ind] = !VALUES.F_NAN
         endif
                  
         ind = where( stregex(/BOOL,norm_anom_flags,bad_lerror_flaglist), count )
         if (count GT 0) then begin
           NORMERRL[ind] = !VALUES.F_NAN
         endif
                  
         ; Invalidate the fit result itself if anomalies were found.
         ind = where(stregex(/BOOL,norm_anom_flags,bad_fit_flaglist), count)
         if (count GT 0) then begin
           NORM    [ind] = !VALUES.F_NAN
           NORMERRU[ind] = !VALUES.F_NAN
           NORMERRL[ind] = !VALUES.F_NAN
         endif 


         ;; ------------------------------------------------------------------------
         ; Build NORM strings.
         errl = (NORM - NORMERRL)
         erru = (NORMERRU  - NORM)
         par_errl_str = string(errl, F='(%"_{-%0.1f}")')
         par_erru_str = string(erru, F='(%"^{+%0.1f}")')
         
         ind = where((errl < erru) LT 0.1, count)
         if (count GT 0) then begin
           par_errl_str[ind] = string(errl[ind], F='(%"_{-%0.2f}")')
           par_erru_str[ind] = string(erru[ind], F='(%"^{+%0.2f}")')
         endif
         
         ind = where((finite(errl) EQ 0) AND (finite(erru) EQ 0), count)
         if (count GT 0) then begin
           par_errl_str[ind] = ''
           par_erru_str[ind] = ''
         endif

         ind = where((finite(errl) EQ 0) AND finite(erru), count)
         if (count GT 0) then begin
           par_errl_str[ind] = '_{\cdots}'
         endif
         
         ind = where((finite(erru) EQ 0) AND finite(errl), count)
         if (count GT 0) then begin
           par_erru_str[ind] = '^{\cdots}'
         endif

         par_str    = string(             NORM,  F='(%" %0.1f")')
         norm_range = '$'+par_str+par_errl_str+par_erru_str+'$'
       
        ; We can't leave NaN string inside the math mode above because $\nodata$ will fail.
        ind = where(finite(NORM) EQ 0, count)
        if (count GT 0) then begin
          norm_range[ind] = '\nodata'
        endif

         par_str    = string( em_offset + NORM,  F='(%" %0.1f")')
         em_range   = '$'+par_str+par_errl_str+par_erru_str+'$'
       
        ; We can't leave NaN string inside the math mode above because $\nodata$ will fail.
        ind = where(finite(NORM) EQ 0, count)
        if (count GT 0) then begin
          em_range[ind] = '\nodata'
        endif

endif

net_counts     = round(  bt.NET_CNTS[band_full] )
median_e       =         bt.ENERG_PCT50_OBSERVED[band_full]
CATALOG_NAME   = strtrim(bt.CATALOG_NAME,2)
MODEL        = strtrim(bt.MODEL,2)
SRC_SIGNIF     =         bt.SRC_SIGNIF[band_full]
PROB_NO_SOURCE =         bt.PROB_NO_SOURCE[band_full]



;; ------------------------------------------------------------------------
;; Process the template.
tempfilename = 'temp.tex'

openw, demo_unit, 'demo.tex', /GET_LUN
openr, in_unit,  template_filename, /GET_LUN

table_name = ''
line = ''
while NOT eof(in_unit) do begin
  readf, in_unit, line
  
  if (table_name EQ '') then begin
    ; Processing template outside of a table.
    if strmatch(line, 'TEMPLATE*') then begin
      ; Open a new table (name parsed from TEMPLATE line) 
      table_name = (strsplit(line,/EXTRACT))[1]

      print
      print, 'opening table ', table_name
      openw, table_unit, tempfilename, /GET_LUN
      !TEXTUNIT = table_unit
      
      ;get_date, date, /TIMETAG
      comment = "% Written by "+creator_string + systime()
      printf, table_unit, comment
      
      ; If table_name has any "_"s they must be escaped for LaTeX.
      printf, demo_unit, table_name, F='(%"\\input{%s}")'

    endif else printf, demo_unit, line

  endif else begin
    ; Processing a table
    if strmatch(line, 'END*') then begin
      ; End of the table found.
      print, 'closing table ', table_name
      free_lun, table_unit
      
      ; Convert "NaN" to \nodata command.
      spawn, string(tempfilename, table_name+'.tex', F='(%"sed ''s/NaN/\\\\nodata/g'' %s >! %s")')

      table_name = ''
      continue
    endif

    ; Copy line to table file.
    printf, table_unit, line
    
    if strmatch(line, '\\startdata*') then begin
      ; Parse data format spec from template .
      fmt = ''
      while 1 do begin
        readf, in_unit, line
        if strmatch(line, '\\enddata*') then break
        fmt = fmt + line
      endwhile
      print, 'FORMAT= '+fmt
      
      ; Now write out the data.
      case table_name of
       ;--------------------------------------------------------------------------------------------
       'src_properties': begin
                  
         prob_no_source_string = string(alog10(PROB_NO_SOURCE), F='(%"%4.1f")')
         ind = where(bt.PROB_NO_SOURCE[band_full] LT 1E-5, count)
         if (count GT 0) then begin
           prob_no_source_string[ind] = "$<$-5"
         endif
         
         
         off_chip = null_flag
         ind = where(bt.FRACEXPO LT 0.9, count)
         if (count GT 0) then off_chip[ind] = 'g'
         
         streak = null_flag
         if (total(strmatch(tag_names(bt),'WARNFRAC')) GT 0) then begin
           ind = where(bt.WARNFRAC GT 0.1, count)
           if (count GT 0) then off_chip[ind] = 's'
         endif else print, 'WARNING!  The "streak" flag cannot be computed.'
                  
         anom_flags = off_chip+null_flag+null_flag+streak
         
         
         ; Convert PROB_KS in three-level flag.
         ; Omit KS result when off_chip flag set.
         var_flag = replicate('a',num_sources)
         
         ind = where(bt.PROB_KS LT 0.05, count)
         if (count GT 0) then var_flag[ind] = 'b'
         
         ind = where(bt.PROB_KS LT 0.005, count)
         if (count GT 0) then var_flag[ind] = 'c'
         
         ind = where((finite(bt.PROB_KS) EQ 0) OR (off_chip NE null_flag), count)
         if (count GT 0) then var_flag[ind] = '\nodata'

         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq, bt.CXOU, bt.RA_FINAL, bt.DEC_FINAL, bt.ERR_DATA, bt.THETA, $
         
         bt.NET_CNTS[band_full], (bt.NET_CNTS_SIGMA_UP[band_full] + bt.NET_CNTS_SIGMA_LOW[band_full])/2, $
         
         (bt.BKG_CNTS[band_full] / bt.BACKSCAL[band_full]), bt.NET_CNTS[band_2000_8000] > 0, bt.PSF_FRAC, $
         
         SRC_SIGNIF, prob_no_source_string, anom_flags, var_flag, (bt.EMAP_TOT/nominal_effective_area)/1000., $
         
         median_e, F=fmt

         ;; Save some important columns for the observer's convenience.

         save, seq, CATALOG_NAME, OBJECT_FINAL, MODEL, RA_FINAL, DEC_FINAL, net_counts, SRC_SIGNIF, PROB_NO_SOURCE, median_e,  FILE='src_properties.sav'
         end
         

       ;--------------------------------------------------------------------------------------------
       'thermal_spectroscopy': begin

         ; Determine which sources belong in this table.  We process both tbabs_vapec and tbabs_2vapec models.
         in_this_table = (bt.SRC_SIGNIF[band_full] GE src_signif_min[0]) AND $
                         (bt.NET_CNTS  [band_full] GE net_counts_min) AND $
                          (strmatch(bt.MODEL, '*apec*'))
         
         ind = where( in_this_table, thermal_count )
         if (thermal_count EQ 0) then begin
           print, 'NO sources in thermal_spectroscopy table'
           goto, END_thermal_spectroscopy
         endif


         ; Work will full-length vectors so user can find index for specific source in plotting tools.  
         ; Sources not in this table with have NaN values.
         NH     = alog10(bt.NH) + nh_offset
         KT     =        bt.KT
         F0P5_2 = alog10(bt.F0P5_2)
         F2_8   = alog10(bt.F2_8)
         FC2_8  = alog10(bt.FC2_8)
         F0P5_8 = alog10(bt.F0P5_8)
         FC0P5_8= alog10(bt.FC0P5_8)
         
         ind = where(in_this_table EQ 0, count)
         if (count GT 0) then begin
           NH     [ind] = !VALUES.F_NAN
           KT     [ind] = !VALUES.F_NAN
           F0P5_2 [ind] = !VALUES.F_NAN
           F2_8   [ind] = !VALUES.F_NAN
           FC2_8  [ind] = !VALUES.F_NAN
           F0P5_8 [ind] = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         ;; Suppress flux columns when any parameter has no fit result.
         invalid_fit = in_this_table AND ((finite(NH_VAL) AND finite(KT_VAL) AND finite(NORM)) EQ 0)
         ind = where(invalid_fit, count)
         if (count GT 0) then begin
           print, count, thermal_count, F='(%"\nThese %d (out of %d total) thermal sources have no good estimate for at least one fit parameter:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           F2_8[ind]    = !VALUES.F_NAN
           FC2_8[ind]   = !VALUES.F_NAN
           F0P5_8[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         ;; Suppress soft flux columns when Nh is large.
         invalid_flux = in_this_table AND (NH_VAL GT logNh_threshold)
         ind = where(invalid_flux, count)
         if (count GT 0) then begin
           print, count, thermal_count, logNh_threshold, F='(%"\nThese %d (out of %d total) thermal sources have log Nh > %f:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         

         nh_frozen_flag = null_flag
         ind = where( nh_frozen, count )
         if (count GT 0) then nh_frozen_flag[ind] = 'A'

         kt_frozen_flag = null_flag
         ind = where( kt_frozen, count )
         if (count GT 0) then kt_frozen_flag[ind] = 'T'

 ;        notes = nh_frozen_flag+kt_frozen_flag
         notes = null_col
         ind = where( strmatch(bt.MODEL, '*2*apec*'), count )
         if (count GT 0) then notes[ind] = '2T'
         
         ;; Write out table.
         ind = where( in_this_table, count )

         
         L0P5_2  = lx_offset+(F0P5_2)
         L2_8    = lx_offset+(F2_8)
         LC2_8   = lx_offset+(FC2_8)
         L0P5_8  = lx_offset+(F0P5_8)
         LC0P5_8 = lx_offset+(FC0P5_8)
         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq[ind], CXOU[ind], $
         (bt[ind]).NET_CNTS[band_full], (bt[ind]).SRC_SIGNIF[band_full], $
         nh_range[ind],  kt_range[ind], em_range[ind], $
 
         (L0P5_2[ind]), (L2_8[ind]), (LC2_8[ind]), (L0P5_8[ind]), (LC0P5_8[ind]), notes[ind], F=fmt

         print

         print, 'Reasons fit parameter error is invalid:'
         print, '  o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
         print, '  s: error computation skipped' 
         print, '  L: lowerlim outside specified range (soft limits in XSPEC)' 
         print, '  U: upperlim outside specified range (soft limits in XSPEC)' 
         print, '  n: tclout error "non-monotonicity detected"' 
         print, '  l: tclout error "hit hard lower limit" OR "search failed in -ve direction"' 
         print, '  u: tclout error "hit hard upper limit" OR "search failed in +ve direction"' 
         print, '  m: tclout error "minimization may have run into problem"' 
         print, '  c: tclout error "reduced chi-squared too high"' 
         print
         print, 'Reasons fit parameter itself is invalid:'
         print, '  r: parameter outside specified range (soft limits in XSPEC)' 
         print, '  o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
         print

         ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
         !TEXTUNIT = 0

         if keyword_set(sky_offset) then begin
           print, ' seq    old name              new name            NH            kT         Norm            NC Emed model'
           forprint, seq[ind], bt[ind].OBJECT, bt[ind].OBJECT_FINAL, nh_anom_flags[ind], kt_anom_flags[ind], norm_anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s -> %s  %s  %s  %s  %4d %3.1f %s")'

         endif else begin
           print, ' seq         name            NH          kT         Norm      NC  Emed      model'
           forprint, seq[ind],                      bt[ind].OBJECT_FINAL, nh_anom_flags[ind], kt_anom_flags[ind], norm_anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s  %s  %s  %s  %4d %3.1f %s")'
         endelse
         
         if NOT keyword_set(quiet) then begin
           dsn = 'thermal model'

           dataset_1d,id1, NH,      XTIT='log Nh', DATASET=dsn
           dataset_1d,id2, KT,      XTIT='kT'
           dataset_1d,id3, LC0P5_8, XTIT='log Lc[0.5:8]', DATASET=dsn
           dataset_1d,id4, F0P5_8,  XTIT='log F[0.5:8]', DATASET=dsn

           dataset_2d,id5, NH, KT, XTIT='log Nh', YTIT='kT', PSYM=1, NAN=[0,-1]
           
           dataset_2d,id6, alog10(net_counts), NH,      XTIT='log NET_CNTS', YTIT='log Nh', PSYM=1, NAN=[1,0], DATASET=dsn
           dataset_2d,id7, alog10(net_counts), KT,      XTIT='log NET_CNTS', YTIT='kT', PSYM=1, NAN=[1,-5]
           dataset_2d,id8, alog10(net_counts), LC0P5_8, XTIT='log NET_CNTS', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[1,20], DATASET=dsn
           dataset_2d,id9, alog10(net_counts), F0P5_8,  XTIT='log NET_CNTS', YTIT='log F[0.5:8]', PSYM=1, NAN=[1,-20], DATASET=dsn

           dataset_2d,id10, NH, LC0P5_8,         XTIT='log Nh', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[0,20], DATASET=dsn
           dataset_2d,id11, KT, LC0P5_8,         XTIT='kT', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[-5,20]

           chi_sqr      = null_val
           cstat       = null_val
           if (total(strmatch(tag_names(bt),'CHI_SQR')) GT 0) then begin
             chi_sqr[ind] = bt[ind].CHI_SQR
             dataset_2d,id12, net_counts, chi_sqr, XTIT='NET_CNTS', YTIT='Reduced chi^2', PSYM=1, DATASET=dsn, NAN=[0,0]
           endif

           if (total(strmatch(tag_names(bt),'CSTAT')) GT 0) then begin
             cstat[ind] = bt[ind].CSTAT
           endif
           
           save, seq, NH, KT, $
             F0P5_2, F2_8, FC2_8, F0P5_8, FC0P5_8, $
             L0P5_2, L2_8, LC2_8, L0P5_8, LC0P5_8, chi_sqr, cstat, FILE='thermal_spectroscopy.sav'
         endif

END_thermal_spectroscopy:         
         end ;thermal_spectroscopy

         
       ;--------------------------------------------------------------------------------------------
       'powerlaw_spectroscopy': begin

         ; Determine which sources belong in this table.
         in_this_table = (bt.SRC_SIGNIF[band_full] GE src_signif_min[0]) AND $
                         (bt.NET_CNTS  [band_full] GE net_counts_min) AND $
                          strmatch(bt.MODEL, '*tbabs_pow*')
         
         ind = where( in_this_table, powerlaw_count )
         if (powerlaw_count EQ 0) then begin
           print, 'NO sources in powerlaw_spectroscopy table'
           goto, END_powerlaw_spectroscopy
         endif


         ; Work will full-length vectors so user can find index for specific source in plotting tools.  
         ; Sources not in this table with have NaN values.
         NH     = alog10(bt.NH) + nh_offset
         PH     =        bt.PH
         F0P5_2 = alog10(bt.F0P5_2)
         F2_8   = alog10(bt.F2_8)
         FC2_8  = alog10(bt.FC2_8)
         F0P5_8 = alog10(bt.F0P5_8)
         FC0P5_8= alog10(bt.FC0P5_8)
         
         ind = where(in_this_table EQ 0, count)
         if (count GT 0) then begin
           NH     [ind] = !VALUES.F_NAN
           PH     [ind] = !VALUES.F_NAN
           F0P5_2 [ind] = !VALUES.F_NAN
           F2_8   [ind] = !VALUES.F_NAN
           FC2_8  [ind] = !VALUES.F_NAN
           F0P5_8 [ind] = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         ;; Suppress flux columns when any parameter has no fit result.
         invalid_fit = in_this_table AND ((finite(NH_VAL) AND finite(PH_VAL) AND finite(NORM)) EQ 0)
         ind = where(invalid_fit, count)
         if (count GT 0) then begin
           print, count, powerlaw_count, F='(%"\nThese %d (out of %d total) powerlaw sources have no good estimate for at least one fit parameter:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, bt[ind].OBJECT
           
           F0P5_2[ind]  = !VALUES.F_NAN
           F2_8[ind]    = !VALUES.F_NAN
           FC2_8[ind]   = !VALUES.F_NAN
           F0P5_8[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         ;; Suppress soft flux columns when Nh is large.
         invalid_flux = in_this_table AND (NH_VAL GT logNh_threshold)
         ind = where(invalid_flux, count)
         if (count GT 0) then begin
           print, count, thermal_count, logNh_threshold, F='(%"\nThese %d (out of %d total) powerlaw sources have log Nh > %f:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         nh_frozen_flag = null_flag
         ind = where( nh_frozen, count )
         if (count GT 0) then nh_frozen_flag[ind] = 'A'

         ph_frozen_flag = null_flag
         ind = where( ph_frozen, count )
         if (count GT 0) then ph_frozen_flag[ind] = 'P'

;         notes = nh_frozen_flag+ph_frozen_flag
         notes = null_col
         
         
         ;; Write out table.
         ind = where( in_this_table, count )

         
         L0P5_2  = lx_offset+(F0P5_2)
         L2_8    = lx_offset+(F2_8)
         LC2_8   = lx_offset+(FC2_8)
         L0P5_8  = lx_offset+(F0P5_8)
         LC0P5_8 = lx_offset+(FC0P5_8)
         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq[ind], CXOU[ind], $
         (bt[ind]).NET_CNTS[band_full], (bt[ind]).SRC_SIGNIF[band_full], $
         nh_range[ind],  ph_range[ind], norm_range[ind], $
 
         (L0P5_2[ind]), (L2_8[ind]), (LC2_8[ind]), (L0P5_8[ind]), (LC0P5_8[ind]), notes[ind], F=fmt

         print

         print, 'Reasons fit parameter error is invalid:'
         print, '  o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
         print, '  s: error computation skipped' 
         print, '  L: lowerlim outside specified range (soft limits in XSPEC)' 
         print, '  U: upperlim outside specified range (soft limits in XSPEC)' 
         print, '  n: tclout error "non-monotonicity detected"' 
         print, '  l: tclout error "hit hard lower limit" OR "search failed in -ve direction"' 
         print, '  u: tclout error "hit hard upper limit" OR "search failed in +ve direction"' 
         print, '  m: tclout error "minimization may have run into problem"' 
         print, '  c: tclout error "reduced chi-squared too high"' 
         print
         print, 'Reasons fit parameter itself is invalid:'
         print, '  r: parameter outside specified range (soft limits in XSPEC)' 
         print, '  o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
         print

         ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
         !TEXTUNIT = 0

         if keyword_set(sky_offset) then begin
           print, ' seq    old name              new name            NH          gamma   Norm            NC Emed model'
           forprint, seq[ind], bt[ind].OBJECT, bt[ind].OBJECT_FINAL, nh_anom_flags[ind], ph_anom_flags[ind], norm_anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s -> %s  %s  %s  %s  %4d %3.1f %s")'

         endif else begin
           print, ' seq         name          NH        gamma        Norm      NC  Emed      model'
           forprint, seq[ind],                      bt[ind].OBJECT_FINAL, nh_anom_flags[ind], ph_anom_flags[ind], norm_anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s  %s  %s  %s  %4d %3.1f %s")'
         endelse
         
         if NOT keyword_set(quiet) then begin
           ; Plot will full-length vectors so user can find index for specific source.
           dsn = 'powerlaw model'

           dataset_1d,id1, NH,      XTIT='log Nh', DATASET=dsn
           dataset_1d,id102, PH,    XTIT='photon index'
           dataset_1d,id3, LC0P5_8, XTIT='log Lc[0.5:8]', DATASET=dsn
           dataset_1d,id4, F0P5_8,  XTIT='log F[0.5:8]', DATASET=dsn

           dataset_2d,id105, NH, PH, XTIT='log Nh', YTIT='photon index', PSYM=1, NAN=[0,-1]
           
           dataset_2d,id6, net_counts, NH,      XTIT='NET_CNTS', YTIT='log Nh', PSYM=1, NAN=[1,0], DATASET=dsn
           dataset_2d,id107, net_counts, PH,    XTIT='NET_CNTS', YTIT='photon index', PSYM=1, NAN=[-1,0]
           dataset_2d,id8, net_counts, LC0P5_8, XTIT='NET_CNTS', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[1,20], DATASET=dsn
           dataset_2d,id9, net_counts, F0P5_8,  XTIT='NET_CNTS', YTIT='log F[0.5:8]', PSYM=1, NAN=[1,-20], DATASET=dsn

           dataset_2d,id10, NH, LC0P5_8,         XTIT='log Nh', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[0,20], DATASET=dsn
           dataset_2d,id111, PH, LC0P5_8,        XTIT='photon index', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[-1,0]

           chi_sqr      = null_val
           cstat       = null_val
           if (total(strmatch(tag_names(bt),'CHI_SQR')) GT 0) then begin
             chi_sqr[ind] = bt[ind].CHI_SQR
             dataset_2d,id12, net_counts, chi_sqr, XTIT='NET_CNTS', YTIT='Reduced chi^2', PSYM=1, DATASET=dsn, NAN=[0,0]
           endif
           
           if (total(strmatch(tag_names(bt),'CSTAT')) GT 0) then begin
             cstat[ind] = bt[ind].CSTAT
           endif
           
           save, seq, NH, PH, $
             F0P5_2, F2_8, FC2_8, F0P5_8, FC0P5_8, $
             L0P5_2, L2_8, LC2_8, L0P5_8, LC0P5_8, chi_sqr, cstat, FILE='powerlaw_spectroscopy.sav'
         endif

END_powerlaw_spectroscopy:         
         end ;powerlaw_spectroscopy
 
 
        else: print, 'No definition found for a table named '+table_name
      endcase
      printf, table_unit, line  ;This is the \enddata line.
    endif ; data section 
    
  endelse ; processing a table
endwhile

free_lun, demo_unit, in_unit
print
print, 'To test tables run:'
print, '  latex demo; dvips -o demo.ps demo; gv --orientation=landscape --scale=2 demo.ps&'
return
end




;;; ==========================================================================
;;; ae_make_movie
;;; ==========================================================================
; Find Floating underflow; run with /VERBOSE & check flatness of flux & energy; get MPEG license and try MPEG output; energy legend; look at color of overlapping sources; 

; See if /MOTION_VEC reduces file size.

;;; .run acis_extract_tools
;;; ae_make_movie,'theta.cat','1874'
;;; ae_make_movie, SCENE_TEMPLATE='obs1874/data/central_1.emap', $
;;;                SCENE_PARAM_FILE='scene.txt, JPEG_BASENAME='theta'
;;; OR
;;; ae_make_movie,'theta.cat','1874'
;;; ae_make_movie, SCENE_TEMPLATE='obs1874/data/central_1.emap', NUM_FRAMES=100, JPEG_BASENAME='theta'

;;; SCENE_PARAM_FILE is 4 column ASCII describing the scene in each frame:
;;;   time_fraction: [0:1] time tag of frame as fraction in interval [TSTART:TSTOP]
;;;   deltaX, deltaY: panning offset in arcseconds
;;;   deltaScale:     multiplicative adjustment to CDELT (degrees/pixel) (e.g. 1.01 means zoom out 1%)

PRO ae_make_movie, catalog_or_srclist, obsname, EXTRACTION_NAME=extraction_name, MIN_COVERAGE=min_coverage, $
  
                   SCENE_TEMPLATE=scene_template, SCENE_PARAM_FILE=scene_param_file, NUM_FRAMES=num_frames, $
                   MPEG_FILENAME=mpeg_filename, JPEG_BASENAME=jpeg_basename, $
                   FWHM=fwhm, SATURATION=saturation, INVERT=invert, $
                   SHOW_FRAMES=show_frames, VERBOSE=verbose

COMMON ae_make_movie, num_sources, sources

creator_string = "ae_make_movie, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()
print, 'http://www.astro.psu.edu/xray/docs/TARA/ae_users_guide.html'
print, 'patb@astro.psu.edu'
print
print, 'Note the following limitations to this method:'
print, '* Light curves and median energies are NOT background subtracted.'
print, '* Sources on multiple CCDs are skipped.'
print, '* Multiple observations are not supported.'
print, '* Bad time intervals are interpolated over.'
print

if (n_elements(min_coverage) NE 1) then min_coverage=0.5
if NOT keyword_set(saturation)   then saturation=0.25

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,3000)

type = size(obsname,/TNAME)
if (type NE 'UNDEFINED') AND (type NE 'STRING') then begin
  print, 'parameter "obsname" must be a string'
  return
endif

src_stats_basename       = 'source.stats'
lc_smooth_basename       = 'source.smooth_lc'


color_manager

;; =============================================================================
;; READ ALL THE SOURCE INFORMATION INTO MEMORY.

if keyword_set(catalog_or_srclist) then begin
  
  if keyword_set(sources) then begin
    ptr_free, sources.TIME, sources.COUNT_RATE, sources.MEDIAN_ENERGY, sources.psf_img
    dum = temporary(sources)
  endif
 
  ;; Input catalog should be an ascii file with source names, e.g. output of 
  ;; prior call with /CHOOSE_REGIONS.
  readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

  ; Trim whitespace and remove blank lines.
  sourcename = strtrim(sourcename,2)
  ind = where(sourcename NE '', num_sources)
  
  if (num_sources EQ 0) then begin
    print, 'ERROR: no entries read from source list ', catalog_or_srclist
    retall
  endif
  
  sourcename = sourcename[ind]
  print, num_sources, F='(%"\n%d sources found in catalog.\n")'
  
  st = {NAME:'', ra:0D, dec:0D, $
        TIME:ptr_new(), COUNT_RATE:ptr_new(), MEDIAN_ENERGY:ptr_new(), $
        nominal_rate:0.0, nominal_energy:0.0, min_energy:0.0, max_energy:0.0, xl:0, yl:0, psf_img:ptr_new()}
  sources = replicate(st, num_sources)


  for ii = 0, num_sources-1 do begin
    sources[ii].NAME = sourcename[ii]

    ;; Construct filenames.
    sourcedir = sourcename[ii] + '/' + extraction_subdir[ii]
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    fit_stats_fn        = sourcedir + src_stats_basename
    lc_smooth_fn    = obsdir + lc_smooth_basename

    if (NOT file_test(fit_stats_fn)) then begin
      print, 'Source ', sourcename[ii], ' not observed.'
      continue
    endif
    
    stats = headfits(fit_stats_fn, ERRMSG=error)
    if (NOT keyword_set(error)) then begin
      sources[ii].ra  = sxpar(stats, 'RA')
      sources[ii].dec = sxpar(stats, 'DEC')
    endif else print, 'WARNING! Could not read '+fit_stats_fn

    ; Read smoothed LC & median energy time series.
    if (NOT file_test(lc_smooth_fn)) then begin
      print, 'Source ', sourcename[ii], ' skipped; no ', lc_smooth_basename, ' found.'
      continue
    endif
    
    pheader = headfits(lc_smooth_fn)
    t = mrdfits(lc_smooth_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + lc_smooth_fn
    
    tmin = min(t.time, MAX=tmax)
    coverage = (tmax-tmin) / (sxpar(pheader, 'TSTOP')-sxpar(pheader, 'TSTART'))
    if (coverage LT min_coverage) then begin
      print, sourcename[ii], coverage, F='(%"Source %s skipped; LC coverage is only %4.2f")'
      continue
    endif
    
    if (n_elements(t) EQ 1) then t = [t[0],t[0]]

    sources[ii].TIME          = ptr_new(t.TIME, /NO_COPY)
    sources[ii].MEDIAN_ENERGY = ptr_new(t.MEDIAN_ENERGY / 1000., /NO_COPY)
    ; Some backward compatibility logic here:
    sources[ii].COUNT_RATE    = ptr_new( tag_exist(t, 'COUNT_RATE') ? t.COUNT_RATE : t.RATE, /NO_COPY)
    
    sources[ii].nominal_rate   = median(*(sources[ii].COUNT_RATE))
    sources[ii].nominal_energy = median(*(sources[ii].MEDIAN_ENERGY))
    sources[ii].min_energy     = min   (*(sources[ii].MEDIAN_ENERGY), MAX=max_energy)
    sources[ii].max_energy     = max_energy
  endfor ;ii

  print, fix(total(ptr_valid(sources.TIME))), min_coverage, F='(%"%d sources with >=%4.2f coverage in lc_smooth_basename.")'

  if NOT keyword_set(scene_template) then return
endif ;keyword_set(catalog_or_srclist)




;; =============================================================================
;; BUILD FRAME IMAGES
!EXCEPT=1

ptr_free, sources.psf_img

;; -----------------------------------------------------------------------------
;; Determine which sources are visible in the scene.
refhd = headfits(scene_template)
extast, refhd, refastr
scene_xsize = sxpar(refhd, 'NAXIS1')
scene_ysize = sxpar(refhd, 'NAXIS2')

scene_xcenter = (scene_xsize-1)/2.0D
scene_ycenter = (scene_ysize-1)/2.0D


if keyword_set(show_frames) then begin
  window, /FREE, XSIZE=scene_xsize<1024, YSIZE=scene_ysize<1024
  show_frames_win = !D.WINDOW
endif


ad2xy, sources.ra, sources.dec, refastr, xindex, yindex 

vis_index = where( ((xindex<yindex) GT 0) AND (xindex LT (scene_xsize-1)) $
                                          AND (yindex LT (scene_ysize-1)) $
                                          AND ptr_valid(sources.TIME), num_vis )

print, num_vis, F='(%"%d sources used for brightness & energy scaling")'
if (num_vis EQ 0) then begin
  return
endif


vis_sources = sources[vis_index]
xindex      = xindex [vis_index]
yindex      = yindex [vis_index]



if NOT keyword_set(fwhm) then begin
  ; Choose a PSF FWHM that's a fixed # of pixels.
  fwhm = 2.0 
endif

psf_exponent = 1.0
help, fwhm, psf_exponent

;; -----------------------------------------------------------------------------
;; Determine time tags of frames.
if keyword_set(scene_param_file) then begin
  readcol, scene_param_file, time_fractions, deltaX_arcsec, deltaY_arcsec, deltaScale, F='F,F,F,F'
  num_frames = n_elements(time_fractions)
  
  if (min(deltaScale) LE 0) then message, 'deltaScale must be positive'
  
endif else if keyword_set(num_frames) then begin
  time_fractions = (0.5 + findgen(num_frames))/num_frames
  deltaX_arcsec  = replicate(0,num_frames)
  deltaY_arcsec  = replicate(0,num_frames)
  deltaScale     = replicate(1,num_frames)
endif else begin
  print, 'You must supply either TIME_TAGS or NUM_FRAMES to specify the desired frame time tags.'
  return
endelse

; Convert time fractions to time tags.
time_fractions = 0 > time_fractions < 1
tstart = sxpar(refhd, 'TSTART')
tstop  = sxpar(refhd, 'TSTOP')
time_tags = tstart + time_fractions * (tstop-tstart)


if keyword_set(jpeg_basename) then begin
  files_to_remove = findfile( jpeg_basename + '.*.jpg', COUNT=count)
  if (count GT 0) then begin
    print, 'removing: ',files_to_remove
    file_delete, files_to_remove, /QUIET
  endif
  
  frame_num = lindgen(num_frames)
  jpeg_filenames = jpeg_basename + string(frame_num, F='(%".%4.4d.jpg")')
  forprint, frame_num, time_tags, TEXTOUT=jpeg_basename+'.lis', COMMENT='frame#    time'
endif


;; -----------------------------------------------------------------------------
;; Scale the brightness so the smallest nominal_rate has a brightness of 20%.
;; Choose a brightness rescaling table so that PSF peak pixels of the visible sources
;; at their nominal_rate fluxes would form a flat distribution over [min_level..1].
;; We'll add extra points to this table to extend it's flux range because we may 
;; need to EXTRAPOLATE the scaling 
;; curve beyond the range of the sources' nominal fluxes.  For example when the
;; brightest source flares we need to extrapolate beyond the nominal range.
;; This is a kind of histogram equalization scaling.
;; Only sources with positive nominal_rate fluxes are considered.
min_level = 0.2
max_level = 1.1

ind = where( vis_sources.nominal_rate GT 0, num_samples )
if (num_samples EQ 0) then begin
  print, 'All sources have zero nominal_rate fluxes; cannot continue.'
  return
endif

temp                 = vis_sources[ind].nominal_rate
nominal_rate_samples = temp[sort(temp)]

if (1) then begin
scaled_rate_samples  = (min_level + (max_level-min_level) * findgen(num_samples) / float(num_samples-1)) 
endif else begin
endelse

lin_params = linfit( nominal_rate_samples, scaled_rate_samples )

large_val = 10*max(nominal_rate_samples)
nominal_rate_samples = [0,nominal_rate_samples,large_val]
scaled_rate_samples  = [0,scaled_rate_samples, lin_params[0] + lin_params[1]*large_val]

if keyword_set(verbose) then begin
  function_1d, id2, nominal_rate_samples, scaled_rate_samples,  LINE=6, PSYM=1, XTIT='nominal rate', YTIT='central pixel value'
endif


;; -----------------------------------------------------------------------------
;; Construct an energy scaling table that makes good use of the color spectrum.
;; We'll use a table which results in a nearly flat distribution of scaled nominal energies
;; in the range [0..1].
;; We'll add extra points to this table to extend it's energy range because we may 
;; need to EXTRAPOLATE the scaling 
;; curve beyond the range of the sources' nominal energies.  For example
;; if the source with the hardest nominal energy gets even harder during a flare then
;; we'll need to scale an energy value outside the range of energies used to construct
;; the scaling curve.
;; This is a kind of histogram equalization scaling.
nominal_energy_samples = (vis_sources.nominal_energy)[sort( vis_sources.nominal_energy )]

scaled_energy_samples  = findgen(num_vis) / float(num_vis-1)

lin_params = linfit( nominal_energy_samples, scaled_energy_samples )

large_val = 10
nominal_energy_samples = [0,nominal_energy_samples,large_val]
scaled_energy_samples  = [0,scaled_energy_samples, lin_params[0] + lin_params[1]*large_val]

;; Determine max & min scaled energies that are possible.
linterp, nominal_energy_samples, scaled_energy_samples, min(vis_sources.min_energy), low_scaled_energy
linterp, nominal_energy_samples, scaled_energy_samples, max(vis_sources.max_energy), high_scaled_energy

if keyword_set(verbose) then begin
  function_1d, id4, nominal_energy_samples, scaled_energy_samples,  LINE=6, PSYM=1, XTIT='nominal energy', YTIT='scaled energy'
  help, low_scaled_energy, high_scaled_energy
endif


;; -----------------------------------------------------------------------------
;; Figure out an appropriate way to normalize PSF to 1.
wing_scale = 1
npix     = [100,100]
centroid = [50,50]
psf_raw = (  psf_gaussian(NPIX=npix, FWHM=fwhm,            CENTROID=centroid) + $
           2*psf_gaussian(NPIX=npix, FWHM=fwhm*wing_scale, CENTROID=centroid))
psf_peak = max( psf_raw )
help, psf_peak

if keyword_set(mpeg_filename) then $
  mpegID = mpeg_open( [scene_xsize,scene_ysize], MOTION_VEC_LENGTH=1, QUALITY=100 )

for frame_num = 0, num_frames-1 do begin

  ;; -----------------------------------------------------------------------------
  ;; Move scene center by the specified offset (in arcseconds) and adjust zoom.
  ; Convert specified offset for this frame from arcseconds to pixels.
  cdelt = abs(refastr.CDELT[0])
  deltaX_pix = (deltaX_arcsec[frame_num] / 3600.) / cdelt
  deltaY_pix = (deltaY_arcsec[frame_num] / 3600.) / cdelt
  
  ; Find RA,DEC at current scene center
  xy2ad, scene_xcenter, scene_ycenter, refastr, scene_ra_center, scene_dec_center
  
  ; Redefine astrometry so that RA,DEC is offset as specified from the scene center.
  ; One is added to new scene center because FITS CRPIX values are 1-based.
  refastr.CRPIX = 1 + [scene_xcenter+deltaX_pix, scene_ycenter+deltaY_pix]
  refastr.CRVAL =     [scene_ra_center, scene_dec_center]
  
  ; Adjust zoom as specified.
print, refastr.CRVAL
;print, deltaScale[frame_num]
  refastr.CDELT = refastr.CDELT * deltaScale[frame_num]


  ;; -----------------------------------------------------------------------------
  ;; Compute positions of sources & find ones visible.
  ad2xy, sources.ra, sources.dec, refastr, xindex, yindex 

  vis_index = where( ((xindex<yindex) GT 0) AND (xindex LT (scene_xsize-1)) $
                                            AND (yindex LT (scene_ysize-1)) $
                                            AND ptr_valid(sources.TIME), num_vis )

  if (num_vis NE 0) then begin
    vis_sources = sources[vis_index]
    xindex      = xindex [vis_index]
    yindex      = yindex [vis_index]
    rates_this_frame    = fltarr(num_vis)
    energies_this_frame = fltarr(num_vis)
  endif else begin
    rates_this_frame    = fltarr(2)
    energies_this_frame = fltarr(2)
  endelse

  scene_brightness   = fltarr(scene_xsize,scene_ysize)
  scene_energy_sum   = fltarr(scene_xsize,scene_ysize)
  
  print, frame_num, deltaX_pix, deltaY_pix, num_vis, F='(%"Frame %d; offset=(%5.2f,%5.2f); %d visible sources")' 

  ;; -----------------------------------------------------------------------------
  ;; Build frame of the scene at the specified time tag.
  for ii = 0, num_vis-1 do begin
    ; Sample the COUNT_RATE and MEDIAN_ENERGY time series at the requested time tags 
    ; and send through the scaling tables constructed earlier.
    ; It's vital to use linterp so that no linear extrapolation will be done. ???

    linterp, *(vis_sources[ii].TIME), *(vis_sources[ii].COUNT_RATE),    time_tags[frame_num], rate
    linterp, *(vis_sources[ii].TIME), *(vis_sources[ii].MEDIAN_ENERGY), time_tags[frame_num], median_energy
    
    linterp, nominal_rate_samples,   scaled_rate_samples,   rate,          scaled_rate
    linterp, nominal_energy_samples, scaled_energy_samples, median_energy, scaled_energy
    rates_this_frame[ii]    = scaled_rate
    energies_this_frame[ii] = scaled_energy

    
    ; Generate a PSF image and add to the scene.  Add weighted energy image to running sum.
    ; There are some floating underflow exceptions we have to mask.
    xl = floor(xindex[ii] - fwhm*wing_scale) > 0
    yl = floor(yindex[ii] - fwhm*wing_scale) > 0
  
    xh = ceil (xindex[ii] + fwhm*wing_scale) < (scene_xsize-1)
    yh = ceil (yindex[ii] + fwhm*wing_scale) < (scene_ysize-1)
    
    save_except = !EXCEPT & !EXCEPT = 0
;    psf_img = (scaled_rate) * $
;             ( psf_gaussian(NPIX=[1+xh-xl,1+yh-yl], FWHM=fwhm, $
;                            CENTROID=[xindex[ii]]-xl,yindex[ii]-yl]) / psf_peak )^psf_exponent

    npix     = [1+xh-xl,1+yh-yl]
    centroid = [xindex[ii]-xl,yindex[ii]-yl]
    psf_raw = (  psf_gaussian(NPIX=npix, FWHM=fwhm,            CENTROID=centroid) + $
               2*psf_gaussian(NPIX=npix, FWHM=fwhm*wing_scale, CENTROID=centroid)) 
    
    psf_img = (scaled_rate/psf_peak) * psf_raw
;print, max(psf_img)                        
    scene_brightness[xl,yl] = scene_brightness[xl:xh,yl:yh] + psf_img
    scene_energy_sum[xl,yl] = scene_energy_sum[xl:xh,yl:yh] + psf_img * scaled_energy

    error = check_math(MASK=32)    ;Clear floating underflow
    !EXCEPT = save_except

    
    if (ii GT 0) AND ((ii MOD 100) EQ 0) then print, ii, ' sources processed'
  endfor ;ii

  ; Apply normalization step in the computation of weighted energy image.
  scene_energy = scene_energy_sum / scene_brightness
  ind = where( finite( scene_energy, /NAN ), count ) 
  if (count GT 0) then scene_energy[ind] = 0

  if keyword_set(verbose) then begin
    dataset_1d, id1, rates_this_frame
    dataset_1d, id3, energies_this_frame
    s='' & read,s
  endif

  ;; -----------------------------------------------------------------------------
  ;; Construct color image using the HSV model.
  
  ; Scale scene_energy to a normalized hue image
  hue_norm = (scene_energy - low_scaled_energy) / ((high_scaled_energy - low_scaled_energy)>1e-8)

  tara_hsv2rgb,  0.0 > hue_norm < 1.0, saturation, scene_brightness > 0, 0, $
                red_data, grn_data, blu_data

  ; This is the place we would introduce a user-supplied background for the scene, e.g. 
  ; red, green, blue planes from a diffuse emission analysis.
  ; Add those into the point source planes, e.g. 
  ; red_data = red_data +  red_bkg * (0.3 / max(red_bkg))
    
  if keyword_set(show_frames) then begin
    wset, show_frames_win
    color_manager, /X_TRUE
  endif
  
  ; Its IMPORTANT to specify HIGH_VALUE below so that all frames are scaled the same!
  ; A HIGH_VALUE < 1 will "clip" the bright stars.
  rgb_scale, red_data, grn_data, blu_data, $
             LOW_VALUE =0, LOW_NORM=0, $
             HIGH_VALUE=0.8, HIGH_NORM=1, INVERT=keyword_set(invert), $
             red_channel, grn_channel, blu_channel, DO_PLOT=keyword_set(show_frames)

  ; Add graphic showing time_fractions.
  bar_height = ceil(0.05*scene_ysize)
  xx = round( (scene_xsize-1) * time_fractions[frame_num] ) 
  red_channel[xx, 0:bar_height] = 1
  grn_channel[xx, 0:bar_height] = 1
  blu_channel[xx, 0:bar_height] = 1
  
  
  ;; -----------------------------------------------------------------------------
  ;; Save color image.
  img = bytarr(3,scene_xsize,scene_ysize)
         
  num_levels = 256       
  img[0,*,*] = floor( num_levels * red_channel ) < (num_levels-1)
  img[1,*,*] = floor( num_levels * grn_channel ) < (num_levels-1)
  img[2,*,*] = floor( num_levels * blu_channel ) < (num_levels-1)

  if keyword_set(mpeg_filename) then $
    mpeg_put, mpegID, FRAME=frame_num, IMAGE=img
  
  if keyword_set(jpeg_basename) then $
    write_jpeg, jpeg_filenames[frame_num], img, TRUE=1, QUALITY=100
endfor; frame_num

if keyword_set(mpeg_filename) then begin
  print, 'Writing ', mpeg_filename, ' ...'
  mpeg_save,  mpegID,  FILENAME=file_expand_path(mpeg_filename)
  mpeg_close, mpegID
endif
        
return
end


;; =============================================================================
;; =============================================================================
PRO test_psfs

img=psf_gaussian(NPIX=200, FWHM=fwhm, CENTROID=100,NDIM=1)
x=indgen(200)
function_1d,id,x,img

; We'd like stars to get "larger" when they flare, but am having a hard time finding a PSF that gives the right look.
; Perhaps FWHM should vary with flux!  That would slow down code.

; This one seems disk-like in movie.
limg = alog(img) > (-exp(1))
function_1d,id,x,limg

; This one seems fuzzy is movie.
function_1d,id,x,img^0.25
return
end


;; =============================================================================
;; =============================================================================
PRO test_colors
show_frames=1

scene_xsize = 300
scene_ysize = 300
color_manager

if keyword_set(show_frames) then begin
  window, /FREE, XSIZE=scene_xsize<1024, YSIZE=scene_ysize<1024
  show_frames_win = !D.WINDOW
endif

ima=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[100,100])
imb=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[200,100])
imc=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[150,200])
scene_brightness=ima+imb+imc
scene_energy_sum=ima+2*imb+4*imc

scene_energy = scene_energy_sum / scene_brightness
ind = where( finite( scene_energy, /NAN ), count ) 
if (count GT 0) then scene_energy[ind] = 0

hue_norm = scene_energy / max(scene_energy)

;function_2d,id,scene_energy

  saturation = 0.5

  tara_hsv2rgb, hue_norm, saturation, scene_brightness > 0, 0, $
                red_data, grn_data, blu_data

  if keyword_set(show_frames) then begin
    wset, show_frames_win
    color_manager, /X_TRUE
  endif

  
  rgb_scale, red_data, grn_data, blu_data, $
             LOW_VALUE =0,  LOW_NORM=0, $
             HIGH_VALUE=0.8, HIGH_NORM=1, $
             red_channel, grn_channel, blu_channel, DO_PLOT=keyword_set(show_frames)

stop
function_2d,id,hue_norm,DATA='hue_norm'
function_2d,id,hue_img,DATA='hue_img'
function_2d,id,red_data,DATA='red_data'
function_2d,id,grn_data,DATA='grn_data'
function_2d,id,blu_data,DATA='blu_data'

return
end
  
;hue_norm=0.5 + 0.2*findgen(200)/199.
;value_data=replicate(1,200)
;make_2d,hue_norm,value_data

;; =============================================================================
;; =============================================================================
PRO make_scene_params, num_frames, filename

openw, unit, filename, /GET_LUN
!TEXTUNIT = unit

; One cycle on full field.
zero = replicate(0,num_frames)
one  = replicate(1,num_frames)
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), zero, zero, one, /NoCOMMENT

; One cycle while linear zooming.
; For f3.reg
xoffset =   8.86 ;arcsec
yoffset = -78.7 ; arcsec
zoom    =  4.69 

deltaX     = replicate(xoffset/float(num_frames)      , num_frames)
deltaY     = replicate(yoffset/float(num_frames)      , num_frames)
deltaScale = replicate(exp( -alog(zoom) / num_frames ), num_frames)
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), deltaX, deltaY, deltaScale, /NoCOMMENT

; One cycle on zoomed field.
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), zero, zero, one, /NoCOMMENT

free_lun, unit

return
end



;; =============================================================================
;; =============================================================================
PRO plot_spectrum, sourcename

base = sourcename +'/' + sourcename
src = mrdfits(base + '.pi',     1, src_header)
bkg = mrdfits(base + '_bkg.pi', 1, bkg_header)

energy = 14.45/1000.*(1+indgen(n_elements(src)))

backscal = sxpar(src_header, 'BACKSCAL') / bkg.BACKSCAL
spectrum = src.counts - backscal * bkg.counts

plot, energy, spectrum[0:547], PSYM=10

ind = where(src.counts NE 0)
function_1d, id, energy[ind], (src.counts)[ind],             LINE=6, PSYM=4, DATASET='source'
ind = where(bkg.counts NE 0)
function_1d, id, energy[ind], -backscal * (bkg.counts)[ind], LINE=6, PSYM=1, DATASET='background'

return
end



;;; ==========================================================================
;;; Set default spectral model preference.
;;; Use this tool to establish a default spectral model preference (in the FIT
;;; keyword BEST_MDL in the primary HDU of source.spectra) prior to running the
;;; interactive tool ae_spectra_viewer
;;; The parameter 'hduname' is a regular expression understood by strmatch().
;;; The parameter 'hduname' can be a scalar or a vector.
;;; ==========================================================================

PRO ae_default_model_preference, catalog_or_srclist, hduname, FORCE=force

run_command, /INIT

fit_stats_basename       = 'source.spectra'
src_stats_basename       = 'source.stats'

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,10000)


readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

if (n_elements(hduname) EQ 1) then hduname = replicate(hduname, num_sources)

if (n_elements(hduname) NE num_sources) then begin
  print, 'ERROR: # of sources in catalog does not match length of hduname vector.'
  retall
endif

for ii = 0, num_sources-1 do begin
  print, sourcename[ii], F='(%"Source: %s")'
  
  ;; Find the HDUs in source.spectra.
  sourcedir = sourcename[ii] + '/' + extraction_subdir[ii] 
  fit_stats_fn  = sourcedir + fit_stats_basename
  fits_open, fit_stats_fn, fcb, /NO_ABORT, MESSAGE=open_error
    
  if keyword_set(open_error) then begin
    print, 'No spectral models found.'
    continue
  end
  
  fits_close, fcb
  
  ; If an existing preference is found, respect it.
  preference = sxpar(fcb.HMAIN, 'BEST_MDL', COUNT=count)
  if (count GT 0) then begin
    if keyword_set(force) then begin
      print, 'Overriding existing preference for ', preference
    endif else begin
      print, 'Respecting existing preference for ', preference
      continue
    endelse
  endif

  ; Find the last HDU matching the specified model name spec.
  ind = where( strmatch(fcb.EXTNAME, hduname[ii], /FOLD_CASE), count )
  
  if (count GT 1) then begin
    ind = ind[count-1]
    print, count, hduname[ii], fcb.EXTNAME[ind],  F='(%"  %d spectral models match %s; using the most recent: %s )")'
    count = 1
  endif
      
  if (count EQ 0) then begin
    print, hduname[ii], F='(%"WARNING! Cannot find any spectral model matching ''%s''")'
    continue
  endif
  
  ;; Write the preference.
  cmd = string(fit_stats_fn, fcb.EXTNAME[ind], $
                 F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=BEST_MDL value=""%s"" comment=""default preferred model""')")

  run_command, cmd
  
  ; Delete any "best model" symlink we find, and remake it.
  link_name = sourcedir + 'spectral_models/best_model'
  if file_test(link_name, /DANGLING_SYMLINK) OR file_test(link_name, /SYMLINK) then file_delete, link_name
  if file_test(link_name) then begin
    print, 'WARNING: existing file'+link_name+' has not been changed.'
  endif else begin
    file_link, hduname[ii], link_name
  endelse
   
  
endfor ;ii
return
end



;; =============================================================================
;; spectra_viewer tool
;;
;; Example: ae_spectra_viewer, 'temp.srclist', KEY=['NH','KT','PH','F0P5_8']
;; =============================================================================

;; =============================================================================
;;; Widget Event Handler Procedure
PRO ae_spectra_viewer_event, event

widget_control, /HOURGLASS
fit_stats_basename       = 'source.spectra'
src_stats_basename       = 'source.stats'


;; Get the state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=st

num_sources = n_elements((*st).sourcename)
num_models  = (*st).num_models

widget_control, (*st).source_number_id, GET_VALUE=source_number                                       
 
sourcedir = (*st).sourcename[source_number-1] + '/' + (*st).extraction_subdir[source_number-1] 
fit_stats_fn  = sourcedir + fit_stats_basename

    
;; Process the event.
new_src_flag = 0
select_model_flag = 0

DestroyFlag = 0
case event.ID of

;--------------------------------------------------------------------------
  (*st).model_table: $
   begin
  if (Event.TYPE EQ 4) then begin
    row_num = Event.SEL_TOP
    if (row_num EQ Event.SEL_BOTTOM) AND (row_num GE 0) AND (row_num LT num_models) then begin
      print, 'Selected model ', (*(*st).model_names)[row_num]
      select_model_flag = 1
      
      cmd = string(fit_stats_fn, (*(*st).model_names)[row_num], $
                 F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=BEST_MDL value=""%s"" comment=""preferred model""')")

      run_command, cmd
      
      ; Delete any "best model" symlink we find, and remake it.
      link_name = sourcedir + 'spectral_models/best_model'
      if file_test(link_name, /DANGLING_SYMLINK) OR file_test(link_name, /SYMLINK) then file_delete, link_name
      if file_test(link_name) then begin
        print, 'WARNING: existing file'+link_name+' has not been changed.'
      endif else begin
        file_link, (*(*st).model_names)[row_num], link_name
      endelse
      
      table_select = [[0,row_num], [n_elements((*st).keylist)-1,row_num]]
      widget_control,  (*st).model_table, SET_TABLE_SELECT=table_select     
    endif
  endif
  end

  
;--------------------------------------------------------------------------
  (*st).provisional_id: $
   begin
   is_provisional = Event.select
   cmd = string(fit_stats_fn, is_provisional ? 'T' : 'F', $
             F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=PROVISNL value=""%s"" datatype=boolean comment=""BEST_MDL is provisional""')")

   run_command, cmd
   end
  
   
;--------------------------------------------------------------------------
  (*st).source_number_id: $
   begin
   new_src_flag = 1
   end

;--------------------------------------------------------------------------
  (*st).source_label_id: $
   begin
   widget_control, (*st).source_label_id, GET_VALUE=source_label                                       
   ind = where(strtrim(source_label[0],2) EQ (*st).source_label, count)
   if (count GE 1) then begin
     source_number = 1+ind[0]
     new_src_flag = 1
   endif else print, 'Cannot find that source label.'
   end
   
;--------------------------------------------------------------------------
  (*st).prev_button: $
   begin
   new_src_flag = 1
   source_number = source_number - 1
   end

;--------------------------------------------------------------------------
  (*st).next_button: $
   begin
   new_src_flag = 1
   source_number = source_number + 1
   end

;--------------------------------------------------------------------------
  (*st).plot_name_list: $
   begin
   new_src_flag = 1
   end

;--------------------------------------------------------------------------
  (*st).done_button: DestroyFlag = 1

;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in fits_viewer'
       help,/ST,Event
       end
endcase

if new_src_flag then begin
  ; Range check the source number.
  source_number = 1 > source_number < num_sources
  this_sourcename   = (*st).sourcename  [source_number-1]
  this_source_label = (*st).source_label[source_number-1]
  widget_control, (*st).source_name_id,   SET_VALUE=this_sourcename
  widget_control, (*st).source_label_id,  SET_VALUE=this_source_label    
  widget_control, (*st).source_number_id, SET_VALUE=source_number    
  print, this_sourcename, this_source_label, F='(%"\n----------------------------\n%s (%s)")'
  this_note = (*st).notes[source_number-1]
  if keyword_set(this_note) then print, this_note
  
  num_columns = n_elements((*st).keylist)  
  
  ; Clear the table     
  if (num_models GT 0) then begin
    ; Unfortunately we cannot get the table to tell us how many rows it has, not even by GET_VALUE., so we have to keep track ourselves.
 ;  widget_control, (*st).model_table, /DELETE_ROWS, USE_TABLE_SELECT=[0,0,num_columns-1,(*st).num_models-1]
    all_rows = replicate(-1,2,(*st).num_models)
    all_rows[1,*] = indgen((*st).num_models)
    widget_control, (*st).model_table, /DELETE_ROWS, USE_TABLE_SELECT=all_rows

    num_models = 0
  endif
  
  
  ;--------------------------------------------------------------------------
  ;; Find the HDUs in source.spectra.
  sourcedir = this_sourcename + '/' + (*st).extraction_subdir[source_number-1] 
  fit_stats_fn  = sourcedir + fit_stats_basename
  fits_open, fit_stats_fn, fcb, /NO_ABORT, MESSAGE=open_error
    
  model_names = ''
  selected_model_index = -1

  if keyword_set(open_error) then begin
  endif else begin
    num_models = fcb.NEXTEND
    if (fcb.NEXTEND GT 0) then begin
      model_names = strtrim(fcb.EXTNAME[1:*], 2)
      
      ; SORT the model names to be convenient for our standard spectral modeling recipe (recipe.txt).
      name_template = ['*_A*', '*_B*', '*_C*', '*_D*', '*_E*', '*_std1*', '*_std2*', '*kT_max*', '*_pow*']
      tail_index = 0L
      for jj = 0,n_elements(name_template)-1 do begin
        ind = where(strmatch(model_names, name_template[jj], /FOLD_CASE), count)
        
        ; Process each match.
        for kk=0,count-1 do begin
          this_index = ind[kk]
          
          ; Only process matches found in the unsorted tail.
          if (this_index LT tail_index) then continue
        
          ; Swap the matched model with the first model in the unsorted tail.
          temp                    = model_names[this_index]
          model_names[this_index] = model_names[tail_index]
          model_names[tail_index] = temp
          tail_index++
        endfor ;kk
      endfor ;jj
      
      
;      case widget_info( (*st).plot_name_list, /DROPLIST_SELECT ) of
;        0: name = '/ldata.ps'
;        1: name = '/icounts.ps'
;      endcase
      
      plot_files = strarr(num_models,2)
      plot_files[*,0] = sourcedir + 'spectral_models/' + model_names + '/ldata.ps'
      plot_files[*,1] = sourcedir + 'spectral_models/' + model_names + '/icounts.ps'
      
      for ii=0,n_elements(plot_files)-1 do begin
        if NOT file_test(plot_files[ii]) then plot_files[ii] = (*st).no_model_file
      endfor
    endif
  endelse
  
  num_gv_pairs  = n_elements((*(*st).gv_pids)) / 2  
  

  ;--------------------------------------------------------------------------
  ;; NOTE that the gv manual says that you can send it a signal
  ;;    kill -HUP
  ;; to reload, rather than using the --watch option.  But it doesn't work on our X11 environment.
  ;;
  ;; I also find that neither symlinks nor hard links will reliably induce gv to redraw.
  ;; Thus, we're forced to COPY the PostScript files we want to display.

  if (num_models EQ 0) then begin
    print, 'No fit results available for ', this_sourcename
    
    ;; Link all the existing gv processes to the null Postscript file.
    for ii=0,num_gv_pairs-1 do begin
        file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,0]
        file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,1]
    endfor

    ; Move on to the next/previous source.
    if ((event.ID EQ (*st).next_button) AND (source_number+1 LE num_sources)) then $
      widget_control, (*st).next_button, SEND_EVENT=event
    if ((event.ID EQ (*st).prev_button) AND (source_number-1 GE 1          )) then $
      widget_control, (*st).prev_button, SEND_EVENT=event
    
  endif else begin
    ;; Display the plots.
    num_gv_pairs_needed = (num_models > num_gv_pairs)

    gv_files= strarr(num_gv_pairs_needed,2)
    gv_files[*,0] = (*st).tempdir + string(indgen(num_gv_pairs_needed), F='(%"%d_g")')                
    gv_files[*,1] = (*st).tempdir + string(indgen(num_gv_pairs_needed), F='(%"%d_c")')                
    
    ; Link up the files we need to display to the gv processes and
    ; link the null file to any extra gv processes.
    for ii=0,num_gv_pairs_needed-1 do begin
      if (ii LT num_models) then begin
        file_copy, /OVERWRITE,   plot_files[ii,0],  gv_files[ii,0] 
        file_copy, /OVERWRITE,   plot_files[ii,1],  gv_files[ii,1] 
      endif else begin
        file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,0]
        file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,1]
      endelse
    endfor
 
    ; Do we need more gv sessions?
    if (num_gv_pairs_needed GT num_gv_pairs) then begin
      gv_logfile = (*st).tempdir + 'gv.log'
      gv_pids = strarr(num_gv_pairs_needed,2)
      if (num_gv_pairs GT 0) then gv_pids[0,0] = *(*st).gv_pids $
                             else file_delete, gv_logfile, /QUIET
      
;      color = ['Cornsilk','Tan','SandyBrown','Goldenrod','Peru','DarkGoldenrod','Chocolate','Sienna','Brown','DarkSalmon','Salmon','IndianRed']
      color = ['Cornsilk','Goldenrod','Chocolate','DarkSalmon', 'Tan','Peru','Sienna','Salmon', 'SandyBrown','DarkGoldenrod','Brown','IndianRed']
      color = [color,color]
      
      seq = num_gv_pairs + indgen(num_gv_pairs_needed-num_gv_pairs)
      row = seq/3
      col = seq MOD 3
      sort_ind = reverse(sort(-100*row+col))
      dx = 400
      dy = 400

      fmt = "(%'gv --watch -xrm ""GV*background:%s"" -geometry +%d+%d --ad=%s %s >>&! %s &')" 
        
      for jj=0,n_elements(seq)-1 do begin
        ind = sort_ind[jj]
        ii = seq[ind]
        xpos =     2*dx*col[ind]
        ypos = 400+  dy*row[ind]

        cmd = string(color[ii], xpos+dx, ypos, (*st).gv_resource_file, gv_files[ii,1], gv_logfile, F=fmt)
        run_command, cmd, result, /UNIX
        gv_pids[ii,1] =  (strsplit(result[0], /EXTRACT))[1] 
        wait, 0.5

        cmd = string(color[ii], xpos   , ypos, (*st).gv_resource_file, gv_files[ii,0], gv_logfile, F=fmt)
        run_command, cmd, result, /UNIX
        gv_pids[ii,0] =  (strsplit(result[0], /EXTRACT))[1] 
        wait, 0.5
      endfor
      
      num_gv_pairs = num_gv_pairs_needed
      
      *(*st).gv_pids = gv_pids
    endif ; creating new gv processes



  ;--------------------------------------------------------------------------
    ;; Populate the table of fit parameters.
    widget_control, (*st).model_table, INSERT_ROWS=num_models
    table       = strarr(num_columns, num_models)
;    table_color = strarr(num_columns, num_models)
    CSTAT       = fltarr(num_models)
    
    ; For each model, find each FITS keyword and format into a cell.
    ; It is vital to reference the extention by name rather than by number because we have re-ordered model_names!
    for ii=0,num_models-1 do begin
      fits_read, fcb, dummy, model_header, /NO_PDU, /HEADER_ONLY, EXTNAME=model_names[ii], /NO_ABORT, MESSAGE=error 
      
      if keyword_set(error) then message, 'BUG!'
      
      CSTAT  [ii] = sxpar(model_header,'CSTAT')
      
      for jj=0,num_columns-1 do begin
       keyname = (*st).keylist[jj]
       value =  sxpar(model_header, keyname, COUNT=count) 
       format = (jj EQ 0) ? '(A)' : '(g10.2)'
       
       if (keyname EQ 'CSTAT') then format = '(g10.4)'
       if (count EQ 1) then table[jj,ii] = string(value, F=format)
       
       if (keyname EQ 'FC0P5_8') then begin                 
         if (alog10(value/sxpar(model_header,'F0P5_8')) GT (*st).flux_correction_limit) then $
           widget_control, (*st).model_table, BACKGROUND_COLOR=[255,100,100], USE_TABLE_SELECT=[[jj,ii],[jj,ii]]
       endif
      endfor
    endfor
    
    ind = where((*st).keylist EQ 'CSTAT', count)
    if (count GT 0) then table[ind,*] = string(CSTAT-min(CSTAT), F='(g10.4)')
    
    widget_control, (*st).model_table, SET_VALUE=table
    
    
    ;; Identify any model selection previously made and stored in the primary HDU..
     selected_model_name = strtrim(sxpar(fcb.HMAIN, 'BEST_MDL'), 2)
     is_provisional      = sxpar(fcb.HMAIN, 'PROVISNL', COUNT=0)
     if (count EQ 0) then is_provisional=0
     
     selected_model_index = where(model_names EQ selected_model_name, count)
     
    if (count EQ 1) then begin
      table_select = [[0,selected_model_index],[num_columns-1,selected_model_index]]
    endif else begin
      table_select = [[-1,-1],[-1,-1]]
    endelse
  
    widget_control,  (*st).model_table,    SET_TABLE_SELECT=table_select     
    widget_control,  (*st).provisional_id, SET_VALUE=is_provisional
  endelse ; num_models GT 0
    
  ; Save the models found so the event handler can see them next time.
  *(*st).model_names = model_names
   (*st).num_models  = num_models    


  if NOT keyword_set(open_error) then fits_close, fcb
  
  
  ;; Show the source statistics.
  src_stats_fn = sourcedir + src_stats_basename
  src_stats    = headfits(src_stats_fn)
  sxdelpar, src_stats, ['SIMPLE','BITPIX','NAXIS','EXTEND','DATE','CREATOR','RA_ML','DEC_ML','QUANTML','QUANTCOR','RA_CORR','DEC_CORR','RA_DATA','DEC_DATA','ERR_DATA','RA_PSF','DEC_PSF','END']
  
  if (widget_info( (*st).stats_header_id, /VALID_ID )) then $
    widget_control, (*st).stats_header_id, SET_VALUE=src_stats
endif  ;new_src_flag


; DONE button
if (DestroyFlag) then begin
  ; Kill the gv processes.
  if (n_elements(*(*st).gv_pids) GT 0) then begin
    run_command, /UNIX, 'kill -TERM ' + strjoin('-' + *(*st).gv_pids,' '), STATUS=status
  endif
  
  
  ; Remove the temp_dir.
  files = file_search((*st).tempdir+'/*', /MATCH_INITIAL_DOT, COUNT=count)
  if (count GT 0) then file_delete, files, /QUIET
  file_delete, (*st).tempdir, /QUIET

  widget_control, top_base, /DESTROY
endif


return
end


;==========================================================================
;;; Widget Creation Routine
;==========================================================================
PRO ae_spectra_viewer, catalog_or_srclist, EXTRACTION_NAME=extraction_name, $
                       KEYLIST=keylist, NOTES=notes, BLOCK=block

if NOT keyword_set(keylist) then keylist = ['NH','KT','KT2','PH','F0P5_2','F2_8','FC2_8','F0P5_8','FC0P5_8','CSTAT','CHI_SQR']

column_labels = ['model',keylist]
flux_correction_limit = '0.5'

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,10000)

; Read full lines by setting delimiter to CR character.
readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';', DELIMIT=string(13B)

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in source list.\n")'

case n_elements(notes) of
  0: notes = strarr(num_sources)
  1: notes = replicate(notes, num_sources)
  num_sources: 
  else: begin
        print, 'ERROR: length of NOTES input not equal to number of sources'
        retall
        end
endcase

; Parse lines with semicolons into source names and notes.
for ii=0,num_sources-1 do begin
  ind = strpos(sourcename[ii],';')
  if (ind NE -1) then begin
    notes     [ii] = strtrim(strmid(sourcename[ii],ind+1) ,2)
    sourcename[ii] = strtrim(strmid(sourcename[ii],0,ind) ,2)
  endif
endfor ;ii

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
param_dir            =         tempdir + 'param/'
help, tempdir

run_command, /INIT, PARAM_DIR=param_dir

; Identify a gv resources file we can use.
result = routine_info( 'ae_spectra_viewer', /SOURCE )
fdecomp, result.PATH, disk, codedir

gv_resource_file = "gv_resources.txt"
if (NOT file_test(gv_resource_file)) then begin
  gv_resource_file = codedir + gv_resource_file

  if (NOT file_test(gv_resource_file)) then begin
    print, 'Cannot find '+gv_resource_file
    retall
  endif
endif

; Identify a PostScript file to display when a gv session is not needed.
; Copy it to the tempdir for speed.
no_model_file       = "no_model.eps"
local_no_model_file = tempdir + no_model_file

if (NOT file_test(no_model_file)) then begin
  no_model_file = codedir + no_model_file

  if (NOT file_test(no_model_file)) then begin
    print, 'Cannot find '+no_model_file
    retall
  endif
endif
file_copy, /OVERWRITE, no_model_file, local_no_model_file

;; Look up the LABEL property of all the sources.
num_sources = n_elements(sourcename)
label = string(1+lindgen(num_sources))  

print, 'Reading source labels ...'
src_stats_basename       = 'source.stats'
for ii = 0, num_sources-1 do begin
  sourcedir = sourcename[ii] + '/' + extraction_subdir[ii] 
  
  stats_fn  = sourcedir + src_stats_basename
  stats = headfits(stats_fn, ERRMSG=error)
  
  if (NOT keyword_set(error)) then begin
    temp    = sxpar(stats, 'LABEL', COUNT=count)
    if (count GT 0) then label[ii] = temp
  endif else print, 'WARNING! Could not read '+stats_fn
endfor ;ii
label = strtrim(label,2)  


top_base = widget_base(TITLE='ACIS Extract Spectra Viewer', /BASE_ALIGN_CENTER, /COLUMN, $
                        /SPACE, /XPAD, /YPAD)

; source_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER )
 button_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER)
 
   plot_name_list = 0L ;widget_droplist( button_base, VALUE=['grouped spectra','cumulative spectra'])
   provisional_id   = cw_bgroup( button_base, ['provisional'], /NONEXCLUSIVE, SET_VALUE=0 )
   source_label_id  = cw_field( button_base, /STRING , /RETURN_EVENTS, XSIZE=5, VALUE='', TITLE='Src Label')                
   source_name_id   = widget_label( button_base, /DYNAMIC_RESIZE, VALUE=' ' )                                
   prev_button      = widget_button( button_base, VALUE='Previous' )
   next_button      = widget_button( button_base, VALUE='  NEXT  ' )
   source_number_id = cw_field( button_base, /INTEGER, /RETURN_EVENTS, XSIZE=4, VALUE=1,  TITLE='#')                
   status_field_id  = widget_label( button_base, /DYNAMIC_RESIZE, VALUE='red: flux correction > '+flux_correction_limit+' dex' )                                
   done_button = widget_button( button_base, VALUE='Dismiss' )


num_models = 10
model_table = widget_table( top_base, ALIGNMENT=2, /ALL_EVENTS, COLUMN_LABELS=column_labels, COLUMN_WIDTHS=[300,replicate(100,n_elements(keylist))], /RESIZEABLE_COLUMNS, /SCROLL, SCR_XSIZE=1500,  XSIZE=n_elements(column_labels), Y_SCROLL_SIZE=num_models, /DISJOINT_SELECTION  )


header_base = widget_base(TITLE='Fits Header', GROUP=top_base, XOFFSET=0, YOFFSET=330, /COLUMN, /SPACE, /XPAD, /YPAD)

  stats_header_id = widget_text( header_base, /SCROLL, XSIZE=80, YSIZE=20 )

state = { source_number_id:source_number_id, source_label_id:source_label_id, source_name_id:source_name_id, $
          prev_button:prev_button, next_button:next_button, status_field_id:status_field_id, $
          model_table:model_table, stats_header_id:stats_header_id, $
          done_button:done_button, plot_name_list:plot_name_list, provisional_id:provisional_id, $
          
          flux_correction_limit:float(flux_correction_limit), $
          
          model_names:ptr_new(/ALLOC), num_models:num_models, $
          
          keylist:['EXTNAME',keylist], sourcename:sourcename, source_label:label, extraction_subdir:extraction_subdir, $
          notes:notes, $
          
          gv_resource_file:gv_resource_file, no_model_file:local_no_model_file, tempdir:tempdir, gv_pids:ptr_new(/ALLOC) }

; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, header_base, /REALIZE
widget_control, top_base,    /REALIZE

event={ID:prev_button, TOP:top_base, HANDLER:top_base}
widget_control, prev_button, SEND_EVENT=event

;dum = ae_spectra_viewer_event({ID:choose_button, TOP:top_base, HANDLER:top_base})

 
  
; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
; widget to behave when called by either widget applications or normal
; programs (which block on tty reads).
xmanager, 'ae_spectra_viewer', top_base, EVENT_HANDLER='ae_spectra_viewer_event', $
          JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0)
return
end



;==========================================================================
;;; Dim source simulator.
;==========================================================================
PRO ae_dim_source_sim, obsdir, num_events


obs_stats = headfits(obsdir+'/obs.stats')

src_radius = sxpar(obs_stats, 'SRC_RAD')

bt=mrdfits(obsdir+'/source.evt', 1, theader)

dim = floor(sqrt(n_elements(bt)/num_events))
region_file = string(obsdir, dim, dim, F='(%"%s/grid%dX%d.reg")') 
event_file  = string(obsdir, dim, dim, F='(%"%s/grid%dX%d.evt")')

x0=median(bt.X)
y0=median(bt.Y)
xoffset= findgen(dim)*3*src_radius
yoffset= findgen(dim)*3*src_radius
make_2d,xoffset,yoffset
help, xoffset,yoffset
xoffset = reform(xoffset,n_elements(xoffset))
yoffset = reform(yoffset,n_elements(xoffset))
;info,xoffset
;info,yoffset

openw,  region2_unit, region_file, /GET_LUN
printf, region2_unit, "# Region file format: DS9 version 3.0"
!TEXTUNIT = region2_unit
forprint, TEXTOUT=5, x0+xoffset, y0+yoffset, replicate(src_radius, n_elements(xoffset)), F='(%"circle(%f,%f,%f)")', /NoCOMMENT
free_lun, region2_unit

num_rows = num_events*dim^2

xoffset = rebin(xoffset, num_rows, /SAMPLE)
yoffset = rebin(yoffset, num_rows, /SAMPLE)

grid = bt[0:num_rows-1]
grid.X = grid.X + xoffset
grid.Y = grid.Y + yoffset

fxaddpar, theader, 'TLMAX18', max(grid.x)+100
fxaddpar, theader, 'TLMAX19', max(grid.y)+100
mwrfits, [bt,grid], event_file, theader, /CREATE

spawn, string(event_file, region_file, F='(%"ds9 %s -region %s &")')
print, 'THETA=',sxpar(obs_stats,'THETA')
return
end



;==========================================================================
;;; Dim source simulator.
;==========================================================================
PRO ae_chart_interface, sourcename, obsname, SKIP_DOWNLOAD=skip_download, $
                        S_AIMPOINT=s_aimpoint, PIPELINE_RANDOMIZATION=pipeline_randomization

creator_string = "ae_chart_interface, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

; AE puts the 1.5 keV PSF first in the file.
psf_energy    = [1.49670, 0.2770, 4.510, 6.40, 8.60]

; These densities generate ~1E5 events (except at 8.6 keV).
chart_density = ( [1.19471,1.19085,2.36809,4.13947,15.7376] ) < 10
                     
src_stats_basename       = 'source.stats'
psf_basename             = 'source.psf'
obs_stats_basename       = 'obs.stats'
env_events_basename      = 'neighborhood.evt'
src_events_basename      = 'source.evt'


;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
param_dir            =         tempdir + 'param/'
help, tempdir

temp_events_fn       = tempdir + 'temp.evt'
temp_region_fn       = tempdir + 'temp.reg'
temp_image_fn        = tempdir + 'temp.img'
marx_events_filename = tempdir + 'marx.evt'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=param_dir

;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn get_sky_limits', 'punlearn dmcopy', 'pset dmcopy clobber=yes', 'punlearn dmcoords']

sourcedir       = sourcename + '/'  
obsdir          = sourcename + '/' + obsname + '/'
chartdir        = obsdir + 'chart'
file_mkdir, chartdir

src_stats_fn    = sourcedir + src_stats_basename
psf_fn          = obsdir +            psf_basename
chart_psf_fn    = obsdir + 'chart_' + psf_basename
stats_fn        = obsdir + obs_stats_basename
env_events_fn   = obsdir + env_events_basename
src_events_fn   = obsdir + src_events_basename

src_stats    = headfits(src_stats_fn, ERRMSG=error)
obs_stats    = headfits(stats_fn, ERRMSG=error)
event_header = headfits(src_events_fn, EXT=1)

;; =====================================================================
;; Calculate information needed to create the PSF.
ra  = sxpar(src_stats,   'RA')
dec = sxpar(src_stats,   'DEC')

;; ------------------------------------------------------------------------
;; Calculate HRMA coordinates from celestial coordinates.
cmd = string(src_events_fn, ra, dec, F="(%'dmcoords %s opt=cel celfmt=deg ra=%10.6f dec=%10.6f')")
run_command, cmd

run_command, /QUIET, 'pget dmcoords theta phi x y chip_id', result
off_angle    = float(result[0])  ; arcmin
phi          = float(result[1])
xpos_catalog = float(result[2])
ypos_catalog = float(result[3])
chip_id      = fix  (result[4])

help, off_angle, phi
energy_list = string(psf_energy,F='(%"%6.4f")')
print, 'Submit a single ChaRT run that includes multiple sources at the coordinates shown above using these energies and ray densities:  '
forprint, energy_list, chart_density  
for ii=0,n_elements(energy_list)-1 do file_copy, '/dev/null', energy_list[ii], /OVERWRITE


if NOT keyword_set(skip_download) then begin
  for ii=0,0 do begin

    tarball_filename = ''
    read, 'When the ChaRT run is completed, enter the URL to the ChaRT tarball: ', tarball_filename
    
    run_command, /UNIX, string(tempdir+'tarball', tarball_filename, F='(%"wget -O %s %s")')
    
    run_command, /UNIX, string(tempdir+'tarball', chartdir, F='(%"gtar -xzvf %s -C %s ")')
  endfor ;ii
endif  

ray_dirname = chartdir
; read, 'Enter name of directory containing ChaRT ray files: ', ray_dirname

ray_filename = file_search(ray_dirname, '*fits', COUNT=count)


;; =====================================================================
;; Process each ray file in the same order as listed in psf_energy.
temp = ray_filename
for ii=0,count-1 do begin
  ;; Figure out the energy of the rays
  cmd = string(temp[ii], F="(%'dmkeypar %s rt_kev echo+')")
  run_command, cmd, result
  
  dum = min( abs(psf_energy - float(result[0])), imin )
  ray_filename[ii] = temp[imin]
endfor ;ii

forprint, ray_filename, psf_energy

psf_header = headfits(psf_fn)

ae_make_psf, EVENT_FILE=env_events_fn, ASPECT_FN='/dev/null', ASPECT_BLUR=sxpar(psf_header, 'ASP_BLUR'), TEMP_DIR=tempdir, S_AIMPOINT=keyword_set(s_aimpoint), PIPELINE_RANDOMIZATION=keyword_set(pipeline_randomization)

;; Convert ChaRT rays into an AE PSF file.
;; Match pixel size and image dimensions of existing PSF. 
skypixel_per_psfpixel = sxpar(psf_header, 'CDELT1P')
ae_make_psf, chart_psf_fn, skypixel_per_psfpixel, sxpar(psf_header, 'NAXIS1')*skypixel_per_psfpixel, psf_energy, 0, ra, dec, $
             X_CAT=xpos_catalog, Y_CAT=ypos_catalog, OFF_ANGLE=off_angle, CHIP_ID=chip_id, EMAP_VAL=sxpar(obs_stats,'EMAP_MED'),$
             SAOSACFile=ray_filename

  
;; =====================================================================
;; Plot a radial profile for the first AE PSF and first ChaRT PSF.
;; Both must be normalized by the appropriate PSF total.
src_radius  = sxpar(obs_stats, 'SRC_RAD') * 2
src_radius  = 20
energy_range = [1.0,2.0]
ae_radial_profile, RA=ra, DEC=dec, EVENTS_FN=env_events_fn, SRC_RADIUS=src_radius, ENERGY_RANGE=energy_range, PSF_FN=chart_psf_fn,$
                   TEMPDIR=tempdir, /PLOT, WIDGET_IDS=plot_ids, PSF_NAME='ChaRT PSF', SOURCENAME=sourcename, $
                   ks_psf, R_MEDIAN, EE_AT_RM
ae_radial_profile, RA=ra, DEC=dec, EVENTS_FN=env_events_fn, SRC_RADIUS=src_radius, ENERGY_RANGE=energy_range, PSF_FN=psf_fn,$
                   TEMPDIR=tempdir, /PLOT, WIDGET_IDS=plot_ids, PSF_NAME='Template PSF', SOURCENAME=sourcename, $
                   ks_psf, R_MEDIAN, EE_AT_RM

;for ii=0,1 do begin
;  psf_img = readfits((ii EQ 0) ? chart_psf_fn : psf_fn, psf_header, /SILENT)
;  print, 'Plotting radial profile at ', sxpar(psf_header,'ENERGY'), ' keV'
;  
;  ; Make an array that has the distances (in units of sky pixels) from each PSF pixel to the source.
;  ; For speed, work with only pixels where the PSF is not zero.
;  skypixel_per_psfpixel = sxpar(psf_header, 'CDELT1P')
;  extast, psf_header, psf2wcs_astr
;  ad2xy, ra, dec, psf2wcs_astr, xind_catalog, yind_catalog
;  
;  dist_circle, psf_distance, size(psf_img, /DIM), xind_catalog, yind_catalog
;  psf_distance  = psf_distance * skypixel_per_psfpixel
;  
;  ; Sort by distance to be ready to form a 1-D model.
;  sort_ind      = sort(psf_distance)
;  psf_distance  = psf_distance [sort_ind]
;  psf_img       = psf_img      [sort_ind]
;
;  ; Form the cumulative distribution function: psf_distn(psf_distance).
;  ; This is the 1-D model of the composite PSF.
;  psf_distn = total(psf_img, /NAN, /DOUBLE, /CUMULATIVE) / sxpar(psf_header,'PSF_TOTL')
;  
;  if (ii EQ 0) then begin
;    chart_psf_distance = psf_distance
;    chart_psf_distn    = psf_distn
;  endif
;endfor
;
;function_1d, id2,       psf_distance,       psf_distn, DATASET='PSF Library', XTIT='distance [skypix]', YTIT='enclosed fraction'
;function_1d, id2, chart_psf_distance, chart_psf_distn, DATASET='ChaRT'


  return  

CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif
return
end ; ae_chart_interface




;==========================================================================
;;; Convert source labels to source names.
;==========================================================================
PRO ae_label2name, labels, collated_table, catalog_names, indexes

all_labels = strtrim(collated_table.LABEL,2)

num_labels = n_elements(labels)
indexes    = lonarr(num_labels)

for ii=0,num_labels-1 do begin
  ind = where(all_labels EQ strtrim(labels[ii],2), count)
  case count of
    0: print, 'WARNING: label '+labels[ii]+' not found'
    1: indexes[ii] = ind
    else: begin
          print, 'ERROR: duplicate label found in catalog'
          return
          end
  endcase
endfor
catalog_names = (collated_table.CATALOG_NAME)[indexes]
return
end



;==========================================================================
;; Routine to create a set of fake sources that "tile" a field of view, to
;; be used with ae_recon_detect.

;; NEIGHBORHOOD_SIZE is the same keyword option used in AE to specify the
;; width and height of the source neighborhood, in arcseconds.
;;
;; The result is a region file tiles.reg showing the tiles.  This can be 
;; hand edited in ds9 to reposition the tiles as desired.
;==========================================================================

PRO ae_neighborhood_tile, emap_filename, NEIGHBORHOOD_SIZE=neighborhood_size

if NOT keyword_set(neighborhood_size) then neighborhood_size = 50 ; arcsec

tile_radius_arcsec = neighborhood_size/2.0 
pitch_arcsec       = neighborhood_size - 10
help, tile_radius_arcsec, pitch_arcsec

emap = readfits(emap_filename, emap_header)
extast, emap_header, emap2wcs_astr
emap_xdim = (size(emap, /DIM))[0]
emap_ydim = (size(emap, /DIM))[1]
arcsec_per_imgpixel = emap2wcs_astr.CDELT[1] * 3600
pitch_imgpix       = round(pitch_arcsec       / arcsec_per_imgpixel)
tile_radius_imgpix = round(tile_radius_arcsec / arcsec_per_imgpixel)


openw,  region_unit, 'tiles.reg', /GET_LUN
printf, region_unit, "# Region file format: DS9 version 3.0"

num_tiles = 0L
for   ii=(tile_radius_imgpix-1), (emap_xdim-1), pitch_imgpix do begin
  for jj=(tile_radius_imgpix-1), (emap_ydim-1), pitch_imgpix do begin
    xy2ad, ii, jj, emap2wcs_astr, ra, dec
    if (emap[ii,jj] GT 0) then begin
      color = 'green'
      num_tiles++
      printf, region_unit, ra, dec, [2,2]*tile_radius_arcsec, color, F="(%'J2000;box %10.6f %10.6f %5.1f"" %5.1f"" # tag={fov} color={%s}')"
    endif
  endfor ;jj
endfor ;ii
free_lun, region_unit

print, num_tiles, ' tiles written to tiles.reg.'
return
end


PRO ae_rm_duplicate_tiles, region_file
      readcol, region_file, lines, F='(A)', DELIM='@'
      
      num_lines = n_elements(lines)
      good = replicate(1B,num_lines)

      result = stregex(lines,'box[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
      x = double(reform(result[1,*]))
      y = double(reform(result[2,*]))
      
      ; Look for non-zero duplicates.
      for ii=0,num_lines-1 do begin
        if (x[ii] EQ 0) then continue
        
        sqr_dist = (x-x[ii])^2 + (y-y[ii])^2
        ind = where(sqr_dist LT 1^2, count)
        if (count EQ 1) then continue
        
        ind=ind[1:*]
        x[ind]    = 0
        y[ind]    = 0
        good[ind] = 0
      endfor

      forprint, TEXTOUT='/tmp/temp.reg', SUBSET=where(good), lines, /NOCOMMENT
      print, 'wrote cleaned regions to /tmp/temp.reg'
      spawn, 'wc /tmp/temp.reg '+region_file
      
      
      lines = lines[where(good, count)] + string(1+indgen(count), F='(%" text={%d}")')
      forprint, TEXTOUT='/tmp/temp.reg', lines, /NOCOMMENT
return
end



;==========================================================================
;;; Find source candidates in AE reconstructions.

;;; Example:
;;;    .r acis_extract_tools
;;;   ae_recon_detect, 'all.srclist', 3, '../../iarray.source_search.evt',  '../../mosaic.emap' 
;;;
;;; The hdunumber parameter is a zero-based HDU number (passed to readfits.pro) that
;;; selects which image in neighborhood.img is searched.
;==========================================================================
PRO ae_recon_detect, catalog_or_srclist, hdunumber, match_events_fn, emap_filename, FLOOR_CNTS=floor_cnts

creator_string = "ae_recon_detect, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

; Choose a photometry aperture of 5x5 image pixels (whose size varies with theta).
cell_size     = 5
cell_halfsize = 1 + floor(cell_size/2)
help, cell_size


if (n_elements(floor_cnts) EQ 0) then floor_cnts=2.5
help, floor_cnts

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
param_dir            =         tempdir + 'param/'
help, tempdir

temp_events_fn       = tempdir + 'temp.evt'
temp_region_fn       = tempdir + 'temp.reg'
temp_image_fn        = tempdir + 'temp.img'
marx_events_filename = tempdir + 'marx.evt'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=param_dir

;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes']


env_image_basename       = 'neighborhood.img'
psf_basename             = 'source.psf'
 
arcsec_per_skypixel = 0.492 ; (0.492 arcsec/skypix)

resolve_routine, 'match_xy', /COMPILE_FULL_FILE

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from event data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

if keyword_set(emap_filename) then begin
  emap    = readfits(emap_filename, emap_hdr)
  extast, emap_hdr, emap_astr

  bkg_map = emap
  bkg_map[*]=0
endif

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

bkg_flux_a  = fltarr(num_sources)
src_area_a  = fltarr(num_sources)
cell_area_a = fltarr(num_sources)
theta_a     = fltarr(num_sources)

openw,  region1_unit, 'recon_fov.reg', /GET_LUN
printf, region1_unit, "# Region file format: DS9 version 3.0"

for ii = 0, num_sources-1 do begin
  sourcedir = sourcename[ii] + '/' 
  composite_img_fn = sourcedir + env_image_basename
  composite_psf_fn = sourcedir + psf_basename

  if NOT file_test(composite_img_fn) then begin
    print, env_image_basename, F='(%"\n-------\nError reading %s")'
    continue
  endif
  composite_img     = readfits(/SILENT, composite_img_fn, hood_hdr)
  maxlik_img        = readfits(/SILENT, composite_img_fn, maxlik_hdr, exten_no=hdunumber)
  stats             = headfits(/SILENT, sourcedir+'source.stats')
  composite_psf_hdr = headfits(/SILENT, composite_psf_fn)
  
  TILE_NUMBER = strtrim(sxpar(stats,'LABEL'),2)
  TILE_NAME   = 'recon_'+TILE_NUMBER
  print, sourcename[ii], TILE_NAME, F='(%"\n-------\nSource: %s (%s)")'
  print, sxpar(maxlik_hdr, 'ML_ITERS'), ' iterations used in reconstruction'
  
  extast, maxlik_hdr, maxlik_astr
  arcsec_per_maxlikpixel = maxlik_astr.CDELT[1] * 3600
  xdim = (size(maxlik_img, /DIM))[0]
  ydim = (size(maxlik_img, /DIM))[1]
  
  ; We'll use the kernel_footprint utility to look up 1-D indexes into the images of each photometry kernel
  ; we need to evaluate, taking array edges into account.
  if keyword_set(kf_id) && ptr_valid(kf_id) then kernel_footprint, kf_id, /DESTROY

  kernel = replicate(1,cell_size,cell_size)
  kernel_footprint, kf_id, /CREATE, IMAGE=maxlik_img, KERNELS=kernel

  ;; Create a graphic depicting the field of view of this reconstruction.
  printf, region1_unit, sxpar(stats,'RA'), sxpar(stats,'DEC'), [xdim,ydim]*arcsec_per_maxlikpixel, sourcename[ii], F="(%'J2000;box %10.6f %10.6f %4.1f"" %4.1f"" # tag={fov} text={%s}')"


  ;; ------------------------------------------------------------------------
  ;; Estimate a uniform background FLUX (counts/skypix^2/s/cm^2) in the neighborhood.
  
  ;; We coarsely rebin the image to push most of the PSF into ~1 pixel, but we
  ;; ensure that the rebinned image retains a reasonable number (100) of pixels.
  radius50 = sxpar(composite_psf_hdr, 'RADIUS50')
  if NOT finite(radius50) then begin
    print, 'Keyword RADIUS50 not found; using on-axis value of 0.43.'
    radius50 = 0.43
  endif

  num_across          = radius50 / (arcsec_per_maxlikpixel)
  
  for rebin_factor= (1 > ceil(num_across * 4.)), 2, -1 do begin
    proposed_xdim = floor((size(composite_img, /DIM))[0] / rebin_factor)
    proposed_ydim = floor((size(composite_img, /DIM))[1] / rebin_factor) 
    if (proposed_xdim*proposed_ydim GE 100) then break
  endfor
  
  print, rebin_factor*arcsec_per_maxlikpixel, F='(%"\nEstimating background in composite data image rebinned to %4.1f arcsec/pixel.")'
  
   ; We choose to crop the data image to avoid incomplete pixels in the rebinned image.
  cmd = string(composite_img_fn, proposed_xdim*rebin_factor,rebin_factor, proposed_ydim*rebin_factor,rebin_factor, temp_image_fn, F="(%'dmcopy ""%s[bin #1=1:%d:%d,#2=1:%d:%d]"" %s')")
  run_command, cmd      
  bigpixel_image = readfits(temp_image_fn, bigpixel_hdr, /SILENT)
  
  ; Resample emap onto bigpixel image.
  hastrom, emap, emap_hdr, bigpixel_emap, dum_hdr, bigpixel_hdr, MISSING=0 

  ; Estimate a bkg in units of cts/bigpix^2/s/cm^2
  estimate_poisson_background, bigpixel_image, EMAP=bigpixel_emap, bkg_per_bigpix, SIGNIFICANCE=0.99, /VERBOSE

  ; Convert to units of cts/skypix^2/s/cm^2.
  extast, bigpixel_hdr, bigpixel_astr 
  arcsec_per_bigpixel = bigpixel_astr.CDELT[1] * 3600
  bkg_flux = bkg_per_bigpix * (arcsec_per_skypixel / arcsec_per_bigpixel)^2

  if keyword_set(emap_filename) then begin
    ; Resample the background tile onto the bkg_map scene.
    bkg_tile = replicate(1, xdim, ydim)
    hastrom, bkg_tile, hood_hdr, emap_hdr, MISSING=0
    ind = where(bkg_tile GT 0, count)
    if (count GT 0) then bkg_map[ind] = bkg_flux
  endif
 
 
  ;; ------------------------------------------------------------------------
  ;; Estimate the size of AE's nominal extraction polygon in this tile.
  THETA     = sxpar(stats,'THETA')
  src_area  = sxpar(stats, 'SRC_AREA')
  if (src_area EQ 0) then src_area  = !PI * sxpar(stats, 'SRC_RAD')^2 
  if (src_area EQ 0) then begin
    print, 'ERROR:  SRC_AREA and SRC_RAD are zero.'
    retall
  endif
  
  cell_area = (cell_size*arcsec_per_maxlikpixel/arcsec_per_skypixel)^2
  theta_a    [ii] = THETA
  src_area_a [ii] = src_area
  cell_area_a[ii] = cell_area
  bkg_flux_a [ii] = bkg_flux
  
  print, 'bkg flux   = ', bkg_flux,       ' (events/skypixel^2/s/cm^2)'
  print, 'SRC_AREA   = ', src_area,       '        (skypixel^2)'   
  print, 'CELL AREA  = ', cell_area,      '        (skypixel^2)' 

  
  ;; ------------------------------------------------------------------------
  ; Resample emap onto maxlik image.
  hastrom, emap, emap_hdr, maxlik_emap, dum_hdr, maxlik_hdr, MISSING=0 

  


  ;; ------------------------------------------------------------------------
  ;; Search for sources.  Because we employ a mechanism to impose a minimum
  ;; separation between sources, we consider candidate source positions ordered
  ;; by decreasing FLUX.
  ;; For the sort that deterines the order of processing, it's important to 
  ;; compute SINGLE-pixel fluxes (maxlik_img/maxlik_emap) rather
  ;; than 5x5 cell fluxes because odd things can occur,
  ;; e.g. a very bright pixel in the recon might not participate in a centroid
  ;; because it fell in the 5x5 ring of a high-flux CELL that was mistakenly 
  ;; considered too early.
  Nentries = 0
  ;; Build a catalog structure suitable for match_xy.
  cat_entry = {source_recon, ID:0L, X:0.0, Y:0.0, X_ERR:0.0, Y_ERR:0.0, RA:0D, DEC:0D, TILE_NAME:'', TILE_NUMBER:0L, X_CELL:0, Y_CELL:0, CELL_COUNTS:0.0, SRC_CNTS:0L, BKG_EXPECTED_IN_APERTURE:0.0, BKG_EXPECTED_OUTSIDE_CELL:0.0, Pb:0.0, THETA:0.0, CATALOG_NAME:'reconstruction' }
  
  cat = replicate(cat_entry, 2000)
  
  flux_map = maxlik_img/maxlik_emap
  good_ind = where(finite(flux_map))
  sort_ind = good_ind[reverse(sort(flux_map[good_ind]))]
  
  accepted_island_map = bytarr(xdim,ydim)
  
  ;; ------------------------------------------------------------------------ 
  ;; Even though our photometry cell may be 5x5, our 
  ;; definition of a local maximum always involves a 3x3 island in the recon 
  ;; image, NOT in any sort of smoothed photometry image.
  ll_neighbor_ind = [0,1,2,3]
  ur_neighbor_ind = [5,6,7,8]
  central_ind     = 4
  
  for jj = 0L, n_elements(sort_ind)-1 do begin
    ;Find the X,Y position of the cell, in the 0-based image coordinate system.
    temp = array_indices(maxlik_img, sort_ind[jj])
    xindex = temp[0]
    yindex = temp[1]
    
    ; We will NOT test for a local maximum if the detection island falls off the edge of the image.
    if ((xindex LT 1) OR (xindex GT xdim-1-1) OR $
        (yindex LT 1) OR (yindex GT ydim-1-1)) then continue
  
    ; Extract 3x3 detection island and for a local maximum
    localmax_island = maxlik_img[xindex-1:xindex+1, yindex-1:yindex+1]

    local_max_found = (localmax_island[central_ind] GE max(localmax_island[ll_neighbor_ind])) AND $
                      (localmax_island[central_ind] GT max(localmax_island[ur_neighbor_ind]))

    if (NOT local_max_found) then continue
  
    ; Do not accept a 3x3 island that overlaps a previously accepted island.
    if (total(/INT, accepted_island_map[xindex-1:xindex+1, yindex-1:yindex+1]) GT 0) then begin
      print, 'Rejected a local max too close to an accepted local max.'
      continue
    endif

    ;; ------------------------------------------------------------------------
    ; Look up the exposure map value at this position.
    this_exposure = maxlik_emap[xindex, yindex]          
    
    ;; Estimate the background (counts) in AE's extraction aperture.
    ;; The aperture size (skypix^2) is estimated by src_area
    ;; The background (events/skypixel^2) is estimated by bkg_flux*this_exposure.
    bkg_per_skypix = bkg_flux * this_exposure
   
    bkg_expected_in_aperture  = 0 > (bkg_per_skypix * src_area)
    bkg_expected_outside_cell = 0 > (bkg_per_skypix * (src_area - cell_area))

    ;; ------------------------------------------------------------------------  
    ;; Perform photometry on a cell in the maxlik image at this location.
    
    ; We use the kernel_footprint utility to return the 1-D indexes of the cell, 
    ; with the central pixel listed first, dealing with array edges.
    kernel_footprint, kf_id, xindex, yindex, 0, pixel_ind
    maxlik_pixels = maxlik_img[pixel_ind]
    if (maxlik_pixels[0] NE maxlik_img[xindex, yindex]) then message, 'BUG in kernel_footprint utility!'
    
    ; For photometry we add up the central pixel (first in list), plus others in cell that:
    ; (a) Are less than central pixel.
    ; (b) Are not in accepted_island_map.
    good_flag     = (maxlik_pixels LT maxlik_pixels[0]) AND (accepted_island_map[pixel_ind] EQ 0)
    good_flag[0]  = 1

    recon_photometry = total(maxlik_pixels * good_flag)
    
    ; We impose an arbitrary lower limit on our photometry estimate to avoid very weak sources that would
    ; probably be rejected in our recipe later anyway.
    if (recon_photometry LT floor_cnts) then continue
    
    
    ;; ------------------------------------------------------------------------  
    ;; Estimate the SRC_CNTS (total counts in aperture) value that AE would find for a source at this location.
    
    ;; Presumably, reconstructions will push SOME of the background falling in the aperture into the source peak
    ;; that we're processing, and push SOME of the background in to random little peaks outside our cell.
    ;; Thus we expect that the recon photometry will fall between SRC_CNTS and NET_CNTS, but we don't know
    ;; in detail its relationship to SRC_CNTS
           
    ; This a simple law that assumes the recon photometry estimates SRC_CNTS.
    SRC_CNTS = round(recon_photometry)

    ; To get good agreement with detection by eye and with wavdetect, we find it necessary to put
    ; in an explicit ad hoc adjustment that boosts SRC_CNTS off-axis.
    ; On-axis we add nothing; at 10' off-axis we add 50% of bkg_expected_outside_cell.
    ;SRC_CNTS = round(counts_in_cell[xindex,yindex] + (bkg_expected_outside_cell * THETA*(0.5/10)))  

    ; This is a flat law, boosting by bkg_expected_outside_cell everywhere.
    ;SRC_CNTS = round(counts_in_cell[xindex,yindex] + bkg_expected_outside_cell) 

    
    ;; ------------------------------------------------------------------------  
    ;; Estimate AE's Pb statistic and compare to a threshold.
    ; We use a Pb threshold more liberal (larger than) that to be used for pruning the catalog.
    Pb_threshold   = 0.02	
    PROB_NO_SOURCE_poisson  = (1 - poisson_distribution(bkg_expected_in_aperture, SRC_CNTS - 1)) > 0

    if (PROB_NO_SOURCE_poisson GT Pb_threshold) then continue
      
    ; Estimate position as the centroid of some set of recon pixels,
    ; NOT as any computation done on the photometry (smoothed) image.
    ; We choose to centroid on the 3x3 island, even though our photometry cell is 5x5, to avoid
    ; severe perturbation of the centroid when the outer ring has a very bright pixel associated
    ; with another source.
    ; This is admittedly confusing: we let the outer ring help the source be significant but do
    ; not let it vote on the centroid.
    offset = [-1,0,1]
    
    make_2d, offset, offset, x_offset, y_offset
    centroid_island       = localmax_island
    
    cat_entry.X           = xindex + total(centroid_island*x_offset) / total(centroid_island)
    cat_entry.Y           = yindex + total(centroid_island*y_offset) / total(centroid_island)
    cat_entry.X_CELL      = xindex
    cat_entry.Y_CELL      = yindex
    cat_entry.CELL_COUNTS = recon_photometry
    cat_entry.SRC_CNTS    = SRC_CNTS
    cat_entry.bkg_expected_in_aperture  = bkg_expected_in_aperture
    cat_entry.bkg_expected_outside_cell = bkg_expected_outside_cell
    cat_entry.Pb          = PROB_NO_SOURCE_poisson
    cat_entry.THETA       = THETA
    
    cat[Nentries] = cat_entry
    Nentries = Nentries + 1
    accepted_island_map[xindex-1:xindex+1, yindex-1:yindex+1] = 1
  endfor ;jj
      
  if (Nentries EQ 0) then begin
    print, 'No sources detected.'
    continue
  endif
      
  cat     = cat[0:Nentries-1]
 
  ;Assign arbitrary position errors of 0.5", with a very weak dependence on distance from the 
  ;field center so that the merging process will favor sources closer to the PSF position.
  distance = sqrt( (cat.X-xdim/2.)^2 + (cat.Y-ydim/2.)^2 ) * arcsec_per_maxlikpixel ;arcseconds
  cat.X_ERR = (0.5 + distance/1E4) / arcsec_per_skypixel 
  cat.Y_ERR = cat.X_ERR
  
 ; Convert from image to celestial coordinates.
  xy2ad, cat.X, cat.Y, maxlik_astr, ra, dec
  cat.RA = ra
  cat.DEC=dec
  print
  print, '(celestial)              (centroid)   (cell,cell) counts_in_cell'
  forprint, ra, dec, cat.X, cat.Y, cat.x_cell, cat.y_cell, cat.cell_counts, F='(%"%10.6f %10.6f  %6.1f %6.1f  %3d %3d %7.1f")'

   ; Convert celestial to the ACIS tangent plane.
  ad2xy, ra, dec, event2wcs_astr, x, y
  ; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
  cat.X  = X+1
  cat.Y  = Y+1

  ; Create a region file in celestial coordinates.
  openw,  region2_unit, sourcedir+'recon.reg', /GET_LUN
  printf, region2_unit, "# Region file format: DS9 version 3.0"
  !TEXTUNIT = region2_unit
  forprint, TEXTOUT=5, /NoCOM, RA, DEC, F='(%"J2000;cross   point %10.6f %10.6f # tag={recon}")'
  free_lun, region2_unit

  
  cat.ID = 1+indgen(Nentries)
  cat.TILE_NAME   = TILE_NAME
  cat.TILE_NUMBER = TILE_NUMBER
  
; help, cat, /st
  print, Nentries, ' sources detected in '+TILE_NAME
  save, cat, bkg_flux, src_area, cell_area,FILE=sourcedir+'recon_detect.sav'
  
  ;; Merge this catalog with the previous ones.
  st = 0.99 ;significance threshold   
  if (n_elements(union_cat) EQ 0) then begin
    union_cat = cat 
  endif 
  union_regfile = 'union.reg'
  match_xy, /QUIET, match_struct, union_cat, 'union_cat', /INIT
  match_xy, /QUIET, match_struct,       cat, TILE_NAME, st, UNION_CAT=union_cat, UNION_REG=union_regfile
endfor ;ii
free_lun, region1_unit
                                                 
save, union_cat, theta_a, src_area_a, cell_area_a, bkg_flux_a, FILE='union.sav'

print, n_elements(union_cat), ' sources detected.'

if keyword_set(emap_filename) then begin
  writefits, 'bkg_map.img', bkg_map, emap_hdr
  spawn, 'ds9 bkg_map.img -region recon_fov.reg &'
endif

spawn, string(match_events_fn, union_regfile, F='(%"ds9 -log %s -region %s -region recon_fov.reg &")')
return
end


;==========================================================================

;;; Perform extractions within specific time ranges.
;;; time_filter should be a string array of CIAO time specifications, e.g. "tstart:tstop".
;;; extraction_name should be a string array of names for the extractions.
;;;
;==========================================================================
PRO ae_timerange_extract, sourcename, obsname, time_filter, extraction_name

resolve_routine, 'ae_recipe', /COMPILE_FULL_FILE

repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir

srclist_fn = tempdir + 'temp.cat'

src_stats_basename       = 'source.stats'
obs_stats_basename       = 'obs.stats'
num_sources = n_elements(time_filter)

;; Look up the existing extraction information.
    sourcedir = sourcename + '/' 
new_sourcedir = sourcename + '/' + extraction_name + '/'
    obsdir    = sourcename + '/' + obsname + '/' 
new_obsdir    = sourcename + '/' + obsname + '/' + extraction_name + '/'

src_stats_fn = sourcedir + src_stats_basename
obs_stats_fn = obsdir    + obs_stats_basename

obs_stats = headfits(obs_stats_fn)   

;; Make a srclist with multiple instances of the sourcename.
forprint, TEXTOUT=srclist_fn, replicate(sourcename,num_sources), /NoCOMMENT

;; Make sub-extraction directories; copy stats files; link extraction regions .
file_mkdir, new_sourcedir, new_obsdir

file_copy, /OVERWRITE, replicate(sourcedir + src_stats_basename,num_sources), new_sourcedir
file_copy, /OVERWRITE, replicate(obs_stats_fn,                  num_sources), new_obsdir

file_delete, /QUIET,                                new_obsdir + 'extract.reg'
file_link, replicate('../extract.reg',num_sources), new_obsdir + 'extract.reg'

  print, F='(%"\nae_timerange_extract: ============================================================")'  
  print,        'ae_timerange_extract: Extracting '+sourcename+' segments: ', extraction_name
  print,   F='(%"ae_timerange_extract: ============================================================\n")'  

ae_standard_extraction, obsname, SRCLIST=srclist_fn, EXTRACTION_NAME=extraction_name, TIME_FILTER=time_filter

; 
; In principle, we could run these for every obsid; the EXPOSURE would be zero for some obsids. 
; We'd have to check carefully whether that would be propagated correctely to the ARF/RMF weighting, and other things.
; For now we require the user to avoid null extractions.
; 
; Then we run a series of MERGE and FIT calls, one for each EXTRACTION_NAME value we have.
; Those results can be collated as is done in the pileup recipe.

acis_extract, srclist_fn, EXTRACTION_NAME=extraction_name, /MERGE

acis_extract, srclist_fn, EXTRACTION_NAME=extraction_name, /FIT_SPECTRA, CHANNEL_RANGE=[35,548], /CSTAT, MODEL_FILENAME='xspec_scripts/thermal/tbabs_vapec.xcm'


return
end


;==========================================================================
;;; Assign a supplied ds9 region expressed in celestial coordinates as the 
;;; extraction region for a source.
;;;
;;; If the optional parameter "obsname" is omitted, the supplied region becomes
;;; the extraction region for all observations of the source.
;;;
;;; ds9 is used to convert celestial coordinates in the supplied region to the
;;; sky coordinates required for extraction regions.
;==========================================================================

PRO ae_set_region, sourcename, celestial_region_fn, obsname

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
;; Some tools that use these temp filenames (e.g addarf and addrmf) are executed from a directory
;; other than cwd, and thus require that the path to tempdir is absolute, not relative.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        =         tempdir + 'param/'
run_command, /INIT, PARAM_DIR=param_dir

obs_stats_basename       = 'obs.stats'
src_region_basename      = 'extract.reg'
env_events_basename      = 'neighborhood.evt'

;; We can NOT search for "extract.reg" because /MERGE could make a file
;; sourcename/extraction_name/extract.reg which would be misinterpreted here as
;; an observation!  We must instead search for "obs.stats" which appears only in observation
;; directories.
pattern = sourcename + '/*/' + extraction_subdir + obs_stats_basename
obs_stats_fn = file_search( pattern, COUNT=num_obs )

if keyword_set(obsname) then begin
  num_obs_found = num_obs
  pattern = sourcename + '/' + obsname + '/' + extraction_subdir + obs_stats_basename
  obs_stats_fn = file_search( pattern, COUNT=num_obs )
  if (num_obs_found GT num_obs) then print, strjoin(obsname,' '), num_obs_found-num_obs, F='(%"WARNING!  Obsname %s was specified; ignoring %d other observations.\n")'
endif 

if (num_obs EQ 0) then begin
  print, 'No extractions found.'
  return
endif

obs_dir = strarr(num_obs)
for jj = 0, num_obs-1 do begin
  fdecomp, obs_stats_fn[jj], disk, dir
  obs_dir[jj] = dir
endfor

env_events_fn = obs_dir + env_events_basename
region_fn     = obs_dir + src_region_basename

print, 'Spawning ds9 to perform coordinate conversions ...'
ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name
for ii=0,num_obs-1 do begin
  ;; Load observation data into ds9.
  ae_send_to_ds9, my_ds9, env_events_fn[ii], celestial_region_fn

  ;; Load region file into ds9 and resave in PHYSICAL coordinates.
  cmd1 = string(my_ds9,                          F='(%"xpaset -p %s regions system physical")')
  cmd2 = string(my_ds9, region_fn[ii],           F='(%"xpaset -p %s regions save %s")')
  run_command, [cmd1,cmd2], /QUIET
endfor

return
end


;==========================================================================
;;; Make some useful plots from the collated AE data products.
;==========================================================================

PRO ae_summarize_catalog, collatefile 

COMMON ae_summarize_catalog, id0, id1, id2, id3, id4, id5, id6, id7, id8

  if ~keyword_set(collatefile) then collatefile = 'all.collated'

  arcsec_per_skypixel  = 0.492 

  bt=mrdfits(collatefile, 1)    
  band_full        = 0  
  BACKGRND         = bt.BACKGRND/1E-9
  SRC_CNTS         = bt.SRC_CNTS[band_full]
  NET_CNTS         = bt.NET_CNTS[band_full]
  flux2            = bt.FLUX2   [band_full]
  PSF_FRAC         = bt.PSF_FRAC
  off_angle        = bt.THETA
  radius50         = (0.85 -0.25*off_angle + 0.10*off_angle^2)  * arcsec_per_skypixel  ; arcseconds
  distance_src2src = bt.DISTANCE_SRC2SRC                        * arcsec_per_skypixel  ; arcseconds
  crowding         = distance_src2src / radius50
  
  dataset_1d, id0, SRC_CNTS        , XTIT='# of extracted counts, 0.5-8 keV'
  dataset_1d, id1, NET_CNTS
  dataset_1d, id2, alog10(flux2)   , XTIT='log flux2 (photon/cm^2/s)'
  dataset_1d, id3, PSF_FRAC        , XTIT='PSF Fraction @1.5keV'
  dataset_1d, id4, distance_src2src, XTIT='distance (arcsec) to nearest neighbor' 
  dataset_1d, id5, crowding        , XTIT='crowding metric (distance/R50) to nearest neighbor' 
  dataset_2d, id6, off_angle, distance_src2src, PSYM=1, XTIT='off-axis angle (arcmin)', YTIT='distance (arcsec) to nearest neighbor'
  dataset_2d, id7, PSF_FRAC, BACKGRND, PSYM=1, XTIT='PSF Fraction @1.5keV', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
  dataset_1d, id8, BACKGRND,  XTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
return
end


;==========================================================================
;;; Perform a very simple scan of source lightcurves for cosmic ray afterglows.
;;; Outputs are sourcename, interval_hist, src_cnts
;==========================================================================
PRO ae_afterglow_scan, catalog_or_srclist, $
      sourcename, interval_hist, src_cnts

src_events_basename      = 'source.evt'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

interval_hist = intarr(11,num_sources)
src_cnts      = lonarr(   num_sources)

for ii = 0, num_sources-1 do begin
  sourcedir   = sourcename[ii] + '/' 
  merged_src_events_fn = sourcedir + src_events_basename
  
  ; Read the composite extracted event list.
  bt = mrdfits(merged_src_events_fn, 1, src_events_hdr, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + merged_src_events_fn
  
  ; Sort events by time.
  bt = bt[sort(bt.TIME)]

  ; Sort timestamps and compute intervals between events in units of exposures.
  time = bt.TIME
  interval = round( (time[1:*] - time) / sxpar(src_events_hdr, 'TIMEDEL') )
  
  ; Compute histogram of intervals.
  interval_hist[*,ii] = histogram(interval, MIN=0, MAX=10)
  src_cnts     [  ii] = n_elements(time)

endfor ;ii
return
end


;==========================================================================
;;; Perform a very simple scan of source lightcurves for cosmic ray afterglows.

;;;
;;; As of May 2008 the CXC's tool acis_run_hotpix misses short-duration afterglow incidents.
;;; The sort of scan on extracted data we're doing here is not as good as one done on raw data, but it's useful.

;;; Outputs are sourcename, suspect_fraction
;;; Example
;;;  
; idl
; .r acis_extract_tools.pro  
; ae_afterglow_report,'all.srclist',max_frame_separation, sourcename, suspect_fraction 
; forprint, /NOCOM, TEXTOUT='agr_step3.srclist', sourcename, SUBSET=reverse(sort(suspect_fraction)) 
; exit        
; idl | & tee agr_step3.log
; .r acis_extract_tools.pro  
; ae_afterglow_report,'agr_step3.srclist',max_frame_separation, sourcename, suspect_fraction

;==========================================================================
PRO ae_afterglow_report, catalog_or_srclist, $                            ; input parameters
      MAX_FRAME_SEPARATION=max_frame_separation, MAX_OFFSET=max_offset, $ ; optional inputs
      sourcename, suspect_fraction, num_inband_suspects                   ; output parameters

if (n_elements(max_frame_separation) EQ 0) then max_frame_separation=3
if (n_elements(max_offset)           EQ 0) then max_offset          =1

energy_range = [500,8000]


src_events_basename      = 'source.evt'
src_stats_basename       = 'source.stats'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

suspect_fraction    = fltarr(num_sources)
num_inband_suspects = lonarr(num_sources)

print, max_frame_separation, max_offset, F='(%"Scanning for pairs of events separated by upto %d exposures and offset by upto %d CCD pixels  ...")'
print, 'Energy Range used for afterglow fraction is ', energy_range

for ii = 0, num_sources-1 do begin
  sourcedir   = sourcename[ii] + '/' 
  merged_src_events_fn = sourcedir + src_events_basename
                           
  ; Read the composite extracted event list.
  bt = mrdfits(merged_src_events_fn, 1, src_events_hdr, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + merged_src_events_fn
                 
  frame_length =       sxpar(src_events_hdr, 'TIMEDEL') 

  ; Sort events by time.
  bt = bt[sort(bt.TIME)]
  
  ; compute intervals between events in units of exposures.
  chipx = bt.CHIPX    
  chipy = bt.CHIPY
  energy= bt.ENERGY
  time  = bt.TIME
  time -= time[0] 
  
  frame  = round(time / frame_length)
  dframe = frame [1:*] - frame
  dx     = chipx [1:*] - chipx
  dy     = chipy [1:*] - chipy
  de     = energy[1:*] - energy
  energy_next = energy[1:*]
                              
  bad_ind = where((dframe LE max_frame_separation) AND (abs(dx) LE max_offset) AND (abs(dy) LE max_offset), count)

  ; Count how many events are suspect.
  num_events            = n_elements(bt)
  is_suspect            = bytarr(num_events)
  if (count GT 0) then begin
    is_suspect[bad_ind  ] = 1
    is_suspect[bad_ind+1] = 1
  endif
  is_inband             = (energy_range[0] LE energy) AND (energy LE energy_range[1])
  
  num_inband_counts       = total(/INT, is_inband)
  num_inband_suspects[ii] = total(/INT, is_inband AND is_suspect)
  suspect_fraction   [ii] = float(num_inband_suspects[ii])/num_inband_counts

  
  ; Write afterglow statistics to source.stats file.
  stats_fn  = sourcedir + src_stats_basename
  stats = headfits(stats_fn, ERRMSG=error)
  
  if keyword_set(error) then begin
    print, 'WARNING! Could not read '+stats_fn
    continue
  endif
    
  fxaddpar, stats, 'AG_CNTS', num_inband_suspects[ii], 'suspected afterglows, in-band, all observations'
  writefits, stats_fn, 0, stats
  
  
  if (num_inband_suspects[ii] EQ 0) then continue
  
  ; Compute the expected number of time intervals less than the threshold,
  ; assuming constant Poisson arrival, using the exponential distribution.
  ;exposure           = float(sxpar(src_events_hdr, 'EXPOSURE'))
  ;lambda             = num_inband_counts / exposure                   ; Poisson mean, counts/second
  ;interval_threshold = (max_frame_separation + 1) * frame_length  ; seconds
  ;frac_expected = 1 - exp(-lambda * interval_threshold)        ; integral of exponential distn, 0:threshold
  
  print, sourcename[ii], num_inband_suspects[ii], num_inband_counts, 100*suspect_fraction[ii], F='(%"\n%s  We suspect %d of %d in-band events (%0.1f%%) are afterglows.")'
  
  if (num_inband_suspects[ii] GT 20) then begin
    print, 'More than 20 suspect events; skipped printing!'      
    continue
  endif
  
  print, 'dfrm    dx    dy        energy    frame MOD 1000'
  forprint, SUBSET=bad_ind, dframe, dx,dy, energy[0:num_events-2],energy_next, frame[0:num_events-2] MOD 1000, F='(%"%4d %5d %5d    %5d->%5d        %3d")'
  
endfor ;ii

; Return info on only the sources with suspect events, sorted by suspect_fraction.
ind = reverse(sort(suspect_fraction))
num_suspect = total(/INT, suspect_fraction GT 0)

sourcename       = sourcename      [ind[0:num_suspect-1]]
suspect_fraction = suspect_fraction[ind[0:num_suspect-1]]

return
end


;==========================================================================


;;; CCD_LIST is a scalar or vector string specifying the CCDs that should be 
;;; included in the emap, e.g. ['012367','0123'].  
;;; If not supplied then DETNAM is used to define the CCD list.

;;; scene_name is a scalar or vector string (same number of elements as CCD_LIST)
;;; specifying the basename of the exposure map(s) to be created.

;;; Aspect histograms and instrument maps will be written to a subdirectory "asphist/".

;==========================================================================
PRO ae_make_emap, obsdata_filename, scene_name, CCD_LIST=ccd_list, ARDLIB_FILENAME=ardlib_fn, $
                  ASPECT_FN=aspect_fn, PBKFILE=pbk_fn, MASKFILE=mask_fn, $
                  MONOENERGY=monoenergy, SPECTRUM_FN=spectrum_fn, ONLYTIME=onlytime, CACHE=cache

creator_string = "ae_make_emap, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

if ~keyword_set(ardlib_fn)   then ardlib_fn  = 'ardlib.par'
if ~keyword_set(aspect_fn)   then aspect_fn  = 'acis.astrometric.asol1'
if ~keyword_set(pbk_fn)      then pbk_fn     = 'acis.pbk0'
if ~keyword_set(mask_fn)     then mask_fn    = 'acis.msk1'
if ~keyword_set(monoenergy)  then monoenergy = 1.0
if ~keyword_set(spectrum_fn) then spectrum_fn= 'NONE'


;; ------------------------------------------------------------------------
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        =         tempdir + 'param/'

run_command, /INIT, PARAM_DIR=param_dir


;; ------------------------------------------------------------------------
;; Deal with ardlib.par.

;; Copy observer-supplied ardlib.par to private PFILES directory.
if NOT keyword_set(ardlib_fn) then begin
  print, 'ERROR: you must supply the path to your observation-specific ardlib.par file via ARDLIB_FN.'
  GOTO, FAILURE
endif

if NOT file_test(ardlib_fn) then begin
  print, ardlib_fn, F='(%"ERROR: ARDLIB_FN %s not found.")'
  GOTO, FAILURE
endif

file_copy, ardlib_fn, param_dir + 'ardlib.par'

;; Remind the user about what's in ardlib.
run_command, 'paccess ardlib r', ardlib_path, /QUIET
print, ardlib_path, F='(%"\n \nExamining important parameters in %s")'

run_command, 'pdump ardlib', ardlib_contents, /QUIET
print, F='(%"\nThe bad pixel list parameters below should correspond to your observation:")'
forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_ACIS?_BADPIX*'))]

print, 'PAUSING FOR 10 SECONDS SO YOU CAN REVIEW ARDLIB INFORMATION ABOVE:'
if  keyword_set(cache)       then print, F='(%"\nWARNING! Using previously computed aspect histograms if available.\n")'
wait, 10


;; ------------------------------------------------------------------------
; Determine which CCDs need to be processed.
bt = mrdfits(obsdata_filename, 1, obsdata_header, /SILENT, STATUS=status)
if (status NE 0) then begin
  print, 'ERROR reading ' + obsdata_filename
  GOTO, FAILURE
endif

if n_elements(ccd_list) EQ 0 then begin
  ; Look up what CCDs are present in observation.
  detnam   = strtrim(sxpar(obsdata_header, 'DETNAM'),2)
  ccd_list = strmid(detnam,5)
endif

num_scenes = n_elements(scene_name)

case num_scenes of
  0: begin
     print, 'ERROR: parameter scene_name must be supplied.'
     GOTO, FAILURE
     end
     
  n_elements(ccd_list): 
     
  else: $
     begin
     print, 'ERROR: lengths of scene_name and CCD_LIST must match.'
     GOTO, FAILURE
     end
endcase 

; Figure out the list of CCDs used in any of the scenes.
flag = bytarr(10)
for jj=0,num_scenes-1 do $
  for ii=0,strlen(ccd_list[jj])-1 do flag[fix(strmid(ccd_list[jj],ii,1))] = 1
active_ccds = where(flag, num_active_ccds)
print, 'Active CCDs are: ', active_ccds


;; ------------------------------------------------------------------------
;; Create aspect histograms and instmaps for all active CCDs.
asphist_dir = 'asphist'
file_mkdir, asphist_dir

run_command, ['punlearn asphist','punlearn mkinstmap','punlearn mkexpmap','punlearn dmregrid','punlearn dmhedit']

base       =  asphist_dir + string(indgen(10), F='(%"/ccd%d.")')
asphist_fn = base + 'asphist'
instmap_fn = base + 'instmap'
emap_fn    = base + 'emap'

for ii=0,num_active_ccds-1 do begin
  ccd_id = active_ccds[ii]
  
  ;; ------------------------------------------------------------------------
  ;; Create an aspect histogram file.
  if ~keyword_set(cache) || ~file_test(asphist_fn[ccd_id]) then begin
    run_command, string(aspect_fn, obsdata_filename, ccd_id, asphist_fn[ccd_id], $
                        F="(%'asphist infile=%s evtfile=""%s[ccd_id=%d]"" outfile=%s dtffile="""" clob+')")  
  endif

  bt = mrdfits(asphist_fn[ccd_id], 1, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + asphist_fn[ccd_id]

  duration = total(bt.duration)
  
  keyname  = string(ccd_id, F='(%"EXPOSUR%d")')
  exposure = sxpar(obsdata_header, keyname)
  
  if (abs(duration-exposure)/duration GT 0.01) then begin
    print, 'WARNING !!!!!'
    print, duration, asphist_fn[ccd_id], keyname, exposure, $
           F='(%"WARNING: Sum of DURATION column (%d) in %s does not match \n\rvalue of keyword %s (%d)!")'
    print, 'It is likely that your aspect histogram was not computed correctly!!!!!'
    wait, 1
  endif

  ;; ------------------------------------------------------------------------
  ;; Create instrument map.
  if keyword_set(onlytime) then begin 
    run_command, string(asphist_fn[ccd_id], pbk_fn, instmap_fn[ccd_id], ccd_id, mask_fn, $
                        F="(%'mkinstmap obsfile=""%s[asphist]"" pbkfile=%s outfile=%s detsubsys=""ACIS-%d;IDEAL"" mirror=""HRMA;AREA=1"" pixelgrid=""1:1024:#1024,1:1024:#1024"" spectrum_fn=NONE            monoenergy=1 maskfile=%s grating=NONE verbose=0 clob+')")  
  endif else begin
    run_command, string(asphist_fn[ccd_id], pbk_fn, instmap_fn[ccd_id], ccd_id, spectrum_fn, mask_fn, $
                        F="(%'mkinstmap obsfile=""%s[asphist]"" pbkfile=%s outfile=%s detsubsys=""ACIS-%d"" pixelgrid=""1:1024:#1024,1:1024:#1024"" spectrumfile=%s monoenergy=1 maskfile=%s grating=NONE verbose=0 clob+')")  
  endelse  
endfor ;ii


;; ------------------------------------------------------------------------
;; Create single-CCD and merged exposure maps for each scene.
for jj = 0, num_scenes-1 do begin
  ; Figure out the list of CCDs used in THIS scene.
  flag = bytarr(10)
  for ii=0,strlen(ccd_list[jj])-1 do flag[fix(strmid(ccd_list[jj],ii,1))] = 1
  active_ccds = where(flag, num_active_ccds)

  ;; ------------------------------------------------------------------------
  ;; Determine span of these CCDs on the sky.
  ;; We want to pad this by about 20 pixels on all sides, to make sure that nothing gets cut off.
  run_command, string(obsdata_filename, strcompress(strjoin(active_ccds,','),/REMOVE_ALL), $
                      F="(%'dmstat ""%s[ccd_id=%s][cols x]"" median=no sigma=no >/dev/null')")  
 
  run_command, /QUIET, 'pget dmstat out_min out_max', result
  xmin = floor(float(result[0])) - 20
  xmax = ceil (float(result[1])) + 20

  run_command, string(obsdata_filename, strcompress(strjoin(active_ccds,','),/REMOVE_ALL), $
                      F="(%'dmstat ""%s[ccd_id=%s][cols y]"" median=no sigma=no >/dev/null')")  
 
  run_command, /QUIET, 'pget dmstat out_min out_max', result
  ymin = floor(float(result[0])) - 20
  ymax = ceil (float(result[1])) + 20

  num_x_pixels = xmax - xmin
  num_y_pixels = ymax - ymin
      
  
  for ii=0,num_active_ccds-1 do begin
    ccd_id = active_ccds[ii]
    
    ;; ------------------------------------------------------------------------
    ;; Create exposure map.
    ;; Use of normalize=no produces a map with units of s*cm^2.
    run_command, string(instmap_fn[ccd_id], emap_fn[ccd_id], asphist_fn[ccd_id], $
                        xmin, xmax, num_x_pixels, ymin, ymax, num_y_pixels, $
                        F="(%'mkexpmap instmapfile=%s outfile=%s asphistfile=%s xygrid=""%d:%d:#%d,%d:%d:#%d""  normalize=no useavgaspect=no verbose=0 clob+')")
  endfor ;ii

  ;; ------------------------------------------------------------------------
  ;; Now combine the single-obsid exposure maps..
  outfile = scene_name[jj]+'.emap'
  
  run_command, string(strjoin(emap_fn[active_ccds],","), outfile, num_x_pixels, num_y_pixels,  $
                      F="(%'dmregrid ""%s"" %s rotang=0 npts=1 bin=""1:%d:1,1:%d:1"" xoffset=0 yoffset=0 rotxcenter=0 rotycenter=0 clob+')")                   

  ;; Report the pixel size of the emap.
  emap_header = headfits(outfile)
  
  print, outfile, sxpar(emap_header,'CDELT1P'), sxpar(emap_header,'CDELT2P'), F="(%'\nThe pixels in exposure map %s are %0.5f X %0.5f sky pixels.')"
endfor ;jj

return


CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif

if (exit_code EQ 0) then return $
else begin
  print, 'ae_make_emap: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end

