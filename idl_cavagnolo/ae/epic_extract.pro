;;; $Id: epic_extract.pro,v 1.1 2008-10-15 19:37:32 cavagnolo Exp $
;;; EPIC Extract. 
;;; Patrick Broos, Penn State University, 2008

@acis_extract

PRO epic_extract, catalog_or_srclist, obsname, obsdata_filename_p, $
                  CONSTRUCT_REGIONS=construct_regions, PSF_FRACTION=psf_fraction, MASK_FRACTION=mask_fraction, $
                  EXTRACT_SPECTRA=extract_spectra


creator_string = "epic_extract, version"+strmid("$Date: 2008-10-15 19:37:32 $", 6, 11)
print, F='(%"\nEPIC Extract: ============================================================")'  
print, creator_string
print, systime()
print, 'http://www.astro.psu.edu/xray/docs/TARA/ae_users_guide.html'
print, 'Join the mail list to receive announcements: https://phoenix.astro.psu.edu/mailman/listinfo/acis-extract'
print, 'Contact: patb@astro.psu.edu'
print, F='(%"EPIC Extract: ============================================================\n")'  
exit_code = 0

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

run_command, /INIT, /SAS, PARAM_DIR=param_dir

temp_events_fn   = tempdir + 'temp.evt'
temp_arf_fn      = tempdir + 'temp.arf'
temp_rmf_fn      = tempdir + 'temp.rmf'
temp_image_fn    = tempdir + 'temp.img'
temp_text_fn     = tempdir + 'temp.txt'
temp_cat_fn      = tempdir + 'temp.cat'
trash_fn         = tempdir + 'trash'

;; We assume that access to /tmp/ will often be faster than access to the event and emap data passed,
;; so let's start by copying those files to a cache.
cache_dir = tempdir + 'cache/'
file_mkdir, cache_dir

if keyword_set(obsdata_filename_p) then begin
  fdecomp, obsdata_filename_p, disk, item_path, item_name, item_qual
  obsdata_filename = cache_dir+ item_name+'.'+item_qual
  file_copy, obsdata_filename_p, obsdata_filename, /VERB
endif

if keyword_set(emap_filename_p) then begin
  fdecomp, emap_filename_p, disk, item_path, item_name, item_qual
  emap_filename = cache_dir+ item_name+'.'+item_qual
  file_copy, emap_filename_p, emap_filename, /VERB
endif


;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


if NOT keyword_set(psf_fraction)    then psf_fraction   =0.70
if NOT keyword_set(mask_fraction)   then mask_fraction  =0.90
if NOT keyword_set(mask_multiplier) then mask_multiplier=1.1
if (n_elements(verbose) EQ 0)       then verbose=1

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,10000)


if NOT keyword_set(energy_range) then energy_range=[0.5,8.0]

if NOT (keyword_set(eband_lo) AND keyword_set(eband_hi)) then begin
  eband_lo = [0.5,  0.5, 2.0,  0.5, 1.7, 2.8,  0.5, 1.5, 2.5,  0.5, 1.0, 2.0, 4.0, 6.0,  0.5, 2.0]
  eband_hi = [8.0,  2.0, 8.0,  1.7, 2.8, 8.0,  1.5, 2.5, 8.0,  1.0, 2.0, 4.0, 6.0, 8.0,  7.0, 7.0]
endif

if NOT keyword_set(neighborhood_size) then neighborhood_size = 50 ; arcsec

if (n_elements(theta_range) NE 2) then theta_range = [0,100.]

type = size(obsname,/TNAME)
dim  = size(obsname,/DIMEN)
case dim of
  0   : fail = 0
  1   : fail = (type NE 'STRING')
  else: fail = (type NE 'STRING') || ~keyword_set(merge_observations)
endcase
if fail then begin
  print, 'ERROR: parameter "obsname" must be a scalar string'
  GOTO, FAILURE
endif

 
nominal_psf_energy = 1.49670

region_colors = ['red','green','cyan','magenta','yellow','blue']


src_stats_basename       = 'source.stats'
src_photometry_basename  = 'source.photometry'
src_image_basename       = 'source.img'
obs_stats_basename       = 'obs.stats'
obs_frac_basename        = 'obs.psffrac'
env_events_basename      = 'neighborhood.evt'
env_image_basename       = 'neighborhood.img'
src_region_basename      = 'extract.reg'
bkg_region_basename      = 'background.reg'
bkg_pixels_region_basename = 'background_pixels.reg'
bkg_emap_basename        = 'background.emap'
src_emap_basename        = 'source.emap'
evt_region_basename      = 'evt.reg'
src_events_basename      = 'source.evt'
bkg_events_basename      = 'background.evt'
src_spectrum_basename    = 'source.pi'
bkg_spectrum_basename    = 'background.pi'
bkg_arf_basename         = 'background.arf'
rmf_basename             = 'source.rmf'
arf_basename             = 'source.arf'
psf_basename             = 'source.psf'
lc_binned_basename       = 'source.binned_lc'
lc_smooth_basename       = 'source.lc'
fit_stats_basename       = 'source.spectra'
modelsubdir              = 'spectral_models/'
event_plot_basename      = 'source.evt.ps'

catalog_src_reg_fn   = tempdir + 'catalog_'+src_region_basename+'.fits'
catalog_bkg_reg_fn   = tempdir + 'catalog_'+bkg_region_basename+'.fits'

temp_src_reg_fn      = tempdir + src_region_basename+'.fits'
temp_bkg_reg_fn      = tempdir + bkg_region_basename+'.fits'

temp_src_spectrum_fn = tempdir + src_spectrum_basename
temp_bkg_spectrum_fn = tempdir + bkg_spectrum_basename

;; Input catalog should be an ascii file with source names. 
readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  GOTO, FAILURE
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'


;; =============================================================================
if keyword_set(construct_regions) then begin
;; =============================================================================
  ;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
  run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes']

  ;; ------------------------------------------------------------------------
  ;; Build a FITS catalog acceptable to the SAS tool "region".
  ;; The ID_INST column seems to be necessary, probably to get PSF fractions.
  ;; These instrument codes come from the emldetect manual.
  case obsname of
    'PN'  : ID_INST = 1
    'MOS1': ID_INST = 2
    'MOS2': ID_INST = 3
    else: begin
          print, 'ERROR: obsname must be MOS1 or MOS2 or PN'
          retall
          end
  endcase
  
  ; Get source coordinates by collating existing source directories.
  acis_extract, catalog_or_srclist, COLLATED_FILENAME='temp.collated', LABEL_FILENAME='label.txt', /SINGLE_OBS, VERBOSE=0

  bt = mrdfits('temp.collated',1)
  num_sources = n_elements(bt)
  
  ; Look for duplicate source names.
  ; ...
  
  ; Build FITS catalog. 
  ; The SAS tools we use later need to know what energy band was used for the detection, probably
  ; to relate region sizes to PSF fractions.  We're just going to fake a 1-2 keV band in the catalog header.
  row = {RA: 0D, DEC: 0D, ID_INST:0, ID_BAND:1, ID_SRC:0}
  cat = replicate(row, num_sources)
  cat.RA      = bt.RA
  cat.DEC     = bt.DEC
  cat.ID_INST = ID_INST
  cat.ID_SRC  = fix(bt.LABEL)
  fxaddpar, src_theader, 'CREATOR',  creator_string
  fxaddpar, src_theader, 'EXTNAME',  'SRCLIST'
                                                                                    
  fxhmake,  p_header, /INITIALIZE, /EXTEND, /DATE
  fxaddpar, p_header, 'CREATOR',  creator_string
  fxaddpar, p_header, 'PN_1_ELO', 1000, 'E_min for Instr PN , band 1'
  fxaddpar, p_header, 'M1_1_ELO', 1000, 'E_min for Instr M1 , band 1'
  fxaddpar, p_header, 'M2_1_ELO', 1000, 'E_min for Instr M2 , band 1'
  fxaddpar, p_header, 'PN_1_EHI', 2000, 'E_max for Instr PN , band 1'
  fxaddpar, p_header, 'M1_1_EHI', 2000, 'E_max for Instr M1 , band 1'
  fxaddpar, p_header, 'M2_1_EHI', 2000, 'E_max for Instr M2 , band 1'
    
  writefits, temp_cat_fn, 0, p_header
  mwrfits, cat, temp_cat_fn, src_theader
  
  ;; ------------------------------------------------------------------------
  ;; Use "region" tool to build source extraction regions, stored one per HDU in a single file.
  
  cmd = string( obsdata_filename, temp_cat_fn, psf_fraction, catalog_src_reg_fn, trash_fn, $
               F="(%'region  eventset=%s:EVENTS  srclisttab=%s:SRCLIST  operationstyle=batch  srcidcol=ID_SRC  shrinkconfused=yes  radiusstyle=enfrac  energyfraction=%4.2f  fovbkgannulus=no  outunit=xy  nobkgellipse=yes  regionset=%s  bkgregionset=%s')")
  run_command, cmd, /SAS
  
  ;; ------------------------------------------------------------------------
  ;; Use "region" tool again to build background regions with sources masked.

  cmd = string( obsdata_filename, temp_cat_fn, mask_fraction, trash_fn, catalog_bkg_reg_fn, $
               F="(%'region  eventset=%s:EVENTS  srclisttab=%s:SRCLIST  operationstyle=batch  srcidcol=ID_SRC  shrinkconfused=yes  radiusstyle=enfrac  energyfraction=%4.2f  fovbkgannulus=no  outunit=xy  nobkgellipse=yes  regionset=%s  bkgregionset=%s')")
  run_command, cmd, /SAS
  
  
  ;; ------------------------------------------------------------------------
  ;; Process each source.
  for ii = 0, num_sources-1 do begin
    print, F='(%"\n===================================================================")'
    
    ;; Construct directory & filenames for composite source.
    sourcedir = sourcename[ii] + '/' + extraction_subdir[ii]
    stats_fn  = sourcedir + src_stats_basename
    file_mkdir, sourcedir   
   
    primary_stats_fn = sourcename[ii] + '/' + src_stats_basename
    if file_test(primary_stats_fn) then begin
       ; An existing set of source properties should be available.  
      stats = headfits(primary_stats_fn, ERRMSG=error)
      if keyword_set(error) then begin
        ; We try a few times because file access can collide with another AE process that's writing the file.
        wait, 0.5
        stats = headfits(primary_stats_fn, ERRMSG=error)
        if keyword_set(error) then begin
          wait, 2
          stats = headfits(primary_stats_fn, ERRMSG=error)
          if keyword_set(error) then message, error
        endif
     endif
    endif else begin
      print, 'ERROR: source.stats file is missing!'
      goto, FAILURE
    endelse
    
    ; We first clean out AE keywords from other stages to avoid confusion.
    ; The alternate approach, retaining only what we need, would prevent the observers
    ; from storing useful keywords in source.stats.
    sxdelpar, stats, ['NUM_OBS', 'EMAP_TOT', 'FRACEXPO', 'WARNFRAC', 'OBSNAME', 'PRIM_OBS', 'SRC_CNTS', 'THETA', 'SRC_RAD', 'PSF_FRAC', 'MSK_RAD', 'RA_DATA', 'DEC_DATA', 'QUANTML', 'RA_ML', 'DEC_ML', 'QUANTCOR', 'RA_CORR', 'DEC_CORR', 'BACKGRND', 'SCAL_MAX', 'SCAL_MIN', 'KS_SPECT', 'EXPOSURE', 'EFFAREA', 'PROB_KS', 'ERR_DATA', 'KS_PSF' ]
   
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'OBJECT', sourcename[ii], 'source name'

    writefits, stats_fn, 0, stats
    
    ;; Construct filenames for specific obsid.
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn  = obsdir + obs_stats_basename
    src_region_fn = obsdir + src_region_basename
    bkg_region_fn = obsdir + bkg_region_basename
    
    file_mkdir, obsdir
    
    ;; ------------------------------------------------------------------------
    ;; Save this source's regions into individual files.
    cmd1 = string(catalog_src_reg_fn, 2+ii, src_region_fn+'.fits', F="(%'dmcopy ""%s[%d]"" %s')")
    cmd2 = string(catalog_bkg_reg_fn, 2+ii, bkg_region_fn+'.fits', F="(%'dmcopy ""%s[%d]"" %s')")
    run_command, [cmd1,cmd2]  
    
    ; SAS tools often don't like special characters like "+" in pathnames, so we have to 
    ; copy files to a local directory.
    file_copy, /OVERWRITE, src_region_fn+'.fits', temp_src_reg_fn
    file_copy, /OVERWRITE, bkg_region_fn+'.fits', temp_bkg_reg_fn
    cmd1 = string(temp_src_reg_fn, src_region_fn, F="(%'cxctods9 table=%s regtype=linear > %s')")
    cmd2 = string(temp_bkg_reg_fn, bkg_region_fn, F="(%'cxctods9 table=%s regtype=linear > %s')")
    run_command, [cmd1,cmd2], /SAS  
    
     
    
    ;; ------------------------------------------------------------------------
    ;; Save observation information.
    fxhmake,  stats, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'OBSNAME',  obsname, 'observation identifier'
;    fxaddpar, stats, 'EMAP_AVG', emap_val_i, 'exposure map value nearest source'
;    fxaddpar, stats, 'THETA',    off_angle, 'off-axis angle (arcmin)'
;    fxaddpar, stats, 'PSF2CAT',  psf2cat_offset_i,  'PSF to catalog offset (sky pixels)'
;    fxaddpar, stats, 'FRACSPEC', target_psf_fraction[ii], 'target PSF fraction'
;    fxaddpar, stats, 'PSF_FRAC', psf_fraction_i, string(fiducial_psf_energy[ii], F='(%"PSF fraction via polyfillv @%6.4f keV")')
;    fxaddpar, stats, 'PSF_ENGY', fiducial_psf_energy[ii], 'fiducial PSF energy (keV)'
;    fxaddpar, stats, 'MSK_RAD',  mask_radius,  'mask radius (sky pixels)'
;    fxaddpar, stats, 'SRC_RAD',  (src_radius_min+src_radius_max)/2., 'source radius (sky pixels)'
;    fxaddpar, stats, 'PGN_AREA', src_area_i, 'src polygon area, via polyfillv (sky pixels^2)'
;    fxaddpar, stats, 'REG_EDIT', 'F', 'T=region edited in ds9'
;    fxaddpar, stats, 'CELLFRAC', cell_frac, 'PSF fraction in 3x3 skypix cell'
;    fxaddpar, stats, 'X_CAT',    xpos_catalog, 'source position (sky coordinates), from catalog'
;    fxaddpar, stats, 'Y_CAT',    ypos_catalog, 'source position (sky coordinates), from catalog'
;    fxaddpar, stats, 'X_PSF',    xpos_psf, 'PSF position (sky coordinates), centroid of image'
;    fxaddpar, stats, 'Y_PSF',    ypos_psf, 'PSF position (sky coordinates), centroid of image'
    writefits, stats_fn, 0, stats
  endfor ; ii
  
  ;; ------------------------------------------------------------------------
  GOTO, CLEANUP
endif ;keyword_set(construct_regions)



;; =============================================================================
if keyword_set(extract_spectra) then begin
;; =============================================================================

  for ii = 0, num_sources-1 do begin
    print, F='(%"\n===================================================================")'
    print, 'Source: ', sourcename[ii]

    ;; Construct filenames.
    obsdir      = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    
    stats_fn    = obsdir + obs_stats_basename
    env_events_fn   = obsdir + env_events_basename
    src_region_fn   = obsdir + src_region_basename
    bkg_region_fn   = obsdir + bkg_region_basename
    src_events_fn   = obsdir + src_events_basename
    src_spectrum_fn = obsdir + src_spectrum_basename
    bkg_spectrum_fn = obsdir + bkg_spectrum_basename
    rmf_fn          = obsdir + rmf_basename
    arf_fn          = obsdir + arf_basename
    event_plot_fn   = obsdir + event_plot_basename
    
    if (NOT file_test(stats_fn)) then begin
      print, 'EXTRACTION SKIPPED: source not observed.'
      continue
    endif

    ;; ------------------------------------------------------------------------
    ;; Extract the src and bkg total-band event lists.
    cmd = string(obsdata_filename, bkg_region_basename+'.fits', bkg_events_basename, $
                 F="(%'evselect table=%s:EVENTS expression=""region(%s)"" filteredset=%s')")
    run_command, cmd, /SAS, DIR=obsdir

    cmd = string(obsdata_filename, src_region_basename+'.fits', src_events_basename, $
                 F="(%'evselect table=%s:EVENTS expression=""region(%s)"" filteredset=%s')")
    run_command, cmd, /SAS, DIR=obsdir

    wideband_events = mrdfits(src_events_fn, 1, src_events_hdr, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + src_events_fn

    wideband_src_counts = sxpar(src_events_hdr, 'NAXIS2')
    if (wideband_src_counts EQ 0) then begin
      ; Since we're not using exposure maps, the only way we can guess that a source position is
      ; not in this observation is by seeing zero events extracted.
      print, 'EXTRACTION SKIPPED: source not observed.'
      file_delete, [stats_fn, src_events_fn, src_spectrum_fn, bkg_spectrum_fn, rmf_fn, arf_fn], /QUIET
      continue
    endif
    
    ;; ------------------------------------------------------------------------
    ;; Extract src and bkg spectra
    spectralbinsize = 15 ; eV
    specchannelmax  = 15000
    case obsname of
      'PN'  : default_filter = '(PATTERN<= 4)&&(FLAG==0)'   ; PN
      'MOS1': default_filter = '(PATTERN<=12)&&(#XMMEA_EM)' ; MOS
      'MOS2': default_filter = '(PATTERN<=12)&&(#XMMEA_EM)' ; MOS
      else: begin
            print, 'ERROR: obsname must be MOS1 or MOS2 or PN'
            retall
            end
    endcase
    
    fmt="(%'evselect table=%s:EVENTS withspectrumset=yes spectrumset=%s withspecranges=yes spectralbinsize=%d specchannelmin=0 specchannelmax=%d expression=""region(%s)&&%s"" updateexposure=yes filterexposure=yes energycolumn=PI ')"

    cmd = string(obsdata_filename, src_spectrum_basename, spectralbinsize, specchannelmax, src_region_basename+'.fits', default_filter, F=fmt)
 
    run_command, cmd, /SAS, DIR=obsdir

    cmd = string(obsdata_filename, bkg_spectrum_basename, spectralbinsize, specchannelmax, bkg_region_basename+'.fits', default_filter, F=fmt)
 
    run_command, cmd, /SAS, DIR=obsdir

    ;; ------------------------------------------------------------------------
    ;; Scale src and bkg spectra and create src ARF.
    
    fmt='(%"arfgen spectrumset=%s setbackscale=yes withbadpixcorr=yes badpixlocation=%s arfset=%s keeparfset=%s detmaptype=psf psfenergy=2 ")'
    
    cmd = string(src_spectrum_basename, obsdata_filename, arf_basename, 'yes', F=fmt)
 
    run_command, cmd, /SAS, DIR=obsdir

    cmd = string(bkg_spectrum_basename, obsdata_filename, trash_fn    , 'no' , F=fmt)
 
    run_command, cmd, /SAS, DIR=obsdir

    ;; ------------------------------------------------------------------------
    ;; Create RMF file.

    fmt='(%"rmfgen spectrumset=%s rmfset=%s detmaptype=psf ")'            

    cmd = string(src_spectrum_basename, rmf_basename, F=fmt)
 
    run_command, cmd, /SAS, DIR=obsdir
    
    
    ;; ------------------------------------------------------------------------
    ;; Insert into the spectral files keyword pointers to the various files.
    openw, unit, temp_text_fn, /GET_LUN
    printf, unit, bkg_spectrum_basename, rmf_basename, arf_basename, creator_string, $
                  F='(%"#add\nBACKFILE = %s / background spectrum\nRESPFILE = %s / RMF name\nANCRFILE = %s /ARF name\nCREATOR = %s")'
    free_lun, unit
    
    cmd = string(src_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd;, /QUIET
    
    
    openw, unit, temp_text_fn, /GET_LUN
    printf, unit,  rmf_basename, arf_basename, creator_string, $
                  F='(%"#add\nBACKFILE = none / background spectrum\nRESPFILE = %s / RMF name\nANCRFILE = %s /ARF name\nCREATOR = %s")'
    free_lun, unit
    
    cmd = string(bkg_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd;, /QUIET
    
    

; We do not simply call especget (as below) because it will use different CHANNEL definitions for the MOS and PN cases, 
; eliminating our ability the MERGE the MOS and PN extractions. 

;    cmd = string(obsdata_filename, src_region_basename+'.fits', bkg_region_basename+'.fits', src_spectrum_basename, bkg_spectrum_basename, arf_basename, rmf_basename, F="(%'especget table=%s:EVENTS srcexp=""region(%s)"" backexp=""region(%s)"" srcspecset=%s bckspecset=%s srcarfset=%s srcrmfset=%s')")
   
  endfor ; ii
  GOTO, CLEANUP
endif  ;keyword_set(extract_spectra)




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
  print, 'acis_extract: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP

END  ; END of acis_extract

