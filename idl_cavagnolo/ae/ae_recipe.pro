;;; $Id: ae_recipe.pro,v 1.2 2008-10-15 19:37:03 cavagnolo Exp $
;;; Patrick Broos, 2004

;;; Programs to automate the AE recipe found in the Getting Started section of 
;;; the AE manual and in recipe.txt.

;!! These scripts assume a very particular set of conventions for paths to 
;!! the various files that AE uses, as shown in the Getting Started section of 
;!! the AE manual and in recipe.txt.

@acis_extract

;#############################################################################
;;; Source List Manager tool

;;; See AE manual for usage.

;;; The master source list is maintained in the file "all.srclist".

;;; Comment lines begin with ";".

;;; We retain entries for REMOVED sources rather than excising them from the list 
;;; to keep sequence numbers stable.  A comment character (;) is used to hide them 
;;; from AE, and a tag "(REMOVED)" is added to distinguish them from regular comments.


PRO ae_source_manager, RA=ra, DEC=dec, POSITION_TYPE=posntype, $
                       NAME=sourcename, LABEL=label, $
                       
                       ADD=add, PROVENANCE=provenance, $

                       REMOVE=remove, TRASH_DIR=trash_dir, $
                       
                       MOVE=move, NO_RENAME=no_rename, $ 
                       
                       UPDATE_POSITIONS_CORR=update_positions_corr, $
                       UPDATE_POSITIONS_DATA=update_positions_data, $
                       
                       SORT_RA=sort_ra, SORT_BULLSEYE=sort_bullseye, $
                       SET_LABEL_AS_SEQUENCE=set_label_as_sequence, $
                       SKIP_BACKUP=skip_backup

src_stats_basename       = 'source.stats'
srclist_fn               = 'all.srclist'
precision  = 1
creator_string = "ae_source_manager, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)


;; For /ADD and /MOVE, the required RA & DEC inputs must be the same length as the
;; optional inputs NAME, LABEL, & PROVENANCE.
if keyword_set(add) OR keyword_set(move) then begin
  num_sources = n_elements(ra)
  if (num_sources EQ 0) then begin
    print, 'ERROR: RA and DEC inputs must be supplied.'
    retall
  endif
  
  if (n_elements(dec) NE num_sources) then begin
    print, 'ERROR: RA and DEC inputs must be same length.'
    retall
  endif
  
  if (size(ra,/TYPE) NE 5) || (size(dec,/TYPE) NE 5) then begin
    print, 'ERROR: RA and DEC inputs must be double precision floats.'
    retall
  endif
  
  case (n_elements(provenance)) of
    num_sources:
    1          : provenance = replicate(provenance, num_sources)
    0          : provenance = replicate('unknown', num_sources)
    else       : begin
                 print, 'ERROR: PROVENANCE input must be same length as RA/DEC.'
                 retall
                 end
  endcase

  case (n_elements(posntype)) of
    num_sources:
    1          : posntype = replicate(posntype, num_sources)
    0          : begin
                 print, 'ERROR: type of source position must be supplied via POSITION_TYPE.'
                 retall
                 end
    else       : begin
                 print, 'ERROR: POSITION_TYPE input must be same length as RA/DEC.'
                 retall
                 end
  endcase

  if keyword_set(sourcename) then begin
    if (n_elements(sourcename) NE num_sources) then begin
      print, 'ERROR: NAME input must be same length as RA/DEC.'
      retall
    endif
  endif 
  
  if keyword_set(label) then begin
    if (n_elements(label) NE num_sources) then begin
      print, 'ERROR: LABEL input must be same length as RA/DEC.'
      retall
    endif
    
    if (total(strmatch(strtrim(label,2),'')) GT 0) then begin
      print, 'ERROR: LABEL input must not be the empty string.'
      retall
    endif
  endif 
  
endif ; /ADD or /MOVE


;; Read any existing source list.  
;; We cannot use readcol because it won't read comment lines with spaces.
if file_test(srclist_fn) then begin
  Nlines = numlines(srclist_fn) 
  if (Nlines GT 0) then begin
    master_srclist = strarr(Nlines)
    openr, unit, srclist_fn, /GET_LUN
    readf, unit, master_srclist
    free_lun, unit
  
    if NOT strmatch(master_srclist[0], ";*") then $
      master_srclist = ['; Master Source List',master_srclist] 
  endif ;(Nlines GT 0)
endif else begin
  if keyword_set(move) OR keyword_set(remove) then print, 'WARNING: file '+srclist_fn+' not found.'
endelse

;; We need master_srclist to have at least one comment line to make the code easier later, so we'll add a comment line if necessary.
if (n_elements(master_srclist) EQ 0) then master_srclist =  '; Master Source List'
  

;; For /MOVE and /REMOVE, existing sources must be identified via NAME or LABEL.
if (keyword_set(move) OR keyword_set(remove)) AND NOT keyword_set(sourcename) then begin
  if NOT keyword_set(label) then begin
    print, 'ERROR: you must identify sources via NAME or LABEL inputs.'
    retall
  endif
  
  ; Look up NAME values using supplied LABEL values.
  print, 'Looking up sources by their LABELs ...'
  label = strtrim(label,2)  
  sourcename  = strarr(n_elements(label))
  for ii = 0, n_elements(master_srclist)-1 do begin
    this_name = master_srclist[ii]
    if strmatch(this_name, ";*") then continue
    
    sourcedir = this_name + '/' 
    
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      this_label  = strtrim(sxpar(stats, 'LABEL', COUNT=count),2)
      if (count GT 0) then begin
        ind = (where(label EQ this_label, count))[0]
        if (count GT 0) then begin
          if (sourcename[ind] NE '') then begin
            print, 'ERROR: Two sources have the same label '+this_label
            retall
          endif
          
          sourcename[ind] = this_name
        endif ; match to this_label found
      endif ; source has LABEL keyword
    endif else print, 'WARNING! Could not read '+stats_fn
  endfor ;ii

  ind = where(sourcename EQ '', count)
  if (count GT 0) then begin
    print, 'ERROR: could not find these LABELs: '
    forprint, label, SUBSET=ind
    retall
  endif

  forprint, label, sourcename, F='(%"LABEL %s == %s")'
endif ; LABEL supplied instead of NAME


;; ADD sources.
if keyword_set(add) then begin
  if NOT keyword_set(sourcename) then begin
    sourcename = strcompress(/REMOVE_ALL, adstring(ra,dec,precision,/TRUNCATE))
  endif
  
  for ii=0,num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' 
    stats_fn  = sourcedir + src_stats_basename
                       
    ; Use existing src_stats file if possible.
    if file_test(stats_fn) then begin
      print, 'WARNING: modifying existing source '+sourcename[ii]
      stats = headfits(stats_fn, ERRMSG=error)
      if keyword_set(error) then message, 'ERROR reading '+stats_fn
    endif else begin
      print, 'Adding source '+sourcename[ii]
      file_mkdir, sourcedir
      fxhmake, stats, /INITIALIZE, /EXTEND, /DATE
      fxaddpar, stats, 'LABEL', 'unlabeled'
    endelse
 
    ; Check whether this source is in the master list.
    dum = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      ; Append the source to the master list and assign a default label as the 1-based 
      ; sequence number in the list, counting removed sources.
      master_srclist = [master_srclist, sourcename[ii]]
      comment_flag = strmatch(master_srclist, ";*")
      removed_flag = strmatch(master_srclist, "*(REMOVED)*")
      seq_num = round(n_elements(master_srclist)-total(comment_flag)+total(removed_flag)) 
      fxaddpar, stats, 'LABEL', strtrim(string(seq_num),2), 'sequence number'
    endif
    
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'RA',               ra[ii], 'source position', F='(F10.6)'
    fxaddpar, stats, 'DEC',             dec[ii], 'source position', F='(F10.6)'
    fxaddpar, stats, 'POSNTYPE',   posntype[ii], 'type of source position'
    fxaddpar, stats, 'PROVENAN', provenance[ii], 'source provenance'

    ; Record any observer-supplied LABEL, overwriting the default label above.
    if keyword_set(label) then fxaddpar, stats, 'LABEL', label[ii]
    
    print, 'Source LABEL is "'+strtrim(sxpar(stats, 'LABEL'),2)+'"'
    
    writefits, stats_fn, 0, stats
  endfor ;ii

  GOTO, WRITE_SRCLIST
endif ; /ADD


;; /MOVE
if keyword_set(move) then begin
  if NOT keyword_set(sourcename) then begin
    print, 'ERROR: existing source name must be supplied via NAME.'
    retall
  endif

  if keyword_set(no_rename) then begin
    sourcename_new = sourcename
  endif else begin
    sourcename_new = strcompress(/REMOVE_ALL, adstring(ra,dec,precision,/TRUNCATE))
  endelse
  
  for ii=0,num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' 
    stats_fn  = sourcedir + src_stats_basename

    ; Before modifying any files, determine if the source will be renamed and if
    ; there are any obstacles to doing so.
    name_is_changing = (sourcename[ii] NE sourcename_new[ii])
    if name_is_changing AND file_test(sourcename_new[ii]) then begin
      print, 'ERROR: revised source name duplicates existing source '+sourcename_new[ii]
      retall
    endif
    
    ; Read existing src_stats file.
    if file_test(stats_fn) then begin
      stats = headfits(stats_fn, ERRMSG=error)
      if keyword_set(error) then message, 'ERROR reading '+stats_fn
    endif else begin
      print, 'ERROR: source directory '+sourcename[ii]+' not found'
      continue
    endelse
 
    ; Compute how far source moved.
    hours_per_degree = 24D/360
    gcirc, 1, sxpar(stats,'RA')*hours_per_degree, sxpar(stats,'DEC'), ra[ii]*hours_per_degree, dec[ii], distance
    msg = string( sourcename[ii], distance, F='(%"%s moved %5.2f arcsec")' )

    ; Check whether this source is in the master list.
    ind = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      print, 'WARNING: source '+sourcename[ii]+' not found in '+srclist_fn
      ; Append the source to the master list and assign a default label as the 1-based 
      ; sequence number in the list, counting removed sources.
      master_srclist = [master_srclist, sourcename[ii]]
      comment_flag = strmatch(master_srclist, ";*")
      removed_flag = strmatch(master_srclist, "*(REMOVED)*")
      seq_num = round(n_elements(master_srclist)-total(comment_flag)+total(removed_flag)) 
      fxaddpar, stats, 'LABEL', string(seq_num), 'sequence number'
    endif else begin
      ; Change the name of this source.
      master_srclist[ind] = sourcename_new[ii]
    endelse
    
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'RA',               ra[ii], 'source position', F='(F10.6)'
    fxaddpar, stats, 'DEC',             dec[ii], 'source position', F='(F10.6)'
    fxaddpar, stats, 'POSNTYPE',   posntype[ii], 'type of source position'
    fxaddpar, stats, 'PREVNAME', sourcename[ii]
    
    ; Record any observer-supplied LABEL.
    if keyword_set(label) then fxaddpar, stats, 'LABEL', label[ii]
    
    print, 'Source LABEL is '+sxpar(stats, 'LABEL')

    ; Remove obsolete information and resave.
    if name_is_changing then sxdelpar, stats, ['OBJECT']

    writefits, stats_fn, 0, stats
    
    ; Rename the source directory.
    if name_is_changing then begin
      print, msg + ' (renamed to '+sourcename_new[ii]+')'
      
      ; Remove files that use the old source name.
      filelist = file_search(sourcedir, sourcename[ii]+'*', COUNT=count)
      if (count GT 0) then file_delete, filelist, /VERBOSE
      
      file_move, sourcedir, sourcename_new[ii]
    endif else print, msg
  endfor ;ii

  GOTO, WRITE_SRCLIST
endif ; /MOVE


;; REMOVE sources
if keyword_set(remove) then begin
  num_sources = n_elements(sourcename)
  
  if NOT keyword_set(trash_dir) then trash_dir = 'trash'

  file_mkdir, trash_dir
  for ii = 0, num_sources-1 do begin
    ; We retain the master sourcelist entry rather than removing it to keep sequence numbers stable.
    ; A comment character (;) is used to hide it from AE, and a tag "(REMOVED)" is added to distinguish it
    ; from regular comments.
    ind = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      print, 'WARNING: source '+sourcename[ii]+' not found in '+srclist_fn
    endif else master_srclist[ind] = ';'+master_srclist[ind]+' (REMOVED)'
    
    print, 'Moving '+sourcename[ii]+'/ to '+trash_dir+' ...'
    file_move, sourcename[ii], trash_dir
  endfor ; ii
  
  GOTO, WRITE_SRCLIST
endif ; /REMOVE


;; SORT source list
if keyword_set(sort_bullseye) or keyword_set(sort_ra) then begin
  ; Split the source list into comments and sources.
  comment_flag = strmatch(master_srclist, ";*")
  comment_ind = where(comment_flag, COMPLEMENT=src_ind, NCOMPLEMENT=num_entries)
  comment_list = master_srclist[comment_ind]

  if (num_entries GT 0) then begin
    ; Remove the comment lines.
    master_srclist = master_srclist[src_ind]
    
    ; Read the RA/DEC values.
    print, 'Looking up source coordinates ...'
    ra  = replicate(!VALUES.D_NAN,num_entries)
    dec = replicate(!VALUES.D_NAN,num_entries)
    for ii=0, num_entries-1 do begin
      sourcedir = master_srclist[ii] + '/' 
      
      stats_fn  = sourcedir + src_stats_basename
      stats = headfits(stats_fn, ERRMSG=error)
      
      if (NOT keyword_set(error)) then begin
        ra[ii]  = sxpar(stats, 'RA')
        dec[ii] = sxpar(stats, 'DEC')
      endif else print, 'WARNING! Could not read '+stats_fn
    endfor ;ii
    
    if keyword_set(sort_ra) then begin
      sort_ind = sort(ra)
    endif else begin
      openw,  region_unit, 'bullseye.reg', /GET_LUN
      printf, region_unit, "# Region file format: DS9 version 3.0"
 
      sort_ind = lonarr(num_entries)
      ; Compute distances and position angles from the median position.
      ra_ref  = median(ra)
      dec_ref = median(dec)
      hours_per_degree = 24D/360
      gcirc,  1, ra_ref*hours_per_degree, dec_ref, ra*hours_per_degree, dec, distance
      posang, 1, ra_ref*hours_per_degree, dec_ref, ra*hours_per_degree, dec, angle
      
      ; Sort by distances.
      distance_sort_ind = sort(distance)
      
      ; Define annuli containing 200 sources.  Within each, sort by angle.
      group_size  = 199
      group_start = 0L
      repeat begin
        group_end = (group_start + group_size-1) < (num_entries-1)
        group_size= 200
        
        printf, region_unit, ra_ref, dec_ref, distance[distance_sort_ind[group_end]], F='(%"J2000;circle %10.6f %10.6f %d\" # tag={bullseye}")' 
        
        group_ind = distance_sort_ind[group_start:group_end]
        
        sorted_group_ind = group_ind[sort(angle[group_ind])]
        
        sort_ind[group_start] = sorted_group_ind
        
        group_start = group_end+1
      endrep until (group_start GE num_entries)
      free_lun, region_unit
      
;      forprint, distance, angle, SUBSET=sort_ind
;      plot, ra, dec, /NODATA
;      for ii=0,(num_entries/10)-1 do begin
;        s=ii*10
;        e=s+9
;        oplot, ra[s:e], dec[s:e], PSYM=1
;        wait,0.5 
;      endfor
    endelse
  endif ; (num_entries GT 0)
  
  master_srclist = [comment_list, master_srclist[sort_ind]]

  GOTO, WRITE_SRCLIST
endif ; /SORT


;; SET_LABEL_AS_SEQUENCE
if keyword_set(set_label_as_sequence) then begin
  ; Assign LABEL values using 1-based sequence numbers.
  print, 'Assigning LABELs using 1-based sequence numbers ...'
  seq_num = 1
  for ii = 0, n_elements(master_srclist)-1 do begin
    this_name = master_srclist[ii]
    if strmatch(this_name, ";*") then continue
    
    sourcedir = this_name + '/' 
    
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      old_label  = strtrim(sxpar(stats, 'LABEL', COUNT=count),2)
      new_label  = strtrim(string(seq_num),2)
      seq_num    = seq_num + 1
      fxaddpar, stats, 'LABEL', new_label, 'sequence number'
      
      msg = this_name+' labeled '+new_label
      if (count GT 0) then begin
        msg = msg+'  (was '+old_label+')'
        fxaddpar, stats, 'PREVLABL', old_label
      endif
      print, msg
    
      writefits, stats_fn, 0, stats
    endif else print, 'WARNING! Could not read '+stats_fn
  endfor ;ii
  return
endif ; /SET_LABEL_AS_SEQUENCE

 
if (keyword_set(update_positions_corr) OR keyword_set(update_positions_data)) then begin
  if NOT keyword_set(sourcename) then begin
    print, 'ERROR: source name must be supplied via NAME.'
    retall
  endif
  
 
  ;; Replace RA & DEC keywords with RA_CORR,DEC_CORR or RA_DATA,DEC_DATA.
  if keyword_set(update_positions_corr) then begin
    ra_kywd    = 'RA_CORR'
    dec_kywd   = 'DEC_CORR'
    posntype_new = 'AE correlation'
  endif else begin
    ra_kywd    = 'RA_DATA'
    dec_kywd   = 'DEC_DATA'
    posntype_new = 'AE mean data'
  endelse
  
  if keyword_set(posntype) then begin
    print, 'ERROR: do not supply POSITION_TYPE; ae_source_manager will assign the value ', posntype_new
    retall
  endif
  
  not_first_recursion = 0
  num_sources = n_elements(sourcename)
  for ii = 0, num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' 
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra_new  = sxpar(stats,  ra_kywd, COUNT=count1)
      dec_new = sxpar(stats, dec_kywd, COUNT=count2)
      
      if (count1 EQ 1) AND (count2 EQ 1) AND finite(ra_new) AND finite(dec_new) then begin
        ae_source_manager, /MOVE, NO_RENAME=keyword_set(no_rename), NAME=sourcename[ii], RA=ra_new, DEC=dec_new, POSITION_TYPE=posntype_new, SKIP_BACKUP=not_first_recursion
        not_first_recursion = 1
      endif else begin
        print, 'WARNING!  Keywords '+ra_kywd+','+dec_kywd+' not defined for '+sourcename[ii]
      endelse
    endif else print, 'ERROR! Could not read '+stats_fn
  endfor
  return
endif ; /UPDATE_POSITIONS_*


WRITE_SRCLIST:
  if NOT keyword_set(skip_backup) then begin
    ;; Maintain several backups of the source list.
    for ii=4,1,-1 do begin
      backup_fn       = string(srclist_fn,ii,  F='(%"%s-%d")')
      older_backup_fn = string(srclist_fn,ii+1,F='(%"%s-%d")')
      if file_test(backup_fn) then file_move, backup_fn, older_backup_fn, /OVERWRITE
    endfor
    
    if file_test(srclist_fn) then file_move, srclist_fn, backup_fn, /OVERWRITE
  endif
  
  ;; Save the modified source list.
  forprint, TEXTOUT=srclist_fn, master_srclist, /NoCOMMENT, /SILENT
  comment_flag = strmatch(master_srclist, ";*")
  removed_flag = strmatch(master_srclist, "*(REMOVED)*")
  print, srclist_fn, n_elements(master_srclist)-total(comment_flag), total(removed_flag), F='(%"Master source list %s contains %d active and %d removed sources.")'
  return
end ; ae_source_manager



;#############################################################################
;;; For a SINGLE obsid construct a catalog that leads to non-overlapping regions.
;;;
;;; Several input files are located using standard naming conventions as shown 
;;; in the Getting Started section of the AE manual.
;;;
;;; The default input event list is ../obsXXXX/spectral.evt; use EVTFILE_BASENAME
;;; to supply the name of a different event list in ../obsXXXX/.

PRO ae_make_catalog, obsname, $
    SRCLIST_FILENAME=srclist_fn, EVTFILE_BASENAME=evtfile_basename, $
    NOMINAL_PSF_FRAC=nominal_psf_frac_kywd, RESTART=restart, _EXTRA=extra

if NOT keyword_set(evtfile_basename) then evtfile_basename = 'spectral.evt'
if NOT keyword_set(srclist_fn)       then srclist_fn     = 'all.srclist'

creator_string = "ae_make_catalog, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

psf_energ         = 1.4967

;; In this program all variables, except the keyword NOMINAL_PSF_FRAC, represent
;; PSF fractions as integer percentages.
nominal_psf_frac  = 90
if keyword_set(nominal_psf_frac_kywd) then nominal_psf_frac= 99 < round(100*nominal_psf_frac_kywd) > 10
min_psf_frac      = 40
initial_down_step =-32
initial_up_step   =  4
minimum_step      =  2
step_ratio        = 2.0

obsdir   = '../obs' + obsname + '/'
catfile  = obsdir + 'all.cat'

evtfile     = obsdir + evtfile_basename
emapfile    = obsdir + 'obs.emap'
aspect_fn   = obsdir + 'obs.asol'

regionfile  = obsdir + 'extract.reg'
collatefile = obsdir + 'all.collated'
model_savefile = obsdir + 'ae_make_catalog.sav'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir

active_catfile = tempdir + 'temp.cat'



;; =====================================================================
;; CONSTRUCT INITIAL CATALOG
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', srclist_fn
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'
                  

;; Read any FRACSPEC values from any existing obs.stats files so that we start our search
;; where the PSF fraction was set from any previous extraction.
print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Running COLLATE stage on ' + srclist_fn
print,   F='(%"ae_make_catalog: ============================================================\n")'  
acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, VERBOSE=0, _EXTRA=extra

bt_initial=mrdfits(collatefile, 1)
if (n_elements(bt_initial) NE num_sources) then message, 'BUG in ae_make_catalog!'


; During the AE runs, the catalog fields RA and DEC are left as zero as a flag 
; telling CONSTRUCT_REGIONS to get the coordinates from the existing source.stats file 
; (to avoid rounding errors from repeated conversion between IDL and FITS).
;
; For sources we have not extracted before, start with the nominal PSF fraction and
; set the initial adjustment to be a large downward step (so we can reasonably rapidly
; find our way to the minimum fraction for sources that need it.)
cat = replicate({sourcename:'', ra:0D, dec:0D, psf_frac:nominal_psf_frac, psf_energ:psf_energ}, num_sources)
cat.sourcename = sourcename
frac_step = replicate(initial_down_step, num_sources)

; For sources that we have extracted before, start with a slightly higher PSF fraction and
; set the initial adjustment to be an upward step of moderate size.
if (total(strmatch(tag_names(bt_initial), 'FRACSPEC')) GT 0) then begin
  FRACSPEC = bt_initial.FRACSPEC * 100
  ind = where(finite(FRACSPEC) AND (FRACSPEC GT 0), count)
  if (count GT 0) then begin
    cat[ind].psf_frac = (FRACSPEC[ind] + minimum_step/2.0) < nominal_psf_frac
    frac_step[ind]    = initial_up_step
  endif
endif

fmt = '(A,1x,F10.6,1x,F10.6,1x,F5.3,1x,F7.5)'


;; =====================================================================
;; Iteratively adjust PSF fractions to eliminate overlapping regions.
;; The strategy for avoiding infinite iteration is as follows:
;; Sources in conflict get smaller.
;; Sources not in conflict get larger.
;; Each source's step size is reduced when it changes direction.
;; When the step size is smaller than minimum_step the source is not adjusted.
;; Thus, it should be the case that the set of sources in play is steadily reduced.

; The dist_to_frac_tried data structure (101xnum_sources) keeps track of the distance to
; the nearest PSF fraction that has been tried so far.
row = abs(indgen(101) - nominal_psf_frac)
dist_to_frac_tried = rebin(reform(row, n_elements(row),  1,/OVERWRITE), n_elements(row), num_sources, /SAMPLE)

match_existing = 0

num_to_process = num_sources
sources_to_process = lindgen(num_to_process)

first_iteration_completed = 0
if keyword_set(restart) then begin
  restore, /V, model_savefile
  GOTO, RESTART
endif

repeat begin
  if first_iteration_completed then begin
    ; Save state in case something crashes.
    save, FILENAME=model_savefile
    print, 'Saved state to ', model_savefile
  endif
  
RESTART:  
  ; Write a temp catalog holding only those sources that need AE processing on this pass.
  forprint, TEXTOUT=active_catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, SUBSET=sources_to_process, F=fmt, /NoCOMMENT, /SILENT

  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running /CONSTRUCT_REGIONS on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  

  acis_extract, active_catfile, obsname, evtfile, /CONSTRUCT_REGIONS, EMAP_FILENAME=emapfile, ASPECT_FN=aspect_fn, MASK_FRACTION=0.98, MASK_MULTIPLIER=1.0, _EXTRA=extra

  
  ; Extract Events (to get SRC_CNTS statistic).
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running /EXTRACT_EVENTS on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  
  acis_extract, active_catfile, obsname, evtfile, /EXTRACT_EVENTS, EMAP_FILENAME=emapfile, REUSE_NEIGHBORHOOD=first_iteration_completed, /REGION_ONLY, _EXTRA=extra

  
  ; Collate the active sources that we've just changed above, and merge with a collation of the full
  ; catalog that we maintain in "bt" (which must be the same length as the "cat" structure).
  ; We need the /SINGLE_OBS option so that SRC_CNTS comes from obs.stats rather than from source.photometry.  
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running COLLATE stage on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  
  acis_extract, active_catfile, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, MATCH_EXISTING=match_existing, VERBOSE=0, _EXTRA=extra
  match_existing = 1

  bt_active=mrdfits(collatefile, 1)
  if (n_elements(bt_active) NE n_elements(sources_to_process)) then message, 'BUG in ae_make_catalog!'

  if (n_elements(bt_active) EQ n_elements(cat)) then begin
    ; The entire catalog is "active" (e.g. on the first iteration), so just use the collated table.
    bt_all = bt_active
  endif else begin
    ; Otherwise, we have to merge the active table back into the full table.
    bt_all[sources_to_process] = bt_active
;    for ii=0, num_to_process-1 do begin
;      ind = sources_to_process[ii]
;      bt_row = bt_all[ind]
;      struct_assign, bt_active[ii], bt_row
;      bt_all[ind] = bt_row
;    endfor ;ii
  endelse
  
  if (n_elements(bt_all) NE n_elements(cat)) then message, 'BUG in ae_make_catalog!'
 
  ;; Note that the speedup trick of collating only the sources we've changed (above) leaves the 
  ;; "neighbor" properties in bt_all with corrupted values.  Thus we must recompute these here:
  make_2d, bt_all.X_CAT, bt_all.X_CAT, xpos_i, xpos_j
  make_2d, bt_all.Y_CAT, bt_all.Y_CAT, ypos_i, ypos_j
          
  ; Compute distances between source positions.
  distance_src2src = sqrt((xpos_i-xpos_j)^2. + (ypos_i-ypos_j)^2.)
  src_num = lindgen(num_sources)
  distance_src2src[src_num,src_num] = 1E10
          
  ; For each source region, find which source has a region most overlapping.
  src_radius       = bt_all.SRC_RAD
  distance_reg2reg = fltarr(num_sources)
  neighbor         = lonarr(num_sources)
  for ii = 0, num_sources-1 do begin
    distance_reg2reg[ii] = min(distance_src2src[*,ii] - src_radius - src_radius[ii], ind, /NAN)
    neighbor[ii] = ind
  endfor   
          
  bt_all.distance_reg2reg = distance_reg2reg
  bt_all.neighbor         = neighbor
  
  
  
  ; Estimate the number of counts not extracted for each source.
  counts_lost = (bt_all.SRC_CNTS/bt_all.FRACSPEC) - bt_all.SRC_CNTS
  
  
  ; The relationship of overlapping regions can be complex.  I think it's essential to process the 
  ; sources sequentially rather than via vector operations.
  ; In order to call AE, each source must have the "sign" of its step chosen:
  ; step_sign =  0: Frac is not changing; source omitted from AE run.
  ; step_sign =  1: Frac is scheduled for increase.
  ; step_sign = -1: Frac is scheduled for reduction.
  step_sign = replicate(0,n_elements(cat))
  
  for ii=0, n_elements(cat)-1 do begin
    if (bt_all[ii].distance_reg2reg LT 0) then begin
      ; This source region is overlapping with that of the neighbor jj.
      jj = bt_all[ii].neighbor
      
      ; If either is already scheduled for reduction then this overlap is being addressed and there's no more work to be done.
      if ((step_sign[ii] EQ -1) OR (step_sign[jj] EQ -1)) then continue

      ; If either is already scheduled for increase then there's a bug.
      if ((step_sign[ii] EQ 1) OR (step_sign[jj] EQ 1)) then message, 'Bug in logic.'

      ; We need to try to schedule one of them to be reduced.
      ; Even if our step size is at the minimum, we will shrink a region to avoid overlap.
      ii_can_reduce = (cat[ii].psf_frac GT min_psf_frac)
      jj_can_reduce = (cat[jj].psf_frac GT min_psf_frac)
      
      if (ii_can_reduce AND jj_can_reduce) then begin
        ; Use counts_lost to choose which should reduce.
        if            (counts_lost[ii] GT counts_lost[jj]) then begin
          step_sign[ii] =  0 ; stay put
          step_sign[jj] = -1 ; reduce
        endif else if (counts_lost[jj] GT counts_lost[ii]) then begin
          step_sign[jj] =  0 ; stay put
          step_sign[ii] = -1 ; reduce
        endif else begin
          ; When counts_lost estimates are equal (e.g. both zero) then reduce the larger PSF fraction.
          if (cat[ii].psf_frac LT cat[jj].psf_frac) then begin
            step_sign[ii] =  0 ; stay put
            step_sign[jj] = -1 ; reduce
          endif else begin
            step_sign[jj] =  0 ; stay put
            step_sign[ii] = -1 ; reduce
          endelse
        endelse
      endif else if ((NOT ii_can_reduce) AND (NOT jj_can_reduce)) then begin
        ; Overlap cannot be fixed; both stay put.
        step_sign[ii] = 0 ; stay put
        step_sign[jj] = 0 ; stay put
      endif else if (     ii_can_reduce  AND (NOT jj_can_reduce)) then begin
        step_sign[jj] =  0 ; stay put
        step_sign[ii] = -1 ; reduce
      endif else if ((NOT ii_can_reduce) AND      jj_can_reduce ) then begin
        step_sign[ii] =  0 ; stay put
        step_sign[jj] = -1 ; reduce
      endif else message, 'Bug in logic.'
    endif else begin
      ; NOT overlapping with anything, so consider increasing.
      ; If we're at the minimum step size then stop trying.
      if (step_sign[ii] NE 0) then message, 'Bug in logic.'
      
      if (cat[ii].psf_frac LT nominal_psf_frac) AND (abs(frac_step[ii]) GT minimum_step) then step_sign[ii] = 1
    endelse
  endfor ;ii
  
  
  ; The direction of the adjustments desired in this iteration are carried in step_sign.
  ; The actual PSF fraction step to take is carried in frac_step, which is a signed quantity.
  ; When the direction of adjustment reverses we reduce the step size.  
  reversing_ind = where( frac_step*step_sign LT 0, count )
  if (count GT 0) then frac_step[reversing_ind] = (-frac_step[reversing_ind]/step_ratio)
  
  ; Adjust the PSF fractions in the catalog.
  sources_to_process = where(step_sign NE 0, num_to_process)

  if (num_to_process EQ 0) then break
  previous_cat_psf_frac = cat.psf_frac
  
  ; For each source we're adjusting, the interval between cat.psf_frac and cat.psf_frac + frac_step 
  ; defines a range of new PSF fractions that are reasonable to take.
  ; For efficiency, we add the further criterion that we will choose the value in that range that
  ; is farthest from any PSF fraction we have already tried for this source.
  for ii=0, num_to_process-1 do begin
    ind = sources_to_process[ii]
    ; Define the range of PSF fractions (expressed here as integer percentages) that are under consideration.
    if (frac_step[ind] GT 0) then begin
      ; Interval extends to the right; clip at nominal_psf_frac.
      min_frac = nominal_psf_frac < (cat[ind].psf_frac + 1             )
      max_frac = nominal_psf_frac < (cat[ind].psf_frac + frac_step[ind])
    endif else begin
      ; Interval extends to the left; clip at min_psf_frac.
      max_frac =     min_psf_frac > (cat[ind].psf_frac - 1             )
      min_frac =     min_psf_frac > (cat[ind].psf_frac + frac_step[ind])    
    endelse                     
    
    ; Select the best candidate in that range.
    dum = max(dist_to_frac_tried[min_frac:max_frac, ind], imax)
    cat[ind].psf_frac = min_frac + imax
    
    ; Update the dist_to_frac_tried data structure.
    dist_to_frac_tried[0,ind] = dist_to_frac_tried[*,ind] < abs(indgen(101) - cat[ind].psf_frac) 
  endfor ;ii                                                                  
  
  ; Defensively range check the catalog and loop to call AE.
  cat.psf_frac = min_psf_frac > cat.psf_frac < nominal_psf_frac
  
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print, num_to_process, F='(%"ae_make_catalog: %d crowded sources are being reprocessed ...")'
  forprint, cat.sourcename, bt_all.label, (cat.psf_frac-previous_cat_psf_frac)/100., cat.psf_frac/100., SUBSET=sources_to_process, F='(%"%s (%s): frac stepped by %5.2f to %4.2f")'

  first_iteration_completed = 1
endrep until (0)


; Report pairs of sources that significantly overlap.
overlap_frac = (-bt_all.distance_reg2reg / 2 / src_radius) > 0
report_flag = overlap_frac GT 0.05
for ii=0,num_sources-1 do $
  if report_flag[ii] then report_flag[neighbor[ii]] = 0

ind = where(report_flag, num_crowded)
if (num_crowded GT 0) then begin
   print, '    Source Label        Source Names        distance_reg2reg/src_radius'
 neighbor_name = (bt_all.CATALOG_NAME)[neighbor]
  ind = ind[reverse(sort(overlap_frac[ind]))]
  print, num_crowded, F='(%"\nae_make_catalog: WARNING!  %d pairs of sources may be significantly overlapping.")'
  forprint, bt_all.label, bt_all.CATALOG_NAME, neighbor_name, overlap_frac, SUBSET=ind, F='(%"%12s  %s <> %s  %7.1f")' 
endif



;; =====================================================================
; Write out the full catalog with RA & DEC filled in so it serves as an archive.
cat.RA  = bt_all.RA
cat.DEC = bt_all.DEC
forprint, TEXTOUT=catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, F=fmt, /NoCOMMENT, /SILENT
print, 'Wrote catalog ', catfile

;; =====================================================================
;; Collate full catalog, and generate a master region file.
print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Running COLLATE stage on the full catalog'
print,   F='(%"ae_make_catalog: ============================================================\n")'  
acis_extract, catfile, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, REGION_FILE=regionfile, MATCH_EXISTING=match_existing, VERBOSE=0, _EXTRA=extra


; Show the regions in ds9.
cmd = string(evtfile, regionfile, F='(%"ds9 -log %s -regionfile %s &")')
spawn, cmd

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif

return
end ; ae_make_catalog



;#############################################################################
;;; Perform a standard extraction on a single obsid.
;;;
;;; Several input files are located using standard naming conventions as shown 
;;; in the Getting Started section of the AE manual.
;;;
;;; The extraction regions must have been constructed previously, e.g. with ae_make_catalog.
;;;
;;; The default input event list is ../obsXXXX/spectral.evt; use EVTFILE_BASENAME
;;; to supply the name of a different event list in ../obsXXXX/.
;;;
;;; Backgrounds are constructed by applying the mask region file ../obsXXXX/mask.reg, 
;;; typically created by ae_make_catalog and containing circular masks from the 
;;; /CONSTRUCT_REGIONS stage.  Additional mask regions can be supplied via EXTRA_MASKFILE.
;;;
;;; By default, if a source has an existing background spectrum then it is retained (not recomputed);
;;; this avoids destroying the backgrounds laboriously comptued by ae_better_background.
;;; If you want to recompute masked backgrounds then specify REUSE_BACKGROUND=0

PRO ae_standard_extraction, obsname, $
    SRCLIST_FILENAME=srclist_fn, EVTFILE_BASENAME=evtfile_basename, EXTRACTION_NAME=extraction_name, $
    EXTRA_MASKFILE=extra_maskfile, $
    REUSE_BACKGROUND=reuse_background, SKIP_TIMING=skip_timing, $
    _EXTRA=extra

if NOT keyword_set(evtfile_basename) then evtfile_basename = 'spectral.evt'

; The default is /REUSE_BACKGROUND
if (n_elements(reuse_background) EQ 0) then reuse_background=1
if keyword_set(srclist_fn) then begin
  if NOT keyword_set(reuse_background) then begin
    print, 'ERROR: To avoid computing a masked background using only a sub-catalog, you cannot pass SRCLIST_FILENAME without also specifying /REUSE_BACKGROUND.'
    retall
  endif
endif else srclist_fn = 'all.srclist'

if (n_elements(extraction_name) EQ 0) then extraction_name=''

creator_string = "ae_standard_extraction, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

obsdir          = '../obs' + obsname + '/'

evtfile         = obsdir + evtfile_basename
bkg_evtfile     = obsdir + 'background.evt'
emapfile        = obsdir + 'obs.emap'
bkg_emapfile    = obsdir + 'background.emap'

ardlib_filename = obsdir + 'ardlib.par'
pbk_filename    = obsdir + 'obs.pbkfile'
maskfile        = obsdir + 'mask.reg'
collatefile     = obsdir + 'all.collated'

if NOT file_test(pbk_filename) then pbk_filename = 'NONE'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir

if (keyword_set(reuse_background) AND file_test(bkg_evtfile)) then begin
  print, 'ae_standard_extraction: WARNING! Any existing background spectra will be reused.'
endif else begin
  param_dir      = tempdir + 'param/'
  temp_events_fn = tempdir + 'temp.evt'
  
  print, F='(%"\nae_standard_extraction: ============================================================")' 
  print, 'ae_standard_extraction: Initializing access to CIAO.'
  print, F='(%"ae_standard_extraction: ============================================================\n")'
  inherited_pfiles = getenv("PFILES")
  run_command, /INIT, PARAM_DIR=param_dir

  ;; =====================================================================
  ;; Make background event list & emap using the masks.
  print, F='(%"\nae_standard_extraction: ============================================================")' 
  print, 'ae_standard_extraction: Constructing background emap and event list'
  print, F='(%"ae_standard_extraction: ============================================================")'
  
  
  ; Build region file containing all the masks.
  if keyword_set(extra_maskfile) then begin
    cmd = string(extra_maskfile,maskfile, F="(%'cp %s %s')")
  endif else begin
    cmd = string(maskfile,       F="(%'echo ""# Region file format: DS9 version 3.0"" >! %s')")
  endelse
  run_command, cmd, /UNIX
    
  cmd = string(obsname,maskfile, F="(%'grep -h background */%s/extract.reg           >>! %s')")
  run_command, cmd, /UNIX
  
  ;; CIAO (Jan 2008) has a bug when update=no is used with exclude which results in cropping of the output image.
  ;; As a workaround we add small regions at the corners of the emap.
  emap      = readfits(emapfile, emap_header)
  extast, emap_header, emap2wcs_astr
  emap_col_dim = (size(emap, /DIM))[0]
  emap_row_dim = (size(emap, /DIM))[1]
  crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
  crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
  cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]
  
  x_sky = crvalP[0] + cdeltP[0] * (([0,emap_col_dim-1]+1) - crpixP[0])
  y_sky = crvalP[1] + cdeltP[1] * (([0,emap_row_dim-1]+1) - crpixP[1])

  openw, unit, maskfile, /GET_LUN, /APPEND
  !TEXTUNIT = unit
  forprint, TEXTOUT=5, x_sky, y_sky, F='(%"circle(%f,%f,1) # tag={bug workaround}")', /NoCOMMENT
  free_lun, unit
  
  
  ; Apply masks to emap.
  print
  print, 'The following dmcopy can run a while on large catalogs.'
 
  cmd = string(emapfile, maskfile, bkg_emapfile, F="(%'dmcopy ""%s[exclude sky=region(%s)][opt full,update=no]"" %s clobber=yes')")
  run_command, cmd
    
    ;; CIAO 3.4 has a bug in dmimgpick that leads to segfaults if the image is not square.
    temp_emap = readfits(bkg_emapfile, temp_emap_header, /SILENT)
    temp_xdim = (size(temp_emap, /DIM))[0]
    temp_ydim = (size(temp_emap, /DIM))[1]
    if (temp_xdim NE temp_ydim) then begin
      print, 'WARNING: working around dmimgpick bug in CIAO 3.4 by forcing emap to be square.'
      temp = fltarr(temp_xdim>temp_ydim, temp_xdim>temp_ydim)
      temp[0,0] = temp_emap
      writefits, bkg_emapfile, temp, temp_emap_header
    endif

    ; Discard events where emap is zero.
  cmd = string(evtfile, bkg_emapfile, temp_events_fn, F="(%'dmimgpick ""%s[cols time,ccd_id,chip,det,sky,pi,energy]"" %s %s method=closest clobber=yes')")
  run_command, cmd
  
  cmd = string(temp_events_fn, bkg_evtfile, F="(%'dmcopy ""%s[#8>1]"" %s clobber=yes')")
  run_command, cmd
    
  cmd = string(bkg_emapfile, maskfile, bkg_evtfile, maskfile, F="(%'ds9 -tile %s -regionfile %s -log %s -regionfile %s &')")
  run_command, cmd
endelse ; constructing background emap and event list


;; =====================================================================
;; Extract Spectra.
print, F='(%"\nae_standard_extraction: ============================================================")' 
print, 'ae_standard_extraction: Running /EXTRACT_SPECTRA stage'
print, F='(%"ae_standard_extraction: ============================================================\n")'
acis_extract, srclist_fn, obsname, evtfile, /EXTRACT_SPECTRA, EMAP_FILENAME=emapfile, ASPHIST_DIR=obsdir+'asphist', ARDLIB_FILENAME=ardlib_filename, PBKFILE=pbk_filename, EXTRACTION_NAME=extraction_name, _EXTRA=extra

if NOT keyword_set(skip_timing) then begin
  ;; =====================================================================
  ;; Timing Analysis.
  print, F='(%"\nae_standard_extraction: ============================================================")'
  print, 'ae_standard_extraction: Running /TIMING stage'
  print, F='(%"ae_standard_extraction: ============================================================\n")'
  acis_extract, srclist_fn, obsname, /TIMING, EXTRACTION_NAME=extraction_name, _EXTRA=extra
endif
  
;; =====================================================================
;; Extract background for each source.
;; We omit any EXTRACTION_NAME supplied so the bkg spectrum will be in the
;; generic (not named) directory.
print, F='(%"\nae_standard_extraction: ============================================================")' 
print, 'ae_standard_extraction: Running /EXTRACT_BACKGROUNDS stage'
print, F='(%"ae_standard_extraction: ============================================================\n")'
acis_extract, srclist_fn, obsname, bkg_evtfile, /EXTRACT_BACKGROUNDS, EMAP_FILENAME=bkg_emapfile, REUSE_BACKGROUND=keyword_set(reuse_background), _EXTRA=extra

;; =====================================================================
;; Collate results.
; Commented out to reduce run times.
;print, F='(%"\nae_standard_extraction: ============================================================")' 
;print, 'ae_standard_extraction: Running COLLATE stage'
;print, F='(%"ae_standard_extraction: ============================================================\n")'
;acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, EXTRACTION_NAME=extraction_name, VERBOSE=0

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
end ; ae_standard_extraction





;#############################################################################
;;; Apply "better" source masking to construct a background event list and emap.
;;; Then re-extract the background for every source.

;;; The entire catalog must already have been processed through the EXTRACT_SPECTRA and
;;; EXTRACT_BACKGROUNDS stages.  This tool will run the MERGE_OBSERVATIONS stage on a 
;;; SINGLE obsid to estimate fluxes for all the sources.
;;; Note that a source might have no net counts in any single observation.

;;; We also need Information about the background flux in this single observation.
;;; We first tried computing a point estimate of the background flux at each source
;;; and then interpolating those irregular samples using trigrid(), but the result was
;;; flawed (e.g. zero background reported in some regions).
;;; So now we're going to use adaptive smoothing to directly compute a background
;;; flux everywhere its needed, in the same band as used for NET_CNTS.
;;;
;;; The default input event list is ../obsXXXX/spectral.evt; use EVTFILE_BASENAME
;;; to supply the name of a different event list in ../obsXXXX/.


PRO ae_better_masking, obsname, EMAP_ENERGY=emap_energy,  $
  
    SRCLIST_FILENAME=srclist_fn, EVTFILE_BASENAME=evtfile_basename,  EXTRACTION_NAME=extraction_name, $
    EMAP_BASENAME=emap_basename, $

		MIN_NUM_CTS=min_counts, EXTRA_MASKFILE=extra_maskfile, THRESHOLD=threshold, $
		
		SKIP_STARFLUX           =skip_starflux, $
		SKIP_POLYMASK           =skip_polymask, $
		SKIP_EXTRACT_BACKGROUNDS=skip_extract_backgrounds, _EXTRA=extra

if NOT keyword_set(threshold)        then threshold = 0.5
if NOT keyword_set(min_counts)       then min_counts=100
if NOT keyword_set(evtfile_basename) then evtfile_basename = 'spectral.evt'
if NOT keyword_set(emap_basename)    then emap_basename    = 'obs.emap'
if keyword_set(srclist_fn) then begin
  print, 'WARNING: The ae_better_masking algorithm should be run on the full catalog of sources.'
endif else srclist_fn = 'all.srclist'

creator_string = "ae_better_masking, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

obsdir       = '../obs' + obsname + '/'
srclist_fn   = 'all.srclist'

evtfile      = obsdir + evtfile_basename
emapfile     = obsdir + emap_basename
bkg_evtfile  = obsdir + 'background.evt'

bkg_emapfile = obsdir + 'background.emap'

polyreg_file    = obsdir + 'polymask.reg'
polymask_file   = obsdir + 'polymask.img'
star_fluxfile   = obsdir + 'star_flux.img'
star_countsfile = obsdir + 'star_counts.img'
bkg_countsfile  = obsdir + 'bkg_counts.img'

collatefile     = obsdir + 'all.collated'

obs_stats_basename       = 'obs.stats'
src_region_basename      = 'extract.reg'
psf_basename             = 'source.psf'
bkg_spectrum_basename    = 'background.pi'
rmf_basename               = 'source.rmf'
arf_basename               = 'source.arf'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        =         tempdir + 'param/'

temp_events_fn   = tempdir + 'temp.evt'
temp_region_fn   = tempdir + 'temp.reg'
temp_image_fn    = tempdir + 'temp.img'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=param_dir

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,10000)

if keyword_set(skip_polymask) AND keyword_set(extra_maskfile) then begin
  print, 'ERROR: /SKIP_POLYMASK conflicts with /EXTRA_MASKFILE'
endif


;; =====================================================================
;; Collect photometry information from the existing single-observation extraction.
print, F='(%"\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")'
print, "DO NOT RUN MULTIPLE INSTANCES OF THIS TOOL SIMULTANEOUSLY!"
print, "The MERGE_OBSERVATIONS stage of AE will be called to compute photometry on a SINGLE obsid"
print, "and that photometry data would be overwritten by a second instance of MERGE_OBSERVATIONS."
print, F='(%"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")'
print, 'Processing will resume in 10s'
wait, 10

print, F='(%"\nae_better_masking: ============================================================")' 
print, 'ae_better_masking: Running MERGE_OBSERVATIONS stage on observation '+obsname
print, F='(%"ae_better_masking: ============================================================\n")'
; We need to run MERGE_OBSERVATIONS using this SINGLE observation for two reasons
; not related to time variability of the sources:
; (1) We must ensure that all the properties collated below refer to one observation.
;     For example, SRC_CNTS is used later with single-obsid background information 
;     in Pb calculations.
; (2) Since a source may have a different PSF in different obsids the effects of 
;     crowding may be quite different.
acis_extract, srclist_fn, obsname, /MERGE_OBSERVATIONS, /SKIP_PSF, /SKIP_NEIGHBORHOOD, /SKIP_TIMING, EBAND_LO=0.5, EBAND_HI=8.0


print, F='(%"\nae_better_masking: ============================================================")' 
print, 'ae_better_masking: Running COLLATE stage on observation '+obsname
print, F='(%"ae_better_masking: ============================================================\n")'
;; Collect the flux & background information from the existing extraction.
;; Do NOT use /SINGLE_OBS because we need the photometry information.
;; It's tempting to use the /MATCH_EXISTING option to save time, but I'm nervous about not having any control over the
;; format of the existing collatefile. 
acis_extract, srclist_fn, obsname, COLLATED_FILENAME=collatefile, VERBOSE=0

bt=mrdfits(collatefile, 1)  
temp = n_elements(bt)

; Sources which were not observed in this obsid are discarded here to make
; the rest of the code easier to write.
; We must be careful to get source names from bt.CATALOG_NAME rather than from reading srclist_fn ourselves!

bt = bt[where(finite(bt.X_CAT), num_sources)]

if (temp-num_sources GT 0) then print, temp-num_sources, F='(%"Ignoring %d sources not in this observation.")'

ind = where(bt.NUM_OBS NE 1, count)
if (count GT 0) then begin
  print, 'ERROR: these sources have NUM_OBS != 1'
  forprint, SUBSET=ind, bt.CATALOG_NAME, bt.NUM_OBS
  GOTO, CLEANUP
endif

; Below we extract the source properties we're going to need.  
; This design is a bit sloppy---we require that all these properties correspond to the single obsid we're working with.
; Thus it's tempting to collate with /SINGLE_OBS so we grab these properties directly from <src>/<obs>/obs.stats, and
; then directly reference the single-obsid ARF and RMF files.
; However, we also need quantities (SRC_CNTS, NET_CNTS, EXPOSURE) calculated in /MERGE, run in single-obsid mode.
; To collate these quantities we must omit the /SINGLE_OBS option.
; Thus, several single-source properties (e.g. SRC_AREA) are actually being collated from <src>/source.stats, 
; rather than from obs.stats.
;
; We could of course eliminate the MERGE stage from this algorithm, instead grabbing the single-obsid quantities we
; need directly from files in <src>/<obs>/, but that would require duplicating some code from the MERGE stage.

CATALOG_NAME = strtrim(bt.CATALOG_NAME,2)
NET_CNTS     = bt.NET_CNTS[0]
EXPOSURE     = bt.EXPOSURE
ENERG_LO     = bt[0].ENERG_LO[0]
ENERG_HI     = bt[0].ENERG_HI[0]
X_CAT        = bt.X_CAT
Y_CAT        = bt.Y_CAT

;; Check for NaN values in critical properties.
vital_values = [NET_CNTS,EXPOSURE,ENERG_LO,ENERG_HI,X_CAT,Y_CAT]
if (~array_equal(finite(vital_values),1)) then begin
  print, 'ERROR: NaN values found in critical columns of '+collatefile
  GOTO, CLEANUP
endif

;; Check for invalid values in critical properties.
temp = NET_CNTS+EXPOSURE+ENERG_LO+ENERG_HI+X_CAT+Y_CAT
ind = where(~finite(temp), count)
if (count GT 0) then begin
  print, 'ERROR: NaN values found in critical columns (NET_CNTS,EXPOSURE,ENERG_LO,ENERG_HI,X_CAT,Y_CAT) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, CLEANUP
endif

temp = EXPOSURE*ENERG_LO*ENERG_HI*X_CAT*Y_CAT
ind = where(temp LE 0, count)
if (count GT 0) then begin
  print, 'ERROR: zero or negative values found in critical columns (EXPOSURE,ENERG_LO,ENERG_HI,X_CAT,Y_CAT) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, CLEANUP
endif

if (~array_equal(strtrim(bt.OBSNAME,2), obsname)) then begin
  print, 'ERROR: the collated photometry in '+collatefile+' appears to come from the wrong observation!'
  GOTO, CLEANUP
endif


;; =====================================================================
;; Read the exposure map which defines the pixelization of the background region.
emap      = readfits(emapfile, emap_header)
extast, emap_header, emap2wcs_astr
emap_col_dim = (size(emap, /DIM))[0]
emap_row_dim = (size(emap, /DIM))[1]

skypix_per_emappix = sxpar(emap_header, 'CDELT1P') ;linear sky pixels per emap pixel


;; =====================================================================
;; Estimate the point source flux, counts in this obsid in the energy  
;; band #0 for each pixel of the emap.
;; We have to use MERGE_OBSERVATIONS results since in any single obsid a 
;; source may have no detected flux.

if keyword_set(skip_starflux) then begin
  star_flux = float(readfits(star_fluxfile))
  goto, SKIP_STARFLUX
endif

star_flux = 0

for ii = 0, num_sources-1 do begin
  obsdir    = CATALOG_NAME[ii] + '/' + obsname + '/' + extraction_subdir[ii]

  ; To support faster processing on multiple computers, we allow the observer to run this
  ; model-building part of the code AFTER the MERGE stage of AE has been run for another
  ; obsid.
  ; Thus, we can NOT expect that any of the merged data products (PSF, ARF, or RMF) 
  ; continue to correspond to the single obsid we're working on here, so we directly
  ; access the single-obsid response files we need.
  psf_fn          = CATALOG_NAME[ii] + '/' + obsname + '/' + psf_basename
  rmf_fn          = obsdir + rmf_basename
  arf_fn          = obsdir + arf_basename


  if (NET_CNTS[ii] LE 0) then begin
    print, 'Skipping source with non-positive NET_CNTS'
    continue
  endif
  
  ;; ------------------------------------------------------------------------
  ; We know that in this single obsid NET_CNTS stellar counts were
  ; observed over band #0.  
  ; The PSF fraction and ARF associated with that observation is 
  ; summarized in the composite ARF. 
  ; Thus we compute a sort of source "flux" by dividing NET_CNTS by the ARF
  ; value at the EMAP_ENERGY and by the EXPOSURE.
  ; This flux normalizes the model (PSF) of the source; later that model is scaled
  ; by the emap to model the expected counts from the source (counts_model).
  ; For this reason we compute a  flux here that uses arf_at_emap_energy, 
  ; rather than use the flux computed in the MERGE stage which uses a mean effective area value.

  ; First, use RMF & ARF to figure out ARF value at the EMAP_ENERGY.
  ae_channel_energy_and_arf, rmf_fn, arf_fn, $
	channel_number, channel_lowenergy, channel_highenergy, channel_midenergy, channel_specresp, channel_psf_frac

  arf_at_emap_energy  = interpol(channel_specresp, channel_midenergy, emap_energy)
  frac_at_emap_energy = interpol(channel_psf_frac, channel_midenergy, emap_energy)

  src_flux = NET_CNTS[ii] / arf_at_emap_energy / EXPOSURE[ii]
  

  ; Now read the PSF for this obsid and regrid to the emap's pixel grid.
  psf_img = readfits(psf_fn, psf_header, /SILENT)

  ; Set all the NaN values to zero to keep total,max,contour, etc. routines happy.
  ind = where(~finite(psf_img), count)
  if (count GT 0) then psf_img[ind] = 0
  
  ; We have to be careful with the normalization of the PSF image.
  ; The PSF on disk is implicitly scaled using PSF_TOTL to sum to <=1 
  ; due to cropping, i.e. total(psf_img)/PSF_TOTL <=1.
  ; Regridding with hastrom does NOT preserve the power -- we must renormalize so
  ; the power is back to the value in the original PSF.
          
  psf_total = sxpar(psf_header, 'PSF_TOTL')
  if (psf_total EQ 0) then begin
    print, "WARNING: obsolete PSFs in "+psf_fn
    power = 1.0
  endif else begin
    power = total(psf_img, /DOUBLE) / psf_total
  endelse
  
  fxaddpar, psf_header, 'EQUINOX', 2000.0      
  hastrom, psf_img, psf_header, emap_header, MISSING=0    
  
  ; We also want to scale our PSF image to be a surface brightness image for the source
  ; in units of counts/s/cm^2/emap_pixel^2.  
  
  normalization = float(src_flux * (power/total(psf_img, /DOUBLE)))
  psf_img = temporary(psf_img)  * normalization
  
  
  if finite(normalization) then begin        
    star_flux = star_flux + psf_img

    counts_in_model = total(psf_img*emap, /DOUBLE)
    
    ; Print both NET_CNTS and total(psf_img*emap) as a consistency check.
    estimated_counts_in_model = NET_CNTS[ii]*power/frac_at_emap_energy  

    diff = abs(counts_in_model - estimated_counts_in_model)
    if ((diff / estimated_counts_in_model) GT 0.20) && (diff GT 1) then begin 
      print, round(estimated_counts_in_model), round(counts_in_model), F='(%"WARNING! (net counts)*(PSF footprint fraction)/(extraction fraction) = %d; counts in source model = %d")'
    endif

  endif else print, 'WARNING: normalization not finite'
endfor ;ii

writefits, star_fluxfile, star_flux, emap_header

SKIP_STARFLUX:


if NOT keyword_set(skip_polymask) then begin
  ;; =====================================================================
  ;; Create a huge region file that contains all the extraction polygons
  ;; plus any EXTRA_MASKFILE supplied by the observer.
  ;; The baseline mask consists of those emap pixels whose centers fall in those regions.

  if keyword_set(extra_maskfile) then begin
    file_copy, extra_maskfile, polyreg_file
    first_index = 0
  endif else begin
    obsdir    = CATALOG_NAME[0] + '/' + obsname + '/' + extraction_subdir[0]
    region_fn = obsdir + src_region_basename
    ae_ds9_to_ciao_regionfile, region_fn, polyreg_file, MAX_VERTEXES=10, /IGNORE_BACKGROUND_TAG
    first_index = 1
  endelse
  
  for ii = first_index, num_sources-1 do begin
    obsdir    = CATALOG_NAME[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    region_fn = obsdir + src_region_basename
        
    ae_ds9_to_ciao_regionfile, region_fn, polyreg_file, MAX_VERTEXES=10, /IGNORE_BACKGROUND_TAG, /APPEND
  endfor ;ii
  
  cmd = string(emapfile, polyreg_file, F="(%'ds9 %s -regionfile %s &')")
  run_command, cmd

  print
  print, 'The following dmcopy can run for hours on large catalogs.'
 
  cmd1 = string(emapfile, polyreg_file, polymask_file, F="(%'dmcopy ""%s[sky=region(%s)][opt full,update=no]"" %s clobber=yes')")
  
  run_command, cmd1

endif ; NOT keyword_set(skip_polymask)
  
  
;; =====================================================================
;; Read emap filtered by polygons; non-zero pixels define initial mask.

star_mask = (readfits(polymask_file) GT 0)

mask_ind = where(star_mask, polymask_count)
print, polymask_count, ' emap pixels masked by polygons'


;; =====================================================================
;; Since some polygons might not include any emap pixels we'll explicitly 
;; mask the pixels that cover source positions.
;; We cannot use xy2ad.pro to convert sky to array index systems.

crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]

for ii = 0, num_sources-1 do begin
  xind_catalog = (crpixP[0] + (X_CAT[ii]-crvalP[0])/cdeltP[0]) - 1
  yind_catalog = (crpixP[1] + (Y_CAT[ii]-crvalP[1])/cdeltP[1]) - 1
  
  star_mask[xind_catalog,yind_catalog] = 1
endfor ;ii

mask_ind = where(star_mask, mask_count)
print, mask_count - polymask_count, ' additional emap pixels masked at source locations'

     
;; =====================================================================
;; I'VE REMOVED THIS STEP BECAUSE CROWDED FIELDS CAN END UP EXCESSIVELY MASKED. 
;;
;; To be conservative we'll enlarge the masks by 1 pixel to take care of
;; pixels that have area under the polygons but centers outside.

; star_mask  = smooth(float(star_mask), 3, /EDGE) GT 0.1
; prev_count = mask_count
; mask_ind   = where(star_mask, mask_count)
; print, mask_count - prev_count, ' additional emap pixels masked by expanded extraction regions'


;; =====================================================================
;; Apply star_mask to an in-band image of the data.

print, 'energy band is ', ENERG_LO, ENERG_HI
run_command, string(emapfile, F="(%'get_sky_limits %s verbose=0 precision=3')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec

if (filterspec EQ '') then message, 'ERROR running get_sky_limits'

cmd = string(evtfile, 1000*[ENERG_LO,ENERG_HI], filterspec, temp_image_fn, F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin %s]"" %s clobber=yes')")
run_command, cmd  

masked_data = readfits(temp_image_fn)
masked_emap = emap

masked_data[mask_ind] = 0
masked_emap[mask_ind] = 0
help, star_mask, masked_data, masked_emap

total_star_counts = total(star_flux * emap, /DOUBLE)
print, 100*total_star_counts/total(/INTEGER,masked_data), total_star_counts, F='(%"Stellar models explain %5.1f%% (%d) of the detected events.")'


;; =====================================================================
;; Iteratively apply additional masking of pixels where flux from stars
;; is large compared to flux from background.
;; Much of this code is taken from adaptive_density_2d.pro.

; As explained in the header comments of adaptive_density_2d.pro
; we crop the data image where exposure is <10% nominal to 
; avoid artifacts in the bkg_flux image. 
; 
; For the same reasons we'll also add those low-exposure field edges 
; to the mask we're making for the background emap & event list.
off_field = where(emap LT (max(emap)/10.), off_field_count)
print, off_field_count, ' emap pixels masked for <10% nominal exposure'
if (off_field_count GT 0) then begin
  star_mask  [off_field] =  1
  masked_data[off_field] =  0
  masked_emap[off_field] =  0
endif

; We can save considerable time by telling adaptive_density_2d to only compute
; background estimates where we need them, which is 
; (a) pixels that are not already masked
; (b) pixels where star_flux is nonzero.  
; The other pixels in bkg_flux will come back with the value NaN.
field_mask = (star_mask EQ 0) AND (star_flux GT 0)

max_radius = 200
print, threshold, F='(%"Masking pixels where star_flux/bkg_flux > %f")'

;; It is vital to the operation of the search below that the radii
;; list starts with r=0 (a kernel consisting of only the central pixel).
;; We have to limit the number of kernels to avoid excessive memory requirements.
skip = 0
repeat begin
  skip = skip + 1
  radii = skip * indgen(1+(max_radius/skip))
  max_radius_index = n_elements(radii)-1
  num_kernels      = n_elements(radii)
endrep until (num_kernels LT 100)

if NOT keyword_set(silent) then print, 'Using kernel radii: ', radii

kernel_footprint, kf_id, /CREATE, IMAGE=masked_data, RADII=radii

print, F='(%"Finished building Tophat kernels.")'
size_spec = size(masked_data)
bright_count = 0L
initial_radius_index = 0 

bkg_flux = make_array( SIZE=size(star_flux), VALUE=!VALUES.f_nan )
help, bkg_flux

repeat begin
  masking_done = 1
  
  ; Find indexes of pixels we're considering masking.
  field_ind = where(field_mask, num_pixels)
  
  ; Sort them by their star_flux.
  sorted_index1D = field_ind[ reverse( sort( star_flux[field_ind] ) ) ]
  print, num_pixels, F='(%"Examining star_flux/bkg_flux in %d pixels ...")'

  checkpoint_rows = round([.2,.4,.6,.8]*num_pixels) > 1
  start_time = systime(1)
  
  for ii = 0L,num_pixels-1 do begin
    if (ii EQ checkpoint_rows[0]) then begin
      elapsed_time = (systime(1)-start_time)
      estimated_total_time = num_pixels * (elapsed_time/checkpoint_rows[0])

      print, checkpoint_rows[0], ceil((estimated_total_time-elapsed_time)/60.),$
	     F='(%"%d pixels processed; estimate %d more minutes to finish")'
      checkpoint_rows = shift(checkpoint_rows,-1)
    endif

    index1D = sorted_index1D[ii]
    index_to_point, index1D, xx, yy, size_spec
    
    ; Find the background level at this pixel's location by searching for a 
    ; circular region that contains min_counts counts.
    ; Arrange for the search to have an efficient starting point and direction.
    radius_index     = initial_radius_index
    search_direction = 0 ;just started
    search_done = 0
;help,ii
    repeat begin
;help,radius_index
      ;; Retrieve the 1-D indexes of pixels that fall under the kernel.
      ;; We're doing Top Hat kernels and simply looking for min_counts
      ;; counts so we ignore the "weight" values returned by kernel_footprint.
      kernel_footprint, kf_id, xx, yy, radius_index, pixel_list
 
      counts = total(/INTEGER,masked_data[pixel_list])
      significance_is_good = (counts GE min_counts)
      
      if significance_is_good then begin
        if (search_direction EQ 1) then begin
          ; We were searching UP from bad kernels and found a good one, so
          ; stop and keep this kernel.
          search_done = 1 
        endif else begin
          ; We just started, or were searching down and found a good one, so
          ; we need to keep going down, if possible.
          if (radius_index LE 0) then begin
            search_done = 1 
          endif else begin
            search_direction = -1 ; down
            radius_index     = radius_index - 1
          endelse
        endelse
        
      endif else begin
        if (search_direction EQ -1) then begin
          ; We were searching DOWN from good kernels and found a bad one, so
          ; stop and keep the next larger kernel.
          radius_index = radius_index + 1
          search_direction = 1 ;up
        endif else begin
          ; We just started (search_direction==0), or were searching up and found a bad one,
          ; so we need to keep going up, if possible.
          if (radius_index GE max_radius_index) then begin
            print, 'WARNING: search truncated at max kernel radius'
            search_done = 1 
          endif else begin
            search_direction = 1 ;up
            radius_index     = radius_index + 1
          endelse
        endelse
      endelse ; significance is bad

    endrep until search_done
    
    ;; Save the next smaller kernel as the starting point for the next pixel.
    ;; The way the search above works, if the starting kernel turns out to
    ;; be the one we're looking for, then we must step down one kernel and 
    ;; then back up, wasting time.  If we start just below the goal, then
    ;; we make one step and we're done.
    initial_radius_index = (radius_index - 1) > 0

    exposure = total(masked_emap[pixel_list])
    bkg_flux[index1D] = counts /exposure
    
    if ((star_flux[index1D]/bkg_flux[index1D]) GT threshold) then begin
;     print, masked_data[index1D], xx, yy, F='(%"masked %d counts in pixel (%4d,%4d)")' 
      masking_done = 0
      bright_count = bright_count + 1
      star_mask  [index1D] = 1
      masked_data[index1D] = 0
      masked_emap[index1D] = 0
      field_mask [index1D] = 0
    endif
    
  endfor

endrep until masking_done
print, bright_count, ' pixels added to mask due to stellar contamination'
kernel_footprint, kf_id, /DESTROY


;; =====================================================================
;; Save results.

mask_ind = where( star_mask, mask_count )
star_flux  [mask_ind] = 0
print, mask_count, ' total emap pixels masked'

; Consistency check.
dum = where(masked_emap[mask_ind] NE 0, count)
if (count GT 0) then message, 'ERROR: mask_ind inconsistent with masked_emap'

;  Multiply by emap to get source & background counts.
star_counts = star_flux * emap
bkg_counts  = bkg_flux  * emap

writefits, bkg_emapfile,  masked_emap, emap_header
writefits, star_countsfile, star_counts, emap_header
writefits, bkg_countsfile,   bkg_counts, emap_header

print, bkg_evtfile, total(star_counts), F='(%"Estimated # of known point source counts contaminating %s is %5.1f")'

;; =====================================================================
;; Background event list is made by discarding events where background emap is zero.
                
;; CIAO 3.4 has a bug in dmimgpick that leads to segfaults if the image is not square.
temp_emap = readfits(bkg_emapfile, temp_emap_header, /SILENT)
temp_xdim = (size(temp_emap, /DIM))[0]
temp_ydim = (size(temp_emap, /DIM))[1]
if (temp_xdim NE temp_ydim) then begin
  print, 'WARNING: working around dmimgpick bug in CIAO 3.4 by forcing emap to be square.'
  temp = fltarr(temp_xdim>temp_ydim, temp_xdim>temp_ydim)
  temp[0,0] = temp_emap
  writefits, bkg_emapfile, temp, temp_emap_header
endif
  
cmd2 = string(evtfile, bkg_emapfile, temp_events_fn, F="(%'dmimgpick ""%s[cols time,ccd_id,chip,det,sky,pi,energy]"" %s %s method=closest clobber=yes')")

cmd3 = string(temp_events_fn, bkg_evtfile, F="(%'dmcopy ""%s[#8>1]"" %s clobber=yes')")
run_command, [cmd2,cmd3]
  

;; =====================================================================
cmd = string(bkg_emapfile, polyreg_file, bkg_evtfile, polyreg_file, star_countsfile, polyreg_file, bkg_countsfile, polyreg_file, F="(%'ds9 -tile -log %s -regionfile %s %s -regionfile %s %s -regionfile %s %s -regionfile %s &')")
run_command, cmd

print
print, 'The 4 panels in ds9 are'
print, ' masked emap      ', bkg_emapfile
print, ' masked event list', bkg_evtfile
print, ' estimated star counts contaminating background', star_countsfile
print, ' estimated background counts                   ', bkg_countsfile


if NOT keyword_set(skip_extract_backgrounds) then begin
  ;; =====================================================================
  ;; Extract background for each source.
  acis_extract, srclist_fn, obsname, bkg_evtfile, /EXTRACT_BACKGROUNDS, EMAP_FILENAME=bkg_emapfile, _EXTRA=extra
  
  
  ;; =====================================================================
  ;; Collate results.
  ; Commented out to reduce run times.
  ;acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, VERBOSE=0
endif


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
end ; ae_better_masking





;#############################################################################
;;; Build background regions which account for contamination from neighboring sources.
;;;
;;; The entire catalog must already have been processed through the EXTRACT_SPECTRA and
;;; EXTRACT_BACKGROUNDS stages.  This tool will run the MERGE_OBSERVATIONS stage on a 
;;; SINGLE obsid to estimate fluxes for all the sources.
;;; Note that a source might have no net counts in any single observation.

;;; The optional input BACKGROUND_MODEL_FILENAME can be used to supply a model (FITS image) 
;;; of any background component not represented by the point sources in the catalog.
;;; For example such a model could be constructed for readout streaks so the streaks will
;;; participate in the algorithm used to contruct background regions.
;;; The BACKGROUND_MODEL_FILENAME should be in units of observed counts (i.e. exposure 
;;; variation is represented in the model).

;;; If MIN_NUM_CNTS is a scalar (or is omitted and defaults to 100) then it specifies the
;;; target number of total counts in the backgrounds for all observations, to be
;;; apportioned based on exposure fraction.
;;; If MIN_NUM_CNTS is a vector (of length equal to the number of sources in SRCLIST_FILENAME
;;; or in the default all.srclist) then it specifies the target number of counts in the
;;; background for this single observation.

PRO ae_better_backgrounds, obsname, EMAP_ENERGY=emap_energy, $ 
  
		THETA_RANGE=theta_range, BACKGROUND_MODEL_FILENAME=background_model_filename, $
  
    SRCLIST_FILENAME=srclist_fn, EVTFILE_BASENAME=evtfile_basename, EXTRACTION_NAME=extraction_name, $
    EMAP_BASENAME=emap_basename, $

		MIN_NUM_CTS=min_num_cts, MIN_EXPOSURE_RATIO=min_exposure_ratio, SPECTRAL_CORRUPTION_THRESHOLD=spectral_corruption_threshold, $
    COMPACTNESS_GAIN=compactness_gain, $
    
    VERBOSE=verbose, PAUSE_FOR_REVIEW=pause_for_review, $
      
    SINGLE_THREAD_STAGE_ONLY=single_thread_stage_only, SKIP_SINGLE_THREAD_STAGE=skip_single_thread_stage, $
    BUILD_MODELS_ONLY=build_models_only, SAVEFILE_BASENAME=savefile_basename, $
    REUSE_MODELS=reuse_models, PLOT=plot

creator_string = "ae_better_backgrounds, version"+strmid("$Date: 2008-10-15 19:37:03 $", 6, 11)
print, creator_string
print, systime()

if (n_elements(verbose) EQ 0)              then verbose=0
if (n_elements(theta_range) NE 2)          then theta_range = [0,100.]
if (n_elements(compactness_gain) NE 1)     then compactness_gain  = 0.01
compactness_gain = float(compactness_gain)

if ~keyword_set(min_exposure_ratio)            then min_exposure_ratio= 4.0
if ~keyword_set(spectral_corruption_threshold) then spectral_corruption_threshold = 0.10
   
if ~keyword_set(evtfile_basename)       then evtfile_basename  = 'spectral.evt'
if ~keyword_set(savefile_basename)      then savefile_basename = 'ae_better_backgrounds.sav'
if ~keyword_set(emap_basename)          then emap_basename     = 'obs.emap'

obsdir        = '../obs' + obsname + '/'
evtfile       = obsdir + evtfile_basename
emapfile      = obsdir + emap_basename
model_savefile= obsdir + savefile_basename
collatefile   = obsdir + 'all.collated'
regionfile    = obsdir + 'extract.reg'
data_image_fn = obsdir + 'inband_data.img'

if keyword_set(srclist_fn) then begin
  if ~keyword_set(reuse_models) then begin
    print, 'WARNING: The ae_better_backgrounds algorithm should be run on the full catalog of sources.'
  endif
endif else srclist_fn = 'all.srclist'

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,10000)

if keyword_set(reuse_models) && (~file_test(model_savefile)) then begin
  print, 'WARNING: MODEL_SAVEFILE '+model_savefile+' not found; ignoring /REUSE_MODELS.'
  reuse_models = 0
endif

if ((~keyword_set(single_thread_stage_only)) && (~keyword_set(reuse_models)) && (~keyword_set(emap_energy))) then begin
  print, 'ERROR: you must supply EMAP_ENERGY in keV'
  retall
endif

arcsec_per_skypixel = 0.492 

; Make spectra with 685 channels to match RMFs from CTI corrector.
DETCHANS = 685

obs_stats_basename         = 'obs.stats'
src_region_basename        = 'extract.reg'
psf_basename               = 'source.psf'
rmf_basename               = 'source.rmf'
arf_basename               = 'source.arf'
bkg_spectrum_basename      = 'background.pi'
bkg_emap_basename          = 'background.emap'
bkg_pixels_region_basename = 'background_pixels.reg'
bkg_events_basename        = 'background.evt'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        =         tempdir + 'param/'

temp_region_fn   = tempdir + 'temp.reg'
temp_image_fn    = tempdir + 'temp.img'
catalog_region_fn= tempdir + 'catalog.reg'
temp_events_fn   = tempdir + 'temp.evt'
temp_text_fn     = tempdir + 'temp.txt'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")
                            

;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=param_dir

if (verbose GE 1) then begin
  my_ds9 = "DS9:ae_better_backgrounds"
  ; Look for an existing ds9 session ...
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  run_command, string(my_ds9, F='(%"xpaaccess %s")'), result, /IGNORE_STATUS, /QUIET
  if (result[0] EQ 'yes') then $
    run_command, 'ds9 -xpa local -tile -title ae_better_backgrounds -log &'
endif
if (verbose GE 2) then begin
  window,0
  window,1
  window,2
  term1=replicate(!VALUES.F_NAN,1000)
  term2=term1
  term3=term2
endif


;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes', 'punlearn dmimgpick', 'pset dmimgpick clobber=yes', 'punlearn dmextract', 'pset dmextract clobber=yes']


; The map "pixel_status" takes on these flag values:
null_status      = 0B  ; not yet considered
masked_status    = 1B  ; not allowed in background region
candidate_status = 2B  ; currently competing to be accepted into the background region
background_status= 5B  ; accepted into the background region


;; =====================================================================
;; Collect photometry information from the existing single-observation extraction.
if keyword_set(reuse_models) then begin
  print, F='(%"\n===================================================================")'
  print, 'WARNING! Using source models saved from previous session in '+model_savefile
  restore, model_savefile, /VERBOSE
  print, F='(%"===================================================================\n")'
endif else begin
  if ~keyword_set(skip_single_thread_stage) then begin
    print, F='(%"\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")'
    print, "DO NOT RUN MULTIPLE INSTANCES OF THIS TOOL SIMULTANEOUSLY on different obsids or catalogs!"
    print, "The MERGE_OBSERVATIONS stage of AE will be called to compute photometry on a SINGLE obsid"
    print, "and that photometry data would be overwritten by a second instance of MERGE_OBSERVATIONS."
    print, F='(%"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")'
    print, 'Processing will resume in 10s'
    wait, 10
  
    print, F='(%"\nae_better_backgrounds: ============================================================")' 
    print, 'ae_better_backgrounds: Running MERGE_OBSERVATIONS stage on observation '+obsname
    print, F='(%"ae_better_backgrounds: ============================================================\n")'
    ; We need to run MERGE_OBSERVATIONS using this SINGLE observation for two reasons
    ; not related to time variability of the sources:           
    ; (1) We must ensure that all the properties collated below refer to one observation.
    ;     For example, SRC_CNTS is used later with single-obsid background information 
    ;     in Pb calculations.
    ; (2) Since a source may have a different PSF in different obsids the effects of 
    ;     crowding may be quite different.
    acis_extract, srclist_fn, obsname, /MERGE_OBSERVATIONS, /SKIP_PSF, /SKIP_NEIGHBORHOOD, /SKIP_TIMING, EBAND_LO=0.5, EBAND_HI=8.0
  
    print, F='(%"\nae_better_backgrounds: ============================================================")' 
    print, 'ae_better_backgrounds: Running COLLATE stage on observation '+obsname
    print, F='(%"ae_better_backgrounds: ============================================================\n")'
    ;; Collect the flux & background information from the existing extraction.
    ;; Do NOT use /SINGLE_OBS because we need the photometry information.
    ;; It's tempting to use the /MATCH_EXISTING option to save time, but I'm nervous about not having any control over the
    ;; format of the existing collatefile. 
    acis_extract, srclist_fn, obsname, COLLATED_FILENAME=collatefile, VERBOSE=0
    
    if keyword_set(single_thread_stage_only) then GOTO, CLEANUP
  endif

  bt=mrdfits(collatefile, 1)
  temp = n_elements(bt)

  ; Sources which were not observed in this obsid are discarded here so that we don't wast time/space
  ; contructing various data structures which have an entry for each source.
  ; We must be careful to get source names from bt.CATALOG_NAME rather than from reading srclist_fn ourselves!
  bt = bt[where(finite(bt.X_CAT), num_sources)]
  
  if (temp-num_sources GT 0) then print, temp-num_sources, F='(%"\nWARNING: Ignoring %d sources not in this observation.")'
endelse

if (verbose GE 1) then run_command, 'egrep "label|polygon" '+regionfile+' >! '+catalog_region_fn

num_sources = n_elements(bt)

ind = where(bt.NUM_OBS NE 1, count)
if (count GT 0) then begin
  print, 'ERROR: these sources have NUM_OBS != 1'
  forprint, SUBSET=ind, bt.CATALOG_NAME, bt.NUM_OBS
  GOTO, CLEANUP
endif

; Below we extract the source properties we're going to need.  
; This design is a bit sloppy---we require that all these properties correspond to the single obsid we're working with.
; Thus it's tempting to collate with /SINGLE_OBS so we grab these properties directly from <src>/<obs>/obs.stats, and
; then directly reference the single-obsid ARF and RMF files.
; However, we also need quantities (SRC_CNTS, NET_CNTS, EXPOSURE) calculated in /MERGE, run in single-obsid mode.
; To collate these quantities we must omit the /SINGLE_OBS option.
; Thus, several single-source properties (e.g. SRC_AREA) are actually being collated from <src>/source.stats, 
; rather than from obs.stats.
;
; We could of course eliminate the MERGE stage from this algorithm, instead grabbing the single-obsid quantities we
; need directly from files in <src>/<obs>/, but that would require duplicating some code from the MERGE stage.

CATALOG_NAME = strtrim(bt.CATALOG_NAME,2)
SRC_CNTS = bt.SRC_CNTS[0] 
NET_CNTS =(bt.NET_CNTS[0])
EXPOSURE = bt.EXPOSURE
ENERG_LO = bt[0].ENERG_LO[0]
ENERG_HI = bt[0].ENERG_HI[0]
MSK_RAD  = bt.MSK_RAD
X_CAT    = bt.X_CAT
Y_CAT    = bt.Y_CAT
RA       = bt.RA
DEC      = bt.DEC
LABEL    = strtrim(bt.LABEL,2)
THETA    = bt.THETA
EMAP_MED = bt.EMAP_MED
; The SRC_AREA property is a hassle.  IF the observer has run ae_make_catalog on a source but then
; (for efficiency) skipped EXTRACT_SPECTRA because the region did not change much, then SRC_AREA
; will be missing.  In such cases we use instead PGN_AREA.
SRC_AREA = bt.SRC_AREA
PGN_AREA = bt.PGN_AREA
ind = where((SRC_AREA EQ 0) OR (~finite(SRC_AREA)), count)
if (count GT 0) then begin
  print, count, F='(%"WARNING: PGN_AREA substituted for SRC_AREA for %d sources.")'
  SRC_AREA[ind] = PGN_AREA[ind]
endif

;; Check for invalid values in critical properties.
temp = SRC_AREA+SRC_CNTS+NET_CNTS+EXPOSURE+ENERG_LO+ENERG_HI+MSK_RAD+X_CAT+Y_CAT+RA+DEC+THETA
ind = where(~finite(temp), count)
if (count GT 0) then begin
  print, 'ERROR: NaN values found in critical columns (SRC_AREA,SRC_CNTS,NET_CNTS,EXPOSURE,ENERG_LO,ENERG_HI,MSK_RAD,X_CAT,Y_CAT,RA,DEC,THETA) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, CLEANUP
endif

temp = SRC_AREA*EXPOSURE*ENERG_LO*ENERG_HI*MSK_RAD*X_CAT*Y_CAT*RA*abs(DEC)*THETA
ind = where(temp LE 0, count)
if (count GT 0) then begin
  print, 'ERROR: zero or negative values found in critical columns (SRC_AREA,EXPOSURE,ENERG_LO,ENERG_HI,MSK_RAD,X_CAT,Y_CAT,RA,DEC,THETA) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, CLEANUP
endif

if (~array_equal(strtrim(bt.OBSNAME,2), obsname)) then begin
  print, 'ERROR: the collated photometry in '+collatefile+' appears to come from the wrong observation!'
  GOTO, CLEANUP
endif

if keyword_set(reuse_models) then begin
  GOTO, CONSTRUCT_BACKGROUNDS
endif

;; =====================================================================
;; Read the exposure map which defines the pixelization of the background region.
emap         = readfits(emapfile, emap_header)
extast, emap_header, emap2wcs_astr
emap_col_dim = (size(emap, /DIM))[0]
emap_row_dim = (size(emap, /DIM))[1]

; To simplify calculations later, scale the emap by the pixel area so it can be
; directly integrated to get bkg_exposurearea (sec*cm^2*skypixel^2).
skypix_per_emappix     = sxpar(emap_header, 'CDELT1P') ;linear sky pixels per emap pixel


;; =====================================================================
;; Construct an in-band image of the data.
print, 'energy band is ', ENERG_LO, ENERG_HI

run_command, string(emapfile, F="(%'get_sky_limits %s verbose=0 precision=3')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec

if (filterspec EQ '') then message, 'ERROR running get_sky_limits'

cmd = string(evtfile, 1000*[ENERG_LO,ENERG_HI], filterspec, data_image_fn, F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin %s]"" %s clobber=yes')")
run_command, cmd  

inband_data = readfits(data_image_fn)


;; =====================================================================
;; IMPORTANT REMARKS ABOUT PIXELIZATION OF THE SOURCE EXTRACTION REGION

;; It is important to keep in mind that actual extraction regions in AE are
;; polygons with corresponding areas that are estimated carefully.
;; Lots of calculations in this algorithm mimick the background subtraction
;; done in AE which scales the background counts by the ratio of the 
;; extraction polygon area and the background region area,
;; represented by the variables src_exposurearea and bkg_exposurearea.
;; As in AE, these are expressed in units of (sec*cm^2*skypixel^2).
;;
;; However at several points this algorithm must integrate a pixelized source
;; model (scaled PSF image) over the extraction region polygon.  
;; The crude method used for this is to sum up the source model pixels that fall
;; inside the region and then scale up by the ratio of the polygon area to the
;; pixelized aperture area.

 
;; =====================================================================
;; Construct a model of each point source by resampling and scaling the PSF.
;; All these models are in units of COUNTS, i.e. the star's flux has been passed through the exposure map.
;; We store only the non-zero portion of these huge sparse arrays.

num_models = num_sources + keyword_set(background_model_filename)

models = replicate({LABEL:'', col_min:0, row_min:0, col_max:-1, row_max:-1, counts_in_model:0.0, data_ptr: ptr_new()}, num_models)
models.LABEL = LABEL

EA_at_emap_energy = fltarr(num_sources)

for ii = 0, num_sources-1 do begin
  print, F='(%"\n===================================================================")'
  print, CATALOG_NAME[ii], LABEL[ii], F='(%"Source: %s (%s)")'  
  obsdir          = CATALOG_NAME[ii] + '/' + obsname + '/' + extraction_subdir[ii]
  obs_stats_fn    = obsdir + obs_stats_basename

  ; To support faster processing on multiple computers, we allow the observer to run this
  ; model-building part of the code AFTER the MERGE stage of AE has been run for another
  ; obsid.
  ; Thus, we can NOT expect that any of the merged data products (PSF, ARF, or RMF) 
  ; continue to correspond to the single obsid we're working on here, so we directly
  ; access the single-obsid response files we need.
  psf_fn          = CATALOG_NAME[ii] + '/' + obsname + '/' + psf_basename
  rmf_fn          = obsdir + rmf_basename
  arf_fn          = obsdir + arf_basename

  
  if (~file_test(obs_stats_fn)) then begin
    print, 'SOURCE MODEL SKIPPED: source not observed.'
    continue
  endif

  ;; ------------------------------------------------------------------------
  ;; Use RMF & ARF to figure out ARF value, PSF fraction, and effective area at the EMAP_ENERGY.
  ae_channel_energy_and_arf, rmf_fn, arf_fn, $
	channel_number, channel_lowenergy, channel_highenergy, channel_midenergy, channel_specresp, channel_psf_frac, channel_EA
                                                                     
  arf_at_emap_energy   = interpol(channel_specresp     , channel_midenergy, emap_energy)
  frac_at_emap_energy  = interpol(channel_psf_frac     , channel_midenergy, emap_energy)
  EA_at_emap_energy[ii]= interpol(channel_EA           , channel_midenergy, emap_energy)

  if (NET_CNTS[ii] LE 0) then begin
    ; We only allow positive NET_CNTS to avoid negative star models in the algorithm.
    print, 'Skipping source with non-positive NET_CNTS'
    continue
  endif
  
  
  ;; ------------------------------------------------------------------------
  ; We know that in this single obsid NET_CNTS stellar counts were
  ; observed over band #0.  
  ; The PSF fraction and ARF associated with that observation is 
  ; summarized in the composite ARF. 
  ; Thus we compute a sort of source "flux" by dividing NET_CNTS by the ARF
  ; value at the EMAP_ENERGY and by the EXPOSURE.
  ; This flux normalizes the model (PSF) of the source; later that model is scaled
  ; by the emap to model the expected counts from the source (counts_model).
  ; For this reason we compute a  flux here that uses arf_at_emap_energy, 
  ; rather than use the flux computed in the MERGE stage which uses a mean effective area value.

  src_flux = NET_CNTS[ii] / arf_at_emap_energy / EXPOSURE[ii]
                 

  ; Now read the PSF for this obsid and regrid to the emap's pixel grid.
  psf_img = readfits(psf_fn, psf_header, /SILENT)

  ; Set all the NaN values to zero to keep total,max,contour, etc. routines happy.
  ind = where(~finite(psf_img), count)
  if (count GT 0) then psf_img[ind] = 0
  
  ; We have to be careful with the normalization of the PSF image.
  ; The PSF on disk is implicitly scaled using PSF_TOTL to sum to <=1 
  ; due to cropping, i.e. total(psf_img)/PSF_TOTL <=1.
  ; Regridding with hastrom does NOT preserve the power -- we must renormalize so
  ; the power is back to the value in the original PSF.
          
  psf_total = sxpar(psf_header, 'PSF_TOTL')
  if (psf_total EQ 0) then begin
    print, "WARNING: obsolete PSFs in "+psf_fn
    power = 1.0
  endif else begin
    power = total(psf_img, /DOUBLE) / psf_total
  endelse
  
  fxaddpar, psf_header, 'EQUINOX', 2000.0      
  hastrom, psf_img, psf_header, emap_header, MISSING=0    
  
  ; We also want to scale our PSF image to be a surface brightness image for the source
  ; in units of counts/s/cm^2/emap_pixel^2.  
  
  normalization = float(src_flux * (power/total(psf_img, /DOUBLE)))
  
  counts_model  = temporary(psf_img) * normalization * emap
  
  if finite(normalization) then begin        
    ; Save the sparse counts model.
    index_to_point, where(counts_model GT 0), col, row, size(counts_model)
    this_model = models[ii]
    this_model.col_min         = min(col)
    this_model.row_min         = min(row)
    this_model.col_max         = max(col)
    this_model.row_max         = max(row)
    this_model.counts_in_model = total(counts_model, /DOUBLE)
    this_model.data_ptr        = ptr_new( counts_model[this_model.col_min:this_model.col_max, this_model.row_min:this_model.row_max] )
    models[ii] = this_model

    ; Print both NET_CNTS and total(psf_img*emap) as a consistency check.
    estimated_counts_in_model = NET_CNTS[ii]*power/frac_at_emap_energy  
    diff = abs(this_model.counts_in_model - estimated_counts_in_model)
    if ((diff / estimated_counts_in_model) GT 0.20) && (diff GT 1) then begin 
      print, round(estimated_counts_in_model), round(this_model.counts_in_model), F='(%"WARNING! (net counts)*(PSF footprint fraction)/(extraction fraction) = %d; counts in source model = %d")'
    endif
  ; print, arf_at_emap_energy, EXPOSURE[ii], src_flux,  F='(%"%d  %d  %g")'
  endif else begin
    print, 'ERROR: normalization not finite'
    GOTO, CLEANUP
  endelse

endfor ;ii

;; Accept the optional model (image) supplied by the observer.
if keyword_set(background_model_filename) then begin
  bkg_img = readfits(background_model_filename, bkg_header, /SILENT)

  power = total(bkgimg, /DOUBLE) 
  
  fxaddpar, bkg_header, 'EQUINOX', 2000.0      
  hastrom, bkg_img, bkg_header, emap_header, MISSING=0    
  
  normalization = float(power/total(bkg_img, /DOUBLE))
  
  counts_model  = temporary(bkg_img) * normalization

  ; Save the sparse counts model.
  index_to_point, where(counts_model GT 0), col, row, size(counts_model)
  this_model = models[num_sources]
  this_model.col_min       = min(col)
  this_model.row_min       = min(row)
  this_model.col_max       = max(col)
  this_model.row_max       = max(row)
  this_model.counts_in_model = total(counts_model, /DOUBLE)
  this_model.data_ptr      = ptr_new( counts_model[this_model.col_min:this_model.col_max, this_model.row_min:this_model.row_max] )
  models[num_sources] = this_model
  
  print, round(this_model.counts_in_model), F='(%"Observer-supplied background model contains %d counts.")'
endif

print, 'Saving models, collated table, data image, and emap to ', model_savefile
save, emap, emap_header, emap2wcs_astr, emap_col_dim, emap_row_dim, skypix_per_emappix, bt, EA_at_emap_energy, inband_data, models, FILE=model_savefile

CONSTRUCT_BACKGROUNDS:

if keyword_set(plot) then begin              
  dataset_1d, id1, (EA_at_emap_energy * EXPOSURE) / EMAP_MED, DENSITY_TITLE='OBS '+obsname, XTIT='(EA@emap_energy * EXPOSURE) / emap_in_aperture' 
  GOTO, CLEANUP
endif

if keyword_set(build_models_only) then GOTO, CLEANUP 
                                     

exposurearea_map       = skypix_per_emappix^2 * emap
exposurearea_map_floor = max(exposurearea_map)/10.

if (verbose GE 1) then begin
  ; Wait for ds9 to register with XPA.  
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  repeat begin
    run_command, string(my_ds9, F='(%"xpaaccess %s")'), result, /IGNORE_STATUS, /QUIET
    if (result[0] EQ 'yes') then break
    print, 'waiting for ds9 to come up...'
    wait,3
  endrep until (0)

  ; Display the event data.  
  run_command, /QUIET, string(my_ds9, data_image_fn, F='(%"xpaset -p %s file  %s")')
endif



;; =====================================================================
; Calculate the positions of the emap pixel centers in physical (x,y) coordinates, 
; keeping in mind the 1-based FITS convention vs the 0-based IDL array index convention.
; We cannot use xy2ad.pro for conversions between array and PHYSICAL (sky) systems.
crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]

emap_pixel_x = (findgen(emap_col_dim)+1-crpixP[0])*cdeltP[0] + crvalP[0]
emap_pixel_y = (findgen(emap_row_dim)+1-crpixP[1])*cdeltP[1] + crvalP[1]
make_2d, emap_pixel_x, emap_pixel_y
  
;; =====================================================================
;; Pre-calculate a normalized grid of positions relative to the source where
;; we will create the initial set of background pixel candidates.
N = 6
seed_grid_ii = (indgen(2*N) - (N-0.5)) / (N-0.5)
seed_grid_jj = seed_grid_ii
make_2d, seed_grid_ii, seed_grid_jj
seed_grid_ii = reform(seed_grid_ii, n_elements(seed_grid_ii))
seed_grid_jj = reform(seed_grid_jj, n_elements(seed_grid_jj))

;; =====================================================================
;; Define a background region for each source.


processing_rate = fltarr(num_sources)

;; When /REUSE_MODELS is specified we need to read the catalog supplied to 
;; see which sources are to have backgrounds computed; see comments below.
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_in_catalog)

if (num_in_catalog EQ 0) then begin
  print, 'ERROR: no entries read from source list ', srclist_fn
  retall
endif

sourcename = sourcename[ind]


case (n_elements(min_num_cts)) of
  n_elements(sourcename):
  1          : 
  0          : min_num_cts = 100
  else       : begin
               print, 'ERROR: MIN_NUM_CTS input must be scalar or must match the length of the SRCLIST: ', n_elements(sourcename)
               retall
               end
endcase

for ii = 0, num_sources-1 do begin
  t0 = systime(1)
  ;; ------------------------------------------------------------------------
  ;; When /REUSE_MODELS is specified the observer may supply a sourcelist that
  ;; does not match the sources found in the previously computed collated table (bt),
  ;; for example if he/she wants to construct a background for only one source
  ;; but wants to consider contamination from the whole catalog.
  ;; We are looping (ii) over the collated table so we can clearly index vectors
  ;; like SRC_CNTS, NET_CNTS, etc.
  ;; As we loop, we will skip any source in bt that is not in the sourcelist
  ;; supplied by the observer.
  srclist_ind = where(CATALOG_NAME[ii] EQ sourcename, count)

  if (count EQ 0) then begin
    print, 'Skipping source not in list: ', CATALOG_NAME[ii]
    continue
  endif 
  

  if (THETA[ii] LT theta_range[0]) || (THETA[ii] GT theta_range[1]) then begin
    print, 'Skipping source not in THETA_RANGE: ', CATALOG_NAME[ii]
    continue
  endif 
  
  print, F='(%"\n===================================================================")'
  print, CATALOG_NAME[ii], LABEL[ii], F='(%"\nSource: %s (%s)")'  
  
  obsdir               = CATALOG_NAME[ii] + '/' + obsname + '/' + extraction_subdir[ii]
  region_fn            = obsdir + src_region_basename
  bkg_emap_fn          = obsdir + bkg_emap_basename
  bkg_pixels_region_fn = obsdir + bkg_pixels_region_basename
  bkg_events_fn        = obsdir + bkg_events_basename
  bkg_spectrum_fn      = obsdir + bkg_spectrum_basename
  obs_stats_fn         = obsdir + obs_stats_basename
  
  if (~file_test(obs_stats_fn)) then begin
    print, 'BACKGROUND CONSTRUCTION SKIPPED: source not observed.'
    continue
  endif

  
  if (size(min_num_cts,/N_DIMENSIONS) EQ 0) then begin
    ; When min_num_cts is a scalar then we must apportion it by exposure fraction.
    ; First, a list of obs.stats files and their observation directories.
    pattern = CATALOG_NAME[ii] + '/*/' + extraction_subdir[ii] + obs_stats_basename
    all_obs_stats_fn = file_search( pattern, COUNT=num_obs )
    
    total_exposure = 0.0
    for jj=0,num_obs-1 do begin
      header = headfits(all_obs_stats_fn[jj], ERRMSG=error )
      if (keyword_set(error)) then message, 'ERROR reading ' + all_obs_stats_fn[jj] 
      total_exposure += sxpar(header, 'EXPOSURE')
    endfor ;jj
    
     min_num_cts_threshold = ceil(min_num_cts * (1 < (EXPOSURE[ii] / total_exposure)))
  endif else begin
    ; When min_num_cts is a vector then it refers to this single observation.
    min_num_cts_threshold = (min_num_cts[srclist_ind])[0]
  endelse
help, min_num_cts_threshold
  
  ;; ------------------------------------------------------------------------
  ;; Identify the emap/model pixels that fall in this source's extraction aperture.
  ;; For speed we have to apply ContainsPoints() on a sub-region of the emap.
  ae_ds9_to_ciao_regionfile, region_fn, temp_region_fn, /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y

  bbox_ind = where((emap_pixel_x GE (min(polygon_x)-1)) AND (emap_pixel_y GE (min(polygon_y)-1)) AND $
                   (emap_pixel_x LT (max(polygon_x)+1)) AND (emap_pixel_y LT (max(polygon_y)+1)) )
  
  bbox_pixel_x = emap_pixel_x[bbox_ind]
  bbox_pixel_y = emap_pixel_y[bbox_ind]
  
  o_poly=obj_new('IDLanROI', polygon_x, polygon_y)

  aperture_ind = where((o_poly->ContainsPoints(bbox_pixel_x,bbox_pixel_y) EQ 1), aperture_count)
  if (aperture_count EQ 0) then begin
    print, 'WARNING: extraction region is smaller than one emap pixel.'
    dum = min(sqrt((bbox_pixel_x-X_CAT[ii])^2 + (bbox_pixel_y-Y_CAT[ii])^2),aperture_ind)
    aperture_count = 1
  endif
  
  aperture_ind = bbox_ind[aperture_ind]
  
  ; The exposure-area of the extraction region is not well estimated by the pixelized aperture represented by aperture_ind,
  ; so as in AE we multiply an accurate geometric area of the polygon by the mean emap value; units are (sec*cm^2*skypixel^2).
  mean_exposure    = mean(emap[aperture_ind], /DOUBLE)
  src_exposurearea = float(SRC_AREA[ii]*mean_exposure)
  
  ; This correction from the pixelized aperture area to the real extraction region area 
  ; must be used later where we again are trying to integrate something over the extraction region.
  aperture_pixelization_correction = SRC_AREA[ii] / ((skypix_per_emappix)^2 * aperture_count)
;help, aperture_count, aperture_pixelization_correction
  
  ;; ------------------------------------------------------------------------
  ;; Decide which region (pixels) around the current source should be eliminated from 
  ;; consideration for the background region for that source.
  ;; At a minimum it seems obvious we want to eliminate the extraction region itself.
  ;; Since our aperture is pixelated we test whether any of several positions in each pixel
  ;; are interior to the polygon.
  masked_emap = emap
  
  pixel_status = replicate(null_status, emap_col_dim, emap_row_dim)
  pixel_status[aperture_ind] = masked_status
  masked_emap [aperture_ind] = 0
  
  hs = abs(cdeltP[0]) * 0.3   ; "half-size" of emap pixel, in skypix units
  
  is_inside = (o_poly->ContainsPoints(bbox_pixel_x+hs,bbox_pixel_y+hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x-hs,bbox_pixel_y+hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x-hs,bbox_pixel_y-hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x+hs,bbox_pixel_y-hs) EQ 1)
  
  mask_ind = where(is_inside, mask_count)
  if (mask_count GT 0) then begin
    pixel_status[bbox_ind[mask_ind]] = masked_status
    masked_emap [bbox_ind[mask_ind]] = 0
  endif

  obj_destroy,o_poly 

  
  ;; We don't really want to apply aggressive masking to the current source because we may
  ;; eliminate regions which are valuable for the goal of minimizing background bias.
  ;; However we have the additional goal of not including too many counts from the current source 
  ;; to fall in its own background region.
  ;; It's not obvious how to balance these desires.  A simple ad hoc penalty we'll adopt is
  ;; to set the model contamination (goal) for the current source to zero; then any source counts
  ;; that fall in the background region act as a penalty.  
  ;; This is done in code found a bit later.

  ;; ------------------------------------------------------------------------
  ;; Nominate a set of initial background pixel candidates consisting of two subsets.
  ;; First, we want to nominate all the pixels adjacent to masked pixels, since for severe
  ;; crowding those close-in pixels are often the best choices.
  ;; Second we want to nominate a grid of pixels covering a broad surrounding area to give
  ;; the search lots of choices, including some far from the source. 
  ;; By using multiple seeds we allow discontiguous regions, but there's nothing wrong with that
  
  index_to_point, where(pixel_status EQ masked_status), temp_col, temp_row, size(pixel_status)
  temp_col = [temp_col+1,temp_col+0,temp_col-1,temp_col+0]
  temp_row = [temp_row+0,temp_row+1,temp_row+0,temp_row-1]
  
  ; Calculate the 0-based column/row position of the source.
  ad2xy, RA[ii], DEC[ii], emap2wcs_astr, src_column, src_row
  
  ; Define a "postage stamp" region around the source, for some VERBOSE displays we show.
  ps_cmin = 0 > round(src_column-(3 * MSK_RAD[ii] / skypix_per_emappix))
  ps_rmin = 0 > round(src_row   -(3 * MSK_RAD[ii] / skypix_per_emappix))
  ps_cmax = round(ps_cmin+(6 * MSK_RAD[ii] / skypix_per_emappix)) < (emap_col_dim-1)
  ps_rmax = round(ps_rmin+(6 * MSK_RAD[ii] / skypix_per_emappix)) < (emap_row_dim-1)

  ; Define a region in which candidate pixel "seeds" will be generated.
  seed_radius = 10 > (2 * MSK_RAD[ii] / skypix_per_emappix)    ; units are array indexes      
  
  nominated_candidates     = replicate({col:0, row:0}, n_elements(temp_col)+n_elements(seed_grid_ii))
  nominated_candidates.col = [temp_col,round(src_column + seed_radius * seed_grid_ii)]
  nominated_candidates.row = [temp_row,round(src_row    + seed_radius * seed_grid_jj)]
  num_candidates = 0L
 
;plot, nominated_candidates.col, nominated_candidates.row, PSYM=3, /YNOZ
  
  ;; ------------------------------------------------------------------------
  ;; To speed up the algorithm, define a search domain that is a subset of the full observation.
  ;; We process only those models that overlap this domain, and we restrict the 
  ;; background region to lie in this domain.
  ;;
  ;; We want this domain to encompass a certain minimum observed area on the sky (1.5x1.5 arcmin^2)
  half_dim = (0.75 * 60) / (arcsec_per_skypixel * skypix_per_emappix)  ; half-width of nominal domain (emap pixel units)
  min_num_active_pixels = (2*half_dim)^2                               ; area of nominal domain

  domain = {col_min:floor(src_column-half_dim) > 1, $
            row_min:floor(src_row   -half_dim) > 1,    $
            col_max: ceil(src_column+half_dim) < (emap_col_dim-2),    $
            row_max: ceil(src_row   +half_dim) < (emap_row_dim-2)}
  step_size = 0
  repeat begin
    domain.col_min -= step_size
    domain.row_min -= step_size
    domain.col_max += step_size
    domain.row_max += step_size
    ; For convenience later, this domain stays 1 pixel away from the emap edges
    domain.col_min >= 1
    domain.row_min >= 1
    domain.col_max <= (emap_col_dim-2)
    domain.row_max <= (emap_row_dim-2)
  
    emap_window = masked_emap[domain.col_min:domain.col_max, domain.row_min:domain.row_max]
    num_active_pixels = total(/INTEGER, emap_window GT 0)
    
    step_size = 1 > ceil((min_num_active_pixels - num_active_pixels) / ((domain.col_max-domain.col_min)+(domain.row_max-domain.row_min)))
  endrep until (num_active_pixels GE min_num_active_pixels)

  print, num_active_pixels * (arcsec_per_skypixel * skypix_per_emappix / 60)^2, num_active_pixels,$
         F='(%"Bkg region domain is %4.1f arcmin^2 (%d emap pixels)")'

  ; The set of models we work with include those whose footprints intersect the domain, AND
  ; the source we're processing (models[ii])!
  model_in_domain = (domain.col_max GE models.col_min) AND $
                    (domain.col_min LE models.col_max) AND $
                    (domain.row_max GE models.row_min) AND $
                    (domain.row_min LE models.row_max)
  model_in_domain[ii] = 1
  
  domain_ind    = where(model_in_domain, num_models)
  domain_models = models[domain_ind]
  
  ; Identify the source we're processing in this sub-list of models, and defensively confirm.
  self_index = (where(domain_ind EQ ii))[0] 

  self_model = domain_models[self_index]
  if (self_model.LABEL NE LABEL[ii]) then message, 'Bug in logic!'

  ;; ------------------------------------------------------------------------
  ;; Estimate the contamination from each source model falling within the extraction aperture
  ;; by integrating each source model over the aperture pixels (col,row).
  index_to_point, aperture_ind, col, row, size(emap)
                 
  aperture_model_counts = fltarr(num_models)
  for jj=0,num_models-1 do begin
    this_model = domain_models[jj]
    ind = where( (col GE this_model.col_min) AND (col LE this_model.col_max) AND $
                 (row GE this_model.row_min) AND (row LE this_model.row_max), count )

    if (count GT 0) then begin
      model_inside_aperture     = (*this_model.data_ptr)[col[ind]-this_model.col_min, row[ind]-this_model.row_min]
      aperture_model_counts[jj] = total(model_inside_aperture,/DOUBLE) * aperture_pixelization_correction
    endif
  endfor ;jj
  
  ; As described earlier, set the model contamination (goal) for the current source to zero; 
  ; then any source counts that fall in the background region act as a penalty.  
  aperture_model_counts[self_index] = 0

  aperture_model_counts_total = total(aperture_model_counts)

  ;; ------------------------------------------------------------------------
  ;; Form a cumulative distribution of exposure as a function of distance from the source.
  ;; Distance is in units of skypix.
  ;; We need to integrate over our search domain in the real exposure map rather than 
  ;; analytically integrate over a disk because the FOV has edges.
  
  dist_circle, array_distance, [size(emap_window,/DIM)], src_column - domain.col_min, src_row - domain.row_min
               ; units are emap pixels
  sort_ind = sort(array_distance)
  
  ; The units of cumulative_pixel_distance are skypixels.
  ; The units of cumulative_exposurearea are (sec*cm^2*skypixel^2).
  sorted_pixel_distance         = skypix_per_emappix   * array_distance[sort_ind]   
  cumulative_pixel_distance     =                        total(/CUMULATIVE, /DOUBLE, sorted_pixel_distance)
  cumulative_exposurearea       = skypix_per_emappix^2 * total(/CUMULATIVE, /DOUBLE, emap_window[sort_ind])
  cumulative_exposurearea_index = 0L
  
  
  ;; =====================================================================
  ;; Define a data structure to hold emap pixels that are candidates to be added to the background region.
  ;; (col,row) are array index coordinates of the pixel.
  ;; pixel_exposurearea is the integral of the emap over the pixel (sec*cm^2*skypixel^2).
  ;; distance is distance from the source in units of skypix.
  ;; model_counts is the integral of all the stellar models over this pixel
  ;; Some of the data types in this structure are chosen for speed in the inner loop later.
  candidate_template = { $
    ; Properties of the pixel itself.
    col:0, row:0, pixel_exposurearea:0D, distance:0D, model_counts:fltarr(num_models), $
      
    ; Properties of the current bkg region expanded by adding this pixel.
    ; "bkg_distance_sum" and "bkg_exposurearea" are DOUBLE because they are running sums over 
    ; the tags "pixel_exposurearea" and "distance".
    ; Everything else is FLOAT to try to speed up the code.
    bkg_distance_sum:0D, bkg_exposurearea:0D, $      
    bkg_scaling:0.0, bkg_scaling_correction:0.0, $                                   
    bkg_model_counts_total:0.0, subtracted_model_counts_total:0.0, $
    bkg_flat_counts:0.0, aperture_flat_counts:0.0, subtracted_flat_counts:0.0, $
    spectral_corruption:0.0, compactness_bias:0.0   }
  
  ; Discard any existing candidate store (CS) data structure.
  CS = 0  &  dum = temporary(CS)

  ;; ------------------------------------------------------------------------
  ;; Iteratively add pixels to the background region until we've found enough counts.
  ;; As a failsafe we stop the loop after n_elements(emap)/10 iterations.
  criteria = bytarr(5)
  bkg_data_counts_total  = 0L
  current_region_metric = 0
  
  ; One pixel candiate structure, named R, is used to store several cumulative bkg region
  ; properties: bkg_exposurearea, bkg_distance_sum.
  ; Vector cumulative properties, bkg_model_counts and subtracted_model_counts, are stored in variables,
  ; R_bkg_model_counts and R_subtracted_model_counts, to save storage.
  R = candidate_template
  R_bkg_model_counts = fltarr(num_models)  ;It's important to be float, not double, for speed in the inner loop later.
       
  for num_bkg_pixels = 0L, n_elements(emap)/10 do begin
    ;; ------------------------------------------------------------------------
    ;; Try to add nominated candidate pixels to the candidate store.
    for ll=0,n_elements(nominated_candidates)-1 do begin
      col = nominated_candidates[ll].col
      row = nominated_candidates[ll].row

      if (col LT domain.col_min) || (col GT domain.col_max) ||  $
         (row LT domain.row_min) || (row GT domain.row_max) then continue
      
      if (pixel_status[col,row] EQ null_status) then begin
;        ; For speed we will discard a nominated candidate pixel if it is adjacent
;        ; to an existing candidate pixel.
;        if (pixel_status[col+1,row  ] EQ candidate_status) || $
;           (pixel_status[col-1,row  ] EQ candidate_status) || $
;           (pixel_status[col  ,row+1] EQ candidate_status) || $
;           (pixel_status[col  ,row-1] EQ candidate_status) then continue
      
        ; We're not interested in candiates where the emap is tiny.
        if (exposurearea_map[col,row] LT exposurearea_map_floor) then continue
        
        pixel_status[col,row] = candidate_status
        
        ; Add the pixel to the candidate store.
        pixel = candidate_template
        pixel.col = col
        pixel.row = row
        pixel.pixel_exposurearea = exposurearea_map[col,row] ; (sec*cm^2*skypixel^2)
        pixel.distance           = sqrt((col - src_column )^2 + (row - src_row)^2) * skypix_per_emappix  ;skypix
        
        ; Look up the number of counts each source model predicts for this pixel.
        ind_models_applicable = where( (col GE domain_models.col_min) AND (col LE domain_models.col_max) AND $
                                       (row GE domain_models.row_min) AND (row LE domain_models.row_max), num_models_applicable )
        
        if (num_models_applicable GT 0) then begin
          model_counts = fltarr(num_models)
        
          for jj=0,num_models_applicable-1 do begin
            ind        = ind_models_applicable[jj]
            this_model = domain_models[ind]
            
            model_counts[ind] = (*this_model.data_ptr)[col-this_model.col_min, row-this_model.row_min]
          endfor ;jj
          
          pixel.model_counts    = temporary(model_counts)
        endif
        
        
        ; Enlarge the candidate store if necessary.
        ; Because we're later doing a very large number of vector operations on 
        ; CS (for efficiency), it's important to size this array 
        ; not too much larger than it needs to be.
        if (num_candidates EQ n_elements(CS)) then begin
          temp = replicate(candidate_template, num_candidates + (10 > 0.05*num_candidates > n_elements(nominated_candidates)))
          
          if (num_candidates GT 0) then temp[0] = CS
          CS = temporary(temp)
         ;print, 'CS enlarged to ', n_elements(CS)
        endif
        
        CS[num_candidates] = pixel
        num_candidates++
      endif ; creating a new candidate pixel
    endfor ;ll

    if (num_candidates LE 0) then begin
      print, num_bkg_pixels * (arcsec_per_skypixel * skypix_per_emappix / 60)^2, num_bkg_pixels, F='(%"WARNING: ran out of candidate pixels for background; accepting a region with area %4.1f arcmin^2 (%d emap pixels).")'
      break
    endif


    ;; ------------------------------------------------------------------------
    ;; We need some mechanism for the metric to favor compactness of the background region
    ;; since, lacking any specific information about the unmodeled background component,
    ;; it seems the most one can say is that the background in the source aperture has better
    ;; correlation with nearby background pixels than it does with distant ones.
    ;;
    ;; If one assumed a model of this background component, then of course one could simply 
    ;; add another goal of minimizing the background subtraction bias for this component.
    ;; I don't know what to assume however, so I've decided to arbitrarily adopt a compactness
    ;; metric which is
    ;;    INTEGRAL of emap over bkg region{ distance_i }
    ;;    -------------------------------------------          minus 1.0
    ;;    INTEGRAL of emap over compact region{ distance_i }
    ;; Where the "compact region" is an annulus with the same exposure-area.
    ;;
    ;; This gives me a metric that is zero for a compact region, and 1.0 when the region is
    ;; "twice as fat" as it could be.  Totally ad hoc, I know!
    ;;
    ;; It would be a lot faster to estimate compact_region_integral_distance by the expression 
    ;;   INTEGRAL{ r * (2*!PI*dr) }, r=0...disk_radius
    ;; where disk_radius is chosen so the disk and actual background regions have the same
    ;; GEOMETRIC area.  However at the field edge such a disk-shaped compact region would fall off
    ;; the exposure map, giving the disk far less exposure-area than the actual background region.
    ;; This we're forced to actually integrate the emap over a disk-shaped region.
    
    while (cumulative_exposurearea[cumulative_exposurearea_index] LE R.bkg_exposurearea) do begin
      cumulative_exposurearea_index++
      if (cumulative_exposurearea_index GE n_elements(cumulative_exposurearea)) then begin
        print, 'WARNING: growth of background region was stopped when an implementation limit was reached.'
        GOTO, REGION_COMPLETE
      endif
    endwhile
    
    ;disk_radius = sorted_pixel_distance[cumulative_exposurearea_index]           ; units of skypix
    
    compact_region_integral_distance = float(cumulative_pixel_distance[cumulative_exposurearea_index]) 
    
   
    ;; ------------------------------------------------------------------------
    ;; There are a lot of confusing variable names required, and they are different
    ;; from the names used in the manual.
    ;;
    ; We have observed scalar quantities, before and after scaling:
    ; bkg_data_counts_total   = N   : counts observed in bkg region
    ;
    ; We have modeled vector quantities with num_models elements, before and after scaling.
    ; aperture_model_counts   : contaminate counts predicted by models in src aperture
    ;                           Sum of this vector is aperture_model_counts_total = Bp
    ; bkg_model_counts        : contaminate counts predicted by models in bkg region  
    ;                           Sum of this vector is this.bkg_model_counts_total = Np
    ; subtracted_model_counts : bkg_model_counts with bkg scaling applied 
    ;                           Sum of this vector is subtracted_model_counts_total = Np * S 
    ;    
    ; We have some derived quanties.
    ; bkg_flat_counts         = Nf     : inferred flat bkg component counts in bkg region 
    ; aperture_flat_counts    = Bf     : inferred flat bkg component counts in aperture
    ; photometric_bias        = DELTAp : Bp - Np*S
    ; bkg_scaling             = S    : background scaling
    ; bkg_scaling_correction  = c    : correction to scaling
    ;                                                        
    ;                   
    ; Quantities associated with the current bkg region use no prefix in their name; temporary
    ; updates to these quantities associated with the candidate region at hand use the "this_" prefix.

    ;; ------------------------------------------------------------------------
    ;; For each pixel that is a candiate for acceptance into the background region, 
    ;; evaluate the region metric we would find if we accepted that pixel.
    ;; EFFICIENCY INSIDE THIS LOOP IS CRITICAL TO PERFORMANCE!!
    ;; WE'VE OMITTED THE /DOUBLE OPTION TO THE TOTAL() CALLS AND USED SOME CODE
    ;; CONSTRUCTS THAT GAIN SPEED BUT LOSE CLARITY.
    ;; When thinking about execution speed, keep in mind that the following variables
    ;; are vectors with num_models elements:
    ;;   aperture_model_counts
    ;;   bkg_model_counts, this.model_counts, this_bkg_model_counts
    ;;   this_subtracted_model_counts
    ;;   photometric_bias
    
;t1=systime(1)


; In debugger, verify there are no type conversions being done (e.g. involving constants) 
; and that there are no needless
; DOUBLE computations (although I do not know if that actually matters for speed).
; 
; Look for intermediate vars to eliminate.
; Look at parentheses.
; Look for constants to pre-compute.
; 
; Think about cache memory usage, and whether use of temporary() can free up memory.

    ;; For efficiency we perform vector calculations on the entire candidate pixel store (CS).
    ;; It's true that CS contains some unused elements, which waste 
    ;; CPU resources, but we assume that num_candidates generally grows almost monotonically
    ;; and we are careful to expand CS only by a little each time.
    
    ; Several global cumulative quantites are incremented/updated here for the candidate pixel
    ; we're considering, and stored for later retrieval if we choose this candidate.
    ; IT IS VERY IMPORTANT, however, that the total number of counts OBSERVED in the background,
    ; bkg_data_counts_total, is NOT updated before this candidate is judged.
    ; We must evaluate the candidates based solely on our MODELS of the contaminating
    ; background components, not considering what DATA the candidate pixel happens to contain.
    ; There are cases where all enlargements of the region are bad, and if we consider what data
    ; lies in the candidates then the algorithm will tend to favor pixels with no data, which
    ; introduces a horrible bias to our search.
    CS.bkg_distance_sum = (R.bkg_distance_sum + CS.distance)
    CS.bkg_exposurearea = (R.bkg_exposurearea + CS.pixel_exposurearea)

    ; Compute the exposure-area ratio, i.e. the nominal background scaling.
    ; Be sure that both terms are in units of (sec*cm^2*skypixel^2).
    CS.bkg_scaling      = src_exposurearea / float(CS.bkg_exposurearea)  

    ; Estimate how many counts from each contaminating source are statistically expected 
    ; to be in the proposed background region, and how many are expected to be subtracted.
    ; Note that one of these array elements represents "self", i.e. how much light from 
    ; the current source will be (mistakenly) subtracted.
    nx = N_elements(R_bkg_model_counts)
    ny = N_elements(CS)
    bkg_model_counts_2d              = rebin(reform(R_bkg_model_counts,    nx, 1,/OVERWRITE), nx, ny, /SAMPLE) + CS.model_counts
    CS.bkg_model_counts_total        = total(bkg_model_counts_2d,1)
    
    subtracted_model_counts_2d       = rebin(reform(CS.bkg_scaling,        1, ny,/OVERWRITE), nx, ny, /SAMPLE) * temporary(bkg_model_counts_2d)
    
    aperture_model_counts_2d         = rebin(reform(aperture_model_counts, nx, 1,/OVERWRITE), nx, ny, /SAMPLE)
    
          
    ; Compute a spectral corruption metric that judges how fairly all the background components are represented
    ; in the background regions we are considering.
    ; This is tricky, and vital to the success of the search.
    ; Many approaches are possible.  We've tried computing this metric after the background has been
    ; adjusted to remove photometric bias, but found very unsatisfactory results (e.g. the algorithm
    ; sometimes latches onto power from distant sources, which it then has to scale down.)
    ;
    ; Thus, we choose to compute the metric using the nominal scaling.
    
    ; The denominator of the metric (its normalization) turns out to be problematic.
    ; (We need a normalization in order to reasonably combine this metric with the compactness metric.)
    ; * It turns out that it must be a constant (i.e. not varying between candidates) in order to avoid pathological behavior in the search.
    ; * If we allow the normalization to become zero, then the metric becomes infinity for all candidates and we lose
    ;   our power to guide the search.
    ;   Thus, we impose a floor of 1.0 in the normalizations below.
    CS.spectral_corruption = total(abs(temporary(aperture_model_counts_2d) - temporary(subtracted_model_counts_2d)),1) $
                             / $
                             (1 > 2.0 * aperture_model_counts_total)
    
    ; As desribed earlier, our "compactness bias" is the ratio between two integrals.
    ; Make sure it's non-negative so that the region_metric_slope calculation later is ok.
    CS.compactness_bias = (compactness_gain * ((float(CS.bkg_distance_sum) / compact_region_integral_distance) - 1.0)) > 0.0

    
    
    ;; ------------------------------------------------------------------------
    ;; Choose which candidate pixel should be accepted.  We try to strike a compromise between the goals of
    ;; minimum bias metric and compactness of the background region by including a compactness penalty in
    ;; the metric.
    ;;
    ;; We tried and rejected a scheme where the set of "good enough" metrics was defined relative to the minimum
    ;; metric in hand; this allowed the metric to creep up with each iteration unchecked.
    ;; 
    ;; We tried and rejected a scheme where we define an absolute range of "good enough" metrics; if at some point in our search
    ;; the min metric is forced out of this range, then distance will suddenly stop playing a role in 
    ;; the search.  This seems undesirable.
    ;;
    ;; Of course if the observer knows something about the structure of the background not related to
    ;; point sources, then supplying a map of that will drive the search to correctly subtract it.

    ;; If we simply accept the candidate pixel with the smallest new metric, then in situations 
    ;; where every candidate is increasing the metric we will sometimes choose candiates with low
    ;; exposure values.  This is short sighted---we're taking small steps towards badness even if
    ;; a larger step might result in a smaller RATE towards badness.
    ;; Thus, we should be picking the candidate with the best metric SLOPE with respect to exposure!
    
    ; Combine the spectral quality and compactness metrics we're trying to simultaneously minimize.
    region_metric = (CS.spectral_corruption + CS.compactness_bias)[0:num_candidates-1] 

    region_metric_slope   = (region_metric - current_region_metric) / float(CS.pixel_exposurearea)
    best_slope            = min(region_metric_slope, accept_ind)
    current_region_metric = region_metric[accept_ind]
    
    if (verbose GE 2) then begin
      ; We put this display code here because we want CS and region_metric_slope to be intact.
      temp = CS[0:num_candidates-1]
      metric = temp.spectral_corruption
      ind_sorted = sort(metric)
      
      scal_low  = metric[ind_sorted[floor(0.1*(num_candidates-1))]]
      scal_high = metric[ind_sorted[ ceil(0.9*(num_candidates-1))]]        
      
      metric_map = replicate(!VALUES.F_NAN, emap_col_dim, emap_row_dim)
     ;metric_map[temp.col, temp.row] = region_metric_slope
      metric_map[temp.col, temp.row] = metric
      ps_to_show = bytscl(metric_map[ps_cmin:ps_cmax,ps_rmin:ps_rmax], MIN=scal_low, MAX=scal_high) 
      wset, 0
      tvscl, rebin(-ps_to_show, 4*(ps_cmax-ps_cmin+1), 4*(ps_rmax-ps_rmin+1), /SAMPLE)
    endif
    

    ;; Accept the chosen pixel into the background region.
    ;; The running sum R_bkg_model_counts is maintained outside the candidate structures.
    R = CS[accept_ind]
    R_bkg_model_counts       += R.model_counts
    pixel_status[R.col,R.row] = background_status
    
    
    ;; As described earlier, the total observed counts in the background, bkg_data_counts_total,
    ;; must NOT be updated in the inner loop prior to evaluation of the candidate.
    ;; Thus we do that here, when the candidate is accepted.
    bkg_data_counts_total    += inband_data[R.col, R.row]

    ;; Remove the chosen pixel from candiate store and metric vector.
    ;; We simply overwrite the chosen pixel with the last one in the list, and
    ;; shorten the list.
   ;region_metric      [accept_ind] = region_metric      [num_candidates-1]
   ;region_metric_slope[accept_ind] = region_metric_slope[num_candidates-1]
    CS[accept_ind] = CS[num_candidates-1]
    num_candidates = num_candidates-1
    
    ;; Nominate new candiates which are adjacent to the background pixel just accepted.
    if (n_elements(nominated_candidates) NE 4) then $
      nominated_candidates = replicate(nominated_candidates[0], 4)
    nominated_candidates.col = (R.col + [1,0,-1,0])
    nominated_candidates.row = (R.row + [0,1,0,-1])

    
    ;; ------------------------------------------------------------------------
    ;; Correct the background scaling (see derivation in the AE manual) to eliminate photometric bias.
    
    ; We need below the subtracted total if NOMINAL scaling were used.
    R.subtracted_model_counts_total = R.bkg_model_counts_total * R.bkg_scaling

    ; Infer how many observed counts in the bkg region are not attributable to our models, i.e.
    ; are from a "flat" component.
    R.bkg_flat_counts      = 0.0 > (bkg_data_counts_total - R.bkg_model_counts_total)
    R.aperture_flat_counts = R.bkg_flat_counts * R.bkg_scaling
      
    if (bkg_data_counts_total LE 0) then begin
      ; When there is no observed background data then we cannot compute any "correction" to the background scaling
      R.bkg_scaling_correction = 1.0

    endif else begin
      ; Compute a "photometric bias" with respect to the modeled contaminating sources, 
      ; i.e. the total counts we think are in the aperture minus the total counts we 
      ; think will be subtracted by the proposed region, assuming nominal scaling.
      photometric_bias          = aperture_model_counts_total - R.subtracted_model_counts_total
      
      ; We have a complication when the models show zero counts expected in the aperture
      ; (aperture_model_counts_total EQ 0) and we estimate no flat component (bkg_flat_counts EQ 0).
      ; In this case a bkg region containing any model counts must be scaled to zero in order to 
      ; eliminate photometric bias.
      ; We avoid this silly result by placing an arbitrary lower limit on the scaling.
      ; We also place an arbitrary upper limit to be conservative.
      R.bkg_scaling_correction = 0.1 > (1.0 + (temporary(photometric_bias) $
                                                / $
                                                ((R.bkg_model_counts_total+R.bkg_flat_counts) * R.bkg_scaling))) $
                                     < 10.0
    endelse ; (bkg_data_counts_total GT 0)
 
    
    ;; ------------------------------------------------------------------------
    ;; Apply the correction to the scaling, and recompute quantities that depend on the scaling.
    
    ; The quantity R.aperture_flat_counts is NOT recomputed; it is a function of the NOMINAL scaling!
    R.bkg_scaling                  *= R.bkg_scaling_correction

    ; The "flat" component would, by definition, be correctly subtracted if we used the nominal scaling.
    ; Thus, the actual flat component we will be subtracting is related to what we wanted to 
    ; subtract by the bkg_scaling correction we have adopted.    
    R.subtracted_flat_counts        = R.aperture_flat_counts * R.bkg_scaling_correction
    
    ;; Some of the vector quantities that were computed earlier for the winning
    ;; candiate were not saved in the candidate structure (for efficiency reasons).
    ;; These must be recomputed here.
    R_subtracted_model_counts       = R_bkg_model_counts       * R.bkg_scaling
    R.subtracted_model_counts_total = R.bkg_model_counts_total * R.bkg_scaling
    
    
    ;; ------------------------------------------------------------------------
    ;; Evaluate the expected (modeled) quality of the rescaled background region, 
    ;; i.e. how well it represents all the bkg components.
   
    ; Compute a spectral corruption metric as simply the fraction of observed bkg data that we think
    ; comes from the "wrong" background component.
    ; In the expressions below we compute the biases (model - subtracted) for each bkg component, including
    ; the flat component, then sum the absolute values of those biases, and then divide by two to handle
    ; double counting, and then normalize by the total counts expected in the aperture.
    R.spectral_corruption = (       (abs(R.aperture_flat_counts  - R.subtracted_flat_counts ) $
                             + total(abs(  aperture_model_counts - R_subtracted_model_counts),1))) $
                            / $
                            (1 > 2.0 * ( R.aperture_flat_counts + aperture_model_counts_total)) 
                   
    
    ;; Print some status.
    if (verbose GE 2) then begin
      ps_to_show = pixel_status[ps_cmin:ps_cmax,ps_rmin:ps_rmax]
      wset, 1
      tvscl, rebin(ps_to_show, 4*(ps_cmax-ps_cmin+1), 4*(ps_rmax-ps_rmin+1), /SAMPLE)
                                       
      if (verbose GE 3) then begin
        ; Save and plot the three terms of the metric:
        term1[num_bkg_pixels] = R.bkg_scaling
        term2[num_bkg_pixels] = R.spectral_corruption
        term3[num_bkg_pixels] = aperture_model_counts_total - R.subtracted_model_counts_total
        wset, 2
        ind_min = 0 > (num_bkg_pixels-200)
        y1 = term1[ind_min:num_bkg_pixels]
        y2 = term2[ind_min:num_bkg_pixels]
        y3 = term3[ind_min:num_bkg_pixels]
        plot,  y1, PSYM=1, YRANGE=minmax([y1,y2,y3]) ; +        is scaling   
        oplot, y2, PSYM=2                            ; *        is spectral corruption
        oplot, y3, PSYM=4                            ; diamond  is photometric bias

        print, bkg_data_counts_total, F='(%"\n%d counts in bkg region")'
        if (verbose GE 4) then begin
          flag = (abs(aperture_model_counts) GT 0.1) OR (abs(R_subtracted_model_counts) GT 0.1)
          ;flag[self_index] = 0
          ind = where(flag, count)
          if (count GT 0) then begin
            forprint, domain_models.LABEL, aperture_model_counts - R_subtracted_model_counts, SUBSET=ind, F='(%"%6s %7.2f")'
          endif
        endif
      endif ;(verbose GE 3)
      print
      print, R.bkg_scaling_correction, F='(%"scaling correction  =%5.2f")'
      print, R.spectral_corruption,    F='(%"spectral corruption =%5.2f")'
      print, R.compactness_bias,       F='(%"compactness metric  =%5.2f")'
  ;      print, 'accepted metric:', (current_region_metric)
    endif ;(verbose GE 2)
 
    
    ;; Evaluate the stopping criteria for defining the background region.

    ; We require a minimum background region area ratio to avoid absurdly tiny regions.
    ; Hopefully this requirement will get us past the early iterations where we risk
    ; noise problems in the various quantities because the unit of area added to the region 
    ; (1 pixel) is large with respect to the current region.
    criteria[0] = ((R.bkg_exposurearea/src_exposurearea) GE min_exposure_ratio)
    
    ; Once that is satisfied, a violation of the spectral corruption threshold is enough to stop the search.
    ; However, this metric is noisy early in the search when we're dealing with small numbers of observed
    ; counts ...
    criteria[1] = (R.spectral_corruption GT spectral_corruption_threshold)
    
    ; If neither of those stops us, then we continue until we feel the Poisson errors in the
    ; background are reasonably small.  This is judged in two ways; both must be satisfied:
    ; A. We require that the statistical uncertainty in the background subtraction should be
    ;    much smaller than the statistical uncertainty in the counts extracted.
    ;    In other words, of the two noise terms in AE's calculation of NET_CNTS_SIGMA_UP,
    ;    the term from the extraction aperture should dominate.
    src_cnts_error        = (1 + sqrt(SRC_CNTS[ii]    + 0.75))
    bkg_subtraction_error = (1 + sqrt(bkg_data_counts_total + 0.75)) * R.bkg_scaling

    criteria[2] = (src_cnts_error  GT 4*bkg_subtraction_error)
    
    ; B. AE is going to use this background to compute the PROB_NO_SOURCE statistic.
    ;    We could propagate the confidence interval on bkg_data_counts_total through the
    ;    computation to get a confidence interval for PROB_NO_SOURCE, but we don't see
    ;    how to define an obvious stopping criteria from that.
    ;    Instead, we'll punt and force the observer to specify a minimum number of counts
    ;    in the background region.
    criteria[3] = (bkg_data_counts_total GE min_num_cts_threshold)
    
    ; Finally, we find that to avoid backgrounds with very few counts we must impose a strict
    ; lower limit, arbitrarily set to 10 counts. 
    ; This requirement can of course drive the spectral corruption over its threshold.
    criteria[4] = (bkg_data_counts_total GE 10)
                  
    ; When we decide we want to stop, we are faced with the problem of computing the
    ; final scaling correction, and final region metrics.
    ; Rather than repeating that complex code, we play the trick of forcing one final
    ; iteration of this loop, using a "null" candidate pixel that does not change anything.
    if criteria[0] && (criteria[1] || (criteria[2] && criteria[3])) && criteria[4] then begin
      break
    endif
    
    ; To catch infinite looping we look for NaN values used in the stopping criteria.
    vital_values = [src_cnts_error, bkg_subtraction_error, R.bkg_scaling, min_exposure_ratio, bkg_data_counts_total]
    if (~array_equal(finite(vital_values),1)) then begin
      message, 'ERROR: NaN value found in stopping criteria'
      GOTO, CLEANUP
    endif
    
  endfor  ; num_bkg_pixels loop adding pixels to background region

REGION_COMPLETE:

  PROB_NO_SOURCE_binomial = binomial(SRC_CNTS[ii], $
                                     SRC_CNTS[ii] + bkg_data_counts_total, $
                                     R.bkg_scaling / (R.bkg_scaling+1D) , /DOUBLE) > 0
  
  PROB_NO_SOURCE_poisson  = (1 - poisson_distribution(bkg_data_counts_total*R.bkg_scaling, SRC_CNTS[ii] - 1)) > 0
    
  dt = systime(1)-t0
  print, num_bkg_pixels, dt, F='(%"Accepted %6d background pixels in %5d seconds.")' 
  processing_rate[ii] = num_bkg_pixels/dt
  
  
  ;; Summarize the quality of the background region.

  ; Some of the vector quantities that were computed earlier for the winning
  ; candiate were not saved in the candidate structure (for efficiency reasons).
  ; These must be recomputed here.
  R_subtracted_model_counts = R_bkg_model_counts * R.bkg_scaling
    
  print, bkg_data_counts_total, R.bkg_exposurearea/src_exposurearea, 1/R.bkg_scaling, F='(%"\nBackground region: %d in-band background counts; actual exposure ratio =%6.1f; adopted exposure ratio =%6.1f")' 
  
  print, SRC_CNTS[ii], src_cnts_error,                    F='(%"SRC_CNTS                              =%7.1f (+-%5.1f)")' 

  print, bkg_data_counts_total*R.bkg_scaling, bkg_subtraction_error, F='(%"bkg counts in aperture                = %6.1f (+-%5.1f)")' 

  print, PROB_NO_SOURCE_binomial, PROB_NO_SOURCE_poisson, F='(%"PROB_NO_SOURCE, actual and asymptotic: %8.2g %8.2g")'
  
  print, "PREDICTED BIAS FROM CONTAMINATING SOURCES:"
  print, "     contaminating source label"
  print, "     |     PSF normalization"
  print, "     |     |     predicted to be in extraction aperture"
  print, "     |     |     |     predicted to be subtracted by background region"
  print, "     |     |     |     |"
  print, round(domain_models[self_index].counts_in_model), R_subtracted_model_counts[self_index], F='(%"  self %5d     0 %5.1f  (counts)")'
  print, R.aperture_flat_counts, R.subtracted_flat_counts,                   F='(%"  flat       %5.1f %5.1f")'

  flag = (abs(aperture_model_counts) GT 0.1) OR (abs(R_subtracted_model_counts) GT 0.1)
  flag[self_index] = 0
  ind = where(flag, count)
  if (count GT 0) then begin
    forprint, domain_models.LABEL, round(domain_models.counts_in_model), (aperture_model_counts), (R_subtracted_model_counts), SUBSET=ind, F='(%"%6s %5d %5.1f %5.1f")'
  endif
  print, R.aperture_flat_counts+aperture_model_counts_total, R.subtracted_flat_counts+total(R_subtracted_model_counts), F='(%"-------------------------\ntotal:       %5.1f %5.1f")'
  print, R.spectral_corruption,  F='(%"spectral corruption =%6.2f")'
  print, R.compactness_bias,     F='(%"compactness metric  =%6.2f")'

  
  ;; Ok, a background region is now defined to be the pixels where pixel_status[col,row] EQ background_status.
  region_index = where(pixel_status EQ background_status)
  index_to_point, region_index, col, row, size(pixel_status)

  ; Make a ds9 region file which marks the masked and background region pixels.
  openw,  region1_unit, bkg_pixels_region_fn, /GET_LUN
  printf, region1_unit, F='(%"# Region file format: DS9 version 3.0 \nglobal color=blue \nJ2000")'
  !TEXTUNIT = region1_unit  
  xy2ad, col, row, emap2wcs_astr, ra_pt, dec_pt
  forprint, TEXTOUT=5, /NoCOM, ra_pt, dec_pt, F='(%"cross point %10.6f %10.6f # tag={bkg region}")'
  
  index_to_point, where(pixel_status EQ masked_status), temp_col, temp_row, size(pixel_status)
  xy2ad, temp_col, temp_row, emap2wcs_astr, ra_pt, dec_pt
  forprint, TEXTOUT=5, /NoCOM, ra_pt, dec_pt, F='(%"x point %10.6f %10.6f # tag={mask region} color=red")'
  
  free_lun, region1_unit

  if (verbose GE 1) then begin
    cmd1 = string(my_ds9,                       F='(%"xpaset -p %s regions delete all")') 
    cmd2 = string(my_ds9, bkg_pixels_region_fn, F='(%"xpaset -p %s regions load %s")')
    cmd3 = string(my_ds9, catalog_region_fn,    F='(%"xpaset -p %s regions load %s")')
    cmd4 = string(my_ds9, RA[ii], DEC[ii],      F='(%"xpaset -p %s pan to %10.6f %10.6f wcs fk5 degrees")')
    run_command, /QUIET, [cmd1,cmd2,cmd3,cmd4]
  endif  


  ;; Write the corresponding cropped background exposure map to a file.
  ;; WE CANNOT USE HEXTRACT BECAUSE THE PHYSICAL COORDINATE SYSTEM (AND WHO KNOWS WHAT ELSE) ARE NOT UPDATED!!!
  bkg_emap               = fltarr(emap_col_dim,emap_row_dim)
  bkg_emap[region_index] = emap[region_index] 
  bkg_emap_header = emap_header

  writefits, temp_image_fn, bkg_emap, bkg_emap_header


  cmd = string(temp_image_fn, 1+min(col), 1+max(col), 1+min(row), 1+max(row), bkg_emap_fn, F="(%'dmcopy ""%s[#1=%d:%d,#2=%d:%d]"" %s clobber=yes')")
  run_command, cmd
 
  ;; CIAO 3.4 has a bug in dmimgpick that leads to segfaults if the image is not square.
  temp_emap = readfits(bkg_emap_fn, temp_emap_header, /SILENT)
  temp_xdim = (size(temp_emap, /DIM))[0]
  temp_ydim = (size(temp_emap, /DIM))[1]
  if (temp_xdim NE temp_ydim) then begin
    print, 'WARNING: working around dmimgpick bug in CIAO 3.4 by forcing emap to be square.'
    temp = fltarr(temp_xdim>temp_ydim, temp_xdim>temp_ydim)
    temp[0,0] = temp_emap
    writefits, bkg_emap_fn, temp, temp_emap_header
  endif
 
  
  
   ;; The set of emap pixels above define the actual background region to apply to the event list.
  ;; BELOW WE REQUIRE EMAP VALUE TO BE >1, INSTEAD OF >0, BECAUSE CIAO 3.0.1 HAS A BUG THAT 
  ;; CAUSES ZERO VALUES TO PASS THE >0 TEST!

  cmd1 = string(evtfile, bkg_emap_fn, temp_events_fn, $
                F="(%'dmimgpick ""%s[cols time,ccd_id,sky,pi,energy]"" %s %s method=closest')")

  cmd2 = string(temp_events_fn, bkg_events_fn, F="(%'dmcopy ""%s[#6>1]"" %s')")
  run_command, [cmd1,cmd2]


  ;; ------------------------------------------------------------------------
  ;; Extract background spectrum.
  ;; NOTE: if we ever implement a time filter on the background data then we must 
  ;; reduce bkg_exposurearea below by the ratio 
  ;; (EXPOSURE from bkg_spectrum_fn)/(EXPOSURE from bkg_events_fn) to account for the lost exposure.
  ;; Such time filtering would mess up MIN_NUM_CTS requirement!
  
  cmd = string(bkg_events_fn, DETCHANS, bkg_spectrum_fn, $
               F="(%'dmextract ""%s[bin pi=1:%d:1]"" %s opt=pha1 error=gaussian')")
  run_command, cmd

  
  ;; ------------------------------------------------------------------------
  ;; Save some statistics about the background region.
  
  ;; ------------------------------------------------------------------------
  ;; The AE convention is that the BACKSCAL keywords in spectrum files, derived from
  ;; integrals of the exposure map, are used to represent geometric
  ;; area, effective area, and integration time differences between the
  ;; source and background regions.  
  ;; The EXPOSURE keywords are NOT used for background scaling.  We set EXPOSURE=0
  ;; in the background spectrum as a flag to signify the AE convention is being used.
  ;;
  ;; Background scaling is a bit confusing.  
  ;; * In this code the variable bkg_scaling (S) is multiplied by the background.
  ;; * The BACKSCAL keyword in obs.stats is 1/bkg_scaling.
  ;; * However the keyword BACKSCAL in the background spectrum file represents
  ;;   the "measure" (integral of the emap) of the background region.
  ;;   It is later combined with BACKSCAL in the source spectrum file to get the
  ;;   actual scaling applied to the background spectrum.
  comment  = 'EXPOSURE not used for bkg scaling'
  comment2 = string(ENERG_LO, ENERG_HI, F="(%'total bkg intensity (photons/cm^2/sec/skypixel^2),, %5.3f:%6.3f keV')")
  comment3 = string(ENERG_LO, ENERG_HI, F="(%'flat bkg intensity (photons/cm^2/sec/skypixel^2),, %5.3f:%6.3f keV')")

; Be SURE to apply the bkg_scaling_correction that we calculated earlier!
  bkg_exposurearea_corrected = R.bkg_exposurearea / R.bkg_scaling_correction
  
  ; Write the keywords that are unique to background.pi.
  openw, unit, temp_text_fn, /GET_LUN
  printf, unit, comment, comment, comment, $
          bkg_exposurearea_corrected, '(sec*cm^2*skypixel^2); '+comment, $
          F='(%"#add\nONTIME = 0 / %s\nLIVETIME = 0 / %s\nEXPOSURE = 0 / %s\nBACKSCAL = %g / %s")'
  free_lun, unit
  
  cmd = string(bkg_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
  run_command, cmd, /QUIET


  ; Write the keywords that are common in background.pi and obs.stats.
  stop_code = strcompress(/REMOVE_ALL,strjoin(reverse(criteria+0)))
  openw, unit, temp_text_fn, /GET_LUN
  printf, unit, creator_string, $
          R.bkg_scaling_correction,                            'scaling correction (already applied)', $
          bkg_data_counts_total / bkg_exposurearea_corrected,  comment2, $
          R.bkg_flat_counts     / bkg_exposurearea_corrected,  comment3, $
          R.spectral_corruption,                               'bkg spectrum metric', $
          R.compactness_bias,                                  'bkg compactness metric', $
          current_region_metric,                               'bkg region metric', $
          stop_code,                                           'stop code for ae_better_backgrounds', $
          F='(%"#add\nCREATOR = %s\nBACKCORR = %f / %s\nBACKGRND = %g / %s\nFLATGRND = %g / %s\nBKGMETR1 = %f / %s\nBKGMETR2 = %f / %s\nBKGMETR = %f / %s\nBB_STOP = %s / %s")'
  free_lun, unit
  
  cmd = string(bkg_spectrum_fn, obs_stats_fn, temp_text_fn, F="(%'dmhedit infile=""%s %s"" filelist=%s')")
  run_command, cmd, /QUIET

  ; Write the keywords unique to obs.stats.
  obs_stats = headfits(obs_stats_fn, ERRMSG=error)
  if keyword_set(error) then message, 'ERROR reading '+obs_stats_fn
  
  fxaddpar, obs_stats, 'BKG_RAD',  0,                     'region is from ae_better_backgrounds'
  fxaddpar, obs_stats, 'BKG_CNTS', bkg_data_counts_total, string(ENERG_LO, ENERG_HI, F="(%'background counts, %5.3f:%6.3f keV')") 
  fxaddpar, obs_stats, 'BACKSCAL', 1/R.bkg_scaling,       'normalization for BKG_CNTS' 

  writefits, obs_stats_fn, 0, obs_stats

               
  if keyword_set(pause_for_review) then begin
    print, F='(%"\nPress return to continue to the next source ...")'
    read, '? ', cmd1
  endif  
endfor ; ii loop over the catalog

ind = where(processing_rate GT 0, count)
if (count GT 0) then print, 'Median processing rate (accepted pixels/s): ', median(processing_rate[ind])

;; =====================================================================
;; Collate results.
; Commented out to reduce run times.
;acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, VERBOSE=0


CLEANUP:
;  if (verbose GE 1) then run_command, string(my_ds9, F='(%"xpaset -p %s exit")'), /QUIET

  if (n_elements(models) GT 0) then ptr_free, models.data_ptr

;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif
return
end ; ae_better_backgrounds



PRO ae_recipe
return
end

