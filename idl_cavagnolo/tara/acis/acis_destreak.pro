;;; $Id: acis_destreak.pro,v 1.2 2008-10-15 19:35:50 cavagnolo Exp $
;;; Patrick Broos, Penn State University

;;; This program identifies sets of event as being the result of ACIS readout streaks
;;; and removes those events.
;;; The advantage of destreaking an event list instead of an image is that the
;;; user can then make images from the event list in any way desired.

;;; The SPLIT STAGE splits up an event list by CCD and makes full-resolution 
;;; exposure maps for each CCD, e.g.
;;;   acis_destreak, /SPLIT, 'myobs.evt'
;;; They optional keyword CCD_LIST is a vector of CCD numbers that should be processed;
;;; if omitted DETNAM is used to identify the CCDs.
;;; The result is a set of files destreak/ccd?.0.evt and destreak/ccd?.emap.

;;; The DESTREAK stage can be run any number of times.  Each run starts with the last
;;; destreak/ccd?.{n}.evt event list found and produces a cleaner destreak/ccd?.{n+1}.evt
;;; event list plus a file destreak/ccd?.{n=1}.removed.evt containing the events removed.
;;;   acis_destreak, /DESTREAK, 'myobs.evt'
;;; CCD_LIST can be supplied to control which CCDs are processed.

;;; The MERGE stage (not yet implemented) merges the last destreak/ccd?.{n}.evt event lists
;;; together.


;; =============================================================================

PRO run_command, command, result, QUIET=quiet

for ii=0,n_elements(command)-1 do begin
  count = 0
  if NOT keyword_set(quiet) then print, command[ii]

  cmd = command[ii]
  
  if (arg_present(result) OR keyword_set(quiet)) then begin
    spawn, cmd, result, COUNT=count, /STDERR, EXIT_STATUS=exit_status
  endif else begin
    spawn, cmd, EXIT_STATUS=exit_status
  endelse

  if (exit_status NE 0) then begin
    print
    if keyword_set(quiet) then print, command[ii]
    if (count GT 0) then forprint, result, TEXTOUT=2
    print, 'ERROR: helper process failed!!'
    stop
  endif

  if (count GT 0) AND (NOT keyword_set(quiet)) then begin
    forprint, result, TEXTOUT=2
    print
  endif
endfor
		 
return
end


;; =============================================================================
PRO acis_destreak, CCD_LIST=ccd_list, $
		   SPLIT=split, obsdata_filename, $
		   DESTREAK=destreak, SHOW_LINES=show_lines, REMOVE_THRESHOLD=remove_threshold, $
		   MERGE=merge

basedir = 'destreak/'

if (n_elements(ccd_list) EQ 0) then begin

  theader = headfits(obsdata_filename, EXT=1)
  detnam  = strmid(strtrim(sxpar(theader, 'DETNAM'), 2), 5)
  
  num_ccds = strlen(detnam)
  ccd_list = intarr(num_ccds)
  
  for ii=0, num_ccds-1 do ccd_list[ii] = fix(strmid(detnam,ii,1))
endif

print, 'CCD LIST: ', ccd_list

num_ccds = n_elements(ccd_list)

title         = string(ccd_list, F='(%"CCD%d")')
base_fn       = basedir + string(ccd_list, F='(%"ccd%d")')
emap_fn       = base_fn + '.emap'

field_range_fn = base_fn + '_range.txt'


;; =============================================================================
if keyword_set(split) then begin
  events_out_fn = base_fn + '.0.evt'
  fluximage_fn  = base_fn + '.0'

  run_command, /QUIET, 'punlearn dmcopy'

  
  file_mkdir, basedir
  
  for ii=0, num_ccds-1 do begin
    ;; Remove all old files for this CCD.
    files_to_remove = findfile( base_fn[ii] + '*', COUNT=count)
    if (count GT 0) then file_delete, files_to_remove, /QUIET

    ;; Filter event list by CCD.
    cmd = string(obsdata_filename, ccd_list[ii], events_out_fn[ii], $
  		F="(%'dmcopy ""%s[ccd_id=%d][cols x,y,energy]"" %s')")
    run_command, cmd
    

    print, 'Reading events ...'
    
    evts = mrdfits(events_out_fn[ii], 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + events_out_fn[ii]
    
    xx = evts.x
    yy = evts.y
  
    
    ;; Determine span of future data image.
    ;; We make dimensions even so we can rebin images by 2 later for display.
    x0 = floor(min(xx))
    y0 = floor(min(yy))

    col = floor( xx - x0 )
    row = floor( yy - y0 )
    num_col = max(col)+1  &  num_row = max(row)+1 

    num_col = num_col + (num_col mod 2)
    num_row = num_row + (num_row mod 2)

    
    ;; Make an exposure-only emap to match span.
    cmd = string( x0, x0+num_col, num_col, y0, y0+num_row, num_row, $
  		  events_out_fn[ii], base_fn[ii], ccd_list[ii], $
  		  F="(%'acis_make_emap.plx -onlytime %d %d %d %d %d %d %s NONE %s -ccdlist=%d -cache -clobber')")
    run_command, cmd
    
    ;; Change TLMIN/TLMAX values in event list so ds9 can bin reasonably.
    openw, unit, field_range_fn[ii], /GET_LUN
    printf, unit, x0, x0+num_col, $
                  y0, y0+num_row, $
                  F='(%"#add\nTLMIN1=%d\nTLMAX1=%d\nTLMIN2=%d\nTLMAX2=%d")'
    free_lun, unit
    
    cmd = string(events_out_fn[ii], field_range_fn[ii], F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd


    ;; Make a flux image to match emap.
    cmd = string( events_out_fn[ii], emap_fn[ii], fluximage_fn[ii], $
  		  F="(%'acis_make_img_match_emap.plx %s %s -outpath %s -clobber')")
    run_command, cmd

  endfor ;ii
endif ;/SPLIT


;; =============================================================================
if keyword_set(destreak) then begin
  if NOT keyword_set(remove_threshold) then remove_threshold = 100

  ;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
  run_command, /QUIET, 'punlearn dmcoords'

  for ii=0, num_ccds-1 do begin

    ;; ------------------------------------------------------------------------
    ;; Figure out the stage number, filenames.
    stage = -1
    repeat begin
      stage = stage + 1
      filename = string(base_fn[ii], stage, F='(%"%s.%d.evt")')
      
      if file_test(filename) then begin
        events_in_fn = filename
      endif else begin
        events_out_fn = filename
        fluximage_fn  = string(base_fn[ii], stage, F='(%"%s.%d.flux.img")')
        break
      endelse
    endrep until (0)
    
    if (stage EQ 0) then message, 'ERROR: cannot find input file ' + filename
    
    removed_fn = string(base_fn[ii], stage, F='(%"%s.%d.removed.evt")')
    print, '------------------------------------------------------------------------'
    print, 'Destreaking ', events_in_fn
    
    ;; ------------------------------------------------------------------------
    ;; Obtain matching emap and data images.

    ;; Read emap and normalize to 1 just in case statistics routine later have
    ;; problems with odd ranges for data, and so we can threshold emap.
    emap = readfits(emap_fn[ii], emap_header)
    norm = max(emap)
    emap = temporary(emap)/norm

    x0 = sxpar(emap_header, 'XMIN')
    y0 = sxpar(emap_header, 'YMIN')
    
    num_col = (size(emap, /DIM))[0]
    num_row = (size(emap, /DIM))[1]
    

    ;; Read events and make image to match emap.
    ;; We make our own image, rather than using acis_make_img_match_emap.plx, 
    ;; because we want REVERSE_INDICES.
    print, 'Making data image to match emap ...'
    pheader = headfits(events_in_fn)
    
    evts = mrdfits(events_in_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + events_in_fn
      
    xx = evts.x
    yy = evts.y
    
    ;; Make an image with reverse indexes to match emap.
    col = floor( xx - x0 )
    row = floor( yy - y0 )
    
    if ((max(col)+1) GT num_col) OR ((max(row)+1) GT num_row) then $
      message, 'ERROR: span of events exceeds exposure map!'
    
    index_1d  = row * long(num_col) + col
    nbins     = num_col*num_row
    data_img  = reform(histogram( index_1d, MIN=0L, MAX=nbins-1, REVERSE_INDICES=rindex ), num_col, num_row)
    

    ;; Expand the first segment of REVERSE_INDICES into a 2-D map of group start
    ;; indexes and group stop indexes that can be easily subscripted to extract
    ;; indexes of events that fall in pixel(row,col), e.g.
    ;; group_indexes[group_start[x,y]:group_stop[x,y]]
    group_start = reform(rindex[0:nbins-1] - (nbins + 1),     num_col,num_row)
    group_stop  = reform(rindex[1:nbins]   - (nbins + 1) - 1, num_col,num_row)
    group_indexes = rindex[nbins+1:*]


    ;; ------------------------------------------------------------------------
    ;; Partition the data/emap image pixels THAT HAVE NON-ZERO EXPOSURE into 
    ;; groups which lie along lines parallel to CHIPY.
    
    ;; Find unit vector (dx,dy) pointing along CHIPY axis.
    chip_center = 512.5
    chipy2      = 900
    
    cmd = string(emap_fn[ii], ccd_list[ii], chip_center, chip_center,  $
    		 F="(%'dmcoords %s opt=chip chip_id=%d chipx=%d chipy=%d')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords logicalx logicaly', result
    x_center = double(result[0])
    y_center = double(result[1])
    help, x_center, y_center
    
    cmd = string(emap_fn[ii], ccd_list[ii], chip_center, chipy2,  $
    		 F="(%'dmcoords %s opt=chip chip_id=%d chipx=%d chipy=%d')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords logicalx logicaly', result
    x2 = double(result[0])
    y2 = double(result[1])
    
    dx = (x2-x_center)
    dy = (y2-y_center) 
    norm = sqrt(dx^2 + dy^2)
    dx = dx / norm
    dy = dy / norm
    
    angle = !RADEG * atan(dy,dx)
    print, 'Angle of CHIPY is ', angle
    
    
    ;; Establish a set of angled lines, spaced 1 pixel apart, defined by 
    ;; the points (px,py) in image coordinates and the vector (dx,dy), 
    ;; that cover the CCD, allowing for dither.
    num_lines = 1024 + 80
    offset_from_center = findgen(num_lines) - num_lines/2
    px = x_center + dy * offset_from_center
    py = y_center - dx * offset_from_center
    
    ;; Convert line representation to "implicit" form: ax + by + c = 0
    a = -dy  
    b =  dx
    c = px*(py+dy) - py*(px+dx)


    ;; Assign each pixel to the closest line.
    ;; We assign a pixel to a line if the distance between them is < 0.5.
    ;; The SIGNED distance from a point (X,Y) is given from geometry references as:
    ;; distance = (a*X + b*Y + c[jj]) / sqrt(a^2 + b^2)
    ;; 
    ;; So, we want to test the condition (abs(distance) LT 0.5), which
    ;; can be rewritten as 
    ;; abs(a*X + b*Y + c[jj]) LT 0.5*sqrt(a^2 + b^2)
    ;; We precompute some terms independent of jj to save time.

    print, 'Assigning pixels to lines ...'
    make_2d, indgen(num_col), indgen(num_row), col, row
    ab_term            = a*col + b*row
    distance_threshold = 0.5 * sqrt(a^2 + b^2)

    ;; We want to ignore (not assign to any line) pixels with small exposure.
    ;; To save time below we accomplish this by poking ab_term with a value
    ;; that will prevent membership.
    index = where(emap LT 0.01, count)
    if (count GT 0) then ab_term[index] = 1E6
    
    if keyword_set(show_lines) then begin
      map = bytarr(num_col, num_row)
      cartoon_dim = 400
      color_manager
      window, 0, XSIZE=cartoon_dim, YSIZE=cartoon_dim, TITLE='acis_destreak: sample lines'
    endif

    line_members = ptrarr(num_lines, /ALLOC)
    num_members  = intarr(num_lines)

    for jj=0, num_lines-1 do begin
      ; Accept pixels close to the line with NONZERO exposure!
      index = where( abs(ab_term + c[jj]) LT distance_threshold, count)
      if (count GT 0) then begin

        if (keyword_set(show_lines) AND (jj mod 64 EQ 0)) then begin
          map[index] = 255
          tv, congrid(map,cartoon_dim,cartoon_dim)
          map[index] = 0
	  print,jj
        endif
        
	num_members[jj]     = count
        *(line_members[jj]) = temporary(index)
      endif
    endfor ;jj

    
    ;; ------------------------------------------------------------------------
    ;; Compute the number of counts to be removed in each pixel.

    ;; Find mean background flux component for each line.
    ;; For a line contaminated by a streak, data pixel values Pi = Fi*Ei + Si, 
    ;; where Fi is the sky flux, Ei is the emap for that pixel, and Si is the
    ;; streak level.  
    ;; Note that Si depends on the emap, which here includes only the exposure time,
    ;; not any QE terms (-onlytime option to acis_make_emap.plx).  QE variations,
    ;; which are very similar in neighboring columns, are carried in the sky flux term (Fi).
    ;;
    ;; Below we're estimating mean(Pi/Ei) = mean(Fi) + mean(Si/Ei) for each line.
    ;; The idea is that mean(Fi) ~= mean(F'i) where F & F' are the fluxes in nearby
    ;; lines.  
    ;; Thus, line_excess_flux ~= mean(Si/Ei), and we estimate Si (count/pixel) by
    ;; Si = line_excess_flux * Ei
    ;; and then try to remove (on average) Si counts from pixel i in line.
        
    ;; Use NaN for lines with no background estimate so that MEDIAN() will ignore them.
    print, 'Finding mean background flux component for each line ...'
    line_bkg_flux = replicate(!VALUES.F_NAN,num_lines)
    for jj=0, num_lines-1 do begin
      if (n_elements(*(line_members[jj])) GT 10) then begin
        index = *(line_members[jj])

	; Division by zero is prevented because line membership requires 
	; non-zero exposure
        flux  = data_img[index] / emap[index] 
        estimate_background, flux, mean_flux
        line_bkg_flux[jj] = mean_flux
      endif
    endfor ;jj

    
    ;; Determine mean flux excess in each line with respect to its neighbors.
    ;; NaN was used for lines with no background estimate so that MEDIAN() will ignore them.
    line_excess_flux = (line_bkg_flux - median(line_bkg_flux, 51, /EVEN)) > 0
    
    ;; Plot line info.
    line_bkg_flux_plot = line_bkg_flux
    ind = wherenan(line_bkg_flux_plot, count)
    if (count GT 0) then line_bkg_flux_plot[ind] = -0.1
    
    line_excess_flux_plot = line_excess_flux
    ind = wherenan(line_excess_flux_plot, count)
    if (count GT 0) then line_excess_flux_plot[ind] = 0
    
    line_number = indgen(num_lines)
    function_1d, id1, line_number, line_bkg_flux_plot,    DATA='line background', LINE=1, XTIT='line #', YTIT='flux (normalized)', TIT=title[ii]
    function_1d, id1, line_number, line_excess_flux_plot, DATA='line excess', LINE=0
    function_1d, id3, line_number, num_members,           DATA='Pixel Groups', YTIT='# pixels under line', XTIT='line #', TIT=title[ii]
    

    
    ;; Choose specific counts to remove from each pixel in each line group.
    ;; We rely below on the fact that the pixels belonging to each line, *(line_members[jj]),
    ;; ordered such that as you move through the list you move along the line from one end
    ;; to the other.
    
    streak_level = fltarr(num_lines)
    remove_img = lonarr(num_col, num_row)

    for jj=0, num_lines-1 do begin
      ; If the line had no background estimate, skip it.
      if (NOT finite(line_bkg_flux[jj])) then continue
      
      members = *(line_members[jj])
      
      ; Compute the desired (goal) counts to remove per pixel; skip lines with low values.
      line_excess_counts = emap[members] * line_excess_flux[jj]
      
      if (total(line_excess_counts) LE remove_threshold) then continue

                        
      ; Walk along the line, removing counts as needed to keep the integral of the removal
      ; goal distribution close to the integral of the actual removal distribution.
      removal_balance = 0.0D
      line_data_counts = data_img[members]

      for kk = 0, num_members[jj]-1 do begin
        ; Increment budget.
        removal_balance = removal_balance + line_excess_counts[kk]
        
        ; Remove counts if necessary & possible.
        num_to_remove = floor(removal_balance < line_data_counts[kk])
        
        if (num_to_remove GE 1) then begin
          removal_balance = removal_balance - num_to_remove
          
          ; Check if pixel is a member of two lines!
          if (remove_img[members[kk]] NE 0) then message, 'ERROR: line groups not disjoint!'
          
          remove_img[members[kk]] = num_to_remove
          
          gstart = group_start[members[kk]]
          gstop  = group_stop [members[kk]]
          
          ; Consistency check.
          if ((gstop-gstart+1) NE line_data_counts[kk]) then $
            message, 'ERROR: reverse indexes not correct'
          
          ; Mark the specified number of events for removal.
          event_index = group_indexes[gstart:gstop]
          xx[event_index[0:num_to_remove-1]] = 0
        endif ;(num_to_remove GE 1)
      endfor ;kk
      
      streak_level[jj] = total(remove_img[members]) / num_members[jj]
    endfor ;jj
    
    function_1d, id4, line_number, streak_level, DATA='Counts Removed', YTIT='# counts removed per pixel', XTIT='line #', TIT=title[ii]
    function_2d, id2, frebin(data_img,   num_col/2, num_row/2, /TOTAL), DATA=events_in_fn, $
    		      X0=x0, Y0=y0, DELTA_X=2, DELTA_Y=2, TIT=title[ii]
    function_2d, id2, frebin(remove_img, num_col/2, num_row/2, /TOTAL), DATA='events removed', $
    		      X0=x0, Y0=y0, DELTA_X=2, DELTA_Y=2

    
    ;; Write filtered event list & removed event list.

    print, 'Writing out event lists ', events_out_fn, ' & ', removed_fn
    fxaddpar, pheader, 'CREATOR', "$RCSfile: acis_destreak.pro,v $, v$Revision: 1.2 $"
    fxaddpar, theader, 'CREATOR', "$RCSfile: acis_destreak.pro,v $, v$Revision: 1.2 $"
    fxaddpar, theader, 'REM_THLD', remove_threshold
    fxaddpar, theader, 'ANGLE',    angle
    writefits, events_out_fn, 0, pheader
    writefits, removed_fn   , 0, pheader
    
    ind_keep = where(xx NE 0, COMPLEMENT=ind_remove, NCOMPLEMENT=remove_count)
    mwrfits, evts[ind_keep],   events_out_fn, theader
    mwrfits, evts[ind_remove], removed_fn   , theader
    
    cmd = string(events_out_fn, field_range_fn[ii], F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd

    cmd = string(removed_fn,    field_range_fn[ii], F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd
    
    lines = replicate( {num_members:0, bkg_flux:0.0, excess_flux:0.0, streak_level:0.0}, num_lines)
    lines.num_members = num_members
    lines.bkg_flux    = line_bkg_flux
    lines.excess_flux = line_excess_flux
    lines.streak_level = streak_level
    mwrfits, lines, removed_fn

    print, 'Writing out flux image ', fluximage_fn
    flux_img = (data_img - remove_img)/emap
    index    = where( emap LE 0, count)
    if (count GT 0) then flux_img[index]=0
    
    fxaddpar, emap_header, 'CREATOR', "$RCSfile: acis_destreak.pro,v $, v$Revision: 1.2 $"
    writefits, fluximage_fn, flux_img, emap_header

    print, total(remove_img, /DOUBLE), ' events removed.'
    print, remove_count, ' events removed'

    ptr_free, line_members
    id1=0L & id2=0L & id3=0L & id4=0L
  endfor ;ii  
endif ;/DESTREAK


;; =============================================================================
if keyword_set(merge) then begin
  events_in_fn = strarr(num_ccds)
  minx = 10000  &  miny = 10000
  maxx = 0      &  maxy = 0
  for ii=0, num_ccds-1 do begin

    ;; ------------------------------------------------------------------------
    ;; Figure out the stage number, filenames.
    stage = -1
    repeat begin
      stage = stage + 1
      filename = string(base_fn[ii], stage, F='(%"%s.%d.evt")')
      
      if file_test(filename) then begin
        events_in_fn[ii] = filename
      endif else begin
        break
      endelse
    endrep until (0)

    if (stage EQ 0) then message, 'ERROR: cannot find input file ' + filename

    print, 'Finding (x,y) range of ', events_in_fn[ii]
    evts = mrdfits(events_in_fn[ii], 1, theader, /SILENT, STATUS=status)
    
    minx = minx < min(evts.x) 
    miny = miny < min(evts.y) 
    maxx = maxx > max(evts.x) 
    maxy = maxy > max(evts.y) 
  endfor
  
  base_fn        = basedir + 'coup_epo'
  events_out_fn  = base_fn + '.evt'
  field_range_fn = base_fn + '_range.txt'

  cmd = string(strjoin(events_in_fn,','), events_out_fn, $
  		F="(%'dmmerge infile=%s, outfile=%s outBlock="""" columnList="""" lookupTab=""""')")
  run_command, cmd
  
  
  openw, unit, field_range_fn, /GET_LUN
  printf, unit, minx, maxx, miny, maxy, $
                  F='(%"#add\nTLMIN1=%d\nTLMAX1=%d\nTLMIN2=%d\nTLMAX2=%d")'
  free_lun, unit
    
  cmd1 = string(events_out_fn, field_range_fn, F="(%'dmhedit infile=%s filelist=%s')")
  cmd2 = string(events_out_fn, F="(%'dmlist %s blocks')")
  run_command, [cmd1,cmd2]

endif ; /MERGE

return
end
