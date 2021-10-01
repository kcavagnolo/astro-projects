;=============================================================================
; $Id: match_xy.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
; Catalog Matching Program 
; Patrick Broos, November 1993, December 2005

; The input structure array cat_slave must contain the following tags/fields:
;   ID: (optional) a unique long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
; A known offset error in cat_slave can be specified via XSHIFT_SLAVE,YSHIFT_SLAVE.
;
;
; The input/output data structure match_state holds three types of information. 
; (1) a master catalog consisting of at least these fields:
;   ID:  long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
;
; The match_state is initialized and a master catalog is established via this call:
;       match_xy, match_state, cat_master, cat_name, /INIT
;
; (2) a copy of all the catalogs we've processed.  Note that any XSHIFT_SLAVE,YSHIFT_SLAVE values that are passed in are ADDED to the X & Y columns of these catalogs.
;
; (3) match information consisting of a Primary Match (PM) structure array and a Secondary Match (SM) structure array.  
;The PM contains an entry for each source in (1):
;   IDm, IDs: a pair of matching ID tags from the master & slave catalogs
;   deltaX, deltaY: offsets between matching entries (master - slave)
;   rank: see discussion below
;   type: {0: isolated source, 1: successful Primary Match, 2: Secondary match, 3: failed Primary match, }
;
; Every entry "a1" in catalog 1 has a unique Primary Match: the entry "a2" in catalog 2 that has the highest match rank.  The Primary Match <a1,a2> is "unsuccessful" if there exists another Primary Match involving either a1 or a2 ,<a1,b2> or <b2,a2>, which has a higher rank.  In other words, Primary Matches are geedy -- once <a1,a2> has been accepted as a successful match then neither a1 nor a2 can participate in another successful primary match.  We're trying to build a self-consistent hypothesis for the correspondence between the two catalogs.  Thus, if we hypothesize that a1 & a2 are the same object then it makes no sense to also hypothesize that a1 & b2 are the same object, even if a1 is the closest object to b2.  Rather it makes more sense to hypothesize that b2 has no match.;
;
; The SM contains a variable number of entries recording secondary matches between (1) and the slave catalog:


; The significance_threshold parameter specifies the formal "significance" required for a match, i.e. our decision criterion is to REJECT match candidates that fall in a region of parameter space containing an area (probability) of (1-significance_threshold parameter).


; If the output parameter UNION_CAT is supplied then the master catalog is extended to include anything in cat_slave that is not matched.  For matched sources the position is taken from cat_slave if its more accurate.

; Some references on the catalog matching problem:
; http://sundog.stsci.edu/first/APM/HTML/node5.html


;=============================================================================
PRO match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type
isolated_type           = 0
failed_primary_type     = 1
successful_primary_type = 2
secondary_type          = 3
init_type               = 4
return
end


PRO match_xy, match_state, cat_slave_p, cat_name, significance_threshold, $
	            XSHIFT_SLAVE=xshift_slave, YSHIFT_SLAVE=yshift_slave, $
              INIT=init, QUIET=quiet, $
              UNION_CAT=union_cat, UNION_REG=union_reg, $
              NGOOD=Ngood, NSECONDARY=Nsecondary
            
COMMON match_xy, idex, idey, idx, idy, idxe, idye, idr1, idr2

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

if (n_elements(xshift_slave) EQ 1) then xshift_slave=xshift_slave[0] else xshift_slave=0.
if (n_elements(yshift_slave) EQ 1) then yshift_slave=yshift_slave[0] else yshift_slave=0.

Nslave     = n_elements(cat_slave_p)

f_nan    = !VALUES.F_NAN


;; Make sure the catalog has an ID column.
if (total(strmatch(tag_names(cat_slave_p),'ID')) GT 0) then begin
  ; Make sure the catalog structure has a name.
  if (tag_names(cat_slave_p, /STRUCTURE_NAME) EQ '') then begin
    print, 'ERROR (match_xy): the catalog '+cat_name+' must use a NAMED (not anonymous) IDL structure!'
    retall
  endif

  ; Check the existing ID column for problems.
  id_type = size(cat_slave_p.ID, /TNAME) 
  if (id_type NE 'INT') and (id_type NE 'LONG') then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be INT or LONG type!'
    retall
  endif
  
  if (n_elements(uniq(cat_slave_p.ID,sort(cat_slave_p.ID))) NE Nslave) then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be unique!'
    retall
  endif
    
  cat_slave = cat_slave_p
  
endif else begin
  ; Add an ID column to the catalog.
  cat_slave    = replicate(create_struct(NAME='SOURCE_'+cat_name, 'ID', 0L, cat_slave_p[0]), Nslave)
  struct_assign, cat_slave_p, cat_slave
  cat_slave.ID = 1+indgen(Nslave)
endelse

;; Make sure all the positions and errors are good numbers.
ind = where((finite(cat_slave.X) AND finite(cat_slave.Y) AND finite(cat_slave.X_ERR) AND finite(cat_slave.Y_ERR)) EQ 0, count)
if (count GT 0) then begin
  print, 'ERROR (match_xy): X,Y,X_ERR,Y_ERR columns in catalog '+cat_name+' have some bad values:'
  forprint, cat_slave.ID, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  retall
endif

ind = where(((cat_slave.X_ERR GT 0) AND (cat_slave.Y_ERR) GT 0) EQ 0, count)
if (count GT 0) then begin
  print, 'ERROR (match_xy): X_ERR,Y_ERR columns in catalog '+cat_name+' have some non-positive values:'
  forprint, cat_slave.ID, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  retall
endif

dataset_1d, idex, cat_slave.X_ERR, DATASET=cat_name, XTIT='X_ERR (pixels)'
dataset_1d, idey, cat_slave.Y_ERR, DATASET=cat_name, XTIT='Y_ERR (pixels)'




; Apply offsets to a COPY of the slave catalog so we don't change it in the caller.
cat_slave.X = cat_slave.X + xshift_slave
cat_slave.Y = cat_slave.Y + yshift_slave

if keyword_set(init) then begin
  ;; Create a brand new match_state and copy the positions from the supplied catalog.
  Ncat = 1

  match_state = { ID:cat_slave.ID, X:cat_slave.X, Y:cat_slave.Y, X_ERR:cat_slave.X_ERR, Y_ERR:cat_slave.Y_ERR, cat_names:strarr(Ncat), sig_thresholds:fltarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
endif else begin
  ; Disallow a slave catalog named the same as the master.
  if (cat_name EQ match_state.cat_names[0]) then begin
    print, 'ERROR (match_xy): the name of this slave catalog, '+cat_name+', conflicts with the name of the master catalog.'
    retall
  endif
endelse

Nmaster    = n_elements(match_state.X)
Nsecondary = 0L


dum = {PMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan, type:isolated_type}
dum = {SMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan }
match_primary   = replicate({PMATCH},Nmaster)
match_secondary = replicate({SMATCH},Nmaster)


if keyword_set(init) then begin
  ; Match slave_cat to itself with type set to init_type, then return.
  match_primary.IDm  = match_state.ID
  match_primary.IDs  = match_state.ID
  match_primary.type = init_type
  
  ptr_free, match_state.catalogs[0], match_state.match_primary[0]

  match_state.sig_thresholds[0]= 0
  match_state.cat_names    [0] = cat_name
  match_state.catalogs     [0] = ptr_new(cat_slave)
  match_state.match_primary[0] = ptr_new(match_primary)

  return
endif

; Initialize match_primary to be isolated sources from match_state.
match_primary.IDm  = match_state.ID
match_primary.type = isolated_type

Xs     = cat_slave.X
Ys     = cat_slave.Y
sig_Xs = cat_slave.X_ERR
sig_Ys = cat_slave.Y_ERR
var_Xs = sig_Xs^2
var_Ys = sig_Ys^2
rank   = fltarr(Nslave)
deltaX = fltarr(Nslave)
deltaY = fltarr(Nslave)

report_frac = [.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0]



;; Let the "null hypothesis" H0 be that these two position measurements are from a single source located at an unknown true position (Xtrue,Ytrue).  We assume we have 4 independent random variables, two X positions and two Y positions.  We need to choose a scheme for rejecting H0, i.e. for declaring that two catalog entries just do not plausibly match.  This means dividing the joint distribution into "acceptance" and "rejection" regions.

;; IF WE KNEW (Xtrue,Ytrue) then we could compute a confidence region on the 4-D jointly Gaussian distribution where the 4 axes are independent.  A simple region would be a "rectangle" in 4-D with an area equal to the significance desired; we would require that EACH measurement fall within a simple 1-D confidence interval for that axis. 

;; BUT, we DO NOT know (Xtrue, Ytrue).  Estimating Xtrue,Ytrue from the data is not appropriate since it will always be "near" the two observed positions.   Thus the more straighforward approach is to examine the offsets between the two observations directly.  Consider two new random variables representing the offset between the observed X & Y positions:
;; deltaX = abs(Xs - Xm)
;; deltaY = abs(Ys - Ym)
;; These are distributed as zero-mean Normals with variance equal to the sum of the two variances. The joint distributuion of these is a 2-D Gaussian.  An natural boundary of the confidence region would be a contour of the distribution, i.e. an ellipse.  All we would need to do is determine the value of the distribution at the appropriate contour, and then if the likelihood of the data exceeds that threshold then the match is accepted.   Unfortunately I cannot find publication of a clear derivation of the relationship between such a "confidence ellipse" (even in 2D) and the integrated probability enclosed.
;;  
;; Thus instead I'm going to use a confidence region that's a rectangle in this 2-D space.  In other words I'll require that each of the deltaX,deltaY offsets fall within a simple 1-D confidence interval for that axis.  If each 1-D confidence interval has a significance (area under the marginal 1-D PDF) of S then the significance of the corresponding confidence region in 2D (area under the 2-D PDF) will be S^2.  So for example to achieve a significance of 0.99 in 2D we require a significance of 0.99^(0.5)  for each 1-D interval.
;;
;; These confidence intervals will be "two-sided", i.e. we want the areas in the two Gaussian tails to add up to 1 - 0.99^(0.5).
;;
;; BEWARE.  One may be tempted to use an alternate approach, namely to compute for EACH 4-tuple the integral of the 4D PDF within the rectangular region (in 4D) bounded by that 4-tuple, and then compare that area to a significance threshold (e.g. 0.99).  In this approach the rectangles can have wild shapes, depending on the data values, as opposed to the "compact" shape of the fixed "acceptance region" used in the first approach. Maybe there's some correspondence between the two approaches, but the second one seems wrong to me.


z_acceptable = gauss_cvf( 0.5 * (1 - significance_threshold^(0.5)) )
;help, z_acceptable

;; Loop through primary catalog computing rank of every match that's judged to be plausible.
for ii=0L,Nmaster-1 do begin
  IDm    = match_state.ID[ii]
  Xm     = match_state.X[ii]
  Ym     = match_state.Y[ii]
  sig_Xm = match_state.X_ERR[ii]
  sig_Ym = match_state.Y_ERR[ii]
  var_Xm = sig_Xm^2
  var_Ym = sig_Ym^2
  
  ; Compute best estimate of true position if H0 is true.  See Bevington section 4.1
  ; This is not actually used in the matching algorithm.
  Xtrue = (Xm*var_Xs + Xs*var_Xm) / (var_Xs + var_Xm)
  Ytrue = (Ym*var_Ys + Ys*var_Ym) / (var_Ys + var_Ym)
  
  ; Normalize the deltaX,deltaY random variables representing offsets between the two measurements.
  z1= abs(Xm - Xs)/sqrt(var_Xm + var_Xs)
  z2= abs(Ym - Ys)/sqrt(var_Ym + var_Ys)

  
  ; Find the data points (source pairs) that fall in the "acceptance region" we've established.
  zmax = z1 > z2   
    
  good = where(zmax LT z_acceptable, Ngood)

  if (Ngood GT 0) then begin
    ; We now have a set of matches that are all judged to be acceptable, i.e. to not be too extremely improbably.  However later we are going to have to rank these proposed matches in order to choose which to accept.  I think the most obvious metric to use for this ranking is the "likelihood", i.e. the height of the 4D jointly Gaussian PDF evaluated at the data 4-tuple.  Consider for a moment how this ranking method will behave when we have a pair of sources exactly coincident, i.e. the 4-tuple of data lies at the peak of the 4D PDF.  If all the measurements are very accurate (small sigma) then the PDF will be narrow and the peak will be high, producing a high rank.  On the other hand if the measurements are very inaccurate the PDF will be broad and the peak will be low, producing a low rank.  That seems to match ones intuition for a well-behaved ranking scheme.
    ;
    ; It's convenient to use the log of the likelihood as the ranking metric.
    rank[good] = -(z1[good]^2 + z2[good]^2)
    
    ; Compute position offsets which are reported in match structures (for use in estimating coordinate system transformations).
    ; We compute master - slave to get the same sign on these offsets as is neede for XSHIFT/YSHIFT inputs.
    deltaX[good] = match_state.X[ii] - Xs[good] 
    deltaY[good] = match_state.Y[ii] - Ys[good] 
    
    if (Ngood EQ 1) then begin
      ; There's only one possible match, the Primary Match.
      prime = good[0]
    endif else begin
      ; The best one is the Primary Match.
      dum  = max(rank[good], imax)
      prime = good[imax]
      
      ; The rest are secondary matches.
      secondary = good[where(good NE prime, NtoAdd)]

      ; Allocate more space in all_records when necessary.
      if ((Nsecondary+NtoAdd) GT n_elements(match_secondary)) then begin
        print, f='(%"extending match_secondary array...")'

        match_secondary = [match_secondary,replicate({SMATCH}, NtoAdd > 4000)]
      endif
      
      new_records = replicate({SMATCH}, NtoAdd)
      new_records.IDm        = IDm
      new_records.IDs        = cat_slave[secondary].ID
      new_records.deltaX     = deltaX   [secondary]
      new_records.deltaY     = deltaY   [secondary]
      new_records.rank       = rank     [secondary]
      
      match_secondary[Nsecondary] = new_records
                      Nsecondary  = Nsecondary + NtoAdd
    endelse
    
    ; Save the Primary Match, marking it as "successful" for now.
    match_primary[ii] = {PMATCH, IDm, cat_slave[prime].ID, deltaX[prime], deltaY[prime], rank[prime], type:successful_primary_type}
    
  endif ;(Ngood GT 0) 
  
  if (float(ii)/Nmaster GT report_frac[0]) then begin
    if NOT keyword_set(quiet) then print, report_frac[0]*100, F='(%"%d%% matches processed")'
    report_frac = shift(report_frac,-1)
  endif
endfor ;ii


;; Consider success of Primary Matches in order of rank.
; The strategy here is that when a source from list 1, P1, is judged to match
; a source from list 2, P2, then all other potential matches involving P1 and P2
; are DISABLED.  
; In other words a given source can NOT participate in multiple matches.
; We do however record "secondary matches" so one can tell if a reported match is
; unique/ambiguous.
;
; The potential downside of this approach is that a spurious match involving 
; source P1 will remove P1 from consideration in subsequent matches, possibly  
; preventing its true match from being recorded.  We minimize this problem by 
; processing matches sorted by their match distance, on the assumption that
; close matches are more reliable than larger ones.

sort_ind = reverse(sort(match_primary.rank))

for ii=0L,Nmaster-1 do begin
  match = match_primary[sort_ind[ii]]
  
  ; If this match is successful then all others involving this slave entry are NOT successful.
  if (match.type EQ successful_primary_type) then begin
  
    fail = where((match_primary.type EQ successful_primary_type) AND $
                 (match_primary.IDs  EQ match.IDs), count)
                 
    if (count EQ 0) then message, 'Bug found!'
    
    if (count GT 1) then begin
      ; Remove the current match from the fail list.
      fail = fail[ where(fail NE sort_ind[ii]) ]
      match_primary[fail].type = failed_primary_type

      print, match.IDm, match.IDs, F='(%"Successful primary match (%d,%d) forced the failure of these primary matches:")'
      forprint, match_primary[fail].IDm, match_primary[fail].IDs
    endif

  endif ;(successful_primary_type)
endfor ;ii


;; Store everything in match_state.  Overwrite any existing entry for this catalog.
ind = where(match_state.cat_names EQ cat_name, count)
if (count EQ 0) then begin
  ; Look for an empty spot.
  ind = where(match_state.cat_names EQ '', count)
endif

if (count EQ 0) then begin
  ; Need to enlarge match_state.
  Ncat = 1+n_elements(match_state.cat_names)
  ind  = Ncat-1
  temp_cat = match_state
  fa = fltarr(Nmaster)

  match_state = { ID:lonarr(Nmaster), X:fa, Y:fa, X_ERR:fa, Y_ERR:fa, cat_names:strarr(Ncat), sig_thresholds:fltarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
  
  struct_assign, temp_cat, match_state
endif

ptr_free, match_state.catalogs[ind], match_state.match_primary[ind], match_state.match_secondary[ind]
match_state.sig_thresholds[ind]= significance_threshold
match_state.cat_names    [ind] = cat_name
match_state.catalogs     [ind] = ptr_new(cat_slave)
match_state.match_primary[ind] = ptr_new(match_primary)
if (Nsecondary GT 0) then $
  match_state.match_secondary[ind] = ptr_new(match_secondary[0:Nsecondary-1])


;; Form the union of the master cat and cat_slave if desired.
if arg_present(union_cat) then begin
  ; Start with the master catalog.
  union_cat = *match_state.catalogs[0]
  
  slave_unmatched_flag = replicate(1B, Nslave)
  
  colors = replicate('cyan', Nmaster)

  ; Update the master position if the slave is better.
  good = where(match_primary.type EQ successful_primary_type, Ngood)
  if (Ngood GT 0) then begin
    for ii=0L,Ngood-1 do begin
      match = match_primary[good[ii]]
      
      ind_m  = where(match_state.ID EQ match.IDm)
      ind_s  = where(cat_slave.ID EQ match.IDs)
      master = union_cat[ind_m]
      slave  = cat_slave[ind_s]

      colors[ind_m] = 'green'

      if (ind_m[0] NE good[ii]) then message, 'Order is suspect!'
      
      if ((slave.X_ERR^2+slave.Y_ERR^2) LT (master.X_ERR^2+master.Y_ERR^2)) then begin
        ; Replace the master entry with the slave entry.
        struct_assign, slave, master
        union_cat[ind_m] = master
      endif
      
      ; Mark the slave entry as matched.
      slave_unmatched_flag[ind_s] = 0
    endfor
  endif
  
  ; Append any unmatched cat_slave entries to union_cat.
  extra = where(slave_unmatched_flag, Nextra)
  if (Nextra GT 0) then begin
    ; Enlarge union_cat to hold extra entries.
    master       = union_cat[0]
    temp         = temporary(union_cat)
    union_cat    = replicate(master, Nmaster+Nextra)
    union_cat[0] = temp
    
    colors = [colors, replicate('blue', Nextra)]
    
    ; Copy the extra slave entries into union_cat.
    for ii=0,Nextra-1 do begin
      struct_assign, cat_slave[extra[ii]], master
      union_cat[Nmaster+ii] = master
    endfor ;ii
    
  endif
  
  ; We have to regenerate unique ID tags.
  ; Even if the catalog was not expanded, an entry could have been replaced by the slave.
  union_cat.ID = lindgen(Nmaster+Nextra)

  print, 'Appended',Nextra,' sources to form union catalog.'  
     
  if keyword_set(union_reg) then begin
    print, 'Union catalog is depicted in the ds9 region file '+union_reg
    catalog_ds9_interface, union_cat, union_reg, /WRITE_REGFILE, COLORS=colors
  endif
endif ;/UNION
    
    

good = where(match_primary.type EQ successful_primary_type, Ngood)
print
print, Ngood,      ' primary matches succeeded'
print, Nsecondary, ' secondary matches'
return
end





;=============================================================================
;;; The input match_state is from match_xy routine.
;;; If the  boolean vector ANALYSIS_MASK is supplied then only those sources are analyzed.
;;; /CUSTOM1 enables some extra plots.
;;; Supplying ARCSEC_PER_PIXEL value makes plots use arcsecond units.
;;;
;;; The output composite_cat is a catalog with columns from all the catalogs in match_state.
;;; The output isolated_flag is a boolean vector marking the sources with no counterparts in any catalog.
;=============================================================================

PRO match_xy_analyze, match_state, ANALYSIS_MASK=analysis_mask, $
                      CUSTOM1=custom1, ARCSEC_PER_PIXEL=arcsec_per_pixel, $
                        
                      composite_cat, isolated_flag

COMMON match_xy

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

f_nan    = !VALUES.F_NAN

Nmaster  = n_elements(match_state.X)
Ncat     = n_elements(match_state.catalogs)

if (n_elements(analysis_mask) EQ 0) then begin
  analysis_mask = replicate(1B, Nmaster)
endif 
if (n_elements(analysis_mask) NE Nmaster) then begin
  print, 'ERROR (match_xy_analyze): ANALYSIS_MASK has the wrong number of elements!'
  return
endif


;; Build a null-filled composite catalog to hold data from all matches.
composite_record = {ID:0L, X:f_nan, Y:f_nan, X_ERR:f_nan, Y_ERR:f_nan}
tag_offset = 5

for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]

  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = create_struct(NAME=tag_names(cat_slave, /STRUCTURE_NAME))
  
  ; Prepend fields from the match record.
  cat_record        = create_struct('IsNull',1B, 'deltaX',f_nan, 'deltaY',f_nan, 'deltaX_error',f_nan, 'deltaY_error',f_nan, 'rank',f_nan, 'type',isolated_type, 'num_SM',0L, null_slave_record)
  
  composite_record  = create_struct(composite_record, cat_name, cat_record)
endfor ;jj

composite_cat = replicate(composite_record, Nmaster)
composite_cat.ID    = match_state.ID
composite_cat.X     = match_state.X
composite_cat.Y     = match_state.Y
composite_cat.X_ERR = match_state.X_ERR
composite_cat.Y_ERR = match_state.Y_ERR


;; These keep track of the type of match each master entry was involved across all catalogs
master_isolated_t   = replicate(0B,Nmaster)
master_failed_t     = replicate(0B,Nmaster)
master_successful_t = replicate(0B,Nmaster)

num_SM      = intarr(Ncat,Nmaster)

;; Open a ds9 region file summarizing the results.
composite_name = strlowcase((routine_names(composite_cat, ARG_NAME=(-1)))[0])
if (composite_name EQ '') then composite_name = 'composite_cat'

regfile1 = composite_name+'1.reg'
regfile2 = composite_name+'2.reg'

openw, regunit1, regfile1, /GET_LUN
openw, regunit2, regfile2, /GET_LUN
printf, regunit1, "# Region file format: DS9 version 3.0"
printf, regunit2, "# Region file format: DS9 version 3.0"

comment1 = "# Master Catalog (diamond), Slave Catalogs (+); successful primary match (green), failed primary match (red), secondary match (magenta), isolated master source (cyan diamond), unused slave source (blue +)"
printf, regunit1, comment1
printf, regunit1
printf, regunit1, min(match_state.X), max(match_state.Y), comment1, F='(%"text %f %f # text={%s} color=red")'


ind_wrap = indgen(Ncat) mod 6
color_list  = (['cyan'   ,'red'  ,'blue'  ,'magenta','yellow','white'])[ind_wrap]
symbol_list = (['diamond','cross','circle','box'    ,'X'     ,'arrow'])[ind_wrap]
comment2 = "# " + strjoin(match_state.cat_names+' ('+color_list+' '+symbol_list+')'  ,', ') + '; matches are green'
printf, regunit2, comment2
printf, regunit2
printf, regunit2, min(match_state.X), max(match_state.Y), comment2, F='(%"text %f %f # text={%s} color=red")'



;; For coding convenience the master catalog is replicated in both 
;; {match_state.ID, match_state.X/Y, match_state.X_ERR/Y_ERR} and 
;; in *match_state.catalogs[0] (where the matches appear as type "init_type"). 
;;
;; The loop below does two jobs: making ds9 regions for slave sources involved in matches, 
;; and adding catalog entries to composite_cat.
;; On the first iteration (jj EQ 0) the match type should be "init_type" and the ds9 regions are skipped.
;; We don't start the loop at 1 because we want to do the second job for all catalogs.

for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]
  Nslave         = n_elements(cat_slave)
  slave_written  = bytarr(Nslave)

  match_primary = *match_state.match_primary[jj]
  
  
  ;; Process the Primary Match table.  
  ; By sorting the successful matches ahead of the unsuccessful and using the slave_written flag array below we can get the slave colors right (i.e. a successful slave is guaranteed to be green even if it is also involved in another match).
  ind_good = where((match_primary.type EQ successful_primary_type) OR $
                   (match_primary.type EQ init_type), Ngood, COMPLEMENT=ind_bad, NCOMPLEMENT=Nbad)

  if     (Ngood EQ 0) then ind=ind_bad $
  else if (Nbad EQ 0) then ind=ind_good $
  else                     ind=[ind_good,ind_bad]   

  for ii=0L,Nmaster-1 do begin
    match  = match_primary[ind[ii]]
    
    save_slave_entry = 0
    
    ind_m  = where(match_state.ID EQ match.IDm)
    ind_s  = where(cat_slave.ID EQ match.IDs, count)

    if (count GT 0) then slave = cat_slave[ind_s] $
                    else slave = 0

    if (ind_m[0] NE ind[ii]) then message, 'Order is suspect!'

    
    if (match.type EQ isolated_type) then begin
      ; No slave region file to write.
      master_isolated_t[ind_m] = 1
      
    endif else if (match.type EQ init_type) then begin
      ; No slave region file to write; just write the slave data to the composite cat.
      save_slave_entry = 1
      
      ; Suppress writing any ds9 regions for this cat.
      slave_written[ind_s] = 1

    endif else begin
      ; Successful and failed PMs here.
      ; Write the slave region and a line segment showing the match.
      if (match.type EQ successful_primary_type) then begin
        ; For a successful match the slave and line are green; use red for unsuccessful.
        color = 'green'
        ltag  = cat_name + ' PM, good'
        thick = 1
        save_slave_entry = 1
        
        master_successful_t[ind_m] = 1

      endif else begin
        color = 'red'
        ltag  = cat_name + ' PM, failed'
        thick = 4
        
        master_failed_t[ind_m] = 1 
      endelse

      if (NOT slave_written[ind_s]) then begin
        ; In reg file #1 write a cross point for the slave involved in this PM.
        ; The color designated successful or failed.
        slave_written[ind_s] = 1
        printf, regunit1,                  slave.x, slave.y, cat_name, color,   F='(%"cross  point %f %f # tag={%s} color=%s")'
        
        ; In reg file #2 write the catalog-specific symbol in green for successful PM.
        if (match.type EQ successful_primary_type) then $
          printf, regunit2, symbol_list[jj], slave.x, slave.y, cat_name, 'green', F='(%"%s point %f %f # tag={%s} color=%s")'
      endif
 
      ; In reg file #1 write a line segment showing PM.
      printf, regunit1, match_state.x[ind_m], match_state.y[ind_m], slave.x, slave.y, ltag, color, thick, F='(%"line %f %f %f %f # tag={%s} color=%s width=%d")'
    endelse
    
    if save_slave_entry then begin
      ; Write slave entry to composite catalog plus fields from match record.
      
      record = composite_cat[ind_m].(tag_offset+jj)
      struct_assign, slave, record
      record.IsNull = 0
      record.deltaX = match.deltaX
      record.deltaY = match.deltaY
      
      record.deltaX_error = sqrt(match_state.X_ERR[ind_m]^2 + slave.X_ERR^2)
      record.deltaY_error = sqrt(match_state.Y_ERR[ind_m]^2 + slave.Y_ERR^2)
      
      record.rank   = match.rank
      record.type   = match.type
      
      composite_cat[ind_m].(tag_offset+jj) = record
    endif
  endfor ;ii

  
  ; Process the Secondary Match table.
  ; Consult slave_written flag before writing slave entry.
  if ptr_valid(match_state.match_secondary[jj]) then begin
    match_secondary = *match_state.match_secondary[jj]
    
    for ii=0L,n_elements(match_secondary)-1 do begin
      match  = match_secondary[ii]
      ind_m  = where(match_state.ID EQ match.IDm)
      ind_s  = where(cat_slave.ID   EQ match.IDs)
      slave  = cat_slave[ind_s]
      
      num_SM[jj,ind_m] = 1+ num_SM[jj,ind_m]
      
      color = 'magenta'
    
      if (NOT slave_written[ind_s]) then begin
        ; In reg file #1 write a cross point for the slave involved in this SM.
        slave_written[ind_s] = 1
        printf, regunit1,                  slave.x, slave.y, cat_name,           color,          F='(%"cross  point %f %f # tag={%s} color=%s")'
        
        ; In reg file #2 we don't show SMs, so just write this as an "unused" source using the catalog-specific symbol & color
        printf, regunit2, symbol_list[jj], slave.x, slave.y, cat_name+' unused', color_list[jj], F='(%"%s point %f %f # tag={%s} color=%s")'
      endif
      
      ; In reg file #1 write a line segment showing SM.
      printf, regunit1, match_state.x[ind_m], match_state.y[ind_m], slave.x, slave.y, cat_name + ' SM', color, F='(%"line %f %f %f %f # tag={%s} color=%s")'
    endfor ;ii
    
    composite_cat.(tag_offset+jj).num_SM = reform(num_SM[jj,*])
  endif ;ptr_valid()
  
  
  ; Write the unused slave entries.
  ind = where(slave_written EQ 0, count)
  for ii=0L,count-1 do begin
    slave  = cat_slave [ind[ii]]
  
    ; In reg file #1 all unused are blue cross points.
    printf, regunit1,                  slave.x, slave.y, cat_name+' unused',                 F='(%"cross  point %f %f # tag={%s} color=blue")'
    
    ; In reg file #2 use the catalog-specific symbol & color
    printf, regunit2, symbol_list[jj], slave.x, slave.y, cat_name+' unused', color_list[jj], F='(%"%s point %f %f # tag={%s} color=%s")'
  endfor

endfor ;jj


;; Add the master entries to the region file.

; Identify the sources with no match in ANY catalog.
isolated_flag = master_isolated_t AND NOT (master_failed_t OR master_successful_t)

print
print, 'These master catalog entries should be reviewed:'
; Since ds9 center-justifies text, we must offset the labels for readability, say by 0.2% of the field.
margin = ((max(match_state.x) - min(match_state.x)) > (max(match_state.y) - min(match_state.y))) / 500.
for ii=0L,Nmaster-1 do begin
  x  = match_state.x[ii]
  y  = match_state.y[ii]
  id = match_state.id[ii]
  color1=''
  color2=''
  
  ; We only warn about isolated sources if they have no match in ANY catalog.
  if (isolated_flag[ii]) then begin
      color1 = color_list[0]
      color2 = color_list[0]
      ptag  = 'master, isolated'
      print, id, x, y, ' isolated'
  endif
  
  ; If PM failed in ANY catalog then we want to report it, but if PM was EVER successful then we want to display it that way.
  if (master_failed_t[ii]) then begin
      color1 = 'red'
      color2 = color_list[0]
      ptag  = 'master, failed'
      print, id, x, y, ' PM failed'
  endif

  if (master_successful_t[ii]) then begin
      color1 = 'green'
      color2 = 'green'
      ptag   = analysis_mask[ii] ? 'master, matched & analyzed' : 'master, matched'
  endif
  
  if (color1 NE '') then begin
    ; Write a diamond region for the master. 
    ; In reg file #1 the color designates isolated, failed PM, successful PM.
    ; In reg file #2 the color designates (isolated,failed PM) vs. successful PM.
    printf, regunit2, symbol_list[0], x, y, ptag, color2, F='(%"%s point %f %f # tag={%s} color=%s")'
    printf, regunit1, symbol_list[0], x, y, ptag, color1, F='(%"%s point %f %f # tag={%s} color=%s")'
    printf, regunit2, x-margin, y-margin, 'ID, '+ptag, ID, color2, F='(%"text %f %f # tag={%s} text={%d} color=%s")'
    printf, regunit1, x-margin, y-margin, 'ID, '+ptag, ID, color1, F='(%"text %f %f # tag={%s} text={%d} color=%s")'
  endif else message, 'Bug ???'
endfor; ii
free_lun, regunit1
free_lun, regunit2

total_SM = total(num_SM,1)
ind = where(total_SM GT 0, count)
if (count GT 0) then begin
  print
  print, 'These master catalog entries have secondary matches:'
  forprint, match_state.ID[ind], match_state.X[ind], match_state.Y[ind], total_SM[ind]
endif

print
print, 'The region file ', regfile1, ' shows details of matching without distinguishing between slave catalogs using these symbols:'
print, comment1
print
print, 'The region file ', regfile2, ' shows each catalog with a different symbol with matches marked in green:'
print, comment2
print

;; Finally, show some match statistics to help the user choose catalog offsets.
for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  
  composite_cat_section = composite_cat.(tag_offset+jj)

  mask = (composite_cat_section.type EQ successful_primary_type) AND analysis_mask  
  
  good_ind = where( mask, Nanalysis, COMPLEMENT=bad_ind, NCOMPLEMENT=Nignore )

  if (jj EQ 0) then begin
  endif else if (Nanalysis EQ 0) then begin
    print, cat_name,' had NO primary matches that can suggest an offset for ', cat_name
  endif else begin
    ;; Examine the deltaX,deltaY offsets.
    deltaX = composite_cat_section.deltaX
    deltaY = composite_cat_section.deltaY
    
    deltaX_error = composite_cat_section.deltaX_error
    deltaY_error = composite_cat_section.deltaY_error
    
    ; The data for the catalog entries we do NOT want to analyze are set to NaN, rather than filtered away, so that the observer can read off indexes in dataset_2d.
    if (Nignore GT 0) then begin
      deltaX[bad_ind]       = f_nan
      deltaY[bad_ind]       = f_nan
      deltaX_error[bad_ind] = f_nan
      deltaY_error[bad_ind] = f_nan
    endif
    
    deltaX_title = 'Xmaster - Xslave'
    deltaY_title = 'Ymaster - Yslave'

    if keyword_set(arcsec_per_pixel) then begin
      deltaX       = arcsec_per_pixel * deltaX
      deltaY       = arcsec_per_pixel * deltaY
      deltaX_error = arcsec_per_pixel * deltaX_error
      deltaY_error = arcsec_per_pixel * deltaY_error
      deltaX_title = deltaX_title + ' (arcsec)'
      deltaY_title = deltaY_title + ' (arcsec)'
    endif

    dataset_1d, idx, deltaX, DATASET=cat_name, XTIT=deltaX_title
    dataset_1d, idy, deltaY, DATASET=cat_name, XTIT=deltaY_title
    
    dataset_2d, idxe, deltaX, deltaX_error, NAN_VALUES=[0,0], DATASET=cat_name, XTIT=deltaX_title, YTIT='Error on deltaX', PSYM=1
    dataset_2d, idye, deltaY, deltaY_error, NAN_VALUES=[0,0], DATASET=cat_name, XTIT=deltaY_title, YTIT='Error on deltaY', PSYM=1
    
    if keyword_set(custom1) then begin
      radius = sqrt(deltaX^2 + deltaY^2)
      theta  = (composite_cat.acis.theta)
      print, 'Median radial offset:', median(radius)
      dataset_2d, idr1, theta, radius, PSYM=1, DATASET=cat_name
      dataset_1d, idr2, radius, DATASET=cat_name
    endif
    
    print
    print, Nanalysis, cat_name, F='(%"The observed offsets (for the specified %d matches) from the slave catalog %s to the master catalog have been plotted.")'
    print, 'To improve the matching, re-run match_xy and supply XSHIFT/YSHIFT values which will be ADDED to the slave catalog.'
    print, 'Some suggested shifts are:'
    print, median(deltaX[good_ind]), median(deltaY[good_ind]), F='(%"median  : XSHIFT=%10.3g, YSHIFT=%10.3g")'
    meanclip, deltaX[good_ind], meanX, sigmaX
    meanclip, deltaY[good_ind], meanY, sigmaY
    print, meanX, meanY, sigmaX, sigmaY,   F='(%"meanclip: XSHIFT=%10.3g, YSHIFT=%10.3g  (clipsigma = %10.3g, %10.3g)")'
  endelse
endfor ;jj

return
end


;=============================================================================
; Estimate various performance rates of the matching algorithm via Monte Carlo simulations.
; This code is performing two different MC simulations simultaneously.

; One simulation estimates the false-positive (and true-negative) rate under the assumption that NO actual counterpart exists.

; The other simulation estimates the false-positive and false-negative (and true-positive) rates under the assumption that a counterpart (fake) does exist.

;=============================================================================
PRO match_xy_simulate, match_state, cat_name, Nsim

COMMON match_xy_simulate, num_cp_correct_match, num_cp_wrong_match, num_cp_false_negative, num_bkg_false_positive, num_bkg_true_negative, id1, id2

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

Nmaster    = n_elements(match_state.X)

;; Find this catalog in match_state.
cat_ind = (where(match_state.cat_names EQ cat_name, count))[0]
if (count EQ 0) then begin
  print, 'ERROR (match_xy_simulate): catalog not found.'
  return
endif

significance_threshold = match_state.sig_thresholds[cat_ind]
cat_slave              = *(match_state.catalogs      [cat_ind])
match_primary          = *(match_state.match_primary [cat_ind])


;; Remove the primary matches from cat_slave in an attempt to eliminate the population of true counterparts.
;; We'll be adding fake counterparts to cat_slave later; the total number of sources will be approximately unchanged.
;; We adopt the position errors of the actual matches as the population of position errors to use later for generating fake counterparts.
retain_ind = replicate(1B, n_elements(cat_slave))
for ii=0,Nmaster-1 do begin
  if (match_primary[ii].type EQ successful_primary_type) then begin
    retain_ind[ where(cat_slave.ID EQ match_primary[ii].IDs) ] = 0
  endif
endfor

X_err_population = cat_slave[where(retain_ind EQ 0)].X_ERR
Y_err_population = cat_slave[where(retain_ind EQ 0)].Y_ERR

dataset_1d, id1, X_err_population
dataset_1d, id1, Y_err_population

cat_slave = cat_slave[where(retain_ind)]
help, cat_slave

;; Choose a minimum catalog offset sufficient to randomize the phase between master and slave.
min_offset = 4 * (max(abs(match_primary.DELTAX)) > max(abs(match_primary.DELTAY)))

;; Generate random catalog offsets.
theta  = 2*!PI*random(Nsim)
radius = (min_offset + min_offset*random(Nsim))

xshift_slave = radius*sin(theta)
yshift_slave = radius*cos(theta)

dataset_2d, id2, xshift_slave, yshift_slave, PSYM=3


;; SIMULATION LOOP
num_cp_correct_match   = lonarr(Nsim)
num_cp_wrong_match     = lonarr(Nsim)
num_cp_false_negative  = lonarr(Nsim)
num_bkg_false_positive = lonarr(Nsim)
num_bkg_true_negative  = lonarr(Nsim)

for ii=0,Nsim-1 do begin
  ;; Generate fake counterparts for the master sources.  

  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = create_struct(NAME=tag_names(cat_slave, /STRUCTURE_NAME))
  cat_fake = replicate(null_slave_record, Nmaster)
  
  ; Assign IDs that are unique and easy to associate with the master source IDs.  
  cat_fake.ID = -(match_state.ID)
  
  ; Generate random counterpart position errors.
  N = n_elements(X_err_population)
  ind = (N-1) < floor(N*random(Nmaster))
  cat_fake.X_ERR = X_err_population[ind]
  cat_fake.Y_ERR = Y_err_population[ind]
  
  ; Generate random counterpart offsets from master positions.
  ; The offset random variables are Normal with variance equal to 
  ; the sum of the variances of master and slave (as assumed in match_xy routine).
  deltaX = sqrt(match_state.X_ERR^2 + cat_fake.X_ERR^2) * random(Nmaster, /NORMAL)
  deltaY = sqrt(match_state.Y_ERR^2 + cat_fake.Y_ERR^2) * random(Nmaster, /NORMAL)

  ; Their positions include the negative of the catalog offset so that
  ; they will end up matching the master positions.
  cat_fake.X = match_state.X - xshift_slave[ii] + deltaX
  cat_fake.Y = match_state.Y - Yshift_slave[ii] + deltaY
  
  
  ;; Perform the match using the augmented slave catalog.
  sim_cat_name = cat_name + '_SIM'
  match_xy, match_state, [cat_slave,cat_fake], sim_cat_name, significance_threshold, $
	    XSHIFT_SLAVE=xshift_slave[ii], YSHIFT_SLAVE=yshift_slave[ii], /QUIET

  ;; Find this catalog in match_state.
  sim_cat_ind = (where(match_state.cat_names EQ sim_cat_name, count))[0]
  if (count EQ 0) then begin
    print, 'ERROR (match_xy_simulate): catalog not found.'
    return
  endif
  help, sim_cat_ind

  ;; Tablulate interesting classes of match results.
  cp_correct_match  = bytarr(Nmaster)  ; Fake counterpart is primary match.
  cp_wrong_match    = bytarr(Nmaster)  ; Bkg source is primary match.
  cp_false_negative = bytarr(Nmaster)  ; No match found.
  
  bkg_false_positive= bytarr(Nmaster) ; Primary or a secondary match is bkg source
 ;bkg_true_negative = bytarr(Nmaster) ; NOT bkg_false_positive

  match_primary = *(match_state.match_primary [sim_cat_ind])
  for jj=0,Nmaster-1 do begin
    match = match_primary[jj]
    if (match.type EQ successful_primary_type) then begin
      ind_m = where(match_state.ID EQ match.IDm)
      if (match.IDm EQ -(match.IDs)) then begin
        ; Primary match was the fake counterpart.
        cp_correct_match[ind_m] = 1 
      endif else begin
        ; Primary match was a background source.
        cp_wrong_match    [ind_m] = 1
        bkg_false_positive[ind_m] = 1
      endelse
    endif else cp_false_negative[jj] = 1 ; No match found.
  endfor ;jj

  ; For a source with a secondary match, it must be the case that the 
  ; no-counterpart simulation would have produced a bkg_false_positive outcome.
  if ptr_valid(match_state.match_secondary[sim_cat_ind]) then begin
    match_secondary = *(match_state.match_secondary[sim_cat_ind])
    
    for jj=0,n_elements(match_secondary)-1 do begin
      match  = match_secondary[jj]
      ind_m  = where(match_state.ID EQ match.IDm)
      bkg_false_positive[ind_m] = 1
    endfor ;jj
  endif
  
  ; Test the flags for obvious inconsistencies.
  test = (cp_correct_match AND cp_wrong_match) OR (cp_wrong_match AND cp_false_negative) OR (cp_false_negative AND cp_correct_match)
  if (total(test) GT 0) then message, 'WRONG!'
  
  test = (cp_correct_match OR cp_wrong_match OR cp_false_negative) EQ 0
  if (total(test) GT 0) then message, 'WRONG!'

  num_cp_correct_match  [ii] = total(cp_correct_match,  /INTEGER)
  num_cp_wrong_match    [ii] = total(cp_wrong_match,    /INTEGER)
  num_cp_false_negative [ii] = total(cp_false_negative, /INTEGER)
  num_bkg_false_positive[ii] = total(bkg_false_positive,/INTEGER)
  num_bkg_true_negative [ii] = Nmaster - num_bkg_false_positive[ii]
endfor ;ii sim loop


;; Test the tabulations for obvious problems.
if (total((num_cp_correct_match+num_cp_wrong_match+num_cp_false_negative) NE Nmaster) NE 0) then message, 'WRONG!'

info, num_cp_correct_match
info, num_cp_wrong_match
info, num_cp_false_negative
info, num_bkg_false_positive
info, num_bkg_true_negative
print
print, 'Nmaster = ', Nmaster

return
end



PRO test2, significance_threshold

dim=100
x=10*indgen(dim)
y=x
make_2d,x,y
Nentries = n_elements(x)
x=reform(x,Nentries)
y=reform(y,Nentries)

entry = {ID:0L, X:0., Y:0., X_ERR:0., Y_ERR:0. }
cat = replicate(entry,Nentries)
cat.ID = 1+indgen(Nentries)

X_ERR = 0.01
Y_ERR = 0.05
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'one', /INIT

X_ERR = 0.5
Y_ERR = 0.1
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'two', significance_threshold

 
return
end



PRO test1, cat1, cat2, match_primary, match_secondary

record = {source, ID:0L, X:0.0, Y:0.0, X_ERR:1.0, Y_ERR:1.0}
cat2 = replicate({source},12)
x=indgen(4)
y=indgen(3)
make_2d,x,y
cat2.x=reform(x+1,12)
cat2.y=reform(y+1,12)
cat2.id=indgen(12)
cat2.X_ERR=0.5
cat2.Y_ERR=1

plot,cat2.x,cat2.y,line=0,psym=4,xrange=[0,4],yrange=[0,4]

cat1 = replicate({source},3)
cat1.x=[2.5,2.5,2.5]
cat1.y=[1.5,2,2.5]
cat1.id=indgen(3)
cat1.X_ERR=1
cat1.Y_ERR=0.5
oplot,cat1.x,cat1.y,line=0,psym=2

significance_threshold=0.1
match_xy,         cat1, cat2, significance_threshold, match_primary, match_secondary

match_xy_analyze, cat1, cat2, significance_threshold, match_primary, match_secondary
return
end





;=============================================================================
; Interactive Editing of Catalogs
;
; It's often useful to be able to prune catalogs interactively, e.g. while displayed
; on top of data in ds9.  
; The two routines below allow you to represent a catalog as a ds9 region file, 
; prune it in ds9, then read it back into IDL and apply that pruning to the 
; actual catalog.
;
; The ds9 property "tag={seq%d}" is used to refererence a region back to the catalog.
;
; The COLORS input (used with /WRITE_REGFILE) can be a vector of ds9 region color names.
;=============================================================================
PRO catalog_ds9_interface, cat, reg_filename, $
                           WRITE_REGFILE=write_regfile, COLORS=colors, $
                           PRUNE_CATALOG=prune_catalog

if keyword_set(write_regfile) then begin
  if NOT keyword_set(colors) then colors = replicate('green',n_elements(cat))
  
  openw, regunit1, reg_filename, /GET_LUN
  printf, regunit1, "# Region file format: DS9 version 3.0"

  tags = tag_names(cat)
  tag_flag = strmatch(tags, 'SHAPE', /FOLD) OR strmatch(tags, 'R', /FOLD) OR strmatch(tags, 'ROTANG', /FOLD)
  is_fits_region = total(tag_flag) EQ 3
  
  catalog_name_available = (total(strmatch(tag_names(cat),'CATALOG_NAME')) EQ 1)

  
  for ii=0,n_elements(cat)-1 do begin
    s=cat[ii]
    
    catalog_tag = catalog_name_available ? 'tag={'+s.CATALOG_NAME+'}' : ''
    
    if is_fits_region then begin
      ; Silly wavdetect can have ellipse radii that are NaN or zero, which can be invisible in ds9!
      ; The max() calls below take care of both.
      printf, regunit1, s.x, s.y, max([1, s.r[0]]), max([1, s.r[1]]), s.rotang, 1+ii, catalog_tag, colors[ii], F='(%"ellipse %f %f %f %f %f # tag={seq%d} %s color=%s")'
    endif else begin
      printf, regunit1, s.x, s.y, 1+ii, catalog_tag, colors[ii], F='(%"cross point %f %f # tag={seq%d} %s color=%s")'
    endelse
    
    printf, regunit1, s.x-1, s.y-1, 1+ii, colors[ii],  F='(%"text %f %f # tag={label} text={%d} color=%s")'

  endfor ;ii
  
  free_lun, regunit1
  print, 'Regions written to '+reg_filename
endif


if keyword_set(prune_catalog) then begin
  ; Read the lines from the region file.
  num_lines = numlines(reg_filename)
  lines = strarr(num_lines)  
  openr, regunit1, reg_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ; Find the lines with sequence tags.
  sequence_numbers = reform( (stregex(lines,'tag=\{seq(.*)\}',/SUB,/EXT))[1,*] )
  
  sequence_numbers = sequence_numbers[where(sequence_numbers NE '')]
  
  print, n_elements(cat) - n_elements(sequence_numbers), F='(%"Pruned %d sources.")'

  ; Sequence numbers are 1-based; array indexing is 0-based.
  good_ind = fix(sequence_numbers)-1
  
  cat = cat[good_ind]
  
endif

return
end



;=============================================================================
; Function to read a catalog represented as a ds9 region file with celestial coordinates.
; Sources can be represented by circle or ellipse regions.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_ds9_cat, region_filename, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


  ; Read the lines from the region file.
  num_lines    = numlines(region_filename)
  lines        = strarr(num_lines)  
  openr, regunit1, region_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ra   = dblarr(num_lines)
  dec  = dblarr(num_lines)
  region_type = strarr(num_lines)
  
  ; Parse "ellipse" regions for RA & DEC values.
  ; This regular expression should work for positive or negative DEC, 
  ; space or comma separation between parameters, 
  ; and with or without () in the region specification.
  result = stregex(lines,'ellipse[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'ellipse'
  endif

    ; Parse "circle" regions for RA & DEC values.
  result = stregex(lines,'circle[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'circle'
  endif

  ind = where(ra NE 0 AND dec NE 0, count)
  if (count EQ 0) then begin
    print, 'ERROR (build_ds9_cat): no source coordinates could be parsed'
    return, 0
  endif

  ra  = ra  [ind]
  dec = dec [ind]
  region_type= region_type[ind]
;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+indgen(Nentries)


;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; We arbitrarily assign a 1" error.
X_ERR = 1.0 / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','RA','DEC','REGION_TYPE']
tag_formats = 'J,F,F,F,F,D,D,A'                 

create_struct, cat, 'source_ds9', tag_names, tag_formats, DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a standard wavdetect FITS catalog
; The optional match_events_fn input provides a new definition for the sky coordinate system.
;=============================================================================
FUNCTION build_wavdetect_cat, catalog_fn, match_events_fn, NAME=cat_name

;; Read FITS catalog.
cat = mrdfits(catalog_fn, 1)

;; The (X_ERR,Y_ERR) from wavdetect are merely statistical errors for the mean event position within the extraction region.  
;; Systematic errors are not represented.
;; These errors can get unrealistically small  which causes us to miss matches that are clearly legitimate.
;; We choose to arbitrarily add in 0.2" error to ACIS positions to help deal with this.
systematic_error = 0.2  / 0.492 ; (0.492 arcsec/skypix)
cat.X_ERR = sqrt(cat.X_ERR^2 + systematic_error^2)
cat.Y_ERR = sqrt(cat.Y_ERR^2 + systematic_error^2)
                                                                                                                               

if keyword_set(match_events_fn) then begin
  ;; Convert celestial coordinates to a new tangent plane.

  ;; Read event file header to define a different sky coordinate system.
  theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
  if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn
  
   ; Build astrometic structure from data header.
  tbinfo, theader, tb_str
  colnames = strlowcase( strtrim(tb_str.ttype,2) )
  wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
  wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')
  
  wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
  wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY
  
  make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                             CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]
  
  ad2xy, cat.ra, cat.dec, event2wcs_astr, x, y
  ; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
  cat.X  = X+1
  cat.Y  = Y+1
endif ;keyword_set(match_events_fn) 

if keyword_set(cat_name) then begin
  if (total(strmatch(tag_names(cat),'CATALOG_NAME')) EQ 0) then begin
    ; We need to add a CATALOG_NAME column to the catalog.
    temp_cat = replicate(create_struct(cat[0], 'CATALOG_NAME', cat_name), n_elements(cat))
    struct_assign, cat, temp_cat
    cat = temp_cat
    cat.CATALOG_NAME = cat_name
  endif else print, 'WARNING!  The catalog already has a CATALOG_NAME tag; ignoring your NAME keyword.'
endif ;keyword_set(cat_name)

help, cat, /st
print, n_elements(cat), ' sources read.'
return, cat
end


;=============================================================================
; Function to build a catalog from the FITS table produced by ACIS Extract 
; in the COLLATED_FILENAME stage.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_AE_cat, collated_fn, match_events_fn

tb = mrdfits(collated_fn, 1)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn[jj]

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb.RA, tb.DEC, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The (EX_DATA,EY_DATA) from AE are merely statistical errors for the mean event position within the extraction region.  
;; Systematic errors (e.g. the extraction region placed in the wrong location) are not represented.
;; These AE errors can get unrealistically small (e.g. 0.07 skypix for a 120 count source on-axis) which causes us to miss matches that are clearly legitimate.
;; We choose to arbitrarily add in 0.2" error to ACIS positions to deal with this.
systematic_error = 0.2  ; arcsec
X_ERR = sqrt(tb.ERR_DATA^2 + systematic_error^2) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR']

entry = create_struct(NAME='source_ACIS', tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0.)
cat   = replicate(entry, Nentries)

;; Populate the structure.
for ii=0,n_tags(tb)-1 do cat.(ii) = tb.(ii)

ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
return, cat
end


;=============================================================================
; Function to read a standard NOMAD catalog in FITS format obtained through Vizier.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_NOMAD_cat, catalog_fn, match_events_fn

tb = mrdfits(catalog_fn, 1)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn[jj]

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb.RAJ2000, tb.DEJ2000, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of mas.
X_ERR = (tb.E_RADEG/1000.) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = (tb.E_DEDEG/1000.) / 0.492 ; (0.492 arcsec/skypix)


;; Evaluate the complex criteria which seem to be involved in deciding if the reported position is suspect.
temp = tb.Xflags
;temp[where(temp EQ '')] = '0'
Xflags = lonarr(Nentries)
reads, temp, Xflags, F='(Z)'


; Bit flags in Xflags column, shown in hex.
;  00001 = UBBIT   : Fails Blaise's test for USNO-B1.0 star
;  00002 = TMBIT   : Fails Roc's test for clean 2MASS star
;  00004 = YB6     : Included in YB6 catalog (Y)
;  00008 = 2MASS   : Included in 2MASS catalog (M)
;  00010 = TYBIT   : Astrometry comes from Tycho2 catalog (T)
;  00020 = XRBIT   : Alternative correlations for same (RA,Dec)
;  00040 = ITMBIT  : Alternative correlations for same 2MASS ID
;  00080 = IUCBIT  : Alternative correlations for same UCAC-2 ID
;  00100 = ITYBIT  : Alternative correlations for same Tycho2 ID
;  00200 = OMAGBIT : Blue magnitude from O (not J) plate (o)
;  00400 = EMAGBIT : Red magnitude from E (not F) plate (e)
;  00800 = TMONLY  : Object found only in 2MASS catalog (M)
;  01000 = HIPAST  : Ast from Hipparcos catalog (H)
;  02000 = SPIKE   : USNO-B1.0 diffraction spike bit set
;  04000 = TYCONF  : Tycho2 confusion flag set
;  08000 = BSCONF  : Bright star has nearby faint source
;  10000 = BSART
  
UBBIT  = (Xflags AND '00001'X ) NE 0
TMBIT  = (Xflags AND '00002'X ) NE 0
XRBIT  = (Xflags AND '00020'X ) NE 0
ITMBIT = (Xflags AND '00040'X ) NE 0
IUCBIT = (Xflags AND '00080'X ) NE 0
ITYBIT = (Xflags AND '00100'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
SPIKE  = (Xflags AND '02000'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
BSCONF = (Xflags AND '08000'X ) NE 0
BSART  = (Xflags AND '10000'X ) NE 0

; Values of the single-character column "r" indicating the origin of the reported position.
;      B = USNO-B1.0 (Monet et al., Cat. I/284)
;      C = UCAC2 (Zacharias et al., Cat. I/289)
;      M = 2MASS catalog of point sources (Cutri et al., Cat. II/246)
;      Y = YB6 Catalog (USNO, unpublished)
;      T = Tycho-2 Catalog (Hog et al., 2000, Cat. I/259)
;      H = Hipparcos catalog (Table I/239/hip_main)
;      o = Palomar-I blue (O) plate (for Bmag)
;      e = Palomar-I red  (E) plate (for Rmag)

fromUSNOB = (tb.R EQ 'B')      
from2MASS = (tb.R EQ 'M')      
fromTYCHO = (tb.R EQ 'T')      

bad1 = fromUSNOB AND (UBBIT or SPIKE)

bad2 = from2MASS AND TMBIT

bad3 = fromTYCHO AND TYCONF

low_quality = bad1 OR bad2 OR bad3 OR BSCONF OR BSART OR XRBIT OR ITMBIT OR IUCBIT OR ITYBIT


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','low_quality']

entry = create_struct(NAME='source_NOMAD', tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'low_quality', 0B)
cat   = replicate(entry, Nentries)

;; Populate the structure.
for ii=0,n_tags(tb)-1 do cat.(ii) = tb.(ii)


ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
return, cat
end


;=============================================================================
; Function to read a standard 2MASS catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_2mass_cat, catalog_fn, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


;; Replace the "null" strings with -99.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/null/-99/g'' %s > %s")')

; The 2MASS catalog actually has 30 columns, but readcol.pro can handle only 25, so we omit the last columns which happen to be unimportant.
column_names = ['ra','dec','err_maj','err_min','err_ang','designation','j_m','j_cmsig','j_msigcom','j_snr','h_m','h_cmsig','h_msigcom','h_snr','k_m','k_cmsig','k_msigcom','k_snr','ph_qual','rd_flg','bl_flg','cc_flg','ndet','gal_contam','mp_flg']

column_formats = 'D,D,D,D,I,A,D,D,D,D,D,D,D,D,D,D,D,D,A,A,A,A,A,I,I'

cmd = "readcol, 'temp.txt'," + strjoin(column_names,',') + ', F=column_formats, skipline=15'
print, cmd
dum=execute(cmd)

;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+indgen(Nentries)


;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The err_maj and err_min columns are error ellipse axes in arcseconds which we choose to average.
X_ERR = ((err_maj+err_min)/2) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR',column_names]
tag_formats = 'J,F,F,F,F,'                 +column_formats

create_struct, cat, 'source_2mass', tag_names, tag_formats, DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end

