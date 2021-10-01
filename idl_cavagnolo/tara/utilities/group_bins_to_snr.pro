;;; $Id: group_bins_to_snr.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;;; Group the bins of a histogram to achieve a specified signal-to-noise ratio.
;;;
;;; See calls in acis_extract.pro to understand usage.


PRO group_bins_to_snr, src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region,$
                       GROUP_WITHOUT_BACKGROUND=group_without_background,$
                       START_INDEX=start_index, STOP_INDEX=stop_index, $
                       SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range,  $
                       VERBOSE=verbose, $
                       this_snr_goal, group_codes

num_channels = n_elements(src_observed_counts)

if keyword_set(group_without_background) then begin
  bkg_observed_counts      = fltarr(num_channels)
  bkg_counts_in_src_region = fltarr(num_channels)
endif

;; Define a typical BACKSCAL value for use in error propagation later.
BACKSCAL = median(bkg_observed_counts / bkg_counts_in_src_region) 
if (NOT finite(BACKSCAL)) then BACKSCAL = 1E20

if (n_elements(start_index) NE 1) then start_index=0
if (n_elements(stop_index)  NE 1) then stop_index=num_channels

this_snr_goal         = snr_range[1]
this_num_groups_range = [num_groups_range[0] > 0, num_groups_range[1] > 3] 

;; ------------------------------------------------------------------------
GROUPING:
      ;; Group the source spectrum.  We believe that one must have the band limits of the spectrum in mind to perform an effective grouping.  Groups must not span the lower or upper PI limits that you wish to impose on the spectrum!  Astonishingly neither the grppha or dmgroup tools provide support for this.
      ;; We will define our group boundaries so that the user's lower energy limit is the start of the second group, and the upper energy limit is the end of a group.
      
      ; The first group starts at the first channel and the second group starts 
      ; at the first channel >= start_index.
      group1 = 0
      group2 = start_index > 0
      
      group_codes = replicate(-1,num_channels)
      group_codes[[group1,group2]] = 1
         
      ; We also set group2 to be the "proposed" group just to have the correct
      ; initial conditions for the loop below.
      proposed_group_start = group2
      SRC_CNTS             = 0L
      BKG_CNTS             = 0L
      scaled_bkg           = 0L
      group_width          = 0
      prev_group_width     = -1
      
      ; Carry on from the start of the second group defining other groups.
      for jj=group2,num_channels-1 do begin        
          ; Calculate the S/N of the group using Gehrels upper errors.
          net_counts = (SRC_CNTS - scaled_bkg)>0
          
          src_cnts_sigma_up = 1 + sqrt(src_cnts + 0.75)
          bkg_cnts_sigma_up = 1 + sqrt(bkg_cnts + 0.75)
          NET_CNTS_SIGMA_UP = sqrt(src_cnts_sigma_up^2 + (bkg_cnts_sigma_up/BACKSCAL)^2)
          
          snr = net_counts / NET_CNTS_SIGMA_UP
;print, SRC_CNTS, BKG_CNTS , net_counts, NET_CNTS_SIGMA_UP

          ; This option allows the observer to obtain a specific number of SRC_CNTS per group (the old style of grouping used in AE).
          if keyword_set(group_without_background) then snr = SRC_CNTS / src_cnts_sigma_up 

          ; Below we use GT instead of GE to ensure that when this_snr_goal is zero
          ; a group with non-positive net counts is avoided.
          ; The test involving "group_width" is used to fight the random appearance of very narrow (and thus very high flux) groups when working with low SNRs.
          if ((snr GT this_snr_goal) AND (group_width GT 0.2*prev_group_width)) then begin
            ; Accept the proposed group and propose the next one.
;help, src_cnts, proposed_group_start, jj 
            group_codes[proposed_group_start] = 1
            proposed_group_start = jj
;help, proposed_group_start
            SRC_CNTS             = 0L
            BKG_CNTS             = 0L
            scaled_bkg           = 0L
            prev_group_width     = group_width
            group_width          = 0
          endif
          
          ; Add the current channel to the proposed group.
          SRC_CNTS   = SRC_CNTS   + src_observed_counts[jj] 
          BKG_CNTS   = BKG_CNTS   + bkg_observed_counts[jj] 
          scaled_bkg = scaled_bkg + bkg_counts_in_src_region[jj]
          group_width= group_width+1
          
          ; If we've passed max_channel then define the last group & stop.
          if (jj GT stop_index) then begin
            group_codes[jj] = 1
            break
          endif
      endfor
      ; If the for loop ends normally (instead of via the break above) then stop_index was higher than the range of the spectrum.  The final proposed group is NOT accepted, and its members end up appended to the last accepted group (which will be larger than requested).

      ;; We futher impose an allowed range on the number of groups.  The SNR goal will be adjusted if this range is violated.  If the SNR goal tries to go below snr_range[0] then the search is terminated and num_groups will remain smaller than allowed for a fit (which is trapped ~20 lines further on below)
      num_groups = fix(total(group_codes>0))
      snr_delta = 0.2
      if ((this_snr_goal GT snr_range[0]) AND (num_groups LT this_num_groups_range[0])) then begin
        ; Lower the SNR goal and eliminate the upper limit on num_groups to prevent infinite looping here.
        if (this_snr_goal - snr_delta - snr_delta GT snr_range[0]) then begin
          ; If there's room to take two steps down then take one now.
          this_snr_goal = (this_snr_goal - snr_delta) > snr_range[0]
        endif else begin
          ; Otherwise take a final step directly to the lower limit, ensuring that the loop will stop next time.
          this_snr_goal = snr_range[0]
        endelse
        
        this_num_groups_range[1] = 10000
        if keyword_set(verbose) then print, this_snr_goal, F='(%"WARNING: too few groups; SNR goal lowered to %0.1f")'
        GOTO, GROUPING
      endif else if (num_groups GT this_num_groups_range[1]) then begin
        this_snr_goal = (this_snr_goal + snr_delta) 
        this_num_groups_range[0] = 0
        if keyword_set(verbose) then print, this_snr_goal, F='(%"WARNING: too many groups; SNR goal raised to %0.1f")'
        GOTO, GROUPING      
      endif
     
return
end

