pro add_xray_sb,system=system,topsy=topsy,turvy=turvy,start_indx=start_indx,bin=bin

if not keyword_set(topsy) and not keyword_set(turvy) then begin
    print,'You must specify /topsy or /turvy!'
    return
endif


;
; Read in catalog
;
cat=mrdfits('/home/rafferty/Catalog/catalog.fits',1)
if (n_elements(start_indx) eq 0) then start_indx=0
if (n_elements(bin) eq 0) then bin=4.0


;
; Loop through catalog
;
for i=start_indx,n_elements(cat)-1 do begin


;
; Check if catalog entry has an X-ray analysis
;
    if (n_elements(system) ne 0) then begin
        if ( strtrim(cat[i].system,2) ne strtrim(system,2) ) then goto,skip_system
    endif
    if (strtrim(cat[i].xray_path,2) eq '') then begin
        print,strtrim(cat[i].system,2)+' does not have an X-ray analysis'
        print,'Skipping ...'
    endif else begin

;
; Check to see if the object is on topsy or turvy
;
        if keyword_set(turvy) then begin
            pos=strpos(strtrim(cat[i].xray_path,2),'birzan')   
            if (pos eq -1) then goto,skip_system
        endif
        if keyword_set(topsy) then begin
            pos=strpos(strtrim(cat[i].xray_path,2),'Rafferty')   
            if (pos eq -1) then goto,skip_system
        endif


;
; Read in annular properties from annuli.txt
;
        cd,strtrim(cat[i].xray_path,2)
        fmt = 'A,F'
        readcol,'./spectra/annuli.txt',skipline=5,f=fmt,junk,annuli_cent,delimiter=':',numline=2,/silent
        readcol,'./spectra/annuli.txt',skipline=11,f=fmt,junk,annuli_prop,delimiter=':',numline=2,/silent
        xc=annuli_cent[0]
        yc=annuli_cent[1]
        ellip=annuli_prop[0]
        pa=annuli_prop[1]
        print,' '
        print,'Physical x centroid [pixels] : ',xc
        print,'Physical y centroid [pixels] : ',yc


;
; Define root directory for CIAO
;
        get_lun,unit
        openr,unit,'ciao_info.txt'
        ciao_root=' '
        readf,unit,ciao_root
        close,unit
        free_lun,unit


;
; Call Mike Wise's tool calc_sb_image.pro
; 
        cd,strtrim(cat[i].xray_path,2)
        print,'Now calculating surface brightness profile...'
        spawn,'gunzip ./images/ccd_img_clean.fits.gz'
        spawn,'gunzip ./spectra/expmap_ccd.fits.gz'
        src_regfile=findfile('regions/ccd_src_alt.reg',count=alt_reg)
        if (alt_reg eq 0) then regfile='./regions/ccd_src.reg' else regfile='./regions/ccd_src_alt.reg'
        calc_sb_imagep,'./images/ccd_img_clean.fits', $
          xc=xc,yc=yc,outfile='./spectra/sb_bin4.dat', $
          xybin=bin,expfile='./spectra/expmap_ccd.fits',$
          ccdregfile='./regions/ccd.reg',regfile=regfile, $
          ciao_path=ciao_root

        print,'...done.'


;
; Read in sb corrected for exposure
;
        scale=0.4919 
        readcol, './spectra/sb_bin4.dat',skipline=16,r1,r2,sb_ec,sb_ecerr,sb,sberr,area
        sb_ec=sb_ec/scale^2.0   ; convert to cnts/s/arcsec^2
        sb_ecerr=sb_ecerr/scale^2.0
        sb_ec_elements=where(sb_ec gt 0.0, count)
        sb_ec_nonzero=sb_ec[sb_ec_elements]
        sb_ecerr_nonzero=sb_ecerr[sb_ec_elements]
        

;
; Update catalog file
;
        print,'Now updating '+strtrim(cat[i].system,2)+'...'
        update_catalog,cat[i].system,'sbx',dblarr(1000)
        update_catalog,cat[i].system,'sbx_err',dblarr(1000)
        update_catalog,cat[i].system,'sbx',sb_ec_nonzero
        update_catalog,cat[i].system,'sbx_err',sb_ecerr_nonzero
        update_catalog,cat[i].system,'sbx_bin',bin

    endelse
skip_system:
endfor


return
end
