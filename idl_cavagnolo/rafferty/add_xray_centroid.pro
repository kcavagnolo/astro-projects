pro add_xray_centroid,system=system,topsy=topsy,turvy=turvy

if not keyword_set(topsy) and not keyword_set(turvy) then begin
    print,'You must specify /topsy or /turvy!'
    return
endif


;
; Read in catalog
;
cat=mrdfits('/home/rafferty/Catalog/catalog.fits',1)


;
; Loop through catalog
;
for i=0,n_elements(cat)-1 do begin


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
        print,' '


;
; Read in image
;
        spawn,'gunzip images/ccd_img_clean.fits.gz'
        im=readfits('images/ccd_img_clean.fits',hd,/silent)
        xoffset=sxpar(hd,'CRVAL1P')
        yoffset=sxpar(hd,'CRVAL2P')
        xi=xc-xoffset
        yi=yc-yoffset
        print,'Image x centroid [pixels] : ',xi
        print,'Image y centroid [pixels] : ',yi


;
; Convert x,y coords to a,d
;
        xyad,hd,xi,yi,a,d       ; a and d are in degrees
        print,' '
        print,'RA centroid [deg] : ',a
        print,'DEC centroid [deg] : ',d
        print,' '

;
; Update catalog file
;
        print,'Now updating '+strtrim(cat[i].system,2)+'...'
        update_catalog,cat[i].system,'ra',a
        update_catalog,cat[i].system,'dec',d


    endelse
skip_system:
endfor


return
end
