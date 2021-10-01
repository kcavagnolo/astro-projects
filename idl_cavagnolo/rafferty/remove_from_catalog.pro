pro remove_from_catalog,system_name,catalog_file=catalog_file
;-----------------------------------------------------------------------
;
; Name: REMOVE_FROM_CATALOG
;
; Purpose: Removes a cluster entry from a centralized catalog
;          
; Inputs:  system_name - Name of system (may include spaces)
;
; Optional: catalog_file - name of catalog fits file to use. If it does not
;			   exist, it will be created. 
;			   (Default =
;			   /home/rafferty/Catalog/catalog.fits)
;         
; Comments:            
;           
; Revision history:
;       written by DR, 2007-3-20
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np ne 1) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'remove_from_catalog, system_name [, catalog_file=catalog_file]'
    return   
endif


;
; Set defaults
;
if (n_elements(catalog_file) eq 0) then catalog_file='/home/rafferty/Catalog/catalog.fits'
num=100                         ; number of elements in each array


;
; Create structure:
; First, check if catalog file already exists; if so, check if entry
; exists; if so, delete entry
;
cat_file=findfile(catalog_file,count=numfiles)
if (numfiles eq 1) then begin
    catalog_old=mrdfits(catalog_file,1,hd)
    nsys=n_elements(catalog_old.system)
    indx=where(strtrim(catalog_old.System,2) eq strtrim(system_name,2))
    if (indx ne -1) then begin
        if (indx eq 0) then begin
            sys_props=catalog_old[1:*]
        endif
        if (indx eq nsys-1) then begin
            sys_props=catalog_old[0:indx-1]
        endif
        if ( (indx ne 0) and (indx ne nsys-1) ) then begin
            sys_props=[catalog_old[0:indx],catalog_old[indx+1:*]]
        endif
        print,' '
        print,'Removing '+strtrim(system_name,2)+' from catalog...'
        mwrfits,sys_props,catalog_file,/create
        print,'...done'
    endif else begin
        print,'ERROR: System not found in catalog.'
        return
    endelse
endif else begin
    print,'ERROR: Catalog file not found.'
    return
endelse


;
; Print status info to screen
;
skip_to_end:
print,' '
print,'REMOVE_FROM_CATALOG complete.'
print,catalog_file+' has been updated.'


;
; Return to IDL
;
return
end
