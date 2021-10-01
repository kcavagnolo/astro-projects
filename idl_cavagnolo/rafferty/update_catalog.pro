pro update_catalog,system_name,tag_name,tag_value,catalog_file=catalog_file
;-----------------------------------------------------------------------
;
; Name: UPDATE_CATALOG
;
; Purpose: Update a cluster property in a centralized catalog
;          
; Inputs:  system_name - Name of system (may include spaces)
;          tag_name - Name of tag to update
;          tag_value - New tag value
;
; Optional: catalog_file - name of catalog fits file to use. If it does not
;			   exist, it will be created. 
;			   (Default =
;			   /home/rafferty/Catalog/catalog.fits)
;         
; Comments:            
;           
; Revision history:
;       written by DR, 2007-2-20
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np ne 3) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'update_catalog, system_name, tag_name, tag_value [, catalog_file=catalog_file]'
    return   
endif


;
; Set defaults
;
if (n_elements(catalog_file) eq 0) then catalog_file='/home/rafferty/Catalog/catalog.fits'


;
; Create structure:
; First, check if catalog file already exists; if so, check if entry
; exists; if so, update entry
;
cat_file=findfile(catalog_file,count=numfiles)

if (numfiles eq 1) then begin
    catalog_old=mrdfits(catalog_file,1,hd)
    indx=where(strtrim(catalog_old.System,2) eq strtrim(system_name,2))

    if (indx ne -1) then begin
        sys_props=catalog_old
        tags=tag_names(sys_props)
        tag_indx=where(tags eq strupcase(tag_name),cnt)
        if cnt eq 0 then begin
            print,'ERROR: '+tag_name+' not found in '+catalog_file
            return
        endif
        sys_props[indx].(tag_indx)=tag_value
        mwrfits,sys_props,catalog_file,/create
    endif else begin
        print,'ERROR: '+system_name+' not found in '+catalog_file
        return
    endelse
endif else begin
    print,'ERROR: '+catalog_file+' not found'
    return
endelse

print,'Tag '+tag_name+' updated in '+catalog_file+'.'
return
end
