pro add_catalog_tag,tag_name,outfile=outfile,array=array,string=string,catalog_file=catalog_file,size=size,value=value
;-----------------------------------------------------------------------
;
; Name: ADD_CATALOG_TAG
;
; Purpose: Add a new tag to a centralized catalog
;          
; Inputs:  tag_name - Name of tag to update
;
; Optional: catalog_file - name of catalog fits file to use. If it does not
;			   exist, it will be created. 
;			   (Default =
;			   /home/rafferty/Catalog/catalog.fits)
;           /array - defines tag as an array
;           /string - defines tag as a string
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
if (np ne 1) then begin
    print,string(7B),'CALLING SEQUENCE: ', $
      'add_catalog_tag, tag_name [, /string, /array, catalog_file=catalog_file, outfile=outfile, size=size]'
    return   
endif


if (n_elements(catalog_file) eq 0) then catalog_file='/home/rafferty/Catalog/catalog.fits'
if (n_elements(size) eq 0) then size=100 

cat=mrdfits(catalog_file,1)

if (n_elements(value) eq 0) then begin
    if not keyword_set(array) and not keyword_set(string) then add_tag,cat,tag_name,0d,new_cat 
    if keyword_set(array) then add_tag,cat,tag_name,dblarr(size),new_cat
    if keyword_set(string) then add_tag,cat,tag_name,' ',new_cat
endif else begin
    if not keyword_set(array) and not keyword_set(string) then add_tag,cat,tag_name,value,new_cat 
endelse

if (n_elements(outfile) eq 0) then outfile=catalog_file
mwrfits,new_cat,outfile,/create

print,'Tag '+tag_name+' added to '+catalog_file+'.'
return
end
