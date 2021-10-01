#! /bin/csh -f

# Purpose: Compresses the important data for the object:
#		- Cleaned image
#		- EVT2 file, BG file, and GTI file
#          
#          
# Inputs: spectra_dir - name of spectra directory, default is 'spectra'
#         
#         
# Comments: 
#           
#           
# Revision history:
#       written by DAR, 2004-11-01
#-----------------------------------------------------------------------

if ($#argv > 1) then
  echo "Syntax: compress <spectra_dir>"
  exit 0
endif
 
if ($#argv < 1) then
  set spectra_dir = spectra
else  
  set spectra_dir = $argv[1]
endif

echo " "
echo "Now compressing files, please wait ..."

# Compress files in Obs
gzip -v *.fits

# Compress files in primary
cd primary
   gzip -v *.fits
cd ..

# Compress files in secondary
cd secondary
   gzip -v *.fits
cd ..
   
# Compress files in reprocessed
cd reprocessed
   gzip -v *.fits
cd ..

# Compress files in background
cd background
   gzip -v *.fits
cd ..

# Compress files in spectra
cd $spectra_dir
   gzip -v *_sou*				# new spectra files
   gzip -v *_bgd.*				# new background files
   gzip -v *.fits
cd ..
   
# Compress files in images
cd images
   gzip -v *.fits
cd ..

# Exit
exit 0
