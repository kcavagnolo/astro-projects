#! /bin/csh -f

# Purpose: Saves the important data for the object:
#		- All plots and logs
#		- Results of model fits
#		- Region files
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
  echo "Syntax: clean_up <spectra_dir>"
  exit 0
endif
 
if ($#argv < 1) then
  set spectra_dir = spectra
else  
  set spectra_dir = $argv[1]
endif

# First list the files to be deleted
echo " "
echo "The following files will be deleted:"
echo " "
ls -l lc_clean_wrap.sl			# script file
ls -l cfn.dat				# cooling function data

# list files in primary
cd primary
  echo "Primary directory:"
  chmod u+w *
  ls -l *_evt2.fits			# original evt2 file
  ls -l *_img2*				# original images
  ls -l *_asol*.fits			# original asol file
  ls -l *_sum2*				# original html files
  ls -l *_fov1.fits			# original fov file
  ls -l *_eph1.fits			# original ehp file
cd ..

# list files in secondary
cd secondary
  echo "Secondary directory:"
  chmod u+w *
  ls -l *_evt1.fits			# evt1 files
  ls -l *_?off1.fits			# aoff1 and soff1 files
  ls -l *_?lt1.fits			# flt1 and mlt1 files
  ls -l *_stat1.fits			# stat1 file
  ls -l *_bias0.fits			# bias0 files
  ls -l *_pbk0.fits			# pbk0 file
  chmod u+w aspect/*
  ls -l aspect/*			# all files in aspect dir
  chmod u+w ephem/*
  ls -l ephem/*				# all files in ephem dir
cd ..

# list files in supporting
#cd supporting
#  echo "Supporting directory:"
#  chmod u+w *
#  ls -l *.*				# all original files
#cd ..

# list files in reprocessed
cd reprocessed
  echo "Reprocessed directory:"
  ls -l acis*evt1.fits			# intermediate evt1 files
  ls -l acis*evt2.fits 			# intermediate evt2 files
  ls -l evt2_ccd.fits			#  "
  ls -l evt2_ccd_clean_no_src.fits	#  "
  ls -l img8_ccd.fits  			# intermediate image
  ls -l evt2_c7.fits			# old intermediate evt2 file
  ls -l evt2_c7_clean_no_src.fits	#  "
  ls -l evt2_c7_clean.fits		#  "
  ls -l img8_c7.fits  			# old intermediate image
cd ..

# list files in background
cd background
  echo "Background directory:"
  ls -l bg_*.*				# intermediate bg files
  ls -l ccd_*.fits			# wavedetect files
  ls -l s3_*.fits			# old wavedetect files
  ls -l cluster_cnts*.*			# count files
  ls -l cnts*.fits			#  "
  ls -l evt2_*.fits			# intermediate evt2 files
  ls -l bgevt2_*.fits			#  "
  ls -l tot_cnts.*			# count files
  ls -l source_cnts.*			#  "
cd ..

# list files in spectra
cd $spectra_dir
  echo "Spectra directory ($spectra_dir):"
#  ls -l *_sou*				# new spectra files
  ls -l reg?.[pw]*  			# old spectra files
  ls -l reg??.[pw]*  			# old spectra files
  ls -l reg?_bg.*  			# old spectra files
  ls -l reg??_bg.*  			# old spectra files
#  ls -l *_bgd.*			# new background files
  ls -l bg.fits				# bg file copy
  ls -l evt2*.fits			# evt2 file copy
  ls -l acisspec_mod			# ciao script
cd ..

# list files in images
cd images
  echo "Images directory:"
  ls -l ccd_img_?.*.fits		# color maps
cd ..


# Next, ask for confirmation
echo " "
echo "Do you want to delete these files (y/n)?"
switch ( $< )		                # Get a line from user

case y:
   # If OK, delete files

   # Delete files in Obs
   unalias rm
     rm lc_clean_wrap.sl		# script file
     rm cfn.dat				# cooling function data

   # Delete files in primary
   cd primary
     rm *_evt2.fits			# original evt2 file
     rm *_img2.fits			# original images
     rm *_asol*.fits			# original asol file
     rm *_sum2*				# original html files
     rm *_fov1.fits			# original fov file
     rm *_eph1.fits			# original ehp file
   cd ..

   # Delete files in secondary
   cd secondary
     rm *_evt1.fits			# evt1 files
     rm *_?off1.fits			# aoff1 and soff1 files
     rm *_?lt1.fits			# flt1 and mlt1 files
     rm *_stat1.fits			# stat1 file
     rm *_bias0.fits			# bias0 files
     rm *_pbk0.fits			# pbk0 file
     rm aspect/*			# all files in aspect dir
     rm ephem/*				# all files in ephem dir
   cd ..
   
   # Delete files in supporting
#   cd supporting
#     rm *.*				# all original files
#   cd ..

   # Delete files in reprocessed
   # Keep "evt2_ccd_clean.fits" and "evt2_ccd_clean_no_ptsrc.fits"
   cd reprocessed
     rm acis*evt1.fits			# intermediate evt1 files
     rm acis*evt2.fits 			# intermediate evt2 files
     rm evt2_ccd.fits			#  "
     rm evt2_ccd_clean_no_src.fits	#  "
     rm img8_ccd.fits  			# intermediate image
     rm evt2_c7.fits			# old intermediate evt2 file
     rm evt2_c7_clean_no_src.fits	#  "
     rm evt2_c7_clean.fits		#  "
     rm img8_c7.fits  			# old intermediate image
   cd ..

   # Delete files in background
   # Keep "bg.fits"
   cd background
     rm bg_*.*				# intermediate bg files
     rm ccd_*.fits			# wavedetect files
     rm s3_*.fits			# old wavedetect files
     rm cluster_cnts*.*			# count files
     rm cnts*.fits			#  "
     rm evt2_*.fits			# intermediate evt2 files
     rm bgevt2_*.fits			#  "
     rm tot_cnts.*			# count files
     rm source_cnts.*			#  "
   cd ..

   # Delete files in spectra
   cd spectra
#    rm *_sou*				# new spectra files
     rm reg?.[pw]*  			# old spectra files
     rm reg??.[pw]*  			# old spectra files
     rm reg?_bg.*  			# old spectra files
     rm reg??_bg.*  			# old spectra files
#    rm *_bgd.*				# new background files
     rm bg.fits				# bg file copy
     rm evt2*.fits			# evt2 file copy
     rm acisspec_mod			# xspec script
   cd ..
   
   # Delete files in images
   cd images
     rm ccd_img_?.*.fits		# color maps
   cd ..

   breaksw
case n:				# If not OK, exit
   echo "No files deleted."
   exit 0
endsw

# Exit
exit 0
