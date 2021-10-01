pro make_isis_script,nH_gal,kT_guess,ab_guess,redshift,lo_energy,hi_energy,bin
;-----------------------------------------------------------------------
;
; Name: MAKE_ISIS_SCRIPT
;
; Purpose: Creates an ISIS script to fit the spectra extracted by
;	   EXTRACT_ANNULI with the wabs*mekal model.
;          
;          
; Inputs:  Initial guesses for the parameters, energy range, and binning
;
;         
; Comments: Assumes standard pipeline naming conventions are followed
;           
;           
; Revision history:
;       written by DR, 2004-6-29
;-----------------------------------------------------------------------
binning=fix(bin)


;
; Open script file
;
get_lun,unit
openw,unit,'wabs_mekal.sl'

;
; Write script
;
printf,unit,'%------------------------------------------------------------'
printf,unit,'%  Name:  WABS_MEKAL.SL'
printf,unit,'%'
printf,unit,'%  Purpose:  Reads in region PHA files and the associated'
printf,unit,'%            ARF, RMF, and background files and fits with'
printf,unit,'%            a wabs*mekal XSPEC model.  Fits with nH varying,'
printf,unit,'%            with nH fixed to the average, and with nH fixed'
printf,unit,'%	          to the galactic value'
printf,unit,'%'
printf,unit,'%  Inputs:  nreg - number of regions'
printf,unit,'%'
printf,unit,'%  Comments:'
printf,unit,'%'
printf,unit,'%  Revision history:'
printf,unit,'%       written by D&L, 2002-12-05'
printf,unit,'%	     added nH galactic fit (DR), 2003-04-09'
printf,unit,'%	     updated to use output of acisspec (DR), 2004-3-11'
printf,unit,'%-------------------------------------------------------------'
printf,unit,' '
printf,unit,' '
printf,unit,'variable infile,bgfile,rmffile,arffile,nreg,nregint,nHtot,nHav,x,d;'
printf,unit,'variable nH_guess,nH_gal,kT_guess,ab_guess,red_guess;'
printf,unit,'variable lo_energy,hi_energy,binning;'
printf,unit,' '
printf,unit,'    % Define the initial guesses for nH, kT,'
printf,unit,'    % abundance, and redshift, as well as the'
printf,unit,'    % desired energy range and binning for fitting:'
printf,unit,'    '
printf,unit,'nH_guess='+strtrim(string(nH_gal),2)+';		% wabs absorption [10^22 cm^-2]'
printf,unit,'nH_gal='+strtrim(string(nH_gal),2)+';		% galactic value of absorption [10^22 cm^-2]'
printf,unit,'kT_guess='+strtrim(string(kT_guess),2)+';		% kT [keV]'
printf,unit,'ab_guess='+strtrim(string(ab_guess),2)+';		% abundance [relative to solar]'
printf,unit,'red_guess='+strtrim(string(redshift),2)+';	        % redshift'
printf,unit,'lo_energy='+strtrim(string(lo_energy),2)+';	% low energy cutoff [keV]'
printf,unit,'hi_energy='+strtrim(string(hi_energy),2)+';	% high energy cutoff [keV]'
printf,unit,'binning='+strtrim(string(binning),2)+';		% number of counts to bin'
printf,unit,' '
printf,unit,'    % Read in the command line arguments'
printf,unit,'    '
printf,unit,'nregint=integer(__argv[2]);'
printf,unit,'x=nregint;'
printf,unit,'nreg=typecast (x, Integer_Type);'
printf,unit,' '
printf,unit,'variable KT=Double_Type [nreg];'
printf,unit,'variable KTlo=Double_Type [nreg];'
printf,unit,'variable KThi=Double_Type [nreg];'
printf,unit,'variable Ab=Double_Type [nreg];'
printf,unit,'variable Ablo=Double_Type [nreg];'
printf,unit,'variable Abhi=Double_Type [nreg];'
printf,unit,'variable nH=Double_Type [nreg];'
printf,unit,'variable nHlo=Double_Type [nreg];'
printf,unit,'variable nHhi=Double_Type [nreg];'
printf,unit,'variable norm=Double_Type [nreg];'
printf,unit,'variable normlo=Double_Type [nreg];'
printf,unit,'variable normhi=Double_Type [nreg];'
printf,unit,'variable redchi=Double_Type [nreg];'
printf,unit,'variable nHavg=Double_Type [nreg];'
printf,unit,'variable nHg=Double_Type [nreg];'
printf,unit,'variable s, n, v, plotfile;'
printf,unit,'variable info_struct=struct{statistic,num_variable_params,num_bins};'
printf,unit,' '
printf,unit,'    % Import the XSPEC module, and'
printf,unit,'    % initialize the model'
printf,unit,' '
printf,unit,'import("xspec");'
printf,unit,'fit_fun ("wabs(1) * mekal(1)");'
printf,unit,' '
printf,unit,'    % Set the cosmology H0, q0, lambda0'
printf,unit,' '
printf,unit,'  xspec_set_cosmo(70,0.5,0.7);'
printf,unit,' '
printf,unit,'    % Load the data, responses'
printf,unit,'    % and a background spectrum.'
printf,unit,' '
printf,unit,'for (i=0;i<nreg;i++) {'
printf,unit,' '
printf,unit,'  infile="reg"+string(i+1)+"_sou.pi";'
printf,unit,'  bgfile="reg"+string(i+1)+"_bgd.pi";'
printf,unit,'  d=load_data(infile);'
printf,unit,'  () = define_bgd (i+1, bgfile);'
printf,unit,' '
printf,unit,' '
printf,unit,'  rebin_data(i+1,binning);'
printf,unit,' '
printf,unit,'    % There are several ways to specify the parameters'
printf,unit,'    % This way is clear and human-readable:'
printf,unit,' '
printf,unit,'  set_par ("wabs(1).nH", nH_guess);'
printf,unit,'  set_par ("mekal(1).kT", kT_guess);'
printf,unit,'  set_par ("mekal(1).Abundanc", ab_guess);'
printf,unit,'  set_par ("mekal(1).Redshift", red_guess);'
printf,unit,' '
printf,unit,'    % Notice the energy range lo-hi keV in each dataset,'
printf,unit,'    % and ignore the rest of the spectrum:'
printf,unit,' '
printf,unit,'  ignore([1:i+1]);'
printf,unit,'  xnotice_en (i+1, lo_energy, hi_energy);'
printf,unit,' '
printf,unit,'    % Thaw nH, kT, abundance'
printf,unit,' '
printf,unit,'  thaw ("wabs(1).nH","mekal(1).kT","mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'    % Set the switch parameter to 1 (interpolate)'
printf,unit,' '
printf,unit,'  set_par ("mekal(1).Switch", 1);'
printf,unit,' '
printf,unit,'    % Do the fit, and'
printf,unit,'    % save the reduced chi-squared value as redchi'
printf,unit,' '
printf,unit,'  () = renorm_counts;'
printf,unit,'  () = fit_counts(&info_struct);'
printf,unit,' '
printf,unit,'  s=info_struct.statistic;'
printf,unit,'  n=info_struct.num_bins;'
printf,unit,'  v=info_struct.num_variable_params;'
printf,unit,'  redchi[i]=s/(n-v);'
printf,unit,' '
printf,unit,'    % .. and generate a plot.'
printf,unit,' '
printf,unit,'  plotfile="reg"+string(i+1)+".ps/cps";'
printf,unit,'  variable pid = plot_open (plotfile);'
printf,unit,' '
printf,unit,'  ylog;'
printf,unit,'  xlog;'
printf,unit,'  xrange(0.5,8.0);'
printf,unit,'  rplot_counts (i+1);'
printf,unit,' '
printf,unit,'  plot_close (pid);'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for nH'
printf,unit,' '
printf,unit,'  (nHlo[i],nHhi[i])=conf( "wabs(1).nH");'
printf,unit,'  while (nHlo[i]==nHhi[i]) (nHlo[i],nHhi[i])=conf( "wabs(1).nH");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for KT'
printf,unit,' '
printf,unit,'  (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,'  while (KTlo[i]==KThi[i]) (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for Ab'
printf,unit,' '
printf,unit,'  (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,'  while (Ablo[i]==Abhi[i]) (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'  % Save the highest and lowest values for norm'
printf,unit,' '
printf,unit,'  (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,'  while (normlo[i]==normhi[i]) (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,' '
printf,unit,'   % Save the best value for nH'
printf,unit,' '
printf,unit,'  nH[i]=get_par("wabs(1).nH");'
printf,unit,' '
printf,unit,'    % Save the best value for KT'
printf,unit,' '
printf,unit,'  KT[i]=get_par("mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the best value for Ab'
printf,unit,' '
printf,unit,'  Ab[i]=get_par("mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'  % Save the best value for norm'
printf,unit,' '
printf,unit,'  norm[i]=get_par("mekal(1).norm");'
printf,unit,' '
printf,unit,'}'
printf,unit,' '
printf,unit,'    % Output values for nH varied'
printf,unit,' '
printf,unit,'writecol("nHvaried.dat",KT,KTlo,KThi,Ab,Ablo,Abhi,nH,nHlo,nHhi,norm,normlo,normhi,redchi);'
printf,unit,' '
printf,unit,'    % Calculate average nH, nHav'
printf,unit,' '
printf,unit,'nHtot=0;'
printf,unit,'for(i=0;i<nreg;i++) {'
printf,unit,'  nHtot+=nH[i];'
printf,unit,'}'
printf,unit,'nHav=nHtot/nreg;'
printf,unit,' '
printf,unit,'    % Fit again with nH fixed to the average value'
printf,unit,' '
printf,unit,'ignore([1:nreg]);'
printf,unit,' '
printf,unit,'for (i=0;i<nreg;i++) {'
printf,unit,' '
printf,unit,'   % Reinitialize the parameters'
printf,unit,' '
printf,unit,'  set_par ("wabs(1).nH", nHav);'
printf,unit,'  set_par ("mekal(1).kT", kT_guess);'
printf,unit,'  set_par ("mekal(1).Abundanc", ab_guess);'
printf,unit,'  set_par ("mekal(1).Redshift", red_guess);'
printf,unit,' '
printf,unit,'    % Notice the energy range lo-hi keV in each dataset,'
printf,unit,'    % and ignore the rest of the spectrum:'
printf,unit,' '
printf,unit,'  ignore([1:i+1]);'
printf,unit,'  xnotice_en (i+1, lo_energy, hi_energy);'
printf,unit,' '
printf,unit,'    % Thaw abundance and redshift'
printf,unit,' '
printf,unit,'  thaw ("mekal(1).Abundanc","mekal(1).kT","mekal(1).Redshift");'
printf,unit,' '
printf,unit,'    % Freeze nH to the average value'
printf,unit,' '
printf,unit,'  freeze("wabs(1).nH");'
printf,unit,' '
printf,unit,'    % Set the shitch parameter to 1 (interpolate)'
printf,unit,' '
printf,unit,'  set_par ("mekal(1).Switch", 1);'
printf,unit,' '
printf,unit,'    % Do the fit, and'
printf,unit,'    % save the reduced chi-squared value as redchi'
printf,unit,' '
printf,unit,'  () = renorm_counts;'
printf,unit,'  () = fit_counts(&info_struct);'
printf,unit,' '
printf,unit,'  s=info_struct.statistic;'
printf,unit,'  n=info_struct.num_bins;'
printf,unit,'  v=info_struct.num_variable_params;'
printf,unit,'  redchi[i]=s/(n-v);'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for KT'
printf,unit,' '
printf,unit,'  (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,'  while (KTlo[i]==KThi[i]) (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for Ab'
printf,unit,' '
printf,unit,'  (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,'  while (Ablo[i]==Abhi[i]) (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for norm'
printf,unit,' '
printf,unit,'  (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,'  while (normlo[i]==normhi[i]) (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,' '
printf,unit,'    % Save the best value for KT'
printf,unit,' '
printf,unit,'  KT[i]=get_par("mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the best value for Ab'
printf,unit,' '
printf,unit,'  Ab[i]=get_par("mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'    % Save the best value for norm'
printf,unit,' '
printf,unit,'  norm[i]=get_par("mekal(1).norm");'
printf,unit,' '
printf,unit,'  nHavg[i]=nHav;'
printf,unit,' '
printf,unit,'}'
printf,unit,' '
printf,unit,'    % Output values for nH fixed'
printf,unit,' '
printf,unit,'writecol("nHfixed.dat",KT,KTlo,KThi,Ab,Ablo,Abhi,nHavg,norm,normlo,normhi,redchi);'
printf,unit,' '
printf,unit,' '
printf,unit,'    % Fit once more with nH fixed to the galactic value'
printf,unit,' '
printf,unit,'ignore([1:nreg]);'
printf,unit,' '
printf,unit,'for (i=0;i<nreg;i++) {'
printf,unit,' '
printf,unit,'   % Reinitialize the parameters'
printf,unit,' '
printf,unit,'  set_par ("wabs(1).nH", nH_gal);'
printf,unit,'  set_par ("mekal(1).kT", kT_guess);'
printf,unit,'  set_par ("mekal(1).Abundanc", ab_guess);'
printf,unit,'  set_par ("mekal(1).Redshift", red_guess);'
printf,unit,' '
printf,unit,'    % Notice the energy range lo-hi keV in each dataset,'
printf,unit,'    % and ignore the rest of the spectrum:'
printf,unit,' '
printf,unit,'  ignore([1:i+1]);'
printf,unit,'  xnotice_en (i+1, lo_energy, hi_energy);'
printf,unit,' '
printf,unit,'    % Thaw abundance and redshift'
printf,unit,' '
printf,unit,'  thaw ("mekal(1).Abundanc","mekal(1).kT","mekal(1).Redshift");'
printf,unit,' '
printf,unit,'    % Freeze nH to the galactic value'
printf,unit,' '
printf,unit,'  freeze("wabs(1).nH");'
printf,unit,' '
printf,unit,'    % Set the shitch parameter to 1 (interpolate)'
printf,unit,' '
printf,unit,'  set_par ("mekal(1).Switch", 1);'
printf,unit,' '
printf,unit,'    % Do the fit, and'
printf,unit,'    % save the reduced chi-squared value as redchi'
printf,unit,' '
printf,unit,'  () = renorm_counts;'
printf,unit,'  () = fit_counts(&info_struct);'
printf,unit,' '
printf,unit,'  s=info_struct.statistic;'
printf,unit,'  n=info_struct.num_bins;'
printf,unit,'  v=info_struct.num_variable_params;'
printf,unit,'  redchi[i]=s/(n-v);'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for KT'
printf,unit,' '
printf,unit,'  (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,'  while (KTlo[i]==KThi[i]) (KTlo[i],KThi[i])=conf( "mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for Ab'
printf,unit,' '
printf,unit,'  (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,'  while (Ablo[i]==Abhi[i]) (Ablo[i],Abhi[i])=conf( "mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'    % Save the highest and lowest values for norm'
printf,unit,' '
printf,unit,'  (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,'  while (normlo[i]==normhi[i]) (normlo[i],normhi[i])=conf( "mekal(1).norm");'
printf,unit,' '
printf,unit,'    % Save the best value for KT'
printf,unit,' '
printf,unit,'  KT[i]=get_par("mekal(1).kT");'
printf,unit,' '
printf,unit,'    % Save the best value for Ab'
printf,unit,' '
printf,unit,'  Ab[i]=get_par("mekal(1).Abundanc");'
printf,unit,' '
printf,unit,'    % Save the best value for norm'
printf,unit,' '
printf,unit,'  norm[i]=get_par("mekal(1).norm");'
printf,unit,' '
printf,unit,'  nHg[i]=nH_gal;'
printf,unit,' '
printf,unit,'}'
printf,unit,' '
printf,unit,'   % Output values for nH fixed to the galactic value'
printf,unit,' '
printf,unit,'writecol("nHgal.dat",KT,KTlo,KThi,Ab,Ablo,Abhi,nHg,norm,normlo,normhi,redchi);'
printf,unit,' '


;
; Close script file
;
close,unit
free_lun,unit


;
; Make the script file executable
;
cmdstring='chmod u+x wabs_mekal.sl'
spawn,cmdstring


;
; Return
;
return
end