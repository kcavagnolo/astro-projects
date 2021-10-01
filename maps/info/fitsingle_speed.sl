#!/usr/bin/env /ASTRO/CIAO_3.1/bin/slsh
%()=evalfile("test_fit.sl"); 

()=evalfile("is_file.sl");
import("sherpa");
import("group");
import("chips");

variable pha,d,derr,dtab,dfilter,dvar;
variable newgrp,newqu,newindex,newerr,newsig,newvar;
variable p,perr,ptab,pfilter,pvar;
variable b,berr,dsub,derrsub;
variable pnewgrp,pnewqu,pnewindex,pnewerr,pnewsig,pnewvar;
variable nr,t,t2,fp,buf,sd_temp,sd_nh,sd_z, sd_abs, fitting;
variable minsnr = 5.;
variable min_ndata = 60; %minimum for having abundance as a free parameter 
variable min_nsrc = 10;
variable sd_dbackscal, sd_bgbackscal, sd_dexptime, sd_bgexptime, mu, nsrc;
variable ndata;
sherpa.clobber=1;

% Read in the data file for the source region
()=sherpa_eval("DATA 1 test_sou.pi");
%()=sherpa_eval("DATA 2 test_psrc.pi");
%()=sherpa_eval("READ errors 2 \"test_psrc.pi[cols CHANNEL, STAT_ERR]\" fitsbin");

% Read in the background from the outer parts of the chip
sherpa.multiback=0;
%()=sherpa_eval("BACK 1 test_bgd.pi");
%()=sherpa_eval("READ BERRORS \"test_bgd.pi[cols CHANNEL,STAT_ERR]\" fitsbin");

% Check how the size of the bg region compares to the source region
%variable sd_dbackscal=get_backscale(1);
%variable sd_bgbackscal=get_bbackscale(1);
% If the bg region is too small, use the Markevitch bg
%if (sd_dbackscal/sd_bgbackscal > 3.) 
%  {
if (is_file("flare") == 1) 
  {
  print("Too many flares in the lightcurve to remove, using local background");
  ()=sherpa_eval("BACK 1 test_bgd.pi");
  ()=sherpa_eval("READ BERRORS \"test_bgd.pi[cols CHANNEL,STAT_ERR]\" fitsbin");
  }
else
  {
  print("Clean lightcurve, using Markevitch background");
  sherpa_eval("BACK 1 mark_bgd.pi"); 
  sherpa_eval("READ BERRORS \"mark_bgd.pi[cols CHANNEL,STAT_ERR]\" fitsbin");
  }

variable sd_dbackscal=get_backscale(1); 
variable sd_bgbackscal=get_bbackscale(1);

% Ungroup both sets
variable old_grp=get_groups(1);
()=sherpa_eval("ungroup 1");
%()=sherpa_eval("ungroup 2");
()=sherpa_eval("bungroup 1");

  ()=set_unsubtract(1);  
  ()=set_notice_all(1);
  d=get_data(1);
  derr=get_errors(1);
  b=get_back(1);
  berr=get_berrors(1);
  sd_dbackscal=get_backscale(1);
  sd_bgbackscal=get_bbackscale(1);
  sd_dexptime=get_exptime(1);
  sd_bgexptime=get_bexptime(1);
  mu=sd_dexptime/sd_bgexptime*sd_dbackscal/sd_bgbackscal;
  dsub=d-mu*b;
  derrsub=sqrt(d+mu^2*b) ; 

  dtab=typecast(d,Integer_Type);
  dtab=dtab-dtab+1;
  dfilter=get_filter(1);
  dtab=1-dfilter;
  derr[where (derr == 0.0)] = 0.000001;
  derrsub[where (derrsub == 0.0)] = 0.000001;
% Group to minimum S/N 
%  (newgrp,newqu) = grpSnr(d, minsnr, 100, dtab, sqrt(derr));
  forever
    {
    %  (newgrp,newqu) = grpSnr(d, minsnr, 100, dtab, sqrt(derr));
    (newgrp,newqu) = grpSnr(dsub, minsnr, 100, dtab, derrsub);
    %()=groupByCounts( 1, 20 );
    (nr,t,t2)=array_info(newgrp);
    print(nr);
    newgrp[35]=1; 
    newindex = where( newgrp[[22:343]] == 1 );
    print("minsnr: "+string(minsnr)+"  data points: "+string(length(newindex)));
    if (length(newindex) > 7 or minsnr == 0) break;
    else minsnr=minsnr-0.5;
    }
  ndata=length(newindex);
  print("number of data points: "+string(ndata));

% Apply the new grouping to the data
  ()=set_unsubtract(1);  
  ()=sherpa_eval("ungroup 1");
  ()=set_groups(1, newgrp);
  ()=sherpa_eval("group 1");
% Apply the same grouping to the background
  ()=sherpa_eval("bungroup 1");
  ()=set_bgroups(1, newgrp);
  ()=sherpa_eval("bgroup 1");
  ()=set_subtract(1);  

% Regroup the psrc spectrum in the same fashion
%  ()=sherpa_eval("ungroup 2");
%  ()=set_notice_all(2);
%  p=get_data(2);
%  perr=get_errors(2);
%  ptab=typecast(d,Integer_Type);
%  ptab=dtab-dtab+1;
%  pfilter=get_filter(1);
%  ptab=1-dfilter;
%  perr[where (perr == 0.0)] = 1.;
%  (pnewgrp,pnewqu) = grpSnr(p*(p>0), minsnr, 100, ptab, perr);
%  (nr,t,t2)=array_info(pnewgrp);
%  print(nr);
%Apply the new binning for the point sources
%  pvar=perr^2;
%  pnewvar=grpGetGroupSum(pvar, pnewgrp );
%  pnewindex = where( pnewgrp == 1 );
%  pnewerr=sqrt(pnewvar[pnewindex]);
%  pnewsig=grpGetGroupSum(p, pnewgrp );
%  ()=set_groups(2, pnewgrp);
%  ()=set_data(2, pnewsig[pnewindex]);
%  ()=set_errors(2, pnewerr);

% Define source models
  ()=set_source_expr(1,"xsphabs[abs]*(xsapec[mek]+xspowerlaw[pwl])");
%  ()=set_source_expr(2,"abs*xspowerlaw[pwl2]");

% Read in the galactic absorption value
  fp = fopen("../n_h.dat", "r");
  ()=fgets (&buf, fp);
  () = fclose (fp);
  sd_temp=atof(buf);
  sd_nh=typecast(sd_temp,Float_Type);
  print(sd_nh);


% Use the fixed galactic nh value (units: sd_nh in 10^20cm^-2, 
% abs needs 10^22 cm^-2) 
  sd_abs=get_par("abs.nh");
  sd_abs.value=sd_nh/100. ;
  ()=set_par(sd_abs);  

% Read in the redshift value
  fp = fopen("../redshift.dat", "r");
  ()=fgets (&buf, fp);
  () = fclose (fp);
  sd_z=atof(buf);
  print(sd_z);

% Use the fixed distance value for the mekal model
()=set_par("mek.redshift", "value", sd_z);


% Restrict energy range to 0.5-5 keV
  ()=set_ignore(1,,0.5);
  ()=set_ignore(1,5.,);
%  ()=set_ignore(2,,0.5);
%  ()=set_ignore(2,5.,);

% Set Powerlaw parameters (unresolved)
()=set_par("pwl.PhoIndx", "min", 0.);
()=set_par("pwl.PhoIndx", "max", 10.);
()=set_par("pwl.PhoIndx", "value", 1.6);
()=set_par("pwl.norm", "min", 0e0);
()=set_par("pwl.norm", "max", 1e0);
()=set_par("pwl.norm", "value", 1e-4);

% Set Powerlaw parameters (resolved)
%()=set_par("pwl2.PhoIndx", "value", 1.6);
%()=set_par("pwl2.PhoIndx", "min", 0.);
%()=set_par("pwl2.PhoIndx", "max", 10.);
%()=set_par("pwl2.norm", "min", 0e0);
%()=set_par("pwl2.norm", "max", 1e2);
%()=set_par("pwl2.norm", "value", 5e-4);

% Set Apec parameters 
()=set_par("mek.kt", "value", 0.5);
()=set_par("mek.kt", "min", 1e-3);
()=set_par("mek.kt", "max", 100);
()=set_par("mek.norm", "min", 0e0);
()=set_par("mek.norm", "max", 1e2);
()=set_par("mek.norm", "value", 1e-3);
()=set_par("mek.abundanc", "min", 0);
()=set_par("mek.abundanc", "max", 10.);
()=set_par("mek.abundanc", "value", 1.0);

% Simultaneously fit the second data set with an absorbed power law and the
% first one with a mekal model + power law, while keeping the power
% law shape fixed. Do this only if you have enough signal in the psrc spectrum
%nsrc=0;
%fp = fopen("nsrc.dat", "r");
%()=fgets (&buf, fp);
%() = fclose (fp);
%sd_temp=atof(buf);
%nsrc=typecast(sd_temp,Float_Type);
nsrc=0;
print(nsrc);
%if (nsrc > min_nsrc ) ()=sherpa_eval("pwl.PhoIndx=>pwl2.PhoIndx");
()=set_frozen("pwl.PhoIndx");

% Freeze/Thaw parameters before fitting
  ()=set_frozen("abs.nh");
  ()=set_frozen("mek.abundanc");
%if (nsrc < min_nsrc ) 
()=set_frozen("pwl.PhoIndx");
%if (ndata < min_ndata ) ()=set_frozen("mek.abundanc");

% Switch to chi squared statistics
()=sherpa_eval("statistic chi gehrels");

% Fit the model
print("start fitting");
%if (nsrc > min_nsrc ) fitting=run_fit();
%else 
fitting=run_fit(1);

% Get the statistics and covariance matrix
%if (nsrc > 10 ) ()=sherpa_eval("goodness");
%else 
()=sherpa_eval("goodness 1");

% Save temperature to singletemp.dat
% 1-sigma uncertainties: lower and upper bounds
variable sd_mektemp=get_par("mek.kt"); 
if (sd_mektemp.value > 0.1 and sd_mektemp.value < 9.) {
  ()=sherpa_eval("covariance 1 mek.kt");
  sd_mektemp=get_par("mek.kt"); 
  variable sd_mektempcov=get_cov();        
  fp = fopen("singletempcov.dat", "w");
  ()=fprintf(fp,"%f\n",sd_mektempcov[0].vlo);
  ()=fprintf(fp,"%f",sd_mektempcov[0].vhi);
  ()=fclose(fp);
  fp = fopen("singletemp.dat", "w");
  ()=fprintf(fp,"%f",sd_mektemp.value);
  ()=fclose(fp);
  }
else {
  fp = fopen("singletempcov.dat", "w");
  ()=fprintf(fp,"%f\n",-1);
  ()=fprintf(fp,"%f\n",-1);
  ()=fclose(fp);
  fp = fopen("singletemp.dat", "w");
  ()=fprintf(fp,"%f",-1);
  ()=fclose(fp);
  }

%variable sd_mekabund=get_par("mek.abundanc"); 
variable sd_mekabund=-1;
if (sd_mekabund.value > 0.01 and sd_mekabund.value < 100.) {
  ()=sherpa_eval("covariance 1 mek.abundanc");
  sd_mekabund=get_par("mek.abundanc"); 
  variable sd_mekabundcov=get_cov();        
  fp = fopen("abund_cov.dat", "w");
  ()=fprintf(fp,"%f\n",sd_mekabundcov[0].vlo);
  ()=fprintf(fp,"%f",sd_mekabundcov[0].vhi);
  ()=fclose(fp);
  fp = fopen("abund.dat", "w");
  ()=fprintf(fp,"%f",sd_mekabund.value);
  ()=fclose(fp);
  }
else {
  fp = fopen("abund.dat", "w");
  ()=fprintf(fp,"%f\n",-1);
  ()=fprintf(fp,"%f\n",-1);
  ()=fclose(fp);
  fp = fopen("abund_cov.dat", "w");
  ()=fprintf(fp,"%f",-1);
  ()=fclose(fp);
  }


% Save all model parameters separately
()=sherpa_eval("SAVE mek single_mek.shp");
()=sherpa_eval("SAVE pwl single_pwl.shp");
%()=sherpa_eval("SAVE pwl2 single_pwl2.shp");

% Save everything to a file
()=sherpa_eval("SAVE ALL singletemp.shp");

% Save the fits in a ps file
%set_log;
%()=sherpa_eval("lplot 2 fit 1 fit 2");
%()=chips_eval("PRINT POSTFILE singletemp.ps");
%set_lin;
%()=sherpa_eval("lplot 4 fit 1 resid 1 fit 2 resid 2");
%()=chips_eval("PRINT POSTFILE singletemp_resid.ps");


% Save the reduced Chi^2 values (total, data 1, data 2)
fp = fopen("singletempchi.dat", "w");
%if (nsrc > min_nsrc ) 
%  {
  variable sd_good=get_goodness(1);
  ()=fprintf(fp,"%f\n",sd_good[0].rstat);
%  ()=fprintf(fp,"%f\n",sd_good[1].rstat);
%  ()=fprintf(fp,"%f\n",sd_good[2].rstat);
%  }
%else
%  {
%  variable sd_good=get_goodness(1);
%  ()=fprintf(fp,"%f\n",sd_good[0].rstat);
%  ()=fprintf(fp,"%f\n",sd_good[0].rstat);
%  ()=fprintf(fp,"%f\n",-1);
%  }
()=fclose(fp);

% Get the correction factor epsilon: non-absorbed / absorbed
()=set_source_expr(1,"mek");
variable sd_mekfluxnoabs=get_pflux(1,[0.3,5.]);
print(sd_mekfluxnoabs);
()=set_source_expr(1,"abs*mek");
variable sd_mekfluxabs=get_pflux(1,[0.3,5.]);
print(sd_mekfluxabs);
if (sd_mekfluxabs.value > 0.) variable sd_epsilon=sd_mekfluxnoabs.value/sd_mekfluxabs.value;
else variable sd_epsilon=-1;
print(sd_epsilon);
fp = fopen("singletempepsilon.dat", "w");
()=fprintf(fp,"%f\n",sd_epsilon);
()=fclose(fp);

% Get the correction factor delta: soft / total
()=set_source_expr(1,"abs*mek");
variable sd_meksoftabs=get_pflux(1,[0.3,1.2]);
print(sd_meksoftabs);
if (sd_mekfluxabs.value > 0.) variable sd_delta=sd_meksoftabs.value/sd_mekfluxabs.value;
else variable sd_delta=-1;
print(sd_delta);
fp = fopen("singletempdelta.dat", "w");
()=fprintf(fp,"%f\n",sd_delta);
()=fclose(fp);

% Get the correction factor alpha: soft / total
()=set_source_expr(1,"abs*pwl");
variable sd_pwlsoftabs=get_pflux(1,[0.3,1.2]);
print(sd_pwlsoftabs);
variable sd_pwlfluxabs=get_pflux(1,[0.3,5.]);
print(sd_pwlfluxabs);
if (sd_pwlfluxabs.value > 0.) variable sd_alpha=sd_pwlsoftabs.value/sd_pwlfluxabs.value;
else variable sd_alpha=-1;
print(sd_alpha);
fp = fopen("singletempalpha.dat", "w");
()=fprintf(fp,"%f\n",sd_alpha);
()=fclose(fp);
fp = fopen("src_hardness.dat", "w");
()=fprintf(fp,"%f\n",sd_alpha/(1.-sd_alpha));
()=fclose(fp);
% Note (05/10/05) Even though this is not consistent with alpha defined in 
% the original notes as being H/(S+H), but rather S/(S+H), it is 
% self-consistent throughout the scripts. SO DON'T CHANGE THIS IF YOU DON'T
% CHANGE ALL SUBSEQUENT SCRIPTS!!!! BUT KEEP IT IN MIND WHEN YOU USE ALPHA!


% Get the correction factor eta:  photon flux / energy flux
()=set_source_expr(1, "mek");
variable sd_mekenergy=get_eflux(1,[0.3,5.]);
variable sd_mekphot=get_pflux(1,[0.3,5.]);
if (sd_mekenergy.value > 0.) variable sd_eta=sd_mekphot.value/sd_mekenergy.value;
else sd_eta=-1;
print(sd_eta);
fp = fopen("singletempeta.dat", "w");
()=fprintf(fp,"%f\n",sd_eta);
()=fclose(fp);


% Save the model values in ASCII format
()=sherpa_eval("WRITE mek single_mek.dat ASCII");
()=sherpa_eval("WRITE pwl single_pwl.dat ASCII");
%()=sherpa_eval("WRITE pwl2 single_pwl2.dat ASCII");
()=set_source_expr(1, "abs*(mek+pwl)");
()=sherpa_eval("WRITE source single_fit.dat ASCII");
()=sherpa_eval("WRITE MODEL single_fitconv.dat ASCII");

% Get the correction factor alpha: soft / total

()=set_source_expr(1,"abs*pwl");
()=set_par("pwl.PhoIndx", "value", 1.6);
()=set_par("pwl.norm", "value", 1e-5);
sd_pwlsoftabs=get_pflux(1,[0.3,1.2]);
print(sd_pwlsoftabs);
sd_pwlfluxabs=get_pflux(1,[0.3,5.]);
print(sd_pwlfluxabs);
sd_alpha=sd_pwlsoftabs.value/sd_pwlfluxabs.value;
print(sd_alpha);
fp = fopen("singletempalpha1.6.dat", "w");
()=fprintf(fp,"%f\n",sd_alpha);
()=fclose(fp);
fp = fopen("src_hardness1.6.dat", "w");
()=fprintf(fp,"%f\n",sd_alpha/(1.-sd_alpha));
()=fclose(fp);

sherpa_eval("exit");





