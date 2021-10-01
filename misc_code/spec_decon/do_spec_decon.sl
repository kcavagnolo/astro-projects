#!/usr/bin/env slsh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% do_spec_decon.sl
%
% Description:
%   Driver script for spectral deprojection using onion skin
%   method (spec_decon.sl)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make sure spec_decon.sl is in your slang_load_path
require ("spec_decon");

  %% Parameters required for the script:

variable init = init_deproject_init_type();

  %% List of spectra to be deprojected. These MUST be listed from
  %% the innermost annulus to the outermost, in order. If you are
  %% using data from multiple detectors, e.g. XMM (MOS1, MOS2, PN),
  %% use a different key for each detector. Each detector must have
  %% the same number of files, and those files must correspond to
  %% the same regions. For example,
  %% init.files["MOS1"] = [ "m1_01.fits", "m1_02.fits", "m1_03.fits" ];
  %% init.files["MOS2"] = [ "m2_01.fits", "m2_02.fits", "m2_03.fits" ];
  %% init.files["PN"] = [ "pn_01.fits", "pn_02.fits", "pn_03.fits" ];
  %%
  %% Background spectra and response files should be specified using
  %% the standard header keywords in the source spectra.

init.files["ACIS"] =
  [
    "01.pi",
    "02.pi",
    "03.pi",
    "04.pi",
    "05.pi",
    "06.pi"
  ];

  %% Spectral model parameters.  Individual parameters may be frozen by
  %% setting 'init.param.freeze = 1' or thawed by setting
  %% 'init.param.freeze = 0'.  The structure names are the same as the
  %% names of the Sherpa model parameters with a few exceptions:
  %%
  %% Model   Sherpa name   structure name
  %% ------  -----------   --------------
  %% mekal   Abund         abund
  %% apec    Abundanc      abund
  %% mekal   Redshift      redshift
  %% apec    Redshift      redshift
  %%
  %% In addition, one may set the initial value for all elements
  %% in a vmekal or vapec model using init.model.abund.  One may
  %% also thaw/freeze all elements using init.model.abund.freeze,
  %% although the usefulness of freezing all elements in a vmekal
  %% or vapec model is not obvious.  Any settings of inidividual
  %% elements' values or thaw/freeze status will override
  %% init.model.abund.
  %%
  %% Different parameters have different default freeze/thaw status:
  %%
  %% parameter  frozen?
  %% ---------  -------
  %% wabs.nH	yes
  %% kT		no
  %% abund*	yes
  %% redshift   yes
  %%
  %% * Including individual abundances in vmekal and vapec.
  %%
  %% Parameters may be linked by setting init.model.param.link to the
  %% name of the parameter to which it is to be linked, e.g.
  %% 'init.model.N.link = "C"' to link nitrogen to carbon in a vmekal
  %% or vapec model.  Any other valid sherpa linking expression, e.g.
  %% 'init.Si.link = "1.2*C"' may also be used.
  %%
  %% Allowed models for use in init_model() are mekal, apec, vmekal,
  %% and vapec.

init.wabs.nH.value = 3.26e-2;
init.model = init_model ("mekal");
init.model.kT.value = 7;
init.model.abund.value = 0.3;
init.model.abund.freeze = 0;
init.model.redshift.value = 0.05506;

  %% Optional foreground model, with or without absorption.  For
  %% each annulus, the normalization will be multplied by the sum
  %% of the BACKSCALEs of the annuli contributing to the spectrum.

init.extra_wabs = NULL;

  %% To create an absorbed spectrum with an independent absorption,
  %% uncomment the following and change the values as necessary:

% init.extra_wabs = init_wabs_model ();
% init.extra_wabs.nH.value = 1e-2;
% init.extra_wabs.nH.freeze = 1;

  %% To create an absorbed spectrum using the global asorption,
  %% uncomment the following:

% init.extra_wabs = "global";

  %% Comment out the following lines or set init.extra_model = NULL
  %% if no foreground model is needed.

init.extra_model = init_model ("mekal");
init.extra_model.kT.value = 0.4;
init.extra_model.kT.freeze = 1;
init.extra_model.abund.value = 0.05;
init.extra_model.abund.freeze = 1;
init.extra_model.redshift.value = 0.0;
init.extra_model.redshift.freeze = 1;
init.extra_model.norm.value = 2.31e-11;
init.extra_model.norm.freeze = 1;

  %% The next three parameters are a physical description of the
  %% ellipsoid being deprojected.

init.ellipse_type = "oblate"; % How the z-axis of the cluster behaves.
			      % May also be "prolate"
init.bmaj_min = 0.0;          % Semi-major axis of inside of inner-most ellipse
init.scale = 1.05623;         % kpc per pixel, where the pixels are in the units
			      % used in the XFLT* header keywords.

  %% Abundances to be used by the spectral models.  May be "an,"
  %% "feld," "aneb," "grsa," or "wilm."  See xspec documentation for
  %% more information.  May also be NULL to use the Sherpa default.

init.abundType = "grsa";

  %% Energy range to use for fitting. Default range is 0.5-8.0.
  %% Setting either parameter to NULL will cause the default to be used
  %% for that end of the energy range.

init.emin = 0.5;
init.emax = 8.0;

  %% Set confidence interval in number of sigma or percent confidence.
  %% Set one or the other, not both. Set both to NULL or comment them out
  %% to use 90% confidence interval.

init.sigma = NULL;
init.conf_interval = NULL;

  %% Save information about each fit. 1 == TRUE, 0 == FALSE.

init.save_plots = 1;	  % save a plot of each spectral fit
init.save_sessions = 1;   % save a Sherpa session for each annulus

  %% Initialize the script -- MUST be called

define_deproj (init);

  %% This function does the real work -- MUST be called

deproject_spectra ();

  %% Write out the data to a text file and plot it. The plot_fit_params
  %% function should be called as
  %% 	'plot_fit_params( par1, ..., parN, [stamp,] outputfile )',
  %% where par1...parN are the Sherpa names of the model parameters
  %% that you wish to plot, and stamp tells the program whether to print
  %% a timestamp on the plot.  The stamp parameter is optional.  Only
  %% parameters that were thawed may be plotted.  Allowed values vary
  %% depending on the spectral model being used:
  %%
  %% Model Name     Allowed values
  %% -------------  ------------------------------------------------------
  %% all models     "nH", "[det].norm"
  %% mekal          "kT", "Abund", "Redshift"
  %% apec           "kT", "Abundanc", "Redshift"
  %% vmekal, vapec  "kT", "He", "C", "O", etc., "redshift"
  %%
  %% Note that the parameters are case-insensitive.  "nH" refers to the
  %% value of nH from the wabs model. "[det].norm" is the normalization
  %% of the model for detector "det".  For example, if the files are
  %% specified above as 'init.files["MOS1"] = [ ... ];' then the
  %% normalization would be given as 'MOS1.norm'.
  %%
  %% The x-axis of the plot will be given along the semi-major axis of
  %% the ellipses.

write_fit_params ("sherpa_deproj.dat");
plot_fit_params ("kT", "Abund", 1, "sherpa_deproj.ps");

seval ("exit");
