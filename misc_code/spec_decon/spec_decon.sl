%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright (C) 2003-2007  Joshua Kempner
%
% spec_decon.sl
%
% Description:
%   Fits concentric annular spectra in Sherpa. The algorithm for
%   determining the contributions of the outer annuli to the spectra of
%   the inner ones was adapted from the program norm_ang by Liz Blanton,
%   with modifications for the case of elliptical (instead of circular)
%   annuli. Uses the same FITS header keywords used by the projct model
%   in XSPEC, so the same files can be used for both purposes without
%   the need for additional files to describe the onion skin geometry.
%   Output is a text file containing the results of the fits and a
%   postscript file with a plot of those results.
%
% Author:
%   Joshua Kempner <josh@kempner.net>
%
% Requirements:
%   CIAO 3.0 or later
%
% History:
%   See changelog
%
% License:
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 2 of the License, or
%   (at your option) any later version.
%
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%
%   You should have received a copy of the GNU General Public License
%   along with this program; if not, write to the Free Software
%   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
% Contact Information:
%   Please send any bug fixes, enhancements, or useful comments by email
%   to josh@kempner.net.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% change to "iftrue" for debugging
#iftrue
_debug_info=1;
_traceback=1;
#endif

import ("sherpa");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% useful data types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typedef struct
{
  value,
  vmin,  % currently not implemented
  vmax,  % currently not implemented
  freeze,
  link
}
Model_Parameter_Type;

typedef struct
{
  nH
}
Wabs_Model_Type;

typedef struct
{
  name,
  kT,
  abund,
  redshift,
  norm
}
Plasma_Model_Type;

typedef struct
{
  name,
  kT,
  abund,
  He, C, N, O, Ne, Na, Mg, Al, Si, S, Ar, Ca, Fe, Ni,
  redshift,
  norm
}
V_Plasma_Model_Type;

typedef struct
{
  ellipse_type,
  bmaj_min,
  files,
  nFiles, nDet,
  emin, emax,
  wabs,
  model,
  abundType,
  extra_wabs,
  extra_model,
  scale,
  save_plots,
  save_sessions,
  sigma,
  conf_interval
}
Deproject_Init_Type;

typedef struct
{
  name,
  val,
  vlo,
  vhi
}
Saved_Param_Type;

public define init_wabs_model () %{{{
{
  variable wabs = @Wabs_Model_Type;
  wabs.nH = @Model_Parameter_Type;
  wabs.nH.freeze = 1;
  return wabs;
}

%}}}

private define init_plasma_model (name) %{{{
{
  variable model = @Plasma_Model_Type;
  model.name = name;
  model.kT = @Model_Parameter_Type;
  model.abund = @Model_Parameter_Type;
  model.redshift = @Model_Parameter_Type;
  model.norm = @Model_Parameter_Type;

  model.kT.freeze = 0;
  model.abund.freeze = 1;
  model.redshift.freeze = 1;
  model.norm.freeze = 0;
  return model;
}

%}}}

private define init_v_plasma_model (name) %{{{
{
  variable model = @V_Plasma_Model_Type;
  model.name = name;
  model.abund = @Model_Parameter_Type;
  model.kT = @Model_Parameter_Type;
  model.He = @Model_Parameter_Type;
  model.C = @Model_Parameter_Type;
  model.N = @Model_Parameter_Type;
  model.O = @Model_Parameter_Type;
  model.Ne = @Model_Parameter_Type;
  model.Na = @Model_Parameter_Type;
  model.Mg = @Model_Parameter_Type;
  model.Al = @Model_Parameter_Type;
  model.Si = @Model_Parameter_Type;
  model.S = @Model_Parameter_Type;
  model.Ar = @Model_Parameter_Type;
  model.Ca = @Model_Parameter_Type;
  model.Fe = @Model_Parameter_Type;
  model.Ni = @Model_Parameter_Type;
  model.redshift = @Model_Parameter_Type;
  model.norm = @Model_Parameter_Type;

  model.kT.freeze = 0;
  % Don't set abundance(s) freeze so they can be set later
  model.redshift.freeze = 1;
  model.norm.freeze = 0;

  return model;
}

%}}}

public define init_model (name) %{{{
{
  if ("mekal" == name or "apec" == name)
    return init_plasma_model (name);
  else if ("vmekal" == name or "vapec" == name)
    return init_v_plasma_model (name);
  else
  {
    message ("*** Model type " + name + " is not supported.");
    __exit (0);
  }
}

%}}}

% initialize a Deproject_Init_Type
public define init_deproject_init_type () %{{{
{
  variable my_init = @Deproject_Init_Type;

  my_init.files = Assoc_Type[Array_Type];
  my_init.wabs = init_wabs_model();

  % extra_model NULL by default
  my_init.extra_wabs = NULL;
  my_init.extra_model = NULL;

  return my_init;
} % init_deproject_init_type()

%}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% global variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

private variable Deproj_Init = @Deproject_Init_Type;
private variable Model_Norms;
private variable Saved_Fits = @Saved_Param_Type;
private variable Fluxes;
private variable Free_Params;
private variable Max_Pairs = 4;
private variable Emin = 0.5;
private variable Emax = 8.0;
private variable _spec_decon_version = 10411;
private variable _spec_decon_version_string = "1.4.11";
private variable _sherpa_version;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% approximation of inverse error function from Sergei Winitzki of
% Ludwig-Maximilians University. The approximation is good to better
% than 4e-4 in relative precision. See
% http://www.theorie.physik.uni-muenchen.de/~serge/erf-approx.pdf for
% details.
private define ierf (x) %{{{
{
  variable a, two_over_pia, ln_one_minus_x_sq, val;
  a = 8 * (PI-3) / (3*PI * (4 - PI));
  two_over_pia = 2 / PI / a;
  ln_one_minus_x_sq = log (1 - x^2);
  val = sqrt (-1 * (two_over_pia + ln_one_minus_x_sq / 2) +
               sqrt ((two_over_pia + ln_one_minus_x_sq / 2)^2 -
                     ln_one_minus_x_sq/a));
  return val;
}

%}}}

% private function to check a struct for a given field name
private define struct_has_field (s, name) %{{{
{
  variable thisName;

  foreach (get_struct_field_names (s))
  {
    thisName = ();
    !if (strcmp (thisName, name))
      return 1;
  }
  return 0;
} % struct_has_field()

%}}}

% private function to replace sherpa_eval so that it can take sprintf-like
% argument lists
private define seval() %{{{
{
  _stk_reverse(_NARGS);
  variable command, format_string = ();
  variable args, retval;
  if (_NARGS > 1)
  {
    _stk_reverse(_NARGS-1);
    args = __pop_args (_NARGS-1);
    command = sprintf (format_string, __push_args (args) );
  }
  else
    command = format_string;

% change iftrue to iffalse for debugging
#iftrue
  retval = sherpa_eval (command);
  if (0 != retval)
    error ("*** Error: unable to execute sherpa command:\n" + command + "\n");
#else
  message (command);
#endif
} % seval()

%}}}

% private function to find current version of sherpa
private define get_sherpa_version () %{{{
{
  if (__is_initialized (&_sherpa_version))
    return;
  _sherpa_version = struct {major, minor, patch};

  % The following would work under all circumstances if sherpa_version
  % had been updated properly in CIAO 3.3. It does work, however, in
  % CIAO 3.2 and 3.1, and possibly earlier versions as well (untested).
  % However, 3.2 and 3.3 will look the same, so only use the results if
  % it returns version 3.1.
  if (__is_initialized(&sherpa_version))
  {
    _sherpa_version.major = sherpa_version / 10000;
    _sherpa_version.minor = (sherpa_version mod 10000) / 100;
    _sherpa_version.patch = sherpa_version mod 100;
    if (andelse{_sherpa_version.major == 3}{_sherpa_version.minor < 2 or _sherpa_version.minor > 3})
      return;
  }

  variable install_dir, version_file, fp, line, version, patch;
  install_dir = getenv ("ASCDS_INSTALL");
  version_file = path_concat (install_dir, "VERSION");
  fp = fopen (version_file, "r");
  if (NULL == fp)
    verror ("Could not open CIAO version file %s", version_file);
  () = fgets (&line, fp);
  () = fclose (fp);

  version = extract_element (line, 1, ' ');
  _sherpa_version.major = integer (extract_element (version, 0, '.'));
  _sherpa_version.minor = integer (extract_element (version, 1, '.'));
  if (NULL != extract_element (version, 2, '.'))
    _sherpa_version.patch = integer (extract_element (version, 2, '.'));
  else
    _sherpa_version.patch = 0;
}

%}}}

% private function to read FITS header key of any type and convert to double
private define _read_key (file, key) %{{{
{
  !if (fits_key_exists (file, key))
  {
    error ("Unable to find key %s in file %s", key, file);
  }

  variable value = fits_read_key (file, key);

  if (String_Type == typeof (value))
    return atof (value);
  else if (Integer_Type == typeof (value))
    return double (value);
  else
    return value;
} % _read_key()

%}}}

private define _get_col_definitions (file) %{{{
{
  variable h = struct { ttype, tform, tunit };
  variable colnum = 0, i;

  while (fits_key_exists (file, "TTYPE" + string (colnum+1)))
    ++colnum;

  h.ttype = String_Type[colnum];
  h.tform = String_Type[colnum];
  h.tunit = String_Type[colnum];
  _for (1, colnum, 1)
  {
    i = ();
    h.ttype[i-1] = fits_read_key (file, "TTYPE" + string (colnum));
    if (fits_key_exists (file, "TFORM" + string (colnum)))
      h.tform[i-1] = fits_read_key (file, "TFORM" + string (colnum));
    else
      h.tform[i-1] = "";
    if (fits_key_exists (file, "TUNIT" + string (colnum)))
      h.tunit[i-1] = fits_read_key (file, "TUNIT" + string (colnum));
    else
      h.tunit[i-1] = "";
  }
  return h;
} % _get_col_definitions()

%}}}

% load the XFLT* keywords into a 2-D array
private define load_filters () %{{{
{
  variable i = 0, j;
  variable keys = assoc_get_keys (Deproj_Init.files);
  variable xflt = Double_Type[Deproj_Init.nFiles, 2*Max_Pairs+3];

  % read the keywords from each file
  foreach (Deproj_Init.files[keys[0]])
  {
    variable file = () + "[1]";
    xflt[i,0] = _read_key (file, "XFLT0001"); % outer semi-major axis
    xflt[i,1] = _read_key (file, "XFLT0002"); % outer semi-minor axis
    xflt[i,2] = _read_key (file, "XFLT0003"); % position angle

    % Check that the ellipses are self-similar
    if ( andelse
	 {i}
	 {
	  (abs (xflt[i,0] / xflt[i,1] - xflt[i-1,0] / xflt[i-1,1]) / 
	   (xflt[i,0] / xflt[i,1]) > 0.01)
	  or
	  (xflt[i,2] - xflt[i-1,2] != 0.0)
	 }
       )
    {
      message ("*** Error: deprojection only works with self-similar ellipses.");
      __exit (1);
    }

    % load the pairs of angles
    j = 3;
    loop (Max_Pairs)
    {
      variable this_key = sprintf ("XFLT00%2d", j+1);
      if (fits_key_exists (file, this_key))
      {
	xflt[i,j] = _read_key (file, this_key);
	this_key = sprintf ("XFLT00%2d", j+2);
	xflt[i,j+1] = _read_key (file, this_key);
	j += 2;
      }
      else
	break;
    }
    i++;
  }

  return xflt;
} % load_filters()

%}}}

private define get_backscales () %{{{
{
  variable backscales, keys, thisKey, file, i, j;

  backscales = Double_Type[Deproj_Init.nDet, Deproj_Init.nFiles];
  keys = assoc_get_keys (Deproj_Init.files);
  i = 0;

  foreach (keys)
  {
    thisKey = ();
    j = 0;
    foreach (Deproj_Init.files[thisKey])
    {
      file = ();
      backscales[i,j] = _read_key (file, "BACKSCAL");
      j++;
    }
    i++;
  }

  return backscales;
} % get_backscales()

%}}}

% take the mean value of a 1-D array
private define mean (a) %{{{
{
  return sum(a) / length(a);
} % mean()

%}}}

% get the normalization coefficients for the deprojection
private define get_normalizations () %{{{
{
  variable xflt = load_filters ();
  variable inner, outer, norm;
  outer = xflt[*,0]; % outer boundaries of annuli
  inner = [ Deproj_Init.bmaj_min, outer[[:-1]] ]; % inner boundaries of annuli
  norm = Double_Type[length (outer), length (outer)];
  
  variable epsilon, epsilon_perp;
  epsilon = 1 - mean (xflt[*,0] / xflt[*,1]);
  if ("oblate" == Deproj_Init.ellipse_type)
    epsilon_perp = 0.0;
  else if ("prolate" == Deproj_Init.ellipse_type)
    epsilon_perp = epsilon;

  variable i, j, n = Deproj_Init.nFiles;
  _for (0, n-1, 1)
  {
    i = ();
    _for (i+1, n-1, 1)
    {
      j = ();
      variable fcorr, fj, fij, rd = Double_Type[4];
      fj = sqrt ( inner[j+1]^2 - inner[j]^2 )^3;

      rd[0] = sqrt ( inner[j+1]^2 - inner[i]^2 )^3;
      rd[1] = sqrt ( inner[j+1]^2 - inner[i+1]^2 )^3;
      rd[2] = sqrt ( inner[j]^2 - inner[i]^2 )^3;
      rd[3] = sqrt ( inner[j]^2 - inner[i+1]^2 )^3;

      fij = (rd[0]-rd[1]-rd[2]+rd[3]);

      norm[i,j] = (fij/fj) / (1 - epsilon) / (1 - epsilon_perp);

      variable k = 1;
      fcorr = 1;
      loop (Max_Pairs)
      {
	!if (NULL == xflt[i,3+k])
	  break;
	fcorr = 0;
	variable theta1 = xflt[i,3+k];
	variable theta2 = xflt[i,4+k];
	fcorr += (1 - epsilon) / 2 / PI *
	  abs ( atan ( tan (theta2) / (1 - epsilon) ) -
		atan ( tan (theta1) / (1 - epsilon) ) );
	k += 2;
      }
      norm[i,j] /= fcorr;
    }
  }

  return norm;
} % get_normalizations()

%}}}

% print the normalization coefficients to a file
public define print_normalizations (norm, filename) %{{{
{
  if (NULL != stat_file (filename))
  {
    vmessage ("*** File %s exists", filename);
    __exit (1);
  }
  variable dims;
  (dims, , ) = array_info (norm);
  variable fp = fopen (filename, "w");
  _for (0, dims[0]-1, 1)
  {
    variable i = ();
    _for (i+1, dims[0]-1, 1)
    {
      variable j = ();
      () = fprintf (fp, "%d\t%d\t%f\n", i, j, norm[i,j]);
    }
  }
  () = fclose (fp);
} % print_normalizations()

%}}}

% print the results of a fit, including errors
private define print_fit_results (goodness, fit) %{{{
{
  variable name;

  vmessage ("%-10s\t%-10s\t%-5s", "dataset", "fit stat.", "dof");
  message ("----------\t---------\t-----\t");
  foreach (goodness)
  {
    variable stats = ();
    if (NULL == stats.dataset)
      name = stats.datatype;
    else
      name = stats.datatype + " " + string(stats.dataset);
    vmessage ("%-10s\t%-9.2f\t%-5d", name, stats.stat, stats.dof);
  }

  vmessage ("%-17s\t%-10s\t%-10s\t%-10s", "name", "value", "min", "max");
  message ("-----------------\t----------\t----------\t----------\t");
  foreach (fit)
  {
    variable conf = ();
    variable names = strchop (conf.name, '.', 0);
    if ("c0" == names[1])
    {
      variable i, detectors;
      i = integer (names[0][[1:]]);
      detectors = assoc_get_keys (Deproj_Init.files);
      name = detectors[i-1] + " norm";
    }
    else
      name = names[1];
    vmessage ("%-17s\t%-10g\t%-10g\t%-10g",
	      name, conf.val, conf.vlo, conf.vhi);
  }
} % print_fit_results()

%}}}

private define save_fit_params (start_idx, fit) %{{{
{
  variable conf, index, name;
  foreach (fit)
  {
    conf = ();
    name = extract_element (conf.name, 1, '.');
    if ("c0" == name)
      name = conf.name;
    index = start_idx +
      where(Saved_Fits.name[[start_idx:start_idx+Free_Params-1]] == name);
    Saved_Fits.val[index] = conf.val;
    Saved_Fits.vlo[index] = conf.vlo;
    Saved_Fits.vhi[index] = conf.vhi;
  }
} % save_fit_params()

%}}}

% adapted from _sherpa_resid_prefunc() in sherpa_plotfns.sl by Doug Burke
static define _lpfd_prefunc_hook (data, label, dataset, type) %{{{
{
  % set up the title
  % - we add the reduced chi-square value IF it is defined
  variable fitres = get_goodness (dataset);  % get_fit() does not work correctly in CIAO 3.0
  variable file = path_basename (get_filename (dataset));
  (file, ) = strreplace (file, "_", "\\_", strlen (file));
  variable title;
  if ( NULL != fitres.rstat )
    title = sprintf ("title '\\raise -2500 %s, \\chi^2_r = %f'", file, fitres.rstat);
  else
    title = sprintf ("title '%s'", file);
  () = chips_eval (title);

  % make the second plot smaller (and remove the gap between the plots)
  () = chips_eval ("d 1 location 0.15 0.9 0.4 0.9");
  () = chips_eval ("d 2 location 0.15 0.9 0.1 0.4");
  
  % set the residual plot to use x-error bars
  sherpa.resplot.x_errorbars = 1;

  % remove the x axis values from the first plot
  () = chips_set_pane (1);
  () = chips_eval ("tickvals x off");

  % as we are a pre-func and the previous command has moved us 
  % back to the first pane we need to go back to the second
  % pane (or else the residuals would be drawn in the first pane)
  %
  () = chips_set_pane (2);
}

%}}}

% from sherpa_plotfns.sl by Doug Burke
% ( xlo, xhi, xscale ) = get_xaxis_info(pane);
%
% find the x-axis range for the given pane
% - changes the current pane to #pane
%
% Note:
%  in CIAO 3.0 the x-axis range will be returned as the
%  logarithm of the values if the scale is logarithmic
%  (really should return the linear values since that is how we
%   set the axis), so we need to re-scale them if necessary
%
private define get_xaxis_info(pane) {
  () = chips_set_pane(pane);
  variable xs = chips_get_xscale;
  variable xlo, xhi;
  ( xlo, xhi ) = chips_get_xrange();
  if ( 0 == xs ) {
    xlo = 10^xlo;
    xhi = 10^xhi;
  }
  return ( xlo, xhi, xs );

} % get_xaxis_info()

% adapted from _sherpa_resid_postfunc() in sherpa_plotfns.sl by Doug Burke
static define _lpfd_postfunc_hook () %{{{
{
  % ignore the sherpa.resplot.y_log setting
  () = chips_set_yscale(1);

  % ensure that the x-axis ranges of the first two panes
  % are the same (even if they do not both have the same scale)
  % In CIAO 3.0 chips_get_xscale() does not "ignore" the scale
  % setting for the axis so we have to convert the values for
  % logged axes
  %
  variable xs1, xlo1, xhi1;
  ( xlo1, xhi1, xs1 ) = get_xaxis_info(1);

  variable xs2, xlo2, xhi2;
  ( xlo2, xhi2, xs2 ) = get_xaxis_info(2);

  % could find out if we need to do this but easier just to do it
  % - originally used the largest range but that can result in
  %   plots with a lot of space, so restrict to the smaller range
  %   (which will likely be the residual part)
  %
  variable xlo = max([xlo1,xlo2]);
  variable xhi = min([xhi1,xhi2]);
  () = chips_set_pane(1);
  () = chips_set_xrange(xlo,xhi);
  () = chips_set_pane(2);
  () = chips_set_xrange(xlo,xhi);

  % Ensure the two panes have the same X-axis scale - we take it
  % from the main plot in case there is a difference.
  % It is not a perfect solution - the scaling is not as good as
  % when the x_log fields match, but it just makes sure the plots
  % are usable
  %
  () = chips_set_xscale(xs1);

} % _sherpa_resid_postfunc

%}}}

private define save_spectrum_plot (annulusNum) %{{{
{
  variable i, file;

  % set up Chips state object
  set_state_defaults ("chips");

  % send a plot to the screen, with each parameter in a new panel
  () = chips_auto_redraw (0);
  chips_clear ();
  () = chips_split (1, 2);

  % set up the pre/postfunc hooks
  sherpa.resplot.prefunc  = &_lpfd_prefunc_hook;
  sherpa.resplot.postfunc = &_lpfd_postfunc_hook;

  % set up the fit plot
  set_log();
  sherpa.fitplot.symbolstyle = "none";
  sherpa.fitplot.x_errorbars = 1;

  () = chips_eval ("colorsys cmyk");
  _for (1, Deproj_Init.nDet, 1)
  {
    i = ();
    % make the plot
    seval("lplot 2 fit " + string(i) + " delchi " + string(i));

    % write the plot to a postscript file
    file = sprintf("ann_%02d", annulusNum);
    if (Deproj_Init.nDet > 1)
      file += "_" + string(i);
    file += ".ps";
    () = chips_eval ("print postfile " + file);
  }

  % restore some options that were set
  sherpa.resplot.prefunc  = NULL;
  sherpa.resplot.postfunc = NULL;
  set_lin();
}

%}}}

private define set_abund_param (sherpa_name, model, param_name) %{{{
{
  variable param, dot_name;
  param = get_struct_field (model, param_name);
  dot_name = "." + param_name;

  () = set_par (sherpa_name + dot_name, "min", 0.0);
  if (NULL == param.value)
    () = set_par (sherpa_name + dot_name, "value", model.abund.value);
  else
    () = set_par (sherpa_name + dot_name, "value", param.value);

  if (0 == param.freeze)
    () = set_thawed (sherpa_name + dot_name);
  else
    () = set_frozen (sherpa_name + dot_name);

  if (NULL != param.link)
    seval (sherpa_name + dot_name + " => " + sherpa_name + "." + param.link);
}

%}}}

private define set_abundances (sherpa_name, model) %{{{
{
  switch (model.name)
    { case "mekal":
      () = set_par (sherpa_name + ".Abund", "min", 0.0);
      () = set_par (sherpa_name + ".Abund", "value", model.abund.value);
      !if (model.abund.freeze)
	() = set_thawed (sherpa_name + ".Abund");
      else
	() = set_frozen (sherpa_name + ".Abund");
    }
    { case "apec":
      () = set_par (sherpa_name + ".Abundanc", "min", 0.0);
      () = set_par (sherpa_name + ".Abundanc", "value", model.abund.value);
      !if (model.abund.freeze)
	() = set_thawed (sherpa_name + ".Abundanc");
      else
	() = set_frozen (sherpa_name + ".Abundanc");
    }
    { case "vmekal" or case "vapec":
      set_abund_param (sherpa_name, model, "He");      % Helium
      set_abund_param (sherpa_name, model, "C");       % Carbon
      set_abund_param (sherpa_name, model, "N");       % Nitrogen
      set_abund_param (sherpa_name, model, "O");       % Oxygen
      set_abund_param (sherpa_name, model, "Ne");      % Neon
      if ("vmekal" == model.name)
      	set_abund_param (sherpa_name, model, "Na");    % Sodium
      set_abund_param (sherpa_name, model, "Mg");      % Magnesium
      set_abund_param (sherpa_name, model, "Al");      % Aluminum
      set_abund_param (sherpa_name, model, "Si");      % Silicon
      set_abund_param (sherpa_name, model, "S");       % Sulfur
      set_abund_param (sherpa_name, model, "Ar");      % Argon
      set_abund_param (sherpa_name, model, "Ca");      % Calcium
      set_abund_param (sherpa_name, model, "Fe");      % Iron
      set_abund_param (sherpa_name, model, "Ni");      % Nickel
    }
} % set_abundances()

%}}}

% fit a single region using the normalization coefficients from outer annuli
private define fit_region (files, norm, backscales) %{{{
{
  variable nModels, model_string, expr_string, i;
  if (NULL == norm)
    nModels = 1;
  else
    nModels = length (norm) + 1;

  % load the data files
  _for (1, Deproj_Init.nDet, 1)
  {
    i = ();
    % Due to bugs of unknown origin, CXC advises against using
    % load_dataset() in Sherpa 3.2 and earlier. This bug has reportedly
    % been fixed in Sherpa 3.3.
    % However, sherpa_eval("data ...") causes a segfault under OS X,
    % so there's really no way to reliably read in data under OS X in
    % versions 3.2 or earlier.
    % See http://asc.harvard.edu/sherpa3.2/bugs/sl_load_dataset.html
    % and http://asc.harvard.edu/sherpa3.2/bugs/sherpa_eval.html
    % for more information.
    get_sherpa_version();
    if (_sherpa_version.major >=3 and _sherpa_version.minor >= 3)
      () = load_dataset (i, files[i-1]);
    else
      seval ("data " + sprintf ("%d %s", i, files[i-1]));

    % Use statistical and systematic errors in the source and
    % background files. Read them in using READ command since
    % set_[b/sys]errors() functions cause Sherpa to become flaky.
    % source errors
    variable tmpFile, cmd;
    tmpFile = readbintab (files[i-1]);
    if (struct_has_field (tmpFile, "STAT_ERR"))
    {
      cmd = sprintf ("READ ERRORS %d \"%s[cols CHANNEL,STAT_ERR]\" fitsbin",
		     i, files[i-1]);
      seval (cmd);
    }
    if (struct_has_field (tmpFile, "SYS_ERR"))
    {
      cmd = sprintf ("READ SYSERRORS %d \"%s[cols CHANNEL,SYS_ERR]\" fitsbin",
		     i, files[i-1]);
      seval (cmd);
    }

    % background errors
    tmpFile = readbintab (get_bfilename(i));
    if (struct_has_field (tmpFile, "STAT_ERR"))
    {
      cmd = sprintf ("READ BERRORS %d \"%s[cols CHANNEL,STAT_ERR]\" fitsbin",
		     i, get_bfilename(i));
      seval (cmd);
    }
    if (struct_has_field (tmpFile, "SYS_ERR"))
    {
      cmd = sprintf ("READ BSYSERRORS %d \"%s[cols CHANNEL,SYS_ERR]\" fitsbin",
		     i, get_bfilename(i));
      seval (cmd);
    }

    % ignore the appropriate energy ranges
    seval ("analysis energy");
    () = set_ignore_bad(i);
    () = set_ignore(i, 0.0, Emin);
    () = set_ignore(i, Emax, 1000.0);

    % subtract the background
    () = set_subtract(i);
  }

  % set up the model string containing the sum of the source models
  variable n = [1:nModels];
  model_string = strjoin (array_map (String_Type, &sprintf, "myModel%d", n),
			  " + ");

  % Add the scaling constant for each model
  model_string = "abs*(" + model_string + ")*c";
  expr_string = array_map (String_Type, &sprintf, "%s%d", model_string,
			   [1:Deproj_Init.nDet]);

  % add the foreground model if it exists
  if (NULL != Deproj_Init.extra_model)
  {
    variable this_extra = array_map (String_Type, &sprintf, "extra_model%d",
				     [1:Deproj_Init.nDet]);
    if (Wabs_Model_Type == typeof (Deproj_Init.extra_wabs))
      expr_string = expr_string + " + extra_abs*" + this_extra;
    else if (andelse
	     {String_Type == typeof (Deproj_Init.extra_wabs)}
	     {"global" == Deproj_Init.extra_wabs})
      expr_string = expr_string + " + abs*" + this_extra;
    else
      expr_string = expr_string + " + " + this_extra;

    _for (0, Deproj_Init.nDet - 1, 1)
    {
      i = ();
      % reset the parameters of extra_model
      () = set_frozen (this_extra[i]);

      variable extra_norm = Deproj_Init.extra_model.norm.value *
			    backscales[i-1];
      () = set_par (this_extra[i] + ".norm", "value", extra_norm);
      !if (Deproj_Init.extra_model.norm.freeze)
	() = set_thawed (this_extra[i] + ".norm");

      () = set_par (this_extra[i] + ".kT", "value",
		    Deproj_Init.extra_model.kT.value);
      !if (Deproj_Init.extra_model.kT.freeze)
	() = set_thawed (this_extra[i] + ".kT");

      () = set_par (this_extra[i] + ".Redshift", "value",
		    Deproj_Init.extra_model.redshift.value);
      !if (Deproj_Init.extra_model.redshift.freeze)
	() = set_thawed (this_extra[i] + ".Redshift");

      set_abundances (this_extra[i], Deproj_Init.extra_model);
    }
  }

  % Set up the model expressions in Sherpa
  _for (1, Deproj_Init.nDet, 1)
  {
    i = ();
    () = set_source_expr (i, expr_string[i-1]);
  }

  % set the normalizations and freeze all parameters of the outer models
  variable thisModel;
  _for (1, nModels-1, 1)
  {
    i = ();
    thisModel = "myModel" + string (i);
    () = set_par (thisModel + ".norm", "value", Model_Norms[i-1] * norm[-i]);
    () = set_frozen (thisModel + ".norm");
    () = set_frozen (thisModel + ".kT");
    switch (Deproj_Init.model.name)
      { case "mekal": () = set_frozen (thisModel + ".Abund"); }
      { case "apec": () = set_frozen (thisModel + ".Abundanc"); }
      { case "vmekal" or case "vapec":
	() = set_frozen (thisModel + ".He");
	() = set_frozen (thisModel + ".C");
	() = set_frozen (thisModel + ".N");
	() = set_frozen (thisModel + ".O");
	() = set_frozen (thisModel + ".Ne");
	() = set_frozen (thisModel + ".Mg");
	() = set_frozen (thisModel + ".Al");
	() = set_frozen (thisModel + ".Si");
	() = set_frozen (thisModel + ".S");
	() = set_frozen (thisModel + ".Ar");
	() = set_frozen (thisModel + ".Ca");
	() = set_frozen (thisModel + ".Fe");
	() = set_frozen (thisModel + ".Ni");
      }
      { case "vmekal":
	() = set_frozen (thisModel + ".Na");
      }
    () = set_frozen (thisModel + ".Redshift");
  }

  % set and freeze/thaw model parameters as desired for the current spectrum
  thisModel = "myModel" + string (nModels);
  % temperature:
  () = set_par (thisModel + ".kT", "min", 0.0);
  () = set_par (thisModel + ".kT", "value",
		Deproj_Init.model.kT.value);
  if (Deproj_Init.model.kT.freeze)
    () = set_frozen (thisModel + ".kT");
  else
    () = set_thawed (thisModel + ".kT");

  % abundance
  set_abundances (thisModel, Deproj_Init.model);

  % redshift
  () = set_par (thisModel + ".Redshift", "min", 0.0);
  () = set_par (thisModel + ".Redshift", "value",
		Deproj_Init.model.redshift.value);
  if (Deproj_Init.model.redshift.freeze)
    () = set_frozen (thisModel + ".Redshift");
  else
    () = set_thawed (thisModel + ".Redshift");

  % normalization
  () = set_frozen (thisModel + ".norm");

  % fit the data
#iffalse
  seval ("show models");
  seval ("save all debug.shp");
#endif
  variable goodness = run_fit ();

  % determine the confidence intervals
  variable conf = run_proj ();
  variable noConvergence = get_num_par_thawed() - length (conf);
  conf = get_proj ();

  % get the flux in the noticed band
  Fluxes[Deproj_Init.nFiles-nModels] = get_eflux(,[Emin,Emax]);

  % save the fit parameters
  variable start = Free_Params * (Deproj_Init.nFiles - nModels);
  save_fit_params (start, conf);

  % print informational message
  variable annulus = Deproj_Init.nFiles - nModels + 1;
  vmessage ("\nFIT RESULTS FOR ANNULUS %d:", annulus);
  if (noConvergence)
  {
    message ("WARNING: Projection did not converge. Error intervals may be incorrect.");
    vmessage ("%d converged out of %d", length (conf), get_num_par_thawed());
  }
  print_fit_results (goodness, conf);
  vmessage ("Flux (%.1f-%.1f keV) = %g %s",
	    Emin, Emax, Fluxes[Deproj_Init.nFiles - nModels].value,
	    Fluxes[Deproj_Init.nFiles - nModels].units);

  variable this_norm = get_par (thisModel + ".norm");
  Model_Norms[nModels-1] = this_norm.value;

  % save plots and Sherpa state files as requested
  if (Deproj_Init.save_plots)
    save_spectrum_plot (annulus);
  if (Deproj_Init.save_sessions)
    seval ("save all ann_" + sprintf("%02d", annulus) + ".shp");
} % fit_region()

%}}}

% fit the full set of spectra
public define fit_regions (norm, backscales) %{{{
{
  variable detectors, theseFiles, i, j;
  detectors = assoc_get_keys (Deproj_Init.files);
  theseFiles = String_Type[length (detectors)];

  % outer-most region is a special case
  _for (0, Deproj_Init.nDet - 1, 1)
  {
    i = ();
    theseFiles[i] = Deproj_Init.files[detectors[i]][-1];
  }
  fit_region (theseFiles, NULL, backscales[*,-1]);

  % inner regions
  _for (-2, -Deproj_Init.nFiles, -1)
  {
    i = ();
    _for (0, Deproj_Init.nDet - 1, 1)
    {
      j = ();
      theseFiles[j] = Deproj_Init.files[detectors[j]][i];
    }
    fit_region (theseFiles, norm[i, [i+1:]], backscales[*,i]);
  }
} % fit_regions()

%}}}

% run the fitting
public define deproject_spectra () %{{{
{
  variable norm = get_normalizations ();
  variable backscales = get_backscales ();
  fit_regions (norm, backscales);
} % deproject_spectra()

%}}}

% write the results of all the fits into an ASCII file
public define write_fit_params (file) %{{{
{
  variable fp;
  variable i;
  variable names, detKeys, line;

  % open output file for writing
  if (NULL != stat_file (file))
  {
    vmessage ("WARNING: Error writing fit parameters. file exists: %s", file);
    message  ("         Continuing without writing output file.");
    return;
  }
  fp = fopen (file, "w");
  if (NULL == fp)
  {
    verror ("*** Error opening file %s", file);
    return;
  }

  % print labels at the top of the file
  names = Saved_Fits.name[[0:Free_Params-1]];
  detKeys = assoc_get_keys (Deproj_Init.files);
  names[[Free_Params-Deproj_Init.nDet:Free_Params-1]] = detKeys + ".norm";
  line = array_map (String_Type, &sprintf, "%8s\t%8s\t%8s",
		    names, "min", "max");
  () = fprintf (fp, "region\t%s\tFlux (%.1f:%.1f)\n", strjoin (line, "\t"), Emin, Emax);

  % print the fit parameters
  _for (0, Deproj_Init.nFiles - 1, 1)
  {
    i = ();
    variable start = i * Free_Params;
    variable finish = (i+1) * Free_Params - 1;
    line = array_map (String_Type, &sprintf, "%8g\t%8g\t%8g",
		      Saved_Fits.val[[start:finish]],
		      Saved_Fits.vlo[[start:finish]],
		      Saved_Fits.vhi[[start:finish]]);
    () = fprintf (fp, "%6d\t", i + 1);
    () = fprintf (fp, "%s", strjoin (line, "\t"));
    () = fprintf (fp, "\t%g\n", Fluxes[i].value);  % print the flux
  }
  () = fclose (fp);
} % write_fit_params()

%}}}

% plot the named fit parameters in a chips window and to a postscript file
public define plot_fit_params (file) %{{{
{
  variable infoStamp, params_to_plot;
  if (_NARGS < 2)
  {
    error ("Usage: plot_fit_params (par1, par2, ..., [stamp,] file)");
    return;
  }
  else
    params_to_plot = __pop_args (_NARGS-1);

  % backward compatibility if infostamp is not used
  if (Integer_Type == typeof (params_to_plot[-1].value))
  {
    infoStamp = params_to_plot[-1].value;
    params_to_plot = params_to_plot[[:-2]];
  }
  else
    infoStamp = 0;

  % set up units for axis labels
  variable units = struct { param, name, unit };
  units.param = [ "nH", "kT", "Abund", "Abundanc",
		  "He", "C", "N", "O", "Ne", "Na", "Mg", "Al", "Si", "S", "Ar",
		  "Ca", "Fe", "Ni",
		  "Redshift", "norm" ];
  units.param = array_map (String_Type, &strup, units.param);
  units.name = [ "n_H", "kT", "Abundance", "Abundance",
		 "He", "C", "N", "O", "Ne", "Na", "Mg", "Al", "Si", "S", "Ar",
		 "Ca", "Fe", "Ni",
		 "Redshift", "Norm." ];
  units.unit = [ "10^{22} cm^{-3}", "keV", "Z_{\\odot}", "Z_{\\odot}",
		 "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}",
		 "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}",
		 "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}", "Z_{\\odot}",
		 "Z_{\\odot}", "Z_{\\odot}",
		 "z", "" ];

  % get the x-axis values form the FITS files
  variable xflt = load_filters ();
  variable inner, outer, r;
  outer = xflt[*,0]; % outer boundaries of annuli
  inner = [ Deproj_Init.bmaj_min, outer[[:-2]] ]; % inner boundaries of annuli
  outer *= Deproj_Init.scale;
  inner *= Deproj_Init.scale;
  r = (inner + outer) / 2.0;

  % get the panels' names
  variable idx, i;
  variable saved_names, detKeys, this_param, v, vlo, vhi;
  detKeys = assoc_get_keys (Deproj_Init.files);
  saved_names = Saved_Fits.name;
  _for (0, Deproj_Init.nFiles - 1, 1)
  {
    i = ();
    idx = [Free_Params-Deproj_Init.nDet:Free_Params-1] + i * Free_Params;
    saved_names[idx] = detKeys + ".norm";
  }
  saved_names = array_map (String_Type, &strup, saved_names);

  % check validity of params_to_plot
  variable nparams = 0;
  _for (0, length (params_to_plot) - 1, 1)
  {
    i = ();
    if (length (where (saved_names == strup (params_to_plot[i].value))))
      nparams++;
    else
    {
      vmessage ("Invalid parameter for plot: %s", params_to_plot[i].value);
      params_to_plot[i].value = "";
    }
  }

  % set up Chips state object
  set_state_defaults ("chips");
  chips.symbolstyle = _chips->none;

  % send a plot to the screen, with each parameter in a new panel
  () = chips_auto_redraw (0);
  chips_clear ();
  () = chips_split (1, nparams);
  i = 1;
  foreach (params_to_plot)
  {
    this_param = ();
    !if (strlen (this_param.value))
      continue;

    % get the data
    idx = where (saved_names == strup (this_param.value));
    v = Saved_Fits.val[idx];
    vlo = Saved_Fits.vlo[idx];
    vhi = Saved_Fits.vhi[idx];

    % set up the chips window
    () = chips_set_pane (i);
    () = chips_set_xrange (0, max(outer)*1.05);
    () = chips_set_yrange (0, max(vhi)*1.05);

    % set the axes and labels
    idx = where (units.param == strup (this_param.value) or
		 (units.param == strup (this_param.value[[-4:]])));
    variable ylabel;
    if ("Norm." == units.name[idx][0])
      ylabel = extract_element (this_param.value, 0, '.') + " " + units.name[idx];
    else
      ylabel = units.name[idx] + " (" + units.unit[idx] + ")";
    if (i != nparams)
      () = chips_eval ("tickvals x off");
    () = chips_eval ("ylabel \"" + ylabel[0] + "\"");
    if (i == nparams)
      () = chips_eval ("xlabel \"r (kpc)\"");

    % plot the data
    () = curve (r, v, r-inner, outer-r, v-vlo, vhi-v);
    () = chips_eval ("errs size 1.5");
    chips_redraw ();
    i++;
  }

  % write a timestamp if requested
  if (infoStamp)
  {
    variable stampString;
    stampString = file + " created by " + getenv("USER") + " at " + time();
    stampString = str_quote_string (stampString, "\\^$[]_", '\\');
    () = chips_label (-0.2*max(outer), -0.125*nparams*max(vhi), stampString,
		      _chips->defcolor, 0.5);
  }

  % keep the plot up until the user kills it
  variable buffer;
  message ("Hit return/enter to continue.");
  () = fgets (&buffer, stdin);

  % finally, write the plot to a postscript file
  () = chips_eval ("colorsys cmyk");
  () = chips_eval ("print postfile " + file);
} % plot_fit_params()

%}}}

% register the models with Sherpa and set the absorption model
private define init_models () %{{{
{
  variable modelName = "xs" + Deproj_Init.model.name;

  % initialize the absorption
  () = create_model ("xswabs", "abs");
  () = set_par ("abs.nH", "value", Deproj_Init.wabs.nH.value);
  if (Deproj_Init.wabs.nH.freeze)
    () = set_frozen ("abs");
  else
    () = set_thawed ("abs");

  % set up each of the models without setting their values
  variable i, thisModel;
  _for (1, Deproj_Init.nFiles, 1)
  {
    i = ();
    thisModel = "myModel" + string (i);
    () = create_model (modelName, thisModel);
    () = set_par (thisModel + ".norm", "value", 1);
    () = set_frozen (thisModel);
  }

  % if the model is vmekal or vapec, set freeze/thaw for elements
  if ("vmekal" == Deproj_Init.model.name or "vapec" == Deproj_Init.model.name)
  {
    if (NULL == Deproj_Init.model.abund.freeze)
      Deproj_Init.model.abund.freeze = 1;
    if (NULL == Deproj_Init.model.He.freeze)
      Deproj_Init.model.He.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.C.freeze)
      Deproj_Init.model.C.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.N.freeze)
      Deproj_Init.model.N.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.O.freeze)
      Deproj_Init.model.O.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Ne.freeze)
      Deproj_Init.model.Ne.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Mg.freeze)
      Deproj_Init.model.Mg.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Al.freeze)
      Deproj_Init.model.Al.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Si.freeze)
      Deproj_Init.model.Si.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.S.freeze)
      Deproj_Init.model.S.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Ar.freeze)
      Deproj_Init.model.Ar.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Ca.freeze)
      Deproj_Init.model.Ca.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Fe.freeze)
      Deproj_Init.model.Fe.freeze = Deproj_Init.model.abund.freeze;
    if (NULL == Deproj_Init.model.Ni.freeze)
      Deproj_Init.model.Ni.freeze = Deproj_Init.model.abund.freeze;
  }
  if ("vmekal" == Deproj_Init.model.name)
    if (NULL == Deproj_Init.model.Na.freeze)
      Deproj_Init.model.Na.freeze = Deproj_Init.model.abund.freeze;

  % set up the extra normalization models
  _for (1, Deproj_Init.nDet, 1)
  {
    i = ();
    thisModel = "c" + string (i);
    () = create_model ("const1d", thisModel);
    seval (thisModel + " integrate off");
    () = set_thawed (thisModel);
    () = set_par (thisModel + ".c0", "value", 1);
  }

  % set up the foreground model if requested
  if (NULL != Deproj_Init.extra_model)
  {
    _for (1, Deproj_Init.nDet, 1)
    {
      i = ();
      () = create_model ("xs" + Deproj_Init.extra_model.name,
			 "extra_model" + string (i));

      % set up the foreground absorption model
      if (Wabs_Model_Type == typeof (Deproj_Init.extra_wabs))
      {
	() = create_model ("xswabs", "extra_abs");
	() = set_par ("extra_abs.nH", "value", Deproj_Init.extra_wabs.nH.value);
	if (Deproj_Init.extra_wabs.nH.freeze)
	  () = set_frozen ("extra_abs");
	else
	  () = set_thawed ("extra_abs");
      }
    }
  }
} % init_models()

%}}}

% initialize the necessary Sherpa variables
private define init_sherpa () %{{{
{
  % set errors to 90% confidence or level requested by user
  if (Deproj_Init.conf_interval != NULL)
  {
    if (Deproj_Init.sigma != NULL)
    {
      message ("*** Set the confidence interval or number of sigma, not both");
      seval ("exit");
    }
    variable interval;
    if (Double_Type != typeof(Deproj_Init.conf_interval))
      Deproj_Init.conf_interval = typecast (Deproj_Init.conf_interval,
                                            Double_Type);
    if (Deproj_Init.conf_interval > 1.0)
      Deproj_Init.conf_interval /= 100.0;
    Deproj_Init.sigma = ierf (Deproj_Init.conf_interval) / sqrt(2) * 2;
  }
  if (Deproj_Init.sigma == NULL)
    sherpa.proj.sigma = 1.64;
  else
    sherpa.proj.sigma = Deproj_Init.sigma;
  vmessage ("Error intervals will be %.2f sigma", sherpa.proj.sigma);

  % reduce output of error/warning messages
  set_verbose(0);

  % increase the maximum number of iterations to 10000
  seval("method levenberg-marquardt");
  seval("levenberg-marquardt.iters = 10000");

  % set abundance tables
  if (andelse {NULL != Deproj_Init.abundType}{strlen (Deproj_Init.abundType)})
    seval("xspec abundan " + Deproj_Init.abundType);
} % init_sherpa()

%}}}

% function that MUST BE CALLED FIRST to set everything else up
public define define_deproj (init_info) %{{{
{
  Deproj_Init = @init_info;

  % set energy limits
  if (Deproj_Init.emin != NULL)
    Emin = Deproj_Init.emin;
  if (Deproj_Init.emax != NULL)
    Emax = Deproj_Init.emax;
  % initialize Sherpa models
  variable detKeys;
  detKeys = assoc_get_keys (Deproj_Init.files);
  Deproj_Init.nDet = length(detKeys);
  Deproj_Init.nFiles = length (Deproj_Init.files[detKeys[0]]);
  init_models ();
  init_sherpa ();

  % normalizations from fits, run from outer-most to inner-most
  Model_Norms = Double_Type[Deproj_Init.nFiles];
  Fluxes = Struct_Type[Deproj_Init.nFiles];

  % initialize names
  variable params, frozen;
  switch (Deproj_Init.model.name)
    { case "mekal":
      params = [ "nH", "kT", "Abund", "Redshift" ];
      frozen = [ Deproj_Init.wabs.nH.freeze, Deproj_Init.model.kT.freeze,
		 Deproj_Init.model.abund.freeze,
		 Deproj_Init.model.redshift.freeze ];
      Free_Params = 4 - Deproj_Init.wabs.nH.freeze - 
	Deproj_Init.model.kT.freeze - Deproj_Init.model.abund.freeze -
	Deproj_Init.model.redshift.freeze;
    }
    { case "apec":
      params = [ "nH", "kT", "Abundanc", "Redshift" ];
      frozen = [ Deproj_Init.wabs.nH.freeze, Deproj_Init.model.kT.freeze,
		 Deproj_Init.model.abund.freeze,
		 Deproj_Init.model.redshift.freeze ];
      Free_Params = 4 - Deproj_Init.wabs.nH.freeze -
	Deproj_Init.model.kT.freeze - Deproj_Init.model.abund.freeze -
	Deproj_Init.model.redshift.freeze;
    }
    { case "vapec":
      params = [ "nH", "kT", "He", "C", "N", "O", "Ne", "Mg", "Al", "Si", "S",
		 "Ar", "Ca", "Fe", "Ni", "Redshift" ];
      frozen = [ Deproj_Init.wabs.nH.freeze, Deproj_Init.model.kT.freeze,
		 Deproj_Init.model.He.freeze, Deproj_Init.model.C.freeze,
		 Deproj_Init.model.N.freeze, Deproj_Init.model.O.freeze,
		 Deproj_Init.model.Ne.freeze, Deproj_Init.model.Mg.freeze,
		 Deproj_Init.model.Al.freeze, Deproj_Init.model.Si.freeze,
		 Deproj_Init.model.S.freeze, Deproj_Init.model.Ar.freeze,
		 Deproj_Init.model.Ca.freeze, Deproj_Init.model.Fe.freeze,
		 Deproj_Init.model.Ni.freeze,
		 Deproj_Init.model.redshift.freeze ];
      Free_Params = 16 - Deproj_Init.wabs.nH.freeze -
	Deproj_Init.model.kT.freeze - Deproj_Init.model.He.freeze -
	Deproj_Init.model.C.freeze - Deproj_Init.model.N.freeze -
	Deproj_Init.model.O.freeze - Deproj_Init.model.Ne.freeze -
	Deproj_Init.model.Mg.freeze - Deproj_Init.model.Al.freeze -
	Deproj_Init.model.Si.freeze - Deproj_Init.model.S.freeze -
	Deproj_Init.model.Ar.freeze - Deproj_Init.model.Ca.freeze -
	Deproj_Init.model.Fe.freeze - Deproj_Init.model.Ni.freeze -
	Deproj_Init.model.redshift.freeze;
    }
    { case "vmekal":
      params = [ "nH", "kT", "He", "C", "N", "O", "Ne", "Na", "Mg", "Al", "Si",
		  "S","Ar", "Ca", "Fe", "Ni", "Redshift" ];
      frozen = [ Deproj_Init.wabs.nH.freeze, Deproj_Init.model.kT.freeze,
		 Deproj_Init.model.He.freeze, Deproj_Init.model.C.freeze,
		 Deproj_Init.model.N.freeze, Deproj_Init.model.O.freeze,
		 Deproj_Init.model.Ne.freeze, Deproj_Init.model.Na.freeze,
		 Deproj_Init.model.Mg.freeze, Deproj_Init.model.Al.freeze,
		 Deproj_Init.model.Si.freeze, Deproj_Init.model.S.freeze,
		 Deproj_Init.model.Ar.freeze, Deproj_Init.model.Ca.freeze,
		 Deproj_Init.model.Fe.freeze, Deproj_Init.model.Ni.freeze,
		 Deproj_Init.model.redshift.freeze ];
      Free_Params = 17 - Deproj_Init.wabs.nH.freeze -
	Deproj_Init.model.kT.freeze - Deproj_Init.model.He.freeze -
	Deproj_Init.model.C.freeze - Deproj_Init.model.N.freeze -
	Deproj_Init.model.O.freeze - Deproj_Init.model.Ne.freeze -
	Deproj_Init.model.Na.freeze - Deproj_Init.model.Mg.freeze -
	Deproj_Init.model.Al.freeze - Deproj_Init.model.Si.freeze -
	Deproj_Init.model.S.freeze - Deproj_Init.model.Ar.freeze -
	Deproj_Init.model.Ca.freeze - Deproj_Init.model.Fe.freeze -
	Deproj_Init.model.Ni.freeze - Deproj_Init.model.redshift.freeze;
    }
  Free_Params += Deproj_Init.nDet;
  variable nindices = Deproj_Init.nFiles * Free_Params;
  variable paramsToFit = String_Type[Free_Params];
  paramsToFit[[0:Free_Params-Deproj_Init.nDet-1]] = params[where(frozen == 0)];
  paramsToFit[[Free_Params-Deproj_Init.nDet:Free_Params-1]] =
    "c" + array_map (String_Type, &string, [1:Deproj_Init.nDet]) + ".c0";
  Saved_Fits.name = String_Type[nindices];
  _for (0, Deproj_Init.nFiles-1, 1)
  {
    variable i = ();
    Saved_Fits.name[[0:Free_Params-1]+i*Free_Params] = paramsToFit;
  }

  % initialize values
  Saved_Fits.val = Double_Type[nindices];
  Saved_Fits.val[[0:nindices-1]] = 0;
  Saved_Fits.vlo = Double_Type[nindices];
  Saved_Fits.vlo[[0:nindices-1]] = 0;
  Saved_Fits.vhi = Double_Type[nindices];
  Saved_Fits.vhi[[0:nindices-1]] = 0;
} % define_deproj()

%}}}

vmessage ("This is spec_decon version %s", _spec_decon_version_string);
provide ("spec_decon");
