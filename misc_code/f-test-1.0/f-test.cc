/*******************************************************************************
Program to compare two fits to the same set of data using the F-Test.
Copyright (C) 2003  Joshua Kempner

version 1.0

Requires the GNU Scientific Library, which is licensed under the GNU
GPL (see below).  This program has been tested against version 1.3 of
the GSL.  No guarantee is made that it will work with any other version
of the library. The GSL is available at
http://www.gnu.org/directory/GNUsl.html

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Please send any bug fixes, enhancements, or useful comments by email to
jkempner@cfa.harvard.edu, or by postal mail to Joshua Kempner,
Harvard-Smithsonian Center for Astrophysics, 60 Garden Street, MS-67,
Cambridge, MA 02138, USA.
*******************************************************************************/

#include <iostream>
#include <cmath>
#include <limits>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_sf_result.h>
#include <gsl/gsl_errno.h>

using namespace std;

typedef numeric_limits<double> NLD;

void help()
{
  cout << "Usage: f-test [chi1 dof1 chi2 dof2]\n"
       << "Additional options:\n"
       << "   -v or --version  - print the version number of f-test\n" 
       << "   -h or --help     - print this message" << endl;
}

void printVersion()
{
  string version = "1.0";
  cout << version << endl;
}

double promptForArg(const char* prompt)
{
  double input;
  while (((cout << prompt << ": ") && !(cin >> input)) || (input <= 0))
  {
    cerr << "  Please enter a positive number" << endl;
    cin.clear();
    cin.ignore(NLD::max(), '\n');
  }
  return input;
}

double probability(double df1, double df2, double F)
{
  gsl_sf_result* result = new gsl_sf_result;
  int status = gsl_sf_beta_inc_e(0.5*df2, 0.5*df1, df2/(df2+df1*F), result);
  // possible errors are GSL_EDOM, GSL_EUNDRFLW
  if (status)
  {
    if (GSL_EDOM == status)
    {
      if (df1 < 0 || df2 < 0 || F < 0)
	cerr << "Error: Chi-squared and degrees of freedom must each be > 0."
	     << endl;
      else
	cerr << "Error calculating probability: " << gsl_strerror(status)
	     << endl;
      return -1;
    }
    else if (GSL_EUNDRFLW == status)
    {
      cerr << "Error calculating probability: " << gsl_strerror(status) << endl;
      return -1;
    }
  }
  double prob = 2.0 * result->val;
  delete result;
  return prob;
}

int main(int argc, char** argv)
{
  double chi1, df1, chi2, df2;

  // check for version request
  if (2 == argc && (!strncmp(argv[1],"-v", 2) || !strncmp(argv[1],"--v", 3)))
  {
    printVersion();
    return 0;
  }

  // check for help request or incorrect usage
  if (((argc > 1 && argc < 5) || argc > 5) &&
      (!strncmp(argv[1],"-h", 2) || !strncmp(argv[1],"--h", 3)))
  {
    help();
    return 0;
  }

  if (argc < 5) // prompt for the arguments if not given on the command line
  {
    chi1 = promptForArg("Chi-squared from first fit");
    df1 = promptForArg("Number of degrees of freedom from first fit");
    chi2 = promptForArg("Chi-squared from second fit");
    df2 = promptForArg("Number of degrees of freedom from second fit");
  }
  else // get the arguments from the command line
  {
    chi1 = atof(argv[1]);
    df1 = atof(argv[2]);
    chi2 = atof(argv[3]);
    df2 = atof(argv[4]);
  }

  // turn off the GSL error handler
  (void)gsl_set_error_handler_off();

  // make F the ratio of the first chi-squared to the second one.
  double F = (chi1 / df1) / (chi2 / df2);
  double prob = probability(df1, df2, F);
  if (prob < 0) return prob; // beta function failed
  cout << "Probability that fit 1 is better than fit 2 is " << prob << endl;

  // now invert the calculation
  prob = probability(df2, df1, 1/F);
  if (prob < 0) return prob; // beta function failed
  cout << "Probability that fit 2 is better than fit 1 is " << prob << endl;

  return 0;
}
