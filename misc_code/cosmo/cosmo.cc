/*******************************************************************************
Definitions file for a cosmology library of general use in observational astronomy
Copyright (C) 2003-2006  Joshua Kempner

Version 2.1.2

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
josh@kempner.net, or by postal mail to Joshua Kempner,
Physics & Astronomy Department, Bowdoin College, 8800 College Station,
Brunswick, ME 04011, USA.
*******************************************************************************/

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <vector>
#include <limits>

#include "cosmo.h"

using namespace std;

// global variables, all of them physical constants
const double c = 2.99792458e5;
const double G = 6.67259e-8;
const double PI = atan(double(1)) * 4;
const double kmPerMpc = 3.08567758e19;
const double tropicalYear = 3.1556926e7; // in seconds

////////////////////////////////////////////////////////////////////////////////
// private member functions for class Cosmo
////////////////////////////////////////////////////////////////////////////////

// initialization function, called by the constructors
void Cosmo::init(const double hNought, const double omegaMatter,
		 const double omegaLambda)
{
    H0_ = hNought;
    OmegaM_ = omegaMatter;
    OmegaL_ = omegaLambda;
    Omegak_ = 1.0 - omegaMatter - omegaLambda;
    if (Omegak_ <= numeric_limits<double>::epsilon())
        Omegak_ = 0;
    q0_ = 0.5 * OmegaM_ - OmegaL_;
    dH_ = c / H0_;
    age_ = romberg(&Cosmo::ageIntegrand, 0, 1000) / H0_ * kmPerMpc;
    dC_ = 0;
    dM_ = 0;
    dA_ = 0;
    dL_ = 0;
    VC_ = 0;
    tL_ = 0;
    z_ = 0;
    scale_ = 0;
    rhoCrit_ = 0;
}

// Romberg integration
double Cosmo::romberg(PFD func, double a, double b)
{
    double h = b - a;     // coarsest panel size
    double dR;			  // convergence
    int np = 1;           // Current number of panels
    const int N = 25;     // maximum iterations
    double prec = 1e-8;	  // desired precision
    vector<double> R(N*N);
    // Compute the first term R(1,1)
    R[0] = h/2 * ((this->*func)(a) + (this->*func)(b));
    
    // Loop over the desired number of rows, i = 2,...,N
    int i,j,k;
    for( i=1; i<N; ++i )
    {
        // Compute the summation in the recursive trapezoidal rule
        h /= 2.0;          // Use panels half the previous size
        np *= 2;           // Use twice as many panels
        double sumT = 0.0;
        for( k=1; k<=(np-1); k+=2 ) 
            sumT += (this->*func)( a + k*h);
        
        // Compute Romberg table entries R(i,1), R(i,2), ..., R(i,i)
        R[N*i] = 0.5 * R[N*(i-1)] + h * sumT;   
        int m = 1;
        for( j=1; j<i; ++j )
        {
            m *= 4;
            R[N*i+j] = R[N*i+j-1] + (R[N*i+j-1] - R[N*(i-1)+j-1]) / (m-1);
        }
        dR = (j > 1) ? R[N*i+j-1] - R[N*(i-1)+(j-2)] : R[0];
        if (fabs(dR) < prec)
            return R[N*i+j-1];
    }
    return R.back();
}

////////////////////////////////////////////////////////////////////////////////
// Public member functions for class Cosmo
////////////////////////////////////////////////////////////////////////////////

// default constructor
Cosmo::Cosmo()
{
    init(71, 0.27, 0.73); // values from initial WMAP release
}

// constructor with non-default cosmological parameters
Cosmo::Cosmo(const double hNought, const double omegaMatter,
	     const double omegaLambda)
{
    init(hNought, omegaMatter, omegaLambda);
}

// copy constructor
Cosmo::Cosmo(const Cosmo& a)
{
    clone(a);
}

// assignment operator
Cosmo& Cosmo::operator=(const Cosmo& a)
{
    if (this == &a) return *this;   // don't assign to self

    clone(a);
    return *this;
}

// sets scale_, and the three distance measures
void Cosmo::setDistances()
{
    // calculate critical density
    rhoCrit_ = 3.0 / 8.0 / PI * SQR(H0_ / kmPerMpc) / G *
    (OmegaL_ + pow(1 + z_, 3) * OmegaM_);
    
    if (!z_)
    {
        dC_ = dM_ = VC_ = dA_ = dL_ = tL_ = 0;
        scale_ = 0;
        return;
    }

    // calculate the line-of-sight comoving distance using Romberg integration
    dC_ = dH_ * romberg(&Cosmo::inverseOfE, 0, z_);
    
    // calculate everything else from the comoving distance
    if (Omegak_ > 0)
    {
        dM_ = dH_ / sqrt(Omegak_) * sinh(sqrt(Omegak_) * dC_ / dH_);
        VC_ = 2 * PI * pow(dH_, 3) / Omegak_ *
            (dM_ / dH_ * sqrt(1 + Omegak_ * SQR(dM_ / dH_)) -
             asinh(fabs(Omegak_) * dM_ / dH_) / sqrt(Omegak_)) / 1e9;
    }
    else if (Omegak_ < 0)
    {
        dM_ = dH_ / sqrt(fabs(Omegak_)) * sin(sqrt(fabs(Omegak_)) * dC_ / dH_);
        VC_ = 2 * PI * pow(dH_, 3) / fabs(Omegak_) *
            (dM_ / dH_ * sqrt(1 + Omegak_ * SQR(dM_ / dH_)) -
             asin(fabs(Omegak_) * dM_ / dH_) / sqrt(fabs(Omegak_))) / 1e9;
    }
    else
    {
        dM_ = dC_;
        VC_ = 4 * PI * pow(dM_, 3) / 3 / 1e9;
    }
    dA_ = dM_ / (1 + z_);
    dL_ = dM_ * (1 + z_);
    tL_ = romberg(&Cosmo::ageIntegrand, 0, z_) / H0_ * kmPerMpc;
    scale_ = dA_ / 648 * PI;
}

// print info about the cosmology to the given ostream (default stream is STDOUT)
// "leader" is prepended to the output
void Cosmo::printParams(ostream& os = cout, const char* leader = "")
{
    os << leader
       << resetiosflags(ios::floatfield) << "H_0 = " << H0_
       << ", Omega_m = " << OmegaM_
       << ", Omega_L = " << OmegaL_;
    if (fabs(Omegak_) > numeric_limits<double>::epsilon())
        os << ", Omega_k = " << Omegak_;
    os << "  (q_0 = " << q0_ << ")";
    os << "\n";
}

// print a verbose summary of all the member data to STDOUT
void Cosmo::printLong()
{
    printParams();
    cout << setprecision(6)
         << "At z = " << z_ << "\n"
         << "  age of the Universe at z      = "
         << (age_ - tL_) / tropicalYear / 1e9 << " Gyr\n"
         << "  lookback time to z            = " << tL_ / tropicalYear / 1e9 << " Gyr\n"
         << "  angular diameter distance d_A = " << dA_ << " Mpc\n"
         << "  luminosity distance d_L       = " << dL_ << " Mpc\n"
         << "  comoving radial distance d_C  = " << dC_ << " Mpc\n";
    if (dM_ != dC_)
        cout << "  comoving transverse distance  = " << dM_ << " Mpc\n";
    cout << "  comoving volume out to z      = " << VC_ << " Gpc**3\n"
         << setprecision(4) << scientific
         << "  critical density at z         = " << rhoCrit_ << " g cm**-3\n"
         << setprecision(6) << fixed
         << "  1\" = " << scale_ << " kpc\n";
    if (scale_)
        cout << "  1 kpc = " << 1/scale_ << "\"" << endl;
}

// print to an ofstream a header line suitable for use with printShort()
// default stream is STDOUT
void Cosmo::printShortHeader(ostream & os = cout)
{
    printParams(os, "# ");
    os << "# z \td_A \td_L \td_M \tscale \t1/scale" << endl;
}

// print (to an ostream) the parameters on a single line.
// default stream is STDOUT
void Cosmo::printShort(ostream & os = cout)
{
    os << setprecision(6)
       << z_ << "\t" << dA_ << "\t" << dL_ << "\t" << dC_ << "\t" << scale_ << "\t"
       << 1/scale_ << endl;
}

// set the cosmological parameters and the secondary stuff derived from them
void Cosmo::setCosmology(const double hNought, const double omegaMatter,
			 const double omegaLambda)
{
    init(hNought, omegaMatter, omegaLambda);
    if (z_) setDistances();
}

// set z_ using user input, then set the things that depend on z_
void Cosmo::setRedshift(const double redshift)
{
    z_ = redshift;
    setDistances();
}

// prompt the user for the cosmological parameters
void Cosmo::getCosmologyFromUser()
{
    double H0_tmp, OmegaM_tmp, OmegaL_tmp;
    while ((H0_tmp = promptForParam("Hubble constant", H0_)) <= 0)
        cerr << "  The Hubble constant must be > 0" << endl;
    while ((OmegaM_tmp = promptForParam("Omega matter", OmegaM_)) < 0)
        cerr << "  Omega matter must be >= 0" << endl;
    OmegaL_tmp = promptForParam("Omega lambda", OmegaL_);
    init(H0_tmp, OmegaM_tmp, OmegaL_tmp);
}


////////////////////////////////////////////////////////////////////////////////
// non-member functions for use with class Cosmo
////////////////////////////////////////////////////////////////////////////////

// determines if the given string is a valid number
int isNumeric(const string& text)
{
    int i, n;
    n = text.length();
    // find non-numeric characters
    i = text.find_first_not_of("-0123456789.");
    if (i >= 0 && i < n)
        return 0;
    // look a for negative sign after the first character
    i = text.find("-", 1);
    if (i > 0 && i < n)
        return 0;
    // now look for multiple decimals
    i = text.find(".");
    if (i >= 0 && i < n)
    {
        i = text.find(".", i+1);
        if (i >= 0 && i < n)
            return 0;
    }
    return 1;
}

// prompts the user to input the value of a variable. Uses defaultVal if
// the user simply hits return, otherwise returns the value input by the
// user.
double promptForParam(const char* description, const double defaultVal)
{
    string temp;
    cout << description << " (" << defaultVal << "): ";
    getline(cin, temp, '\n');
    if (temp.length())
    {
        if (isNumeric(temp))
            return atof(temp.c_str());
        else
        {
            cerr << "  Not a valid number" << endl;
            return promptForParam(description, defaultVal);
        }
    }
    return defaultVal;
}

