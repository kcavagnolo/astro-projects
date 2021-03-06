//      Adaptive Binning Program
//      Binning module - class to allow different methods of binning
//                       to be used
//      Described in Sanders and Fabian (submitted)
//      Routines for adaptively binning data
//      Copyright (C) 2000 Jeremy Sanders
//      Contact: jss@ast.cam.ac.uk
//               Institute of Astronomy, Madingley Road,
//               Cambridge, CB3 0HA, UK.

//      See the file COPYING for full licence details.

//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.

//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.

//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#include <iostream>
#include <fstream>
#include <strstream>
#include <cassert>
#include <cmath>
#include "binmodule.hh"
#include <FITSFile.h>

using std::sqrt;
using std::fabs;
using std::string;
using std::ifstream;
using std::ostrstream;
using std::istrstream;

namespace AdaptiveBin
{

  // return true if file exists
  bool checkfileexists(const string &fn)
  {
    ifstream file(fn.c_str());
    return( file );
  }

  binmodule::~binmodule()
  {
  }

  ////////////////////////////////

  count_binmodule::count_binmodule(const arglist &al)
  {
    double background = 0.;

    if(al.size() != 1 || al.size() != 2)
      throw invalidargs_exception();

    if( al.size() == 2 ) {
      if(al[1].substr(0,3) != "bg=")
	throw invalidargs_exception();

      istrstream bg(al[1].substr(3).c_str());
      bg >> background;
      if(! bg) throw invalidargs_exception();
    }

    setf(al[0], background);
  }

  count_binmodule::count_binmodule(const string &fname,
				   double background)
  {
    setf(fname, background);
  }

  void count_binmodule::setf(const string &fname,
				   double background)
  {
    m_background = background;
    if( ! checkfileexists(fname) )
      throw invalidargs_exception();

    CFITSFile file(fname.c_str(), CFITSFile::existingro);
    m_image = file.GetImage();
    m_posn = file.GetPosn();
  }

  int count_binmodule::xw()
  {
    return m_image.GetXW();
  }

  int count_binmodule::yw()
  {
    return m_image.GetYW();
  }

  double count_binmodule::value(const pixlist &pl)
  {
    assert(pl.size() != 0);

    double tot=0.;
    for(int i=pl.size()-1; i>=0; i--)
      tot += m_image.GetPixel(pl[i].x(), pl[i].y());

    return tot/pl.size() - m_background;
  }

  double count_binmodule::fracerror(const pixlist &pl,
				    bool binerror)
  {
    assert(pl.size() != 0);

    double tot=0.;
    for(int i=pl.size()-1; i>=0; i--)
      tot += m_image.GetPixel(pl[i].x(), pl[i].y());

    const double bg = pl.size()*m_background;

    // error in tot=sqrt(tot), error in bg=sqrt(bg)
    return sqrt(tot + bg)/(tot - bg);
  }

  void count_binmodule::getposn(CFITSPosn *posn)
  {
    *posn = m_posn;
  }

  void count_binmodule::selectvalue(const string &spec)
  {
    if(spec != "count(0)")
      throw invalidvalue_exception();
  }

  string count_binmodule::get_value_descr()
  {
    return "count(0)";
  }

  //////////////////////////////////////////////////////
  // ratio_binmodule
  ratio_binmodule::ratio_binmodule(const arglist &al)
  {
    assert( al.size() > 0 );

    for(unsigned i=0; i<al.size(); i++) {
      string fname = al[i];
      double bg = 0.;

      if(i+1 < al.size()) {
	// check to look for background specs
	if( al[i+1].substr(0,3) == "bg=") {
	  istrstream bgs(al[i+1].substr(3).c_str());
	  bgs >> bg;
	  if(!bgs) throw invalidargs_exception();
	  i++; // ignore next arg
	}
      }  // check if background
      
      m_counts.push_back( count_binmodule(fname, bg) );
    } // loop over names

    m_value = vcount;
    m_valparam[0] = m_valparam[1] = 0;
  }

  double ratio_binmodule::value(const pixlist &pl)
  {
    assert(m_valparam[0] < m_counts.size());
    assert(m_valparam[1] < m_counts.size());

    switch(m_value) {
    case vcount:
      return m_counts[m_valparam[0]].value(pl);
    case vratio:
      return m_counts[m_valparam[0]].value(pl) /
	m_counts[m_valparam[1]].value(pl);
    }

    return -1.;
  }

  double ratio_binmodule::fracerror(const pixlist &pl,
				    bool binerror)
  {
    assert(m_valparam[0] < m_counts.size());
    assert(m_valparam[1] < m_counts.size());

    if( ! binerror ) {
      switch(m_value) {
      case vcount:
	return m_counts[m_valparam[0]].fracerror(pl, binerror);
      case vratio:
	const double e1 = m_counts[m_valparam[0]].fracerror(pl, binerror);
	const double e2 = m_counts[m_valparam[1]].fracerror(pl, binerror);
	return sqrt(e1*e1+e2*e2);
      }
      return -1.;
    } else {
      double totsqd = 0.;
      for(unsigned i=0; i<m_counts.size(); i++) {
	const double e = m_counts[i].fracerror(pl, binerror);
	totsqd += e*e;
      }
      return sqrt(totsqd);
    }
  }
  
  void ratio_binmodule::getposn(CFITSPosn *out)
  {
    assert(m_counts.size() > 0);
    m_counts[0].getposn(out);
  }

  int ratio_binmodule::xw()
  {
    assert(m_counts.size() > 0);
    return( m_counts[0].xw());
  }
  int ratio_binmodule::yw()
  {
    assert(m_counts.size() > 0);
    return( m_counts[0].yw());
  }

  string ratio_binmodule::get_value_descr()
  {
    ostrstream o;

    switch(m_value) {
    case vcount: 
      o << "count(" << m_valparam[0] << ")";
      break;
    case vratio:
      o << "ratio(" << m_valparam[0] << ", "
	<< m_valparam[1] << ")";
      break;
    }

    o << '\0';
    return o.str();
  }

  void ratio_binmodule::selectvalue(const string &s)
  {
    if(s.substr(0, 6) == "count(") {
      m_value = vcount;
      istrstream n(s.substr(6).c_str());
      n >> m_valparam[0];
      m_valparam[1] = 0;
      if(!n || m_valparam[0]>=m_counts.size() )
	throw invalidvalue_exception();
      return;
    }

    if(s.substr(0, 6) == "ratio(") {
      m_value = vratio;
      istrstream n(s.substr(6).c_str());
      n >> m_valparam[0];
      if( n.peek() == ',' ) n.ignore();
      n >> m_valparam[1];
      if(!n || m_valparam[0]>=m_counts.size() ||
	 m_valparam[1]>=m_counts.size() )
	throw invalidvalue_exception();
      return;
    }

    throw invalidvalue_exception();
  }

}  // namespace

