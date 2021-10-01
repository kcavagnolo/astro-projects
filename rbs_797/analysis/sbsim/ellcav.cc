// Elliptical cavities embedded in a beta model atmosphere

#include <iostream>
#include <fstream>
#include <cstring>
#include <cmath>
#include <gsl/gsl_sf_gamma.h>
#include <stdlib.h>

class Ellipsoid;


// Vectors for use by Ellipsoid
class Vec3 {
private:
  double x[3];

  class badformat {};

public:
  // Uses default copy constructor and copy assignment
  Vec3 (const double *p) {memcpy (&x[0], p, 3 * sizeof (double));}
  Vec3 () {memset (&x[0], 0, 3 * sizeof (double));}

  double dot (const Vec3& y) const;
  double norm () const {return sqrt (this->dot (*this));}
  Vec3& operator*= (const double& a);
  Vec3 operator* (const double& a) const {Vec3 t (*this); t *= a; return t;}
  Vec3& operator/= (const double& a) {return *this *= 1.0 / a;}
  Vec3& operator+= (const Vec3& a);
  Vec3 operator+ (const Vec3& a) const {Vec3 t (*this); t += a; return t;}
  Vec3& operator-= (const Vec3& a);
  Vec3 operator- (const Vec3& a) const {Vec3 t (*this); t -= a; return t;}
  Vec3 orthog (const Vec3& a) const {
    Vec3 t (*this); t -= a * (this->dot (a) / a.dot (a)); return t;
  }
  Vec3 cross (const Vec3& a) const;
  void dyadic (const Vec3 a, double *d) const;
  Vec3 contract (const double *d) const;
  friend std::ostream& operator<< (std::ostream& dest, const Vec3& v);
  friend std::istream& operator>> (std::istream& sce, Vec3& v);
};

// Dot product
double Vec3::dot (const Vec3& y) const {
  return x[0] * y.x[0] + x[1] * y.x[1] + x[2] * y.x[2];
}

// Right scalar multiplication
Vec3& Vec3::operator*= (const double& a) {
  x[0] *= a;
  x[1] *= a;
  x[2] *= a;
  return *this;
}

// Basic addition
Vec3& Vec3::operator+= (const Vec3& a) {
  x[0] += a.x[0];
  x[1] += a.x[1];
  x[2] += a.x[2];
  return *this;
}

// Basic subtraction
Vec3& Vec3::operator-= (const Vec3& a) {
  x[0] -= a.x[0];
  x[1] -= a.x[1];
  x[2] -= a.x[2];
  return *this;
}

Vec3 Vec3::cross (const Vec3& a) const {
  Vec3 t;
  t.x[0] = x[1] * a.x[2] - x[2] * a.x[1];
  t.x[1] = x[2] * a.x[0] - x[0] * a.x[2];
  t.x[2] = x[0] * a.x[1] - x[1] * a.x[0];
  return t;
}

// Dyadic product with accumulation
void Vec3::dyadic (const Vec3 a, double *d) const {
  for (int j = 0; j < 3; ++j) {
    for (int i = 0; i < 3; ++i) {
      // Fortran order
      d [i + 3 * j] += x[i] * a.x[j];
    }
  }
}

// Left multiply by the matrix d
Vec3 Vec3::contract (const double *d) const {
  double res[3];
  for (int i = 0; i < 3; ++i) {
    res[i] = 0.0;
    for (int j = 0; j < 3; ++j) {
      res[i] += d [i + 3 * j] * x[j];
    }
  }
  return Vec3 (res);
}

std::ostream& operator<< (std::ostream& dest, const Vec3& v) {
  dest << "(" << v.x[0] << ", " << v.x[1] << ", " << v.x[2] << ")";
  return dest;
}

std::istream& operator>> (std::istream& sce, Vec3& v) {
  // No checking
  char c1, c2, c3, c4;
  sce >> c1 >> v.x[0] >> c2 >> v.x[1] >> c3 >> v.x[2] >> c4;
  if (c1 != '(' || c2 != ',' || c3 != ',' || c4 != ')')
    throw Vec3::badformat ();
  return sce;
}
    

// Ellipsoid defined by its centre and a quadratic form
class Ellipsoid {
private:
  const Vec3 c;        // Centre
  const double *const q;  //Quadratic form

  class notorthogonal {};

  double *mkq (const Vec3& a, const Vec3& b, const double& c);
  double *cpq (const double* qin);

public:
  Ellipsoid (const Vec3& pos, const Vec3& axis, const Vec3& bxis, 
	     const double& cxis);
  Ellipsoid (const Ellipsoid& a);
  ~Ellipsoid () {delete[] q;}
  void dumpq () const;
  int solve (const Vec3& pos, const Vec3& dir, Vec3& u, Vec3& v) const;
  bool higher (const Ellipsoid& a, const Vec3& dir) const;
};

// Assemble the quadratic form.
// a = offset from the centre to the ellipsoid on one principal axis
// b = offset along another principal axis
// c = length of third principal axis
double *Ellipsoid::mkq (const Vec3& a, const Vec3& b, const double& c) {
    double alam = 1.0 / a.norm ();
    Vec3 e1 = a * alam;  // Unit vector for first axis
    double blam = 1.0 / b.norm ();
    static const double eps = 1e-15;
    double check = e1.dot (b) * blam;
    //    if (fabs (check) > eps)
    //throw notorthogonal ();
    Vec3 e2 = b * blam;  // Unit vector for second axis
    double clam = 1.0 / c;
    Vec3 e3 = e1.cross (e2);  // Unit vector for third axis
    // Accumulate the quadratic form
    double *qt = new double[9];
    memset (qt, 0, 9 * sizeof (double));
    e1 *= alam;
    e1.dyadic (e1, qt);
    e2 *= blam;
    e2.dyadic (e2, qt);
    e3 *= clam;
    e3.dyadic (e3, qt);
    return qt;
}

// Copy the quadratic form for the copy constructor
double *Ellipsoid::cpq (const double* qin) {
  double *d = new double[9];
  memcpy (d, qin, 9 * sizeof (double));
  return d;
}

Ellipsoid::Ellipsoid (const Vec3& pos, const Vec3& axis, const Vec3& bxis, 
		      const double& cxis)
  : c (pos), q (mkq (axis, bxis, cxis)) {
}

Ellipsoid::Ellipsoid (const Ellipsoid& a) : c (a.c), q (cpq (a.q)) {
}

void Ellipsoid::dumpq () const {
  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      std::cout << q [i + 3 * j] << "  ";
    }
    std::cout << std::endl;
  }
}

// Points where the line pos + lambda * dir intersects the Ellipsoid
int Ellipsoid::solve (const Vec3& pos, const Vec3& dir, Vec3& u, 
		      Vec3& v) const {
  Vec3 d (pos - c), f (d.contract (q));
  double cc = d.dot (f) - 1.0;
  double bb = 2.0 * dir.dot (f);
  double aa = dir.dot (dir.contract (q));
  double disc = bb * bb - 4.0 * aa * cc;
  if (disc < 0.0)
    // No solutions
    return 0;
  if (disc == 0.0) {
    // One solution
    double lam = -bb / (2.0 * aa);
    u = pos + dir * lam;
    return 1;
  }
  // Two solutions
  double tv = sqrt (disc);
  double lam = (-bb - tv) / (2.0 * aa);
  u = pos + dir * lam;
  lam = (tv - bb) / (2.0 * aa);
  v = pos + dir * lam;
  return 2;
}

// Is first Ellipsoid above 2nd wrt direction dir?
bool Ellipsoid::higher (const Ellipsoid& a, const Vec3& dir) const {
  return c.dot (dir) > a.c.dot (dir);
}


// Really belong to Cavmod
static const double ezc[] = {0.0, 0.0, 1.0};
static const Vec3 ez (ezc);


// Beta model with two cavities
class Cavmod {
private:
  const double acore;
  const double beta;
  const Ellipsoid cava, cavb;

  class outoforder {};
  class overlap {};
  class badbeta {};

  const Ellipsoid& upper (const Ellipsoid& a, const Ellipsoid& b);
  const Ellipsoid& lower (const Ellipsoid& a, const Ellipsoid& b);

public:
  // Not intended to be public
  int elsol (const double& x, const double& y, double *z) const;

  Cavmod (const double& a, const double& b, const Ellipsoid& ra, 
	  const Ellipsoid& rb);
  double sb (const double& x, const double& y) const;
  double pixsb (const double& xmin, const double& ymin, const double& pixwid,
		int npix) const;
};

// Get the lower cavity
const Ellipsoid& Cavmod::upper (const Ellipsoid& a, const Ellipsoid& b) {
  return a.higher (b, ez) ? a : b;
}

// Get the upper cavity
const Ellipsoid& Cavmod::lower (const Ellipsoid& a, const Ellipsoid& b) {
  return a.higher (b, ez) ? b : a;
}

// NB: Cavity a is the lower one
Cavmod::Cavmod (const double& a, const double& b, const Ellipsoid& ra, 
		const Ellipsoid& rb) 
  : acore (a), beta (b), cava (lower (ra, rb)), cavb (upper (ra, rb)) {
  if (beta <= 1.0 / 6.0)
    throw badbeta ();
}

// Find limits of cavities in z at the specified position
int Cavmod::elsol (const double& x, const double& y, double *z) const {
  double posc[] = {x, y, 0.0};
  Vec3 pos (posc);
  Vec3 u, v;
  int nsol = 0, isol = cava.solve (pos, ez, u, v);
  if (isol == 2) {
    z[0] = ez.dot (u);
    z[1] = ez.dot (v);
    if (z[0] > z[1])
      // Should not happen
      throw outoforder ();
    nsol = 2;
  }
  isol = cavb.solve (pos, ez, u, v);
  if (isol == 2) {
    z[nsol++] = ez.dot (u);
    z[nsol++] = ez.dot (v);
    if (z[nsol - 2] > z[nsol - 1])
      // Should not happen
      throw outoforder ();
    if (nsol == 4 && z[1] > z[2])
      // Should only happen if the cavities overlap
      throw overlap ();
  }

  return nsol;
}

double Cavmod::sb (const double& x, const double& y) const {
  double z[4];
  int nsol = elsol (x, y, z);
//   std::cout << "Number of solutions: " << nsol << "\nz values:";
//   for (int i = 0; i < nsol; ++i) {
//     std::cout << " " << z[i];
//   }
//   std::cout << std::endl;
  double pom2 = x * x + y * y, a2 = acore * acore, power = 3.0 * beta - 0.5;
  // SB without any cavities
  double raw = pow (1.0 + pom2 / a2, -power);
  // Compute deletions in pairs
  double del = 0.0;
  for (int i = 0; i < nsol; ++i) {
    // Compute the (non-overlapping) deletions due to cavities
    double zz = z[i], z2 = zz * zz;
    // This is the contribution to the integral for [0, z].
    double t0 = gsl_sf_beta_inc (0.5, power, z2 / (a2 + pom2 + z2));
    if (zz < 0.0)
      t0 = -t0;
    if (i % 2)
      t0 = -t0;
    del += t0;
  }
  return (1.0 + 0.5 * del) * raw;
}

double Cavmod::pixsb (const double& xmin, const double& ymin, 
		      const double& pixwid, int npix) const {
  double av = 0.0;
  for (int i = 0; i < npix; ++i) {
    double x = xmin + (i + 0.5) * pixwid / npix;
    for (int j = 0; j < npix; ++j) {
      double y = ymin + (j + 0.5) * pixwid / npix;
      av += sb (x, y);
    }
  }
  return av / (npix * npix);
}


// Set up cavity model
Cavmod& mkmodel (const char *fname) {
  std::ifstream sce (fname);

  Vec3 centre, axis, bxis;
  double cxis;

  // First ellipsoid
  sce >> centre;
  sce.ignore (256, '\n');  // To allow trailing comment
  std::cout << "First ellipsoid centre: " << centre << "\n";
  sce >> axis;
  sce.ignore (256, '\n');
  std::cout << "First axis: " << axis << "\n";
  sce >> bxis;
  sce.ignore (256, '\n');
  std::cout << "Second axis: " << bxis << "\n";
  sce >> cxis;
  sce.ignore (256, '\n');
  std::cout <<   "Third axis length: " << cxis << "\n";
  Ellipsoid ella (centre, axis, bxis, cxis);
  Vec3 u, v;
  if (ella.solve (centre, ez, u, v) != 2) {
    std::cout << "No depth to first ellipsoid!!" << std::endl;
  } else {
    std::cout << "z limits along line of sight through centre: " << u.dot (ez)
	      << ", " << v.dot (ez) << std::endl;
  }

  // Second ellipsoid
  sce >> centre;
  sce.ignore (256, '\n');
  std::cout << "Second ellipsoid centre: " << centre << "\n";
  sce >> axis;
  sce.ignore (256, '\n');
  std::cout << "First axis: " << axis << "\n";
  sce >> bxis;
  sce.ignore (256, '\n');
  std::cout << "Second axis: " << bxis << "\n";
  sce >> cxis;
  sce.ignore (256, '\n');
  std::cout << "Third axis length: " << cxis << "\n";
  Ellipsoid ellb (centre, axis, bxis, cxis);
  if (ellb.solve (centre, ez, u, v) != 2) {
    std::cout << "No depth to second ellipsoid!!" << std::endl;
  } else {
    std::cout << "z limits along line of sight through centre: " << u.dot (ez)
	      << ", " << v.dot (ez) << std::endl;
  }

  double acore, beta;
  sce >> acore >> beta;
  sce.ignore (256, '\n');
  std::cout << "acore and beta: " << acore << ", " << beta << std::endl;

  return *(new Cavmod (acore, beta, ella, ellb));
}




// Simple tests for Vec3 and Ellipsoid
int ellip_main (int argc, char **argv) {
  double ac[] = {1.0, 0.0, 0.0};
  Vec3 a (ac);
  std::cout << "a = " << a << std::endl;
  double bc[] = {0.0, 1.0, 0.0};
  Vec3 b (bc);
  std::cout << "b = " << b << std::endl;
  double cc[] = {0.0, 0.0, 1.0};
  Vec3 c (cc);
  std::cout << "c = " << c << std::endl;

  std::cout << "a cross b = " << a.cross (b) << std::endl;
  std::cout << "b cross a = " << b.cross (a) << std::endl;
  std::cout << "b cross c = " << b.cross (c) << std::endl;
  std::cout << "c cross b = " << c.cross (b) << std::endl;
  std::cout << "c cross a = " << c.cross (a) << std::endl;
  std::cout << "a cross c = " << a.cross (c) << std::endl;

  double centre[] = {3.0, 5.0, 7.0};
  Vec3 pos (centre);
  Vec3 axis = (a + b) * 2.0;
  Vec3 bxis = (b - a) * 3.0;
  double cxis = 5.0;
  Ellipsoid ep (pos, axis, bxis, cxis);
  ep.dumpq ();

  Vec3 u, v;
  int nsol = ep.solve (pos, bxis, u, v);
  std::cout << "nsol: " << nsol << std::endl;
  if (nsol > 0) {
    std::cout << "First solution: " << u << std::endl;
    if (nsol > 1) {
      std::cout << "Second solution: " << v << std::endl;
    }
  }
  return 0;
}


void ellcav_usage (char **argv) {
  std::cerr << "Usage: " << argv[0] << " <model description> <output file>\n";
  exit (EXIT_FAILURE);
}

int main (int argc, char **argv) {
  if (argc != 3)
    ellcav_usage (argv);

  Cavmod& cmod = mkmodel (argv[1]);

  double xmin, xmax, ymin, ymax;
  std::cout << "Range of x and y to image (xmin, ymin, xmax)?\n";
  std::cin >> xmin >> ymin >> xmax;
  ymax = ymin + (xmax - xmin);
  int nside;
  std::cout << "Number of samples on a side? ";
  std::cin >> nside;
  double width = (xmax - xmin) / nside;

  std::ofstream dest (argv[2]);
  for (int j = 0; j < nside; ++j) {
    double y = ymin + j * (ymax - ymin) / nside;
    for (int i = 0; i < nside; ++i) {
      double x = xmin + i * (xmax - xmin) / nside;
      dest << cmod.pixsb (x, y, width, 4) << " ";
    }
    dest << "\n";
  }

  return 0;
}
