# This file was automatically generated by SWIG (http://www.swig.org).
# Version 1.3.40
#
# Do not make changes to this file unless you know what you are doing--modify
# the SWIG interface file instead.
# This file is compatible with both classic and new-style classes.

from sys import version_info
if version_info >= (2,6,0):
    def swig_import_helper():
        from os.path import dirname
        import imp
        fp = None
        try:
            fp, pathname, description = imp.find_module('__sphere', [dirname(__file__)])
        except ImportError:
            import __sphere
            return __sphere
        if fp is not None:
            try:
                _mod = imp.load_module('__sphere', fp, pathname, description)
            finally:
                fp.close()
            return _mod
    __sphere = swig_import_helper()
    del swig_import_helper
else:
    import __sphere
del version_info
try:
    _swig_property = property
except NameError:
    pass # Python < 2.2 doesn't have 'property'.
def _swig_setattr_nondynamic(self,class_type,name,value,static=1):
    if (name == "thisown"): return self.this.own(value)
    if (name == "this"):
        if type(value).__name__ == 'SwigPyObject':
            self.__dict__[name] = value
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    if (not static) or hasattr(self,name):
        self.__dict__[name] = value
    else:
        raise AttributeError("You cannot add attributes to %s" % self)

def _swig_setattr(self,class_type,name,value):
    return _swig_setattr_nondynamic(self,class_type,name,value,0)

def _swig_getattr(self,class_type,name):
    if (name == "thisown"): return self.this.own()
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError(name)

def _swig_repr(self):
    try: strthis = "proxy of " + self.this.__repr__()
    except: strthis = ""
    return "<%s.%s; %s >" % (self.__class__.__module__, self.__class__.__name__, strthis,)

try:
    _object = object
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0



def hmsdms_to_degdeg(*args):
  return __sphere.hmsdms_to_degdeg(*args)
hmsdms_to_degdeg = __sphere.hmsdms_to_degdeg

def degdeg_to_hmsdms(*args):
  return __sphere.degdeg_to_hmsdms(*args)
degdeg_to_hmsdms = __sphere.degdeg_to_hmsdms

def degdeg_to_dmsdms(*args):
  return __sphere.degdeg_to_dmsdms(*args)
degdeg_to_dmsdms = __sphere.degdeg_to_dmsdms

def calculate_angular_separation(*args):
  return __sphere.calculate_angular_separation(*args)
calculate_angular_separation = __sphere.calculate_angular_separation

def calculate_offset_position(*args):
  return __sphere.calculate_offset_position(*args)
calculate_offset_position = __sphere.calculate_offset_position

def xyz_to_llr(*args):
  return __sphere.xyz_to_llr(*args)
xyz_to_llr = __sphere.xyz_to_llr

def xyz_to_geo_llh(*args):
  return __sphere.xyz_to_geo_llh(*args)
xyz_to_geo_llh = __sphere.xyz_to_geo_llh

def geo_llh_to_xyz(*args):
  return __sphere.geo_llh_to_xyz(*args)
geo_llh_to_xyz = __sphere.geo_llh_to_xyz

def calculate_hour_angles_at_elevation_limit(*args):
  return __sphere.calculate_hour_angles_at_elevation_limit(*args)
calculate_hour_angles_at_elevation_limit = __sphere.calculate_hour_angles_at_elevation_limit

def time_to_dhms(*args):
  return __sphere.time_to_dhms(*args)
time_to_dhms = __sphere.time_to_dhms

def calculate_local_sky_position(*args):
  return __sphere.calculate_local_sky_position(*args)
calculate_local_sky_position = __sphere.calculate_local_sky_position

def calculate_pierce_point(*args):
  return __sphere.calculate_pierce_point(*args)
calculate_pierce_point = __sphere.calculate_pierce_point


