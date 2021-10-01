;+
; NAME:
;	recursigrate
; PURPOSE:
;	Perform multivariate integration through recursive invocation
;	of IDL's univariate Romberg integrators.
; CATEGORY:
;	Mathematics
; CALLING SEQUENCE:
;	recursigrate(Func, Domain, [eps=]   to be integrated.  this function must accept a
;	Func - A scalar string specifying the name of a user-supplied
;	IDL function to be integrated.  this function must accept a
;	single vector argument x and return a scalar result.  it must
;	be defined over the closed intervals [ai, Bi] contained in the
;	domain specified by parameter Domain.
;	Domain - N by 2 array of intervals defining the integration domain
; OPTIONAL INPUTS:
;	none   Bi] contained in the
; KEYWORD PARAMETERS:
;	eps - The desired fractional accuracy. For single-precision
;	calculations the default value is 1E-6
; OUTPUTS:
;	The result will be a double scalar number containing the
;	result of the integration.
; OPTIONAL OUTPUTS:
;	none
; COMMON BLOCKS:
;	params
; SIDE EFFECTS:
;	none
; RESTRICTIONS:
;	none
; PROCEDURE:
;	none
; EXAMPLE:
;	To integrate the unit_sphere function over the domain [[-1.5,-1.5],[1.5,1.5]]
;	and print the result:
;		print, recursigrate('unit_sphere', [[-1.5,-1.5],[1.5,1.5]])
; MODIFICATION HISTORY:
;	$Log: dav_recursigrate.pro,v $
;	Revision 1.1.1.1  2008-05-13 19:04:42  cavagnolo
;
;
;	Revision 1.3  2008/02/24 20:06:50  ventimig
;	Added documentation headers to all of the files.
;	   @Revision@
;	Revision 1.2  2008/02/24 18:10:26  ventimig
;	Adding lowercase versions of programs.
;	
;	Revision 1.1.1.1  2008/02/20 03:56:08  ventimig
;	Survey Properties
;	
;	Revision 1.5  2008/02/11 16:56:46  ventimig
;	Had to move the variables from the params common block up higher in
;	the file, in order to avoid a compilation error.
;
;	Revision 1.4  2008/02/11 16:54:19  ventimig
;	Moved the qrom_dispatch function inside of the dav_recursigrate file,
;	and also added the dav_ namespace characters to its name.
;
;	Revision 1.3  2008/02/11 16:49:58  ventimig
;	dav_
;
;	Revision 1.2  2008/02/11 16:21:27  ventimig
;	Moved the recursigrand function inside of the recursigrate file, since
;	the function corresponding to the latter depends on the former.
;
;	Revision 1.1  2008/02/11 16:18:48  ventimig
;	Initial revision
;
;	Revision 1.3  2007/12/20 19:24:15  ventimig
;	Changed the RCS tag in the comment block from a Revision tag, to a Log
;	tag.
;
;-
function dav_qrom_dispatch, f, a, b, eps=eps
  case finite(a) + finite(b) of
     2 : val = qromb(f, a, b, eps=eps)
     1 : val = qromo(f, finite(a) ? a : -1D99, finite(b) ? b : 1D99, eps=eps, /midinf)
     0 : val = qromo(f, -1D99, 0, eps=eps, /midinf) + $
               qromo(f, 0, 1D99, eps=eps, /midinf)
  endcase
  return, val
end

function dav_recursigrand, x_k
  common params, f, D, i, eps, x, k, N
  x[k] = x_k
  if k lt N - 1 then begin
     k = k + 1
     val = dav_qrom_dispatch('dav_recursigrand', D[k, 0], D[k, 1], eps=eps)
     k = k - 1
     return, val
  endif
  i++
  return, call_function(f, x)
end

function dav_recursigrate, Func, Domain, eps=epsilon
  forward_function dav_qrom_dispatch, dav_recursigrate
  common params
  if(n_elements(Domain[*,0]) gt 8) then begin
     print, 'Integration domains of dimensionality > 8 should be integrated using MCMC.'
     return, 0
  endif
  eps = n_elements(epsilon) eq 0 ? 1E-6 : epsilon
  f = Func
  D = Domain
  N = n_elements(D[*, 0])
  x = dblarr(N)
  k = 0
  i = 0L
  except = !except
  !except = 0
  val = dav_qrom_dispatch('dav_recursigrand', D[0,0], D[0,1], eps=eps)
  discard = check_math()
  !except = except
  return, val
end
