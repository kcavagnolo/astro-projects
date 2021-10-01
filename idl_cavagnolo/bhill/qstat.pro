PRO QSTAT, Data, Mean, Sig, Npix, Pc50, $
           NOPRINT=noprint, NF=nf, TEXTOUT=textout, DESCRIP=descr
;+
; NAME:
;   qstat
;
; PURPOSE:
;   Quick statistics on an array.
;
; MAJOR TOPICS:
;   Statistics.
;
; CALLING SEQUENCE:
;   qstat, data, mean, sig, npix, pc50
;
; INPUT PARAMETERS:
;   data        Array of numbers to be characterized.
;
; INPUT KEYWORDS:
;   noprint     Flag.  Omit printing if set.
;   nf          Numerical format.
;   textout     The usual GSFC TEXTOUT parameter.
;   descrip     Descriptive line for hardcopy.
;                       
; OUTPUT PARAMETERS:
;   mean        Mean of data array.
;   pc50        Median of data array.
;   sig       Standard deviation (scatter) of data array.
;   npix        Number of elements in data array.
;
; HISTORY:  11 Dec. 1996 - Written.  RSH, HSTX
;            7 Feb. 1997 - Help added.  RSH 
;           19 Aug. 1998 - Min and max printed.  RSH
;           28 Aug. 1998 - Robust stats added.  RSH
;           21 Sep. 1998 - Robust keyword.  RSH
;           22 Sep. 1998 - Forward_function decl.  RSH
;           28 Oct. 1998 - Finite numbers only.  RSH
;            2 Nov. 1998 - Robust stuff deleted, improved display.  RSH
;            5 Aug. 1999 - TEXTOUT copied from imlist.pro.  RSH
;           28 June 2000 - IDL V5 and idlastro standards.  RSH
;           20 Sep. 2000 - More readable format.  RSH
;           16 Nov. 2000 - Eliminated redundant printing of mean.  RSH
;-

on_error, 2

IF n_params(0) LT 1 THEN BEGIN
   print,'CALLING SEQUENCE:  qstat, data, mean, sig, npix, pc50'
   print,'KEYWORD PARAMETER:  noprint, nf, textout'
   RETURN
ENDIF

IF n_elements(nf) LT 1 THEN nf='(G9.3)'
fw = strlen(string(1.0,format=nf))
bla = string(replicate(32b,fw))
minl = strmid('Min'+bla, 0, fw)
maxl = strmid('Max'+bla, 0, fw)
meanl = strmid('Mean'+bla, 0, fw)
medl = strmid('Median'+bla, 0, fw)
sigl = strmid('Sigma'+bla, 0, fw)
mm1sl = strmid('Mean-sig'+bla, 0, fw)
mp1sl = strmid('Mean+sig'+bla, 0, fw)
mm2sl = strmid('Mean-2sig'+bla, 0, fw)
mp2sl = strmid('Mean+2sig'+bla, 0, fw)

IF NOT keyword_set(textout) THEN textout=!TEXTOUT

IF datatype( TEXTOUT ) NE 'STR' THEN BEGIN
    textout = textout > 2                  ;Don't use /MORE
    IF (textout GE 3) and (textout NE 5) THEN hardcopy = 1 ELSE hardcopy = 0
ENDIF ELSE hardcopy = 1

textopen, 'QSTAT', TEXTOUT = textout, /STDOUT  

IF hardcopy THEN BEGIN   ;Direct output to a disk file
    printf,!TEXTUNIT,'QSTAT: ' + systime(0)
    IF NOT keyword_set( DESCR ) THEN BEGIN
       descr = ''
       read,'Enter a brief description to be written to disk: ',descr
    ENDIF
    printf,!TEXTUNIT,descr
    printf,!TEXTUNIT,' '
ENDIF  

npix  = n_elements(data)

w = where(finite(data), nfin)

IF nfin LE 0 THEN BEGIN
    printf, !TEXTUNIT, 'No finite numbers in data array.'
    textclose,textout=textout
    RETURN
ENDIF

dataw = data[w]

mean  = total(dataw, /double) / npix
sig   = sqrt(total((dataw - mean)^2, /double) / (npix-1))
pc50  = median(dataw, /even)
sep   = string(replicate(32b,6))

IF NOT keyword_set(noprint) THEN BEGIN
    printf, !TEXTUNIT, ' '
    printf, !TEXTUNIT, 'N finite = ',strn(nfin), $
           '                        (N total = ', strn(npix), ')'
    printf, !TEXTUNIT,' '
    printf, !TEXTUNIT, bla, sep, minl, sep, medl, sep, maxl
    printf, !TEXTUNIT, bla, sep, string(min(dataw),form=nf), sep, $
        string(pc50,form=nf), sep, string(max(dataw),form=nf)
    printf, !TEXTUNIT, ' '
    printf, !TEXTUNIT, bla, sep, bla, sep, sigl
    printf, !TEXTUNIT, bla, sep, bla, sep, string(sig,form=nf)
    printf, !TEXTUNIT, ' '
    printf, !TEXTUNIT, mm2sl, sep, mm1sl, sep,  meanl, $
        sep, mp1sl, sep, mp2sl
    printf, !TEXTUNIT, string(mean-2*sig,format=nf), $
                       sep, string(mean-sig,format=nf), $
                       sep, string(mean,format=nf), $
                       sep, string(mean+sig,format=nf), $
                       sep, string(mean+2*sig,format=nf)
    dataw = (dataw-mean)/sig
    w = where(dataw LT -2, cm2)
    w = where(dataw GE -2 and dataw LT -1, cm12)
    w = where(dataw GE -1 and dataw LE  0, cm01)
    w = where(dataw GT 0 and dataw LE 1, c01)
    w = where(dataw GT 1 and dataw LT 2, c12)
    w = where(dataw GT 2, c2)
    printf, !TEXTUNIT,' '
    printf, !TEXTUNIT,'Histogram in standard deviations:'
    printf, !TEXTUNIT,' '
    printf, !TEXTUNIT,'        s<-2    -2<=s<-1    -1<=s<=0      ' $
        + '0<s<=1      1<s<=2         s>2'
    printf, !TEXTUNIT,'        ----    --------    --------      ' $
        + '------      ------         ---'
    printf, !TEXTUNIT, cm2, cm12, cm01, c01, c12, c2,form='(6I12)'
    printf, !TEXTUNIT,' '
ENDIF

textclose,textout=textout

RETURN

END

