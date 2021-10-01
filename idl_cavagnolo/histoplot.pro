pro HISTOPLOT, data, bs, bw, cum=cum, invcum=invcum, min=min, max=max, log=log, $
               overplot=overplot, fraction=fraction, linestyle=linestyle, $
               xrange=xrange, title=title, xtitle=xtitle, ytitle=ytitle, $
               xstyle=xstyle, ystyle=ystyle, errplot=errplot, $
               color=color, psym=psym, s25=s25, ysf=ysf, verbose=verbose, $
               filename=filename, colorfill=colorfill, fillcolor=fillcolor, $
               drawlines=drawlines, bins=bins, omin=omin, omax=omax, _extra=_extra

;+
; NAME:
;	HISTOPLOT
;
; PURPOSE:
;	Histogram Plotting Routine
;
; INPUTS:
;	DATA	data to be plotted
;	BS	data bin separation
;	BW	data bin width (OPTIONAL, by default BW = BS and bins are disjoint)
;
; KEYWORDS:
;	CUM	Cumulative histogram (sums over increasing data values)
;	INVCUM	Inverse cumulative histogram (sums over decreasing data values)
;	MIN	Minimum value of data (or of alog10(data) IF LOG is set) to be plotted
;	MAX	Maximum value of data (or of alog10(data) IF LOG is set) to be plotted
;	LOG	Logarithmically bin data (IF set, BS and XRANGE refer to the log-value)
;	OVERPLOT	Overplot
;	FRACTION	Plot fractional  histogram values rather than numerical ones
;	LINESTYLE	Passed to PLOT, defaults to 2
;	XRANGE	Passed to PLOT, defaults to [min,max]
;	TITLE	Passed to PLOT, defaults to ''
;	XTITLE	Passed to PLOT, defaults to ''
;	YTITLE	Passed to PLOT, defaults to ''
;	ERRPLOT	Plot +- SQRT(N) errors "around" histogram values
;	COLOR	Passed to PLOT, defaults to 255
;	PSYM	Passed to PLOT, defaults to 10 (i.e. histogram)
;	S25	Plot histogram values / binsize * bincenter^2.5 (e.g. for astronomical source counts)
;	YSF	Y-axis Scale Factor (histogram values are multiplied by YSF)
;	VERBOSE	Print histogram values (and also values +- sqrt(values) IF ERRPLOT is set) to terminal
;	FILENAME	Print histogram values (and also values +- sqrt(values) IF ERRPLOT is set) to FILENAME
;	_EXTRA	Extra keywords (passed to PLOT)
;
; NOTES:
;	MIN : When MIN = 0 is specIFied in calling the procedure things can get messy.
;	Try and use a small MIN, instead!
;
;	COLOR : When graphical output is being sent to the PS device, one must
;	set COLOR = 254 (i.e. black in the CURRENT version of COLOURS) in order
;	FOR B&W effects to work properly.
;
; MODIFICATION HISTORY:
;	01 May 2001	Written by Mattia Vaccari
;
;	09 Oct 2003	Miscellaneous modIFications: in particular, now handles
;			logarithmically-binned histograms
;	14 Nov 2003	Minor debugging
;	04 Dec 2003	FRACTION keyword added and MIN/MAX debugging
;	19 Jan 2004	COLOR keyword added and LINESTYLE debugging
;	20 Jan 2004	COLOR keyword debugging
;	16 Nov 2004	Array containing histogram values is now zero-valued at
;			extremes, and thus looks better in plot
;	29 Aug 2005	MIN and MAX keywords behaviour and other debugging
;	16 Jan 2006	S25 and YSF keywords added
;	19 Jan 2006	VERBOSE keyword added and LOG/MIN/MAX debugging
;	21 Jan 2006	INVCUM and PSYM keywords added
;	24 Jan 2006	XSTYLE, YSTYLE and ERRPLOT keywords added
;	06 Mar 2006	S25 debugging (Finally Correct!?)
;	14 Mar 2006	Contents of Output (to Screen & File) changed
;	31 Mar 2006	Error bars plotted the same color of data points
;	02 Apr 2006	Debugging of terminal output toggled by
;	VERBBOSE keyword
;       30 Nov 2007     KWC added colors and lines options
;-

ON_ERROR,2

IF NOT(keyword_set(bw)) THEN bw = bs

imin = where(data EQ min(data))
auxmin = min(data)

IF NOT keyword_set(log) THEN $
  data[imin[0]] = floor(min(data)/bs)*bs ELSE $
  data[imin[0]] = 10.^(floor(min(alog10(data))/bs)*bs)

IF NOT keyword_set(min) THEN IF keyword_set(log) THEN min = min(alog10(data)) ELSE min = min(data)
IF NOT keyword_set(max) THEN IF keyword_set(log) THEN max = max(alog10(data)) ELSE max = max(data)
IF NOT keyword_set(title) THEN title = ''
IF NOT keyword_set(xtitle) THEN xtitle = ''
IF NOT keyword_set(ytitle) THEN ytitle = ''
IF NOT keyword_set(xstyle) THEN xstyle = 1
IF NOT keyword_set(ystyle) THEN ystyle = 0
IF NOT keyword_set(psym) THEN psym = 10
IF NOT keyword_set(overplot) THEN IF NOT keyword_set(linestyle) THEN linestyle = 2 ELSE $
                                  IF NOT keyword_set(linestyle) THEN linestyle = 0
IF keyword_set(log) THEN $
  binned_data = histogram(alog10(data),binsize = bs,min = min,max = max,locations=loci) ELSE $
  binned_data = histogram(data,binsize = bs,min = min,max = max,locations=loci)
IF keyword_set(cum) THEN FOR i = 1,n_elements(binned_data)-1 DO $
  binned_data[i] = total(binned_data[i-1:i])
IF keyword_set(invcum) THEN FOR i = 1,n_elements(binned_data)-1 DO $
  binned_data[n_elements(binned_data)-1-i] = total(binned_data[n_elements(binned_data)-i-1:n_elements(binned_data)-i])

bd = binned_data
bins = n_elements(loci)
omin = min
omax = max

IF keyword_set(errplot) THEN BEGIN
    bdm = binned_data-sqrt(binned_data)
    bdp = binned_data+sqrt(binned_data)
ENDIF

binned_data = [0,binned_data,0]
bd = [0,bd,0]
IF keyword_set(errplot) THEN BEGIN
    bdm = [0,bdm,0]
    bdp = [0,bdp,0]
ENDIF

IF keyword_set(fraction) THEN BEGIN
    binned_data = binned_data/float(n_elements(data))
    IF keyword_set(errplot) THEN BEGIN
        bdm = bdm/float(n_elements(data))
        bdp = bdp/float(n_elements(data))
    ENDIF
ENDIF

binx = (findgen(n_elements(binned_data)-2)+0.5)*bs+min
binx = [min(binx)-bs,binx,max(binx)+bs]

IF NOT keyword_set(xrange) THEN IF keyword_set(log) THEN xrange = 10.^[min,max] ELSE xrange = [min(binx),max(binx)]

IF keyword_set(S25) THEN BEGIN
    IF keyword_set(log) THEN binned_data = binned_data/(10.^(binx+bs/2.)-10.^(binx-bs/2.))*(10.^binx)^2.5 $
    ELSE binned_data = binned_data/bs/binx^2.5
    IF keyword_set(errplot) THEN BEGIN
        IF keyword_set(log) THEN BEGIN
            bdm = bdm/(10.^(binx+bs/2.)-10.^(binx-bs/2.))*(10.^binx)^2.5
            bdp = bdp/(10.^(binx+bs/2.)-10.^(binx-bs/2.))*(10.^binx)^2.5
        ENDIF ELSE BEGIN
            bdm = bdm*binx^2.5
            bdp = bdp*binx^2.5
        ENDELSE
    ENDIF
ENDIF

IF keyword_set(YSF) THEN BEGIN
    binned_data = binned_data*ysf
    IF keyword_set(errplot) THEN BEGIN
        bdm = bdm*ysf
        bdp = bdp*ysf
    ENDIF
ENDIF

IF keyword_set(overplot) THEN BEGIN
    IF keyword_set(log) THEN $
      oplot,10.^binx,binned_data,psym = psym,linestyle = linestyle,color = color ELSE $
      oplot,binx,binned_data,psym = psym,linestyle = linestyle,color = color
ENDIF ELSE BEGIN
    IF keyword_set(log) THEN $
      plot, 10.^binx, binned_data, $
            psym = psym, xrange = xrange,$
            xstyle = xstyle, ystyle = ystyle, $
            /xlog, title = title, $
            xtitle = xtitle, ytitle = ytitle, $
            color = color, _extra = _extra ELSE $
      plot, binx, binned_data, $
            psym = psym, xrange = xrange, $
            xstyle = xstyle, ystyle = ystyle, $
            title = title, xtitle = xtitle, ytitle = ytitle, $
            color = color,_extra = _extra
ENDELSE

IF NOT keyword_set(fillcolor) THEN fillcolor = 255
IF keyword_set(colorfill) THEN BEGIN
    loadct, 39, /silent
    FOR j = 0L,N_Elements(binned_data)-2 DO BEGIN
        xa = binx[j]+0.5*bs
        xb = binx[j]-0.5*bs
        IF keyword_set(log) THEN BEGIN
            xa = 10.^(xa)+10.^(0.5)*bs
            xb = 10.^(xb)-10.^(0.5)*bs
        ENDIF
        ya = !Y.CRange[0]
        yb = binned_data[j]
        px = xrange[0] > [xa, xa, xb, xb, xa] < xrange[1]
        py = min(binned_data) > [ya, yb, yb, ya, ya] < max(binned_data)
        PolyFill, px, py, COLOR = fillcolor, _Extra = extra
    ENDFOR
    IF keyword_set(log) THEN BEGIN
        plot, 10.^binx, binned_data, $
              /nodata, /noerase, psym = psym, xrange = xrange,$
              xstyle = xstyle, ystyle = ystyle, $
              /xlog, title = title, $
              xtitle = xtitle, ytitle = ytitle, $
              color = color, _extra = _extra
        oplot, 10.^binx, binned_data, psym = 10, _Extra = extra
    ENDIF ELSE BEGIN
        plot, binx, binned_data, $
              /nodata, /noerase, psym = psym, xrange = xrange,$
              xstyle = xstyle, ystyle = ystyle, $
              title = title, xtitle = xtitle, ytitle = ytitle, $
              color = color, _extra = _extra
        oplot, binx, binned_data, psym = 10, _Extra = extra
    ENDELSE
ENDIF

IF keyword_set(drawlines) THEN BEGIN
    FOR j = 0L,N_Elements(binned_data)-2 DO BEGIN
        y  = maken(0, binned_data[j], 100)
        IF keyword_set(log) THEN BEGIN
            x1 = replicate(10.^(binx[j]+0.5*bs), 100)
            x2 = replicate(10.^(binx[j]-0.5*bs), 100)
        ENDIF ELSE BEGIN
            x1 = replicate(binx[j]+0.5*bs, 100)
            x2 = replicate(binx[j]-0.5*bs, 100)
        ENDELSE
        IF (x1[0] LT xrange[1] AND x1[0] GT xrange[0]) THEN oplot, x1, y, _Extra = extra
        IF (x2[0] LT xrange[1] AND x2[0] GT xrange[0]) THEN oplot, x2, y, _Extra = extra
    ENDFOR
ENDIF

IF keyword_set(errplot) THEN IF keyword_set(log) THEN $
  errplot,10.^binx,bdm,bdp,color = color ELSE $
  errplot,binx,bdm,bdp,color = color

IF keyword_set(verbose) THEN IF keyword_set(errplot) THEN $
  print,transpose([[binx],[bd],[binned_data],[bdp-binned_data]]) ELSE $
  print,transpose([[binx],[bd],[binned_data]])

IF keyword_set(filename) THEN BEGIN
  openw,lun,filename,/get_lun
  IF keyword_set(errplot) THEN $
    printf,lun,transpose([[binx],[bd],[binned_data],[bdp-binned_data]]),FORmat = '(4f16.7)' ELSE $
    printf,lun,transpose([[binx],[bd],[binned_data]]),FORmat = '(3f16.7)'
  close,lun & free_lun,lun
ENDIF

data[imin[0]] = auxmin

END
