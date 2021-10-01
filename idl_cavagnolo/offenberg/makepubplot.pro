pro MakePubPlot, X, Y, PPlots, Ordinate, Annotation, Postscript=PostScript, $
Window=window,FILENAME=filename,LANDSCAPE = landscape, ENCAP = encap, $
NOCLOSE=noclose, XERROR=xerror, YERROR=yerror
;+
;  PROCEDURE:
;	MakePubPlot
;  PURPOSE:
;	Draw plots that were designed using ApJPLOT to the screen (or a 
;	PostScript file).
;  CALLING SEQUENCE:
;	MakePubPlot, X, Y, F
;	MakePubPlot, X, Y, Plots, Ordinate, Annotation
;	MakePubPlot, X, Y, F [,/POSTSCRIPT,/LANDSCAPE, FILE=filename, $
;			WINDOW=w, XERROR=xerr, YERROR=yerr]
;  INPUTS:
;	X - Data to be plotted along the X-axis.  May be an N element vector
;	    or an NxM element array.
;	Y - Data to be plotted along the Y-axis.  May be an N element vector 
;	    or an NxM element array. (If X is a 2-d array, Y must be the same
;	    size as X).
;	F - Result from an execution of PubPlot (SEE pubplot.pro)
;	- or -
;	Plots - Structure used internally by PubPlot containing global plot
;		information
;	Ordinate - Structure used internally by PubPlot containing individual
;		   ordinate information
;	Annotation - Structure used internally by PubPlot containing text
;		     annotation information.
;  KEYWORD INPUTS:
;       XERROR - Uncertainty in X coordinate for error bars (if user-defined 
;		 error bars are specified).  Must be a vector of the same size
;		 as X.
;       YERROR - Uncertainty in Y coordinate for error bars (if user-defined 
;		 error bars are specified).  Must be an array of the same size
;	 	 as Y (i.e. NxM elements).
;	POSTSCRIPT - If present and non-zero, open a Postscript file and put
;		     plot there.
;	LANDSCAPE - If present and non-zero, PostScript file will be in 
;		    Landscape rather than Portrait mode.  Ignored if the
;		    keyword POSTSCRIPT is not specified.
;	ENCAP 	 -  If present and non-zero, an Encapsulated PostScript file
;		    will be created.  Ignored if POSTSCRIPT keyword is not
;		    specified.
;	FILENAME -  If present, the string included will be used as the name
;		    for the PostScript output file (idl.ps if not present).
;		    Ignored if the keyword POSTSCRIPT is not specified.
;	WINDOW -    If present, uses this window rather than the current 
;		    window.
;		
;  OUTPUTS:
;	Plot is generated to the current window or a PostScript file.
;  SIDE EFFECTS:
;	The current window is erased and the plot is put there unless WINDOW
;	keyword is specified.
;	If no window is present at the start of the routine, one will be 
;	created.
;  RESTRICTIONS:
;	Must be on a windowing system.  
;	The inputs {X, Y, F} or {X, Y, Plots, Ordinate, Annotation} must be
;	supplied.
;  COMMON BLOCKS:
;	None.
;  HISTORY:
;	Written by J.D. Offenberg, Hughes STX, Feb 3, 1993
;	Support for Histogram plotting type added.  JDO, Mar 11, 1993
;	Support for error bars, multiple X vectors added.  JDO, Mar 23, 1993
;	Support for plotdate added.  JDO, 26 Mar, 1993
;	Support for legends added.  Name changed from ApJMakePlot to 
;		MakePubPlot  JDO, April 1993
;	Encapsulated PostScript added.  JDO, March 1994
;	Bug in centering of PostScript files fixed.  JDO, November 1994
;-

IF keyword_set(window) and not(keyword_set(Postscript)) THEN $
	window, window, XSIZE=640, YSIZE=640 

tagnames = tag_names(PPlots)
IF (total(tagnames eq "PLOTS") ne 0) THEN BEGIN	;Is there a PPlots.Plots?
	Annotation = PPlots.Annotation		;Then PPLOTS is complete 
	Ordinate = PPlots.Ordinate		;structure...extract fields
	Plots = PPlots.Plots			
endIF ElSE Plots = PPlots

IF keyword_set(Postscript) then BEGIN
	set_plot,'PS'

	ztest = (DATATYPE(filename) eq "STR")*4 + keyword_set(LANDSCAPE)*2 + $
		 keyword_set(ENCAP)

	CASE ztest of
	7:	device,filename=filename,/LANDSCAPE,/ENCAP, $
          xsize=Plots.PSSize(0), ysize=Plots.PSSize(1),     $ 
          inches=(Plots.PSUnits eq 0) 
        6:	device,filename=filename,/LANDSCAPE,ENCAP=0, xsize=9.5, $
          ysize=7, /inches
        5:	device,filename=filename,/ENCAP,        $
          xsize=Plots.PSSize(0), ysize=Plots.PSSize(1), $
          inches=(Plots.PSUnits eq 0)
	4:	device,filename=filename,ENCAP=0, xsize=7, ysize=5, /inches
	3:	device,filename='idl.ps',/LANDSCAPE,/ENCAP, $
          xsize=Plots.PSSize(0), ysize=Plots.PSSize(1),     $
          inches=(Plots.PSUnits eq 0)
        2:	device,filename='idl.ps',/LANDSCAPE,ENCAP=0, xsize=9.5, $
          ysize=7, /inches
	1:	device,filename='idl.ps',/ENCAP,        $
          xsize=Plots.PSSize(0), ysize=Plots.PSSize(1), $
          inches=(Plots.PSUnits eq 0)
	0:	device,filename='idl.ps',ENCAP=0, xsize=7, ysize=5, /inches
	ELSE:	BEGIN
			message,/inf,'ERROR--should not reach this point.'
			return
		END
	endCASE
endIF
szy = size(y)
IF (szy(0) eq 2) THEN OrdNum = szy(2) else Ordnum = 1

PSYM = Ordinate.PSYM
Linestyle=Ordinate.LineStyle

IF (PSYM(0) gt 8) THEN BEGIN
	MakeSym, PSYM(0)
	PSYM(0) = 8
endIF else if (PSYM(0) eq 8) THEN PSYM(0) = 10

IF ((LineStyle(0) gt -1) AND (PSYM(0) ne 10)) THEN PSYM(0) = -PSYM(0)

IF (Plots.Type AND 2 ne 0) THEN BEGIN 	;Can't allow negative XRanges
	IF (Plots.XRange(0) le 0) THEN Plots.XRange(0) = 10^Plots.XRange(0)
	IF (Plots.XRange(1) le 0) THEN Plots.XRange(1) = 10^Plots.XRange(1)
endIF
IF (Plots.Type AND 1 ne 0) THEN BEGIN	;Can't allow negative YRanges
	IF (Plots.YRange(0) le 0) THEN Plots.YRange(0) = 10^Plots.YRange(0)
	IF (Plots.YRange(1) le 0) THEN Plots.YRange(1) = 10^Plots.YRange(1)
endIF

Plot, X(*,0), Y(*,0), XRANGE = Plots.Xrange, Yrange = Plots.Yrange, $
	TITLE=Plots.Title, XTITLE=Plots.Xtitle, YTITLE=Plots.Ytitle, $
	PSYM=PSYM(0), SYMSIZE=Ordinate(0).symsize, THICK = Ordinate(0).Thick, $
	LINESTYLE=LineStyle(0), XSTYLE = Plots.Xstyle, YSTYLE = Plots.Ystyle, $
	XTYPE = ((Plots.Type AND 2) ne 0), YTYPE = ((Plots.Type AND 1) ne 0), $
	CHARSIZE = Plots.CharSize, CHARTHICK = Plots.CharThick


;Draw X error bars, if needed.
IF (Ordinate(0).XErrBType ne 0) THEN BEGIN
    	CASE Ordinate(0).XErrBType OF
	    1:	Xerr = replicate(Ordinate(0).XErrNum, n_elements(X(*,0)))
	    2:  Xerr = X(*,0) * Ordinate(0).XErrNum
	    3:  Xerr = sqrt(X(*,0)) * Ordinate(0).XErrNum
	    4:	IF (keyword_set(XERROR)) THEN Xerr = Xerror(*,0) $
		ELSE Xerr = intarr(n_elements(X(*,0)))
	 ELSE:	Xerr = intarr(n_elements(X(*,0)))
   	endCASE
	FOR i=0, n_elements(x(*,0))-1 DO $
	    OPlot, [X(i,0)-Xerr(i),X(i,0)+Xerr(i)],[Y(i,0),Y(i,0)]

;Draw X error hats, if needed.
	IF (Ordinate(0).XHat ne 0) THEN BEGIN
	    V = convert_coord(X(*,0)-Xerr,Y(*,0),/DATA, /TO_DEVICE)
	    V2 = convert_coord(V(0,*),V(1,*) - $
	               	Ordinate(0).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    V3 = convert_coord(V(0,*),V(1,*) + $
		       	Ordinate(0).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    FOR i=0, n_elements(X(*,0))-1 DO $
		OPlot, [V2(0,i),V3(0,i)],[V2(1,i),V3(1,i)], $
			THICK = Ordinate(0).XHatThick
	    W = convert_coord(X(*,0)+Xerr,Y(*,0),/DATA, /TO_DEVICE)
	    W2 = convert_coord(W(0,*),W(1,*) - $
		       	Ordinate(0).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    W3 = convert_coord(W(0,*),W(1,*) + $
		   	Ordinate(0).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    FOR i=0, n_elements(X(*,0))-1 DO $
		OPlot, [W2(0,i),W3(0,i)],[W2(1,i),W3(1,i)], $
			THICK = Ordinate(0).XHatThick
	endIF
endIF

;Draw Y error bars, if needed.
IF (Ordinate(0).ErrBType ne 0) THEN BEGIN
    	CASE Ordinate(0).ErrBType OF
	    1:	Yerr = replicate(Ordinate(0).ErrNum, n_elements(Y(*,0)))
	    2:  Yerr = Y(*,0) * Ordinate(0).ErrNum
	    3:  Yerr = sqrt(Y(*,0)) * Ordinate(0).ErrNum
	    4:	IF (keyword_set(YERROR)) THEN Yerr = Yerror(*,0) $
		ELSE Yerr = intarr(n_elements(Y(*,0)))
	 ELSE:	Yerr = intarr(n_elements(Y(*,0)))
   	endCASE
	FOR i=0, n_elements(Y(*,0))-1 DO $
	    OPlot, [X(i,0),X(i,0)], [Y(i,0)-Yerr(i),Y(i,0)+Yerr(i)]

;Draw Y error hats, if needed.
	IF (Ordinate(0).Hat ne 0) THEN BEGIN
	    V = convert_coord(X(*,0), Y(*,0)-Yerr, /DATA, /TO_DEVICE)
	    V2 = convert_coord(V(0,*) - Ordinate(0).HatLength*!D.X_Size/1000., $
	               	V(1,*),/DEVICE,/TO_DATA)
	    V3 = convert_coord(V(0,*) + Ordinate(0).HatLength*!D.X_Size/1000., $
			V(1,*), /DEVICE,/TO_DATA)
	    FOR i=0, n_elements(Y(*,0))-1 DO $
		OPlot, [V2(0,i),V3(0,i)],[V2(1,i),V3(1,i)], $
			THICK = Ordinate(0).HatThick
	    W = convert_coord(X(*,0), Y(*,0)+Yerr, /DATA, /TO_DEVICE)
	    W2 = convert_coord(W(0,*) - Ordinate(0).HatLength*!D.X_Size/1000., $
				W(1,*), /DEVICE,/TO_DATA)
	    W3 = convert_coord(W(0,*) + Ordinate(0).HatLength*!D.X_Size/1000., $
			W(1,*),/DEVICE,/TO_DATA)
	    FOR i=0, n_elements(Y(*,0))-1 DO $
		OPlot, [W2(0,i),W3(0,i)],[W2(1,i),W3(1,i)], $
			THICK = Ordinate(0).HatThick
	endIF
endIF

IF (OrdNum gt 1) THEN BEGIN
	FOR I = 1, Ordnum-1 do BEGIN
		IF ((PSYM(I) ne 0) or (LineStyle(I) ne -1)) then BEGIN
		    IF (PSYM(I) gt 8) THEN BEGIN
			    MakeSym, PSYM(I)
			    PSYM(I) = 8
		    endIF ELSE IF (PSYM(I) eq 8) THEN PSYM(I) = 10
		    IF ((LineStyle(I) gt -1) AND (PSYM(I) NE 10)) then $
			PSYM(I) = -PSYM(I)

		    Oplot, X(*,I), Y(*,I), PSYM=PSYM(I), $
			LINESTYLE=LineStyle(I), SYMSIZE = Ordinate(i).SymSize,$
			THICK = Ordinate(i).Thick
		endIF

;Draw X error bars, if needed.
    IF (Ordinate(i).XErrBType ne 0) THEN BEGIN
    	CASE Ordinate(i).XErrBType OF
	    1:	Xerr = replicate(Ordinate(0).XErrNum, n_elements(X(*,i)))
	    2:  Xerr = X(*,i) * Ordinate(i).XErrNum
	    3:  Xerr = sqrt(X(*,i)) * Ordinate(i).XErrNum
	    4:	IF (keyword_set(XERROR)) THEN Xerr = Xerror(*,i) $
		ELSE Xerr = intarr(n_elements(X(*,i)))
	 ELSE:	Xerr = intarr(n_elements(X(*,i)))
   	endCASE
	FOR j=0, n_elements(x(*,i))-1 DO $
	    OPlot, [X(j,i)-Xerr(j),X(j,i)+Xerr(j)],[Y(j,i),Y(j,i)]

;Draw X error hats, if needed.
	IF (Ordinate(i).XHat ne 0) THEN BEGIN
	    V = convert_coord(X(*,i)-Xerr,Y(*,i),/DATA, /TO_DEVICE)
	    V2 = convert_coord(V(0,*),V(1,*) - $
	              Ordinate(i).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    V3 = convert_coord(V(0,*),V(1,*) + $
		      Ordinate(i).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    FOR j=0, n_elements(X(*,i))-1 DO $
		OPlot, [V2(0,j),V3(0,j)],[V2(1,j),V3(1,j)], $
			THICK = Ordinate(i).XHatThick
	    W = convert_coord(X(*,i)+Xerr,Y(*,i),/DATA, /TO_DEVICE)
	    W2 = convert_coord(W(0,*),W(1,*) - $
		      Ordinate(i).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    W3 = convert_coord(W(0,*),W(1,*) + $
		      Ordinate(i).XHatLength*!D.Y_Size/1000.,/DEVICE,/TO_DATA)
	    FOR j=0, n_elements(X(*,i))-1 DO $
		OPlot, [W2(0,j),W3(0,j)],[W2(1,j),W3(1,j)], $
			THICK = Ordinate(i).XHatThick
	endIF
    endIF

;Draw Y error bars, if needed.
    IF (Ordinate(i).ErrBType ne 0) THEN BEGIN
    	CASE Ordinate(i).ErrBType OF
	    1:	Yerr = replicate(Ordinate(i).ErrNum, n_elements(Y(*,i)))
	    2:  Yerr = Y(*,i) * Ordinate(i).ErrNum
	    3:  Yerr = sqrt(Y(*,i)) * Ordinate(i).ErrNum
	    4:	IF (keyword_set(YERROR)) THEN Yerr = Yerror(*,i) $
		ELSE Yerr = intarr(n_elements(Y(*,i)))
	 ELSE:	Yerr = intarr(n_elements(Y(*,i)))
   	endCASE
	FOR j=0, n_elements(y(*,i))-1 DO $
	    OPlot, [X(j,i),X(j,i)], [Y(j,i)-Yerr(j),Y(j,i)+Yerr(j)]

;Draw Y error hats, if needed.
	IF (Ordinate(i).Hat ne 0) THEN BEGIN
	    V = convert_coord(X(*,i), Y(*,i)-Yerr, /DATA, /TO_DEVICE)
	    V2 = convert_coord(V(0,*) - Ordinate(i).HatLength*!D.X_Size/1000.,$
				V(1,*), /DEVICE,/TO_DATA)
	    V3 = convert_coord(V(0,*) + Ordinate(i).HatLength*!D.X_Size/1000.,$
				V(1,*), /DEVICE,/TO_DATA)
	    FOR j=0, n_elements(Y(*,i))-1 DO $
		OPlot, [V2(0,j),V3(0,j)],[V2(1,j),V3(1,j)], $
			THICK = Ordinate(i).HatThick
	    W = convert_coord(X(*,i), Y(*,i)+Yerr, /DATA, /TO_DEVICE)
	    W2 = convert_coord(W(0,*) - Ordinate(i).HatLength*!D.X_Size/1000.,$
				 W(1,*), /DEVICE,/TO_DATA)
	    W3 = convert_coord(W(0,*) + Ordinate(i).HatLength*!D.X_Size/1000.,$
				W(1,*), /DEVICE,/TO_DATA)
	    FOR j=0, n_elements(Y(*,i))-1 DO $
		OPlot, [W2(0,j),W3(0,j)],[W2(1,j),W3(1,j)], $
			THICK = Ordinate(1).HatThick
	endIF
    endIF
	
    endFOR

endIF


FOR j = 0, n_elements(Annotation) -2 do BEGIN
	xyouts, Annotation(j).x, Annotation(j).y, Annotation(j).text, /Data, $
		CHARSIZE=Plots.ACharSize * Annotation(J).CharSize, $
		CHARTHICK=Plots.ACharThick * Annotation(J).CharThick, $
		ORIENTATION = Annotation(J).Orientation
endFOR

;If specified, create and plot the legend.

IF ((Plots.LegendOn eq 1) AND (total(Ordinate.LegendItem) gt 0)) THEN BEGIN
	legpsym = [0]
	legline = [0]
	legtxt = ['']
	LegDel = Plots.LegendDelChar
	IF (LegDel eq "") THEN LegDel = " "
	FOR ii = 0, OrdNum-1 DO BEGIN	
		IF (Ordinate(ii).LegendItem) THEN BEGIN
			LegPsym = [LegPsym, Ordinate(ii).PSYM]
			LegLine = [LegLine, Ordinate(ii).LineStyle]
			LegTxt = [Legtxt, Ordinate(ii).Title]
		endIF
	endFOR
	LegTxt = " "+LegDel +" "+ LegTxt(1:*)
	LegPsym = LegPsym(1:*)
	LegLine = LegLine(1:*)
	xstep = !D.X_CH_SIZE * 2 * Plots.LegendSize
	VV = convert_coord(Plots.LegendX, Plots.LegendY,/DATA,/TO_DEVICE)
	yll = VV(1)
	xll = VV(0)

;If specified, plot the box for the legend.

	IF (Plots.LegendBox) THEN BEGIN
		xlen = (!D.X_CH_SIZE * (max(strlen(LegTxt))) + 3*xstep + 4) * $
			Plots.LegendSize
		ylen = (!D.Y_CH_SIZE * (n_elements(LegTxt) + 1) + 4) * $
			Plots.LegendSize
		tvbox, [xlen, ylen], xll + xlen/2., yll + ylen/2.
	endIF

	FOR ii=0, (n_elements(LegTxt) - 1) do BEGIN
		yll = yll + !D.Y_CH_SIZE * Plots.LegendSize
		    IF (LegPsym(ii) gt 8) THEN BEGIN
			    MakeSym, LegPsym(ii)
			    LegPsym(ii) = 8
		    endIF ELSE IF (LegPsym(ii) eq 8) THEN LegPsym(ii) = 0
		    IF (LegLine(ii) gt -1) then LegPsym(ii) = -LegPsym(ii)


		IF (LegPsym(ii) gt 0) THEN $
			Plots, xll+2*xstep, yll + !D.Y_CH_SIZE/3.0, $
				psym=LegPsym(ii), /DEVICE, $
				SYMSIZE = Plots.LegendSize $
		ELSE IF (LegPsym(ii) le 0) THEN $
			Plots, [xll+xstep, xll+3*xstep], $
				!D.Y_CH_SIZE/3.0 + [yll,yll],$
				psym=LegPsym(ii), LineStyle=LegLine(ii), $
				/DEVICE, SYMSIZE = Plots.LegendSize

		xyouts,xll + 3*xstep,yll,LegTxt(ii), /DEVICE, $
				CHARSIZE=Plots.Legendsize

	endFOR 
endIF


IF (Plots.PlotDate ne 0) then PLOTDATE,'PUBPLOT'

IF keyword_set(Postscript) THEN BEGIN
	if not keyword_set(NOCLOSE) then device,/close
	set_plot,'x'
endIF

End	;routine
