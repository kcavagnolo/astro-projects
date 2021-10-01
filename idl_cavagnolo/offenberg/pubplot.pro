;+
; NAME: 
;	PUBPLOT
; CALLING SEQUENCE:
;	PubPlot,X,Y
;	PubPlot,X,Y,F, OUTPUT = FF
;	PubPlot,X,Y,F, XERROR =xerr, YERROR =yerr, OUTPUT=FF
; ALTERNATIVE CALLING SEQUENCE:
;	PubPlot,X0,Y0,X1,Y1,X2,Y2,...,X9,Y9
; PURPOSE:
;	Create a graphical interface to the PLOT routine and allow the user
;	to set/unset/reset various PLOTTING keywords.  Intended to aid in 
;	making slick plots suitable for publications.
; INPUTS:
;	X - Values to be plotted along the X-axis.  X should be either an
;	    N-element vector or an NxM element array.
;	Y - Values to be plotted along the Y-axis.  Y should either be an
;	    N-element vector or an NxM element array (If X is a 2-D array,
;           Y must be of the same size.)
;    -or-
;	X0...X9 - A vector of values to be plotted along the X-axis.  Each Xn 
;		  will be treated as a single data set.  Up to 10 X vectors 
;		  may be supplied.
;	Y0...Y9 - A vector of values to be plotted along the Y-axis.  Each Yn
;		  will be treated as a single data set.  Up to 10 Y vectors
;		  may be supplied.
;	NOTE: For each Xn supplied, a Yn with the same number of elements must 
;	      also be supplied and vice-versa.  
; OPTIONAL KEYWORD INPUTS:
;	INPUT  - Structure returned by a previous execution of PubPlot (see
;	 	 the "OUTPUT", below).  If specified, PubPlot will use the 
;		 data contained within INPUT to initialize the plot.
;	XERROR - Uncertainty in X coordinate for error bars.  May be either
;		 a scalar or a vector of the same size as X.
;	YERROR - Uncertainty in Y coordinate for error bars.  May be a scalar,
;		 or an array of the same size as Y (i.e. NxM elements).
;
;	If you have a series of vectors you wish to catenate into a single
;	XERROR or YERROR array, use "prepubplot.pro".
; KEYWORD OUTPUT:
;	OUTPUT - Structure containing a record of the final state of the plot.
;	     The same plot can be recovered by using the same data structure as
;	     the INPUT keyword on a subsequent execution of PubPlot.  The plot
;	     can be redrawn to the screen using FF and the routine MakePubPlot
;	     (See makepubplot.pro).  FF can also be used to record the user's 
;	     individual preferences and used with any X, Y data set.
;
; EXAMPLES:
;	To save your plot:
;		PUBPLOT, X, Y, OUTPUT=F         ;[Create your plot and exit]
;		SAVE, F, X, Y, Filename="MyPlot.dat"
;
;	Alternative calling Sequence:
;		PUBPLOT, X1, Y1, X2, Y2, X3, Y3, OUTPUT=F
;		SAVE, F, X, Y, Filename="MyPlot.dat"
;
;	To recover a saved plot and resume PubPlot:
;		restore,'MyPlot.dat"
;		FF = PUBPLOT, X, Y, F
;
;	To recover a saved plot and put it on the screen:
;		restore,'MyPlot.dat"
;		MakePubPlot, X, Y, F	;(See also makepubplot.pro)
;
; INCLUDED ROUTINES:
;	The following routines are in this file:
;	PUBPLOT
;	PUBPLOT_EVENT
;	CUSTOMIZE_EVENT
;	CUSTOM_XYOUTS
;	GET_TEXT_EVENT
;	GET_XYOUTS_POS
;	GET_XYOUTS_TXT
;	PSPUB_EVENT
;	SET_ARCH
;	XYRM_EVENT
;	ERRORBAR
;	ERRORBAR_EVENT
;	Except for PUBPLOT, all of these are meant to be privately used.  
;	See also makepubplot.pro
; SIDE EFFECTS:
;	Numerous widgets will be created and destroyed.  A plotting window will
;	be opened if one is not already open.  This routine will take over the
;	current graphics window if one is open.
; COMMON BLOCKS:
;	The following common blocks are used by routines within this file:
;	PUBPLOT, PUBPLOTARCH, CUSTOMIZE, PLOTOUT, PLOTDATA, PSPUB, WLEGDATA
;	ERRBARDATA
;	These are all meant to be privately used.  Changes to these blocks
;	may not be documented.
; RESTRICTIONS:
;	Will not work on 3-D data.  
;	Requires that both X and Y input arguments be specified.
;	IDL Widgets must be present.
; HISTORY
;	Written 		Joel D. Offenberg, HSTX, Feb 3, 1993
;	Added histogram plotting option, corrected minor position bug on
;		Vax version	JDO, Mar 11, 1993
;	Added error bar handling, ability to use multiple X vectors.
;				JDO, Mar 23, 1993
;	Added support for adding plot date.  JDO, Mar 26, 1993.
;	Added support for legends.  Converted main routine from function to
;	procedure.  Renamed PUBPLOT from ApJPLOT.  JDO, Apr 1993
;	Added Encapsulated PostScript and extra plot symbols.  JDO, March 1994
;	Added alternative (multiple vector) calling sequence.  JDO, April 1994
;	Fixed bugs: INPUT keyword now works, as does the ENCAPSULATED PS option
;							       JDO, April 1994
;	Fixed a bug in centering PostScript files.  JDO, November 1994	
;	Fixed a bug so that it works for Alpha and other architectures.  JDO,
;			November 1996
;-


;
;	GET_TEXT_EVENT
;	This routine is the event handler for the GET_TEXT widget created in
;	GET_XYOUTS_TEXT.   When the text event occurs, the routine adds an
;	element to the Annotation list and gives it the proper values.
;
;	WIDGETS: GET_TEXT widget is destroyed when the event concludes.
;	INPUTS:	event (from GET_TEXT widget)
;	OUTPUTS: Annotation in PLOTOUT common block is changed.
;
pro GET_TEXT_EVENT, event	
	common PUBPLOT, Done, PlotPS, Ttext, XTtext, YTtext, LinLin, $
			LinLog, LogLin, LogLog, SymbolList,SymbolLarger, $
			SymbolSmaller, LineList, LineThicker, LineThinner, $
			XLowerTxt, XUpperTxt, YLowerTxt, YUpperTxt, $
			Xlower,Xupper,Ylower,Yupper, XYOutsOn, XYOutsCs, $
			XYOutsRM, XYOutLarger, XYOutSmaller, XYOutThicker, $
			XYOutNarrower, ErrorBarOn, LegendOn, TitleLarger, $
			TitleSmaller, TitleThicker, TitleNarrower, $
			ExtendedX, ExtendedY, SuppressX, SuppressY, $
			BoxX, BoxY, PlotDateOn, slopex, slopey, xll, yll
	common PLOTOUT, Plots, Ordinate, Annotation
	common PLOTDATA, x, y, Ordnum, xerr, yerr, XErrSet, YErrSet
	common MESSAGE, xmgs,ymgs

	widget_control,get_value=text,event.id

	Annotation = [Annotation(0),Annotation]	;Add another element to stack

	Annotation(0).x = xmgs
	Annotation(0).y = ymgs
	Annotation(0).text = text(0)
	Annotation(0).Orientation = 0
	Annotation(0).CharSize = 1.
	Annotation(0).CharThick = 1	
	widget_control,event.top,/DESTROY

;Now that there is annotation to remove/change, turn "REMOVE" and "CUSTOM" 
;buttons on.
	widget_control,XYoutsRM,SENSITIVE=1
	widget_control,XYoutsCs,SENSITIVE=1
	MakePubPlot, X, Y, Plots, Ordinate, Annotation, $
		     XERROR = Xerr, YERROR = Yerr

	return
end

;
;	GET_XYOUTS_POS
;	This routine prompts the user to supply a position for an annotation
;	string by clicking on the plot.  A message widget is created and 
;	destroyed, but not registered (no events are possible).
;
;	WIDGETS: A widget is created and destroyed
;	INPUTS: None
;	OUTPUTS: xmgs, ymgs in MESSAGE common block are set
;	
pro GET_XYOUTS_POS, xgs, ygs

	common MESSAGE, xmgs,ymgs


;Get position from window
	messagebase=widget_base(/row)
	msgtxt =widget_label(messagebase, value='Click on the left edge of text')
	widget_control,/REALIZE,messagebase
        wshow,!D.Window          ;Make the current window active
	cursor,xmgs,ymgs,3,/data
	widget_control,/DESTROY,messagebase
	xgs = xmgs
	ygs = ymgs
	return
end
;
;	GET_XYOUTS.TXT
;	This routine sets up the GET_TEXT widget and call to GET_TEXT_EVENT.
;
;	WIDGETS: The GET_TEXT widget is created
;	INPUTS: None
;	OUTPUTS: None
;
pro GET_XYOUTS_TXT

	messagebase=widget_base(/column)
	msglabel = widget_label(messagebase, value='ENTER NOTATION TEXT')
	msgtxt=widget_text(messagebase,/EDITABLE,/FRAME)
	msglabel2 = widget_label(messagebase, value="Hit <CR> when done")
	widget_control,/REALIZE, messagebase
	widget_control,/INPUT_FOCUS,msgtxt
	xmanager,'GET_TEXT',messagebase,/MODAL,EVENT_HANDLER="GET_TEXT_EVENT"
	return
end
;
;	CUSTOMIZE_EVENT
;	This routine is the event handler for the CUSTOMIZE widget.  It handles
;	all of the events created by selecting "CUSTOM ANNOTATION" button on
;	the main widget.  
;
;	WIDGETS: The CUSTOMIZE widget is destroyed if "quit" is selected.
;	INPUTS:	event (caused by CUSTOMIZE widget)
;	OUTPUTS: Annotation in PLOTOUT common block is updated.
;
pro CUSTOMIZE_EVENT, event

	common PLOTDATA, x, y, Ordnum, xerr, yerr, XErrSet, YErrSet
	common PLOTOUT, Plots, Ordinate, Annotation
	common CUSTOMIZE, CustomText, CustomPos, CustomLarger, CustomSmaller, $
			  CustomThicker, CustomThinner, CustomClockwise, $
			  CustomAClockwise, CustomDone
	
	IF (event.id eq CustomDone) then BEGIN
		widget_control,/DESTROY,event.top
		RETURN
	endIF

	whtest = where(CustomText eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) then BEGIN
		widget_control,event.id,get_value=text
		Annotation(whtest).text = text(0)
	endIF
	whtest = where(CustomPos eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN BEGIN
		GET_XYOUTS_POS, xmgs, ymgs
		Annotation(whtest).X = xmgs
		Annotation(whtest).Y = ymgs
	endIF 
	whtest = where(CustomLarger eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).CharSize = Annotation(whtest).CharSize + 0.1
	whtest = where(CustomSmaller eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).CharSize = Annotation(whtest).CharSize - 0.1
	whtest = where(CustomThicker eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).CharThick = Annotation(whtest).CharThick + 1.
	whtest = where(CustomThinner eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).CharThick = Annotation(whtest).CharThick - 1.
	whtest = where(CustomClockwise eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).Orientation=Annotation(whtest).Orientation-10
	whtest = where(CustomAClockwise eq event.id) & whtest = whtest(0)
	IF (whtest ne -1) THEN $
		Annotation(whtest).Orientation=Annotation(whtest).Orientation+10

	MakePubPlot, x, y , Plots, Ordinate, Annotation, $
		     XERROR = Xerr, YERROR = Yerr
	return
end
;
;	CUSTOM_XYOUTS
;	This routine sets up the CUSTOMIZE widget and calls CUSTOMIZE_EVENT.
;	Entering a carriage return in a text widget will cause the text of 
;	that widget to change to the new value.  Clicking on a button will
;	cause the named event to occur.
;
;	WIDGETS: The CUSTOMIZE widget is created.
;	INPUTS:	 None
;	OUTPUTS: All elements of the CUSTOMIZE common block are set.
pro CUSTOM_XYOUTS
	
	common PLOTOUT, Plots, Ordinate, Annotation
	common CUSTOMIZE, CustomText, CustomPos, CustomLarger, CustomSmaller, $
			  CustomThicker, CustomThinner, CustomClockwise, $
			  CustomAClockwise, CustomDone

	num = n_elements(Annotation) - 1
	CustomEach = lonarr(num)
	CustomText = lonarr(num)
	CustomPos = lonarr(num)
	CustomLarger = Lonarr(num)
	CustomSmaller = Lonarr(num)
	CustomThicker = Lonarr(num)
	CustomThinner = Lonarr(num)
	CustomClockwise = Lonarr(num)
	CustomAClockwise = Lonarr(num)

	CustomBase = widget_base(title="CUSTOMIZE ANNOTATION",/COLUMN,/FRAME)
	CustomDone = widget_button(CustomBase,value="DONE")
	FOR i=0, num - 1 do BEGIN
		CustomEach(i) = widget_base(CustomBase,/row)
		CustomText(i) = widget_text(CustomEach(I),xsize = 20, /FRAME, $
				ysize = 1,value = Annotation(I).Text,/EDITABLE)
		CustomPos(i) = widget_button(CustomEach(I),value='POSITION')
		CustomLarger(i) = widget_button(CustomEach(I),value='LARGER')
		CustomSmaller(i) = widget_button(CustomEach(I),value='SMALLER')
		CustomThicker(i) = widget_button(CustomEach(I),value='THICKER')
		CustomThinner(i) = widget_button(CustomEach(I),value='THINNER')
		CustomClockwise(i) = widget_button(customEach(i), $
					value='CLOCKWISE')
		CustomAClockwise(i) = widget_button(CustomEach(I), $
					value='ANTI-CLOCK')
	endFOR
	widget_control,custombase,/realize
	xmanager,"CUSTOMIZE",custombase,/MODAL,EVENT_HANDLER="CUSTOMIZE_EVENT"
	return
end
;
;	XYRM_EVENT
;	This routine is the event handler for the XYRM widget.  Selecting a 
;	string will cause that string to disappear from the ANNOTATION stack.
;	
;	WIDGETS: Upon completion of the event, the XYRM widget is destroyed.
;	INPUTS: event (generated by XYRM widget).
;	OUTPUTS: Annotation (in PLOTOUT common block) is updated.
;		 If there are no remaining elements in Annotation, suspend
;		 the "REMOVE ANNOTATION" and "CUSTOM ANNOTATION" buttons.
;
pro XYRM_EVENT, event
	
	common PLOTOUT, Plots, Ordinate, Annotation

	common PLOTDATA, x, y, Ordnum, xerr, yerr, XErrSet, YErrSet

	common PUBPLOT, Done, PlotPS, Ttext, XTtext, YTtext, LinLin, $
			LinLog, LogLin, LogLog, SymbolList,SymbolLarger, $
			SymbolSmaller, LineList, LineThicker, LineThinner, $
			XLowerTxt, XUpperTxt, YLowerTxt, YUpperTxt, $
			Xlower,Xupper,Ylower,Yupper, XYOutsOn, XYOutsCs, $
			XYOutsRM, XYOutLarger, XYOutSmaller, XYOutThicker, $
			XYOutNarrower, ErrorBarOn, LegendOn, TitleLarger, $
			TitleSmaller, TitleThicker, TitleNarrower, $
			ExtendedX, ExtendedY, SuppressX, SuppressY, $
			BoxX, BoxY, PlotDateOn, slopex, slopey, xll,yll
	
;The UValues contain the indices of the elements in the Annotation array
;The highest-index element is a null, which is replaced here by a 'cancel'
	mm = n_elements(Annotation) - 1
	widget_control,event.id,GET_UVALUE = UValue
	IF (UValue eq 0) THEN 			$
		Annotation = Annotation(1:*)	$
	ELSE IF (UValue lt mm) THEN		$
		Annotation = [Annotation(0:Uvalue-1),Annotation(Uvalue+1:*)]
	;ELSE (UVAlue eq mm) --> DO NOTHING

	widget_control,event.top,/DESTROY

;If there is only the null annotation left, then turn off "REMOVE" and "CUSTOM"
;buttons.
	IF (n_elements(Annotation) lt 2) then BEGIN
		widget_control, XYOutsRM, SENSITIVE=0
		widget_control, XYOutsCs, SENSITIVE=0
	END
	MakePubPlot, X, Y, Plots, Ordinate, Annotation	, $
		     XERROR = Xerr, YERROR = Yerr
	RETURN
end
;
;	PSPUB_EVENT
;	This routine is the event handler for the PSPUB widget for selecting 
;	PostScript output methods.  Plot is directed to a PS file (default is
;	IDL.PS, user can modify this), which is then directed to the PS printer
;	Printer selection same as in PSPLOT.PRO (See PSPLOT.PRO) if needed.
;
;	WIDGETS: The PSPUB widget is destroyed
;	INPUTS:	 event (generated by PSPUB widget).
;	OUTPUTS: The plot is directed to a PostScript file, PostScript printer
;		 or both.
;
pro PSPUB_EVENT, event
	common PLOTDATA, x, y, Ordnum, xerr, yerr, XErrSet, YErrSet


	common PSPUB,	PSPlotText, PSCancel, PSPlot, PSFile, PSPlotFile, $
			PSFileName, PSLandscape, PSEncapFile, PSXSize, $
			PSYSize, PSCM

	common PLOTOUT, Plots, Ordinate, Annotation

;These are the editable text widgets in the PSPUB widget.  Read them for 
;every event, so that the values will get changed, even if the user didn't
;hit a <CR>.

	widget_control,PSXSize,GET_VALUE=psx
	Plots.PSSize(0) = psx(0)

	widget_control,PSYSize,GET_VALUE=psx
	Plots.PSSize(1) = psx(0)

	widget_control,PSPlotText,GET_VALUE=filename
	PSFileName = filename(0)
	IF (strpos(PSFileName,".")  lt 0) THEN PSFileName = PSFileName+".ps"

	CASE event.id OF
	PSLandscape:	BEGIN
				Plots.Landscape = byte(event.select)
			END
	PSCancel:	BEGIN
				widget_control,event.top,/DESTROY
			END
	PSFile:		BEGIN
				MakePubPlot, X, Y, Plots, Ordinate, $
		     			     XERROR = Xerr, YERROR = Yerr,$
					     Annotation, /POSTSCRIPT, $
					     FILENAME=PSFileName, $
					     LANDSCAPE = Plots.Landscape
				Widget_control,event.top,/DESTROY
			END
	PSPlot:		BEGIN
				MakePubPlot, X, Y, Plots, Ordinate, $
				     	     XERROR = Xerr, YERROR = Yerr, $
					     Annotation, /POSTSCRIPT, $
					     FILENAME=PSFileName, $
					     LANDSCAPE = Plots.Landscape
				psplot,PSFileName,/DELETE
				Widget_control,event.top,/DESTROY
			END
	PSPlotFile:	BEGIN
				MakePubPlot, X, Y, Plots, Ordinate, $
				     	     XERROR = Xerr, YERROR = Yerr, $
					     Annotation, /POSTSCRIPT, $
					     FILENAME=PSFileName, $
					     LANDSCAPE = Plots.Landscape
				psplot,PSFileName
				Widget_control,event.top,/DESTROY
			END
	PSEncapFile:	BEGIN
				MakePubPlot, X, Y, Plots, Ordinate, $
				     	     XERROR = Xerr, YERROR = Yerr, $
					     Annotation, /POSTSCRIPT, $
					     FILENAME=PSFileName, $
					     LANDSCAPE = Plots.Landscape, $
					     /ENCAP
				Widget_control,event.top,/DESTROY
			END
	PSCM:		BEGIN	;Use inches unless "CM" is actively set.
				widget_control,event.id,GET_VALUE=psx
				IF (psx(0) eq "INCHES") then BEGIN
					Plots.PSUnits = 1
					widget_control,event.id,$
							SET_VALUE="CENTIM"
				endIF ELSE BEGIN
					Plots.PSUnits = 0
					widget_control,event.id,$
							SET_VALUE="INCHES"
				endELSE
			END
	ELSE:		BEGIN
				;Do nothing.  
			END
	endCASE
end

;	ERRORBAR_EVENT
;	Event handler for the ERRORBAR widget.  This Widget allows the user
;	to add, customize and/or remove error bars around the data.  
pro ERRORBAR_EVENT, event
	common PLOTDATA, x, y, OrdNum, xerr, yerr, XErrSet, YErrSet

	common PLOTOUT, Plots, Ordinate, Annotation

	common ERRBARDATA, ErrDone, ErrSText, ErrSNone, ErrSConstant, $
			ErrSLinear, ErrSSquareRt, ErrSUser, ErrHat, $
			ErrHLarger , ErrHSmaller, ErrhTHicker, ErrHThinner, $
			ErrHBase2

	IF (event.id eq errdone) THEN BEGIN
		widget_control,event.top,/DESTROY
		return
	endIF
	
	FOR i=0, ordnum-1 DO BEGIN
	    CASE (event.id) OF
	    ErrSText(i):	BEGIN
				   widget_control,ErrSText(i), GET_VALUE=etxtv
				   Ordinate(i).XErrNum = double(etxtv(0))
				END
	    ErrSNone(i):        BEGIN
				    Ordinate(i).XErrBType = 0
				    widget_control, ErrSText(i), $
					SET_VALUE= 'NONE',SENSITIVE=1
				    Ordinate(i).XErrNum = 0
				    
				END
	    ErrSConstant(i):	BEGIN
				    Ordinate(i).XErrBType = 1
				    widget_control,ErrSText(i),GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE")) THEN BEGIN
					Ordinate(i).XErrNum = 1.0
					widget_control, ErrSText(i), $
						SET_VALUE= '1.0',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).XErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSLinear(i):	BEGIN
				    Ordinate(i).XErrBType = 2
				    widget_control,ErrSText(i),GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE"))  THEN BEGIN
					Ordinate(i).XErrNum = .10
					widget_control, ErrSText(i), $
						SET_VALUE='.10',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).XErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSSquareRt(i):	BEGIN
				    Ordinate(i).XErrBType = 3
				    widget_control,ErrSText(i),GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE")) THEN BEGIN
					Ordinate(i).XErrNum = .10
					widget_control, ErrSText(i), $
						SET_VALUE='.10',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).XErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSUser(i):	BEGIN
				    Ordinate(i).XErrBType = 4
				    Ordinate(i).XErrNum = 0.0
				    widget_control, ErrSText(i), $
					SET_VALUE="USER", SENSITIVE=0
				END
	    ErrHat(i):		BEGIN
				    IF (event.select eq 1) THEN BEGIN
					Ordinate(i).XHat = 1
					widget_control,ErrHBase2(i),SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).XHat = 0
					widget_control,ErrHBase2(i),SENSITIVE=0
				    endELSE
				END
	     ErrHLarger(i):	BEGIN
				    Ordinate(i).XHatLength = $
						    Ordinate(i).XHatLength + 1
				    widget_control,ErrHSmaller(i),SENSITIVE=1
				END
	     ErrHSmaller(i):	BEGIN
				    Ordinate(i).XHatLength = $
						    Ordinate(i).XHatLength - 1
				    IF (Ordinate(i).XHatLength eq 0) THEN $
				      widget_control,ErrHSmaller(i),SENSITIVE=0
				END
	     ErrHThicker(i):	BEGIN
				    Ordinate(i).XHatThick = $
						     Ordinate(i).XHatThick + 1
				    widget_control,ErrHThinner(i),SENSITIVE=1
				END
	     ErrHThinner(i):	BEGIN
				    Ordinate(i).XHatThick = $
						     Ordinate(i).XHatThick -1
				    IF (Ordinate(i).XHatThick eq 1) THEN $
				      widget_control,ErrHThinner(i),SENSITIVE=0
				END
	    ErrSText(i+Ordnum): BEGIN
				   widget_control,ErrSText(i+OrdNum), $
								GET_VALUE=etxtv
				   Ordinate(i).ErrNum = double(etxtv(0))
				END
	    ErrSNone(i+OrdNum): BEGIN
				    Ordinate(i).ErrBType = 0
				    Ordinate(i).ErrNum = 0
				    widget_control, ErrSText(i+OrdNum), $
						SET_VALUE= 'NONE',SENSITIVE=1
				END
	    ErrSConstant(i+OrdNum):BEGIN
				    Ordinate(i).ErrBType = 1
				    widget_control,ErrSText(i+OrdNum),$
								GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE")) THEN BEGIN
					Ordinate(i).ErrNum = 1.0
					widget_control, ErrSText(i+OrdNum), $
						SET_VALUE= '1.0',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).ErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSLinear(i+OrdNum):BEGIN
				    Ordinate(i).ErrBType = 2
				    widget_control,ErrSText(i+OrdNum),$
								GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE"))  THEN BEGIN
					Ordinate(i).ErrNum = .10
					widget_control, ErrSText(i+OrdNum), $
						SET_VALUE='.10',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).ErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSSquareRt(i+OrdNum):BEGIN
				    Ordinate(i).ErrBType = 3
				    widget_control,ErrSText(i+OrdNum),$
								GET_VALUE=etxtv
				    IF ((etxtv(0) eq "USER") OR $	
				       (etxtv(0) eq "NONE")) THEN BEGIN
					Ordinate(i).ErrNum = .10
					widget_control, ErrSText(i+OrdNum), $
						SET_VALUE='.10',SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).ErrNum = double(etxtv(0))
				    endELSE
				END
	    ErrSUser(i+OrdNum): BEGIN
				    Ordinate(i).ErrBType = 4
				    Ordinate(i).ErrNum = 0.0
				    widget_control, ErrSText(i+OrdNum), $
					SET_VALUE="USER", SENSITIVE=0
				END
	    ErrHat(i+OrdNum):	BEGIN
				    IF (event.select eq 1) THEN BEGIN
					Ordinate(i).Hat = 1
					widget_control,ErrHBase2(i+OrdNum),$
								    SENSITIVE=1
				    endIF ELSE BEGIN
					Ordinate(i).HAT = 0
					widget_control,ErrHBase2(i+OrdNum),$
								    SENSITIVE=0
				    endELSE
				END
	     ErrHLarger(i+OrdNum):BEGIN
				    Ordinate(i).HatLength = $
						    Ordinate(i).HatLength + 1
				    widget_control,ErrHSmaller(i+OrdNum),$
								   SENSITIVE=1
				END
	     ErrHSmaller(i+OrdNum):BEGIN
				    Ordinate(i).HatLength = $
						    Ordinate(i).HatLength - 1
				    IF (Ordinate(i).HatLength eq 0) THEN $
				      widget_control,ErrHSmaller(i+OrdNum),$
								   SENSITIVE=0
				END
	     ErrHThicker(i+OrdNum):BEGIN
				    Ordinate(i).HatThick = $
						    Ordinate(i).HatThick + 1
				    widget_control,ErrHThinner(i+OrdNum), $
							           SENSITIVE=1
				END
	     ErrHThinner(i+OrdNum):BEGIN
				    Ordinate(i).HatThick = $
						    Ordinate(i).HatThick - 1
				    IF (Ordinate(i).HatThick eq 1) THEN $
				      widget_control,ErrHThinner(i+OrdNum),$
							    	    SENSITIVE=0
				END
	     ELSE:
				
	     endCASE
	endFOR
	MakePubPlot, X, Y, Plots, Ordinate, Annotation, $
		     XERROR=xerr, YERROR=yerr
end

;
;	WLEGEND_EVENT
;	Event handler for the Legend widget.   This widget allows the user
;	to add, remove and/or customize the plot's legend.

pro WLEGEND_EVENT, event

	common WLEGDATA, OrdLabel, LegLarger, LegSmaller, $
			 LegBOn, LegBText, LegDone, LegPos, LegBox, LegAText

	common PLOTOUT, Plots, Ordinate, Annotation

	common PLOTDATA, x, y, OrdNum, xerr, yerr, XErrSet, YErrSet

	CASE event.id OF
	LegDone:  widget_control,event.top, /DESTROY
	LegPos:   BEGIN
			GET_XYOUTS_POS, xx, yy
			Plots.LegendX = xx
			Plots.LegendY = yy
		  END
	LegAText: BEGIN
			widget_control,event.id, GET_VALUE=Atxt
			Plots.LegendDelChar = strmid(Atxt(0),0,1)
		  END
	LegBox:   Plots.LegendBox = event.select
	LegLarger: BEGIN
			Plots.LegendSize = Plots.LegendSize + 0.10
			widget_control, LegSmaller, SENSITIVE=1
		   END
	LegSmaller: BEGIN
			Plots.LegendSize = Plots.LegendSize - 0.10
			IF (Plots.LegendSize lt 0.10) THEN $
				widget_control, LegSmaller, SENSITIVE=0
		    END
	ELSE:	BEGIN
			FOR i=0, Ordnum-1 DO BEGIN
				IF (event.id eq LegBOn(i)) THEN BEGIN
					Ordinate(i).LegendItem = event.select
				endIF ELSE $
				IF (event.id eq LegBText(i)) THEN BEGIN
				    widget_control,event.id, GET_VALUE=BTxt
				    Ordinate(i).Title = Btxt(0)
				    widget_control,OrdLabel(i), $
					SET_VALUE = Ordinate(i).Title
				endIF
			endFOR
		END
	endCASE

	MakePubPlot, X, Y, Plots, Ordinate, Annotation, $
		     XERROR=xerr, YERROR=yerr

end
;
;	WLEGEND
;	Sets up the widget for the legend interface.  Most of the guts are in
;	the event handler, WLEGEND_EVENT and in LEGEND.PRO

pro WLEGEND

	common WLEGDATA, OrdLabel, LegLarger, LegSmaller, $
			 LegBOn, LegBText, LegDone, LegPos, LegBox, LegAText

	common PLOTDATA, x, y, OrdNum, xerr, yerr, XErrSet, YErrSet

	common PLOTOUT, Plots, Ordinate, Annotation

	common PUBPLOTARCH, Base4xsz, BigOrdScrX, BigOrdScrY, AxisStyX, $
			AxisStyY, AxisStySz, AxisStyL1, AxisStyL2, AxisStyL3, $
			TitleOffset, XTitleOffset, YTitleOffset, WFont, $
			ErrBXSz, RangeOff0, RangeOff1, RangeOff2, XRangeOff

	LegBase = widget_base(Title="LEGEND", /FRAME, /COLUMN)
	LegDone = widget_button(LegBase,Value="DONE", FONT=Wfont)
	LegPos = widget_button(LegBase,Value="SET POSITION",FONT=WFont)
	LegABase = widget_base(LegBase,/ROW)
	LegALabel = widget_label(LegABase,value="Delmiter: ",FONT=WFont)
	LegAText = widget_text(LegABase,/EDITABLE, value=Plots.LegendDelChar,$
				FONT=WFont,/FRAME)
	LegTbase = widget_base(LegBase, /ROW,/FRAME)
	LegTBase2 = widget_base(LegTBase,/Column)
	LegLarger = widget_button(LegTBase2,value="LARGER",FONT=WFont)
	LegSmaller = widget_button(LegTBase2,value="SMALLER", FONT=WFont)
	LegTbase3 =widget_base(LegTBase,/Column,/Nonexclusive)
	LegBox = widget_button(LegTbase3,value="BOX",FONT=WFONT)
	IF (Plots.LegendBox eq 1) THEN widget_control,LegBox,SET_BUTTON=1

	LegBBase = widget_base(LegBase,/FRAME,/COLUMN)

	LegBBase0 = widget_base(LegBBase,/ROW)
	LegBLabel1 = widget_label(LegBBase0, VALUE="LEGEND ON",FONT=WFont)
	LegBLabel2 = widget_label(LegBBase0, VALUE="TITLE",FONT=WFont)

	LegBBase1 = lonarr(OrdNum)
	LegBBase2 = lonarr(OrdNum)
	LegBBase3 = lonarr(OrdNum)
	LegBOn = LegBBase1
	LegBText = LegBOn


	FOR i=0, OrdNum -1 do BEGIN
		LegBBase1(i) = widget_base(LegBBase,/ROW)
		LegBBase2(i) = widget_base(LegBBase1(i), /ROW, /NONEXCLUSIVE)
		LegBOn(i) = widget_button(LegBBase2(i),value="")
		IF (Ordinate(i).LegendItem) THEN $
			widget_control, LegBOn(i), SET_BUTTON = 1
		LegBBase3(i) = widget_base(LegBBase1(i),/ROW)
		LegBText(i) = widget_text(LegBBase3(i),/EDITABLE,/FRAME, $
				FONT=WFont,Value=Ordinate(i).Title)
	endFOR

	widget_control,/REALIZE, LegBase	
	xmanager, "WLEGEND", LegBase, /MODAL, EVENT_HANDLER = "WLEGEND_EVENT"
end


;
;	ERRORBAR
;	Sets up widget for error bar interface.  Most of the guts are in the
;	event handler, ERRORBAR_EVENT

pro ERRORBAR
	common PLOTDATA, x, y, OrdNum, xerr, yerr, XErrSet, YErrSet

	common PUBPLOTARCH, Base4xsz, BigOrdScrX, BigOrdScrY, AxisStyX, $
			AxisStyY, AxisStySz, AxisStyL1, AxisStyL2, AxisStyL3, $
			TitleOffset, XTitleOffset, YTitleOffset, WFont, $
			ErrBXSz, RangeOff0, RangeOff1, RangeOff2, XRangeOff

	common PLOTOUT, Plots, Ordinate, Annotation

	common ERRBARDATA, errdone, errstext, errsnone, errsconstant, $
			ErrSLinear, errssquarert, errsuser, errhat, $
			errhlarger, errhsmaller, errhthicker, errhthinner, $
			ErrHBase2

;Just about all of the widgets are going to be arrays.  Set up the arrays now.
	errabase = widget_base(title="ERROR BARS",/FRAME,/COLUMN)
	errdone  = widget_button(errabase, value="DONE")
	errbbase = widget_base(errabase,/ROW)
	esbase = lonarr(OrdNum)
	esxbase = lonarr(OrdNum)
	esybase = lonarr(OrdNum)
	esbase2 = lonarr(OrdNum * 2)
	errslabel = esbase2
	errstext = esbase2
	errsnone = esbase2
	errsconstant=esbase2
	ErrSLinear =esbase2
	errssquarert = esbase2
	errsuser = esbase2
	errhbase = esbase2
	errhbase1 = esbase2
	errhbase2 = esbase2
	errhat = esbase2
	errhlarger = esbase2
	errhsmaller = esbase2
	errhthicker = esbase2
	errhthinner = esbase2
;Set up the widgets.
	FOR i=0, 2*OrdNum-1 DO BEGIN	
	    IF (i lt OrdNum) THEN BEGIN
	        esbase(i) = widget_base(errbbase,/COLUMN,XSIZE = ErrBXSz )
		esxbase(i) = widget_base(esbase(i),/COLUMN)
		ebase = esxbase(i)
		errslabel= widget_label(ebase, value='X DATA #'+ $
					strcompress(string(i+1),/REMOVE_ALL), $
					FONT=WFONT)
	    endIF $
	    ELSE BEGIN
		esybase(i-OrdNum) = widget_base(esbase(i-OrdNum),/COLUMN)
		ebase = esybase(i-Ordnum)
		errslabel = widget_label(ebase,value="Y DATA #"+ $
				strcompress(string(i-OrdNum+1),/REMOVE_ALL), $
				FONT=WFONT )
	    endELSE

	    ErrSText(i) = widget_text(ebase,/EDITABLE,/FRAME, FONT=WFONT)
	    esbase2(i) = widget_base(ebase,/COLUMN,/EXCLUSIVE,/FRAME)

	    ErrSNone(i) = widget_button(esbase2(i),value="OFF", FONT=WFONT)
	    ErrSConstant(i)=widget_button(esbase2(i),value="CONSTANT",$
			FONT=WFONT)
	    ErrSLinear(i) =widget_button(esbase2(i),value="PROPORTIONAL",$
			FONT=WFONT)
	    ErrSSquareRt(i)=widget_button(esbase2(i),value="SQUARE RT",$
			FONT=WFONT)
	    ErrSUser(i)=widget_button(esbase2(i),value="USER VALUE",FONT=WFONT)

	    IF (i lt OrdNum) THEN BEGIN
		CASE Ordinate(i).XErrBType OF
		1:	BEGIN
			     widget_control, ErrSConstant(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 1, $
					SET_VALUE = string(Ordinate(i).XErrNum)
			END
		2:	BEGIN 
			     widget_control, ErrSLinear(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE =1, $
					SET_VALUE = string(Ordinate(i).XErrNum)
			END
		3:	BEGIN
			     widget_control, ErrSSquareRt(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 1, $
					SET_VALUE = string(Ordinate(i).XErrNum)
			END
		4:	BEGIN
			     widget_control, ErrSUser(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 0, $
					SET_VALUE = "USER"
			END
		ELSE:	BEGIN
			     widget_control,ErrSNone(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 0, $
					SET_VALUE = "NONE"
			END
		endCASE
	    endIF ELSE BEGIN
		CASE Ordinate(i-OrdNum).ErrBType OF
		1:	BEGIN
			     widget_control, ErrSConstant(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 1, $
				SET_VALUE = string(Ordinate(i-OrdNum).ErrNum)
			END
		2:	BEGIN 
			     widget_control, ErrSLinear(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE =1, $
				SET_VALUE = string(Ordinate(i-OrdNum).ErrNum)
			END
		3:	BEGIN
			     widget_control, ErrSSquareRt(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 1, $
				SET_VALUE = string(Ordinate(i-OrdNum).ErrNum)
			END
		4:	BEGIN
			     widget_control, ErrSUser(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 0, $
				SET_VALUE = "USER"
			END
		ELSE:	BEGIN
			     widget_control,ErrSNone(i),/SET_BUTTON
			     widget_control, ErrSText(i), SENSITIVE = 0, $
				SET_VALUE = "NONE"
			END
		endCASE
	    endELSE

	    ErrHBase(i) = widget_base(ebase,/COLUMN,/FRAME)
	    ErrHBase1(i) = widget_base(errhbase(i),/NONEXCLUSIVE)
	    ErrHat(i) = widget_button(errhbase1(i),value='HATS',FONT=WFONT)
	    ErrHBase2(i) = widget_base(errhbase(i),/COLUMN)
	    ErrHLarger(i) = widget_button(errhbase2(i),value='LONGER',$
								FONT=WFONT)
	    ErrHSmaller(i) = widget_button(errhbase2(i),value='SHORTER',$
								FONT=WFONT)
	    ErrHThicker(i) = widget_button(errhbase2(i),value='THICKER',$
								FONT=WFONT)
	    ErrHThinner(i) = widget_button(errhbase2(i),value='THINNER', $
								FONT=WFONT)
;The hat thickness starts out at the minimum value, so disable the "thinner"
;option.
	    widget_control,ErrHThinner(i),SENSITIVE=0
	    IF (i lt OrdNum) THEN BEGIN
		IF (Ordinate(i).XHat eq 0) THEN $ 
	    		widget_control,ErrHBase2(i),SENSITIVE=0
	    endIF ELSE BEGIN
		IF (Ordinate(i-OrdNum).Hat eq 0) THEN $
	    		widget_control,ErrHBase2(i),SENSITIVE=0
	    endELSE
	endFOR

;

	WIDGET_CONTROL, ERRABASE, /REALIZE
	XMANAGER,'ERRORBAR',ERRABASE,/MODAL,EVENT_HANDLER = "ERRORBAR_EVENT"
END
		
;
;	SET_ARCH
;	NO-BRAIN ROUTINE TO SET CERTAIN ITEMS BASED ON THE VERSION OF IDL
;	BEING RUN (I.E. VAX OR SUN).  ITEMS ARE OUTPUT IN THE PUBPLOTARCH 
;	COMMON BLOCK
;
PRO SET_ARCH
	COMMON PUBPLOTARCH, BASE4XSZ, BIGORDSCRX, BIGORDSCRY, AXISSTYX, $
			AXISSTYY, AXISSTYSZ, AXISSTYL1, AXISSTYL2, AXISSTYL3, $
			TITLEOFFSET, XTITLEOFFSET, YTITLEOFFSET, WFONT, $
			ErrBXSz, RangeOff0, RangeOff1, RangeOff2, XRangeOff

	VERSIONARCH = STRUPCASE(!VERSION.ARCH)
	IF ((VERSIONARCH EQ "VAX") or (VERSIONARCH EQ "ALPHA")) THEN BEGIN
		BASE4XSZ = 200
		BIGORDSCRX = 300
		BIGORDSCRY = 580
		AXISSTYX = 160
		AXISSTYY = 192
		AXISSTYSZ = 28
		AXISSTYL1 = 2
		AXISSTYL2 = 25
		AXISSTYL3 = 47
		TITLEOFFSET  = 6
		XTITLEOFFSET = 38
		YTITLEOFFSET = 70
		WFONT = '-*-HELVETICA-BOLD-R-NORMAL-*-12-*-*-*-*-*-*-*'
		ErrBXSz = 180
		RangeOff0 = 2
		RangeOff1 = 0
		RangeOff2 = 60
		XRangeOff = [10,0,10]
		
	ENDIF ELSE $
	IF (VERSIONARCH EQ "SPARC") THEN BEGIN
		BASE4XSZ = 190
		BIGORDSCRX = 320
		BIGORDSCRY = 580
		AXISSTYX = 159
		AXISSTYY = 182
		AXISSTYSZ = 50
		AXISSTYL1 = 5
		AXISSTYL2 = 42
		AXISSTYL3 = 80
		TITLEOFFSET  = 6
		XTITLEOFFSET = 36
		YTITLEOFFSET = 65
		WFONT = '-*-HELVETICA-BOLD-R-NORMAL-*-12-*-*-*-*-*-*-*'
		ErrBXSz = 180
		RangeOff0 = 2
		RangeOff1 = 40
		RangeOff2 = 60
		XRangeOff = [10,0,10]

	ENDIF ELSE BEGIN
		BASE4XSZ = 190
		BIGORDSCRX = 320
		BIGORDSCRY = 580
		AXISSTYX = 159
		AXISSTYY = 182
		AXISSTYSZ = 50
		AXISSTYL1 = 5
		AXISSTYL2 = 42
		AXISSTYL3 = 80
		TITLEOFFSET  = 6
		XTITLEOFFSET = 36
		YTITLEOFFSET = 65
		WFONT = '-*-HELVETICA-BOLD-R-NORMAL-*-12-*-*-*-*-*-*-*'
		ErrBXSz = 180
		RangeOff0 = 2
		RangeOff1 = 40
		RangeOff2 = 60
		XRangeOff = [10,0,10]

	endELSE
END


;
;	PUBPLOT_EVENT
;	EVENT HANDLER FOR THE LARGE PUBPLOT WIDGET.  
;
;	WIDGETS: PSPUB, XYRM WIDGETS CAN BE CREATED.  PUBPLOT WIDGET WILL BE
;		 DESTROYED IF "DONE" IS SELECTED.
;	INPUTS:	 EVENT (GENERATED BY PUBPLOT WIDGET)
;	OUTPUTS: A PLOT IS GENERATED AND PUT ON THE SCREEN AND/OR INTO A 
;		 POSTSCRIPT FILE AND/OR SENT TO A POSTSCRIPT PRINTER
;		 ELEMENTS IN PLOTOUT COMMON BLOCK MAY ALL BE ALTERED.
;
PRO PUBPLOT_EVENT, EVENT

	COMMON PLOTDATA, X, Y, ORDNUM, XERR, YERR, XERRSET, YERRSET
	common PUBPLOT, Done, PlotPS, Ttext, XTtext, YTtext, LinLin, $
			LinLog, LogLin, LogLog, SymbolList,SymbolLarger, $
			SymbolSmaller, LineList, LineThicker, LineThinner, $
			XLowerTxt, XUpperTxt, YLowerTxt, YUpperTxt, $
			Xlower,Xupper,Ylower,Yupper, XYOutsOn, XYOutsCs, $
			XYOutsRM, XYOutLarger, XYOutSmaller, XYOutThicker, $
			XYOutNarrower, ErrorBarOn, LegendOn, TitleLarger, $
			TitleSmaller, TitleThicker, TitleNarrower, $
			ExtendedX, ExtendedY, SuppressX, SuppressY, $
			BoxX, BoxY, PlotDateOn, slopex, slopey, xll, yll

	COMMON PSPUB,	PSPLOTTEXT, PSCANCEL, PSPLOT, PSFILE, PSPLOTFILE, $
			PSFILENAME, PSLANDSCAPE, PSEncapFile, PSXSize, $
			PSYSize, PSCM
	
	COMMON PLOTOUT, PLOTS, ORDINATE, ANNOTATION

	CASE EVENT.ID OF
	DONE:	BEGIN
			WIDGET_CONTROL,/DESTROY, EVENT.TOP
			RETURN
		END
	PLOTPS:	BEGIN
			;Initialize value of landscape to zero
			Plots.Landscape = 0
			
			PSPlotBase= WIDGET_BASE(/FRAME,/COLUMN,$
				    TITLE='POSTSCRIPT PLOT')
			PSPlotLabel = WIDGET_LABEL(PSPLOTBASE,$
				       VALUE="POSTSCRIPT FILE:")
			PSFileName = "idl.ps"
			PSPlotText = WIDGET_TEXT(PSPLOTBASE,/EDITABLE,/FRAME,$
				      VALUE = PSFILENAME)
			PSLSBase = WIDGET_BASE(PSPLOTBASE,/EXCLUSIVE)
			PSLandscape = WIDGET_BUTTON(PSLSBASE,VALUE="LANDSCAPE")

                        PSSizeRem = WIDGET_LABEL(PSPlotBase,value="Size " + $
                                                 "ignored for non-encap PS")
			PSSizeBase = WIDGET_BASE(PSPlotBase,/ROW)
			PSXSizeL = Widget_label(PSSizeBase,value="X: ")
			PSXSize = WIDGET_TEXT(PSSizeBase,/EDITABLE,/FRAME, $
						value=string(Plots.PSSize(0)))
			PSYSizeL = Widget_label(PSSizeBase,value="Y: ")
			PSYSize = WIDGET_TEXT(PSSizeBase,/EDITABLE,/FRAME,$
						value=string(Plots.PSSize(1)))
			PSUnitBase = WIDGET_BASE(PSSizeBase,/ROW)
			IF (Plots.PSUnits) THEN xxval = "CENTIM" $
			else xxval = "INCHES"
			PSCM = WIDGET_BUTTON(PSUnitBase,value=xxval)
			
	
			PSButtonbase= WIDGET_BASE(/ROW,PSPLOTBASE)
			PSCancel = WIDGET_BUTTON(PSBUTTONBASE,$
						  VALUE="CANCEL")
			PSPlot = WIDGET_BUTTON(PSBUTTONBASE,$
					       VALUE="TO PRINTER")
			PSFile = WIDGET_BUTTON(PSBUTTONBASE,$
						VALUE="TO FILE")
			PSEncapFile = WIDGET_BUTTON(PSButtonBase, $
						VALUE="TO ENCAP PS FILE")
			PSPlotFile = WIDGET_BUTTON(PSBUTTONBASE,$
				     VALUE="TO PRINTER & FILE")
			WIDGET_CONTROL,PSPLOTBASE,/REALIZE	
			XMANAGER,'PSPUB',PSPLOTBASE,/MODAL,$
				EVENT_HANDLER="PSPUB_EVENT"
			RETURN
		END
	TTEXT:	BEGIN
			WIDGET_CONTROL,EVENT.ID,GET_VALUE=TITLE
			PLOTS.TITLE = TITLE(0) 
		END
	XTTEXT:	BEGIN
			WIDGET_CONTROL,EVENT.ID,GET_VALUE=XTITLE
			PLOTS.XTITLE = XTITLE(0)
		END
	YTTEXT:	BEGIN
			WIDGET_CONTROL,EVENT.ID,GET_VALUE=YTITLE
			PLOTS.YTITLE = YTITLE(0)
		END
	TITLELARGER:	BEGIN
				PLOTS.CHARSIZE = PLOTS.CHARSIZE + 0.10
				WIDGET_CONTROL,TITLESMALLER,SENSITIVE = 1
			END
	TITLESMALLER:	BEGIN
				PLOTS.CHARSIZE = PLOTS.CHARSIZE - 0.10
				IF (PLOTS.CHARSIZE LE 0.10) THEN $
				    WIDGET_CONTROL,TITLESMALLER,SENSITIVE = 0
			END
	TITLETHICKER:	BEGIN
				PLOTS.CHARTHICK = PLOTS.CHARTHICK + 1.
				WIDGET_CONTROL,TITLENARROWER,SENSITIVE = 1
			END
	TITLENARROWER:	BEGIN
				PLOTS.CHARTHICK = PLOTS.CHARTHICK - 1.
				IF (PLOTS.CHARTHICK LE 1.0) THEN $
				    WIDGET_CONTROL,TITLENARROWER,SENSITIVE = 0
			END
	LINLIN:	PLOTS.TYPE = 0B
	LINLOG:	PLOTS.TYPE = 1B
	LOGLIN:	PLOTS.TYPE = 2B
	LOGLOG:	PLOTS.TYPE = 3B
	XYOUTSON:   BEGIN
			GET_XYOUTS_POS
			GET_XYOUTS_TXT
		    END
	XYOUTSCS:   BEGIN
			CUSTOM_XYOUTS
		    END
	
;REMOVE AN XYOUTS STRING.  USER IS GIVEN A MENU OF ITEMS FROM WHICH THEY SELECT
;ONE TO REMOVE.  REMEMBER THAT THE ANNOTATION STACK CONTAINS A NULL ELEMENT AS
;THE HIGHEST-NUMBERED ONE.  THAT ELEMENT IS REPLACED WITH THE "CANCEL" OPTION.
	XYOUTSRM:    BEGIN
			MM = N_ELEMENTS(ANNOTATION)
			VALUES = STRMID(ANNOTATION.TEXT,0,30)
			VALUES(MM-1) = "CANCEL"
			XMENU,VALUES, BUTTONS=XYRMBUTTONS, BASE=MENUBASE, $
			TITLE="SELECT STRING TO REMOVE",/NO_RELEASE, $
				UVALUE = INDGEN(MM)
			WIDGET_CONTROL,MENUBASE,/REALIZE
			XMANAGER, "XYRM",MENUBASE,/MODAL, $
				EVENT_HANDLER="XYRM_EVENT"
			RETURN
		     END
	XYOUTLARGER:  BEGIN
			PLOTS.ACHARSIZE = PLOTS.ACHARSIZE + 0.2
			WIDGET_CONTROL,XYOUTSMALLER, SENSITIVE = 1
		      END
	XYOUTSMALLER: BEGIN
			PLOTS.ACHARSIZE = PLOTS.ACHARSIZE - 0.2
			IF (PLOTS.ACHARSIZE LE 0.2) THEN $
			    WIDGET_CONTROL,XYOUTSMALLER, SENSITIVE = 0
		      END
	XYOUTTHICKER: BEGIN
			PLOTS.ACHARTHICK = PLOTS.ACHARTHICK + 1.0
			WIDGET_CONTROL,XYOUTNARROWER, SENSITIVE = 1
		      END
	XYOUTNARROWER: BEGIN
			PLOTS.ACHARTHICK = PLOTS.ACHARTHICK - 1.0
			IF (PLOTS.ACHARTHICK LE 1.0) THEN $
			    WIDGET_CONTROL,XYOUTNARROWER, SENSITIVE = 0
		       END
	ERRORBARON:	BEGIN
			 ERRORBAR
			END
	LegendOn:	BEGIN
			 	Plots.LegendON = 1
			 	WLegend
			END
	PlotDateON:	BEGIN
			    Plots.PlotDate = event.select
			END
	XLOWER:	BEGIN
			EVAL = Event.Value/slopex + xll
			PLOTS.XRANGE(0) = EVAL
			Widget_control, XLowerTxt, SET_VALUE= $
			    strcompress(string(eval,f='(e9.2E3)'),/REMOVE_ALL)
		END

	XLowerTxt: BEGIN
			widget_control,Event.Id, GET_VALUE = Eval
			Eval = double(Eval(0))
			Plots.XRange(0) = Eval
			Widget_control, XLower, $
				SET_VALUE=(slopex*(Eval-xll)<100)>0
		   END
	XUPPER:	BEGIN
			Eval = Event.Value/slopex + xll
			PLOTS.XRANGE(1) = Eval
			Widget_control, XUpperTxt, SET_VALUE= $
			    strcompress(string(Eval,f='(e9.2E3)'),/REMOVE_ALL)
		END
	XUpperTxt: BEGIN
			widget_control,Event.Id, GET_VALUE = Eval
			Eval = double(Eval(0))
			Plots.XRange(1) = Eval
			Widget_control, XUpper, $ 
				SET_VALUE = (slopex*(Eval-xll)<100)>0
		   END
	YLOWER:	BEGIN
			Eval = Event.Value/slopey + yll
			PLOTS.YRANGE(0) = Eval
			Widget_control, YLowerTxt, SET_VALUE= $
			    strcompress(string(eval,f='(e9.2E3)'),/REMOVE_ALL)
		END
	YLowerTxt: BEGIN
			widget_control,Event.Id, GET_VALUE = Eval
			Eval = double(Eval(0))
			Plots.YRange(0) = Eval
			Widget_control, YLower, $
				 SET_VALUE= (slopey*(Eval-yll)<100)>0
		   END
	YUPPER:	BEGIN
			Eval = Event.Value/slopey + yll
			PLOTS.YRANGE(1) = Eval
			Widget_control, YUpperTxt, SET_VALUE= $
			    strcompress(string(Eval,f='(e9.2E3)'),/REMOVE_ALL)
		END
	YUpperTxt: BEGIN
			widget_control,Event.Id, GET_VALUE = Eval
			Eval = double(Eval(0))
			Plots.YRange(1) = Eval
			Widget_control, YUpper, $
				 SET_VALUE= (slopey*(Eval-yll) < 100)>0
		   END
	EXTENDEDX: BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 1 IF NOT SET.
				PLOTS.XSTYLE = PLOTS.XSTYLE OR 2 $
			ELSE	$			;SET BIT 1 IF NOT SET.
				PLOTS.XSTYLE = PLOTS.XSTYLE AND NOT 2
		   END
	EXTENDEDY: BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 1 IF NOT SET.
				PLOTS.YSTYLE = PLOTS.YSTYLE OR 2 $
			ELSE	$			;UNSET BIT 1 IF SET.
				PLOTS.YSTYLE = PLOTS.YSTYLE AND NOT 2
		   END
	SUPPRESSX: BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 2 IF NOT SET.
				PLOTS.XSTYLE = PLOTS.XSTYLE OR 4 $
			ELSE	$			;UNSET BIT 2 IF SET
				PLOTS.XSTYLE = PLOTS.XSTYLE AND NOT 4
		   END
	SUPPRESSY: BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 2 IF NOT SET.
				PLOTS.YSTYLE = PLOTS.YSTYLE OR 4 $
			ELSE	$			;UNSET BIT 2 IF SET
				PLOTS.YSTYLE = PLOTS.YSTYLE AND NOT 4
		   END
	BOXX: 	   BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 3 IF NOT SET.
				PLOTS.XSTYLE = PLOTS.XSTYLE OR 8 $
			ELSE	$			;UNSET BIT 3 IF SET
				PLOTS.XSTYLE = PLOTS.XSTYLE AND NOT 8
		   END
	BOXY: 	   BEGIN
			IF (EVENT.SELECT EQ 1) THEN $	;SET BIT 3 IF NOT SET.
				PLOTS.YSTYLE = PLOTS.YSTYLE OR 8 $
			ELSE	$			;UNSET BIT 3 IF SET
				PLOTS.YSTYLE = PLOTS.YSTYLE AND NOT 8
		   END
	ELSE:	BEGIN
		;IS THE EVENT A SYMBOLLIST OR LINELIST EVENT?
		FOR I=0, ORDNUM -1 DO BEGIN
			IF (EVENT.ID EQ SYMBOLLIST(I)) THEN $
				Ordinate(i).PSYM = event.index $
			ELSE IF (event.id eq SymbolLarger(i)) THEN BEGIN 
				Ordinate(i).SymSize = Ordinate(i).SymSize + 0.2
				widget_control,SymbolSmaller(i), SENSITIVE=1
			endIF $
			ELSE IF (event.id eq SymbolSmaller(i)) THEN BEGIN
				Ordinate(i).SymSize = Ordinate(i).SymSize - 0.2
				IF (Ordinate(i).SymSize le 0.2) THEN $
				    widget_control,SymbolSmaller(i),SENSITIVE=0
			endIF $ 
			ELSE IF (event.id eq LineList(i)) THEN $
				Ordinate(i).Linestyle = event.index -1 $
			ELSE IF (event.id eq LineThicker(i)) THEN BEGIN
				Ordinate(i).Thick = Ordinate(i).Thick + 1 
				widget_control,LineThinner(i), SENSITIVE=1
			endIF $
			ELSE IF (event.id eq LineThinner(i)) THEN BEGIN
				Ordinate(i).Thick = Ordinate(i).Thick - 1
				If (Ordinate(i).Thick le 1.0) THEN $
				    Widget_control,LineThinner(i), SENSITIVE=0
			endIF
		endFOR
		END
	endCASE

	MakePubPlot, X, Y, Plots, Ordinate, Annotation, $
		     XERROR = Xerr, YERROR = Yerr
end

;
;	PUBPLOT
;	This routine creates the main PUBPLOT widget and calls PUBPLOT_EVENT.
;
;	WIDGETS: PUBPLOt widget is created.
;	INPUTS:  See main program prologue
;	OUTPUTS: See main program prologue
;
pro pubplot, xx, yy, x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9, $
	     XERROR=xerror, YERROR=yerror, OUTPUT=output, INPUT=ff

;+
;	XX = X axis values
;	YY = Y axis (ordinate) values.  Y has N elements or is NxM matrix where
;		N is the number of elements in X.
;	X1...X9 = Optional additional X axis inputs.  If they are present, each
;	 	  Xn will be one data set.
;	Y1...Y9 = Optional additional Y axis inputs.  If they are present, each
;		  Yn will be one data set.  
;	FF = Structure returned by previous run of PubPlot, contains
;	NOTE: For each X variable supplied, the corresponding Y variable must
;		also be supplied.  Non-matching X,Y pairs will be ignored.
;	See main prologue at the top of this file.		 
;-
	common PLOTDATA, x, y, OrdNum, xerr, yerr, XErrSet, YErrSet
	common PUBPLOT, Done, PlotPS, Ttext, XTtext, YTtext, LinLin, $
			LinLog, LogLin, LogLog, SymbolList,SymbolLarger, $
			SymbolSmaller, LineList, LineThicker, LineThinner, $
			XLowerTxt, XUpperTxt, YLowerTxt,YUpperTxt, $
			Xlower,Xupper,Ylower,Yupper, XYOutsOn, XYOutsCs, $
			XYOutsRM, XYOutLarger, XYOutSmaller, XYOutThicker, $
			XYOutNarrower, ErrorBarOn, LegendOn, TitleLarger, $
			TitleSmaller, TitleThicker, TitleNarrower, $
			ExtendedX, ExtendedY, SuppressX, SuppressY, $
			BoxX, BoxY, PlotDateOn, slopex, slopey, xll, yll
	common PUBPLOTARCH, Base4xsz, BigOrdScrX, BigOrdScrY, AxisStyX, $
			AxisStyY, AxisStySz, AxisStyL1, AxisStyL2, AxisStyL3, $
			TitleOffset, XTitleOffset, YTitleOffset, WFont, $
			ErrBYSz, RangeOff0, RangeOff1, RangeOff2, XRangeOff
	common PLOTOUT, Plots, Ordinate, Annotation
	common WLEGDATA, OrdLabel, LegLarger, LegSmaller, $
			 LegBOn, LegBText, LegDone

npar = n_params()
IF (npar lt 2) THEN BEGIN
	message,/inf,"You must supply data to be plotted."
	message,/inf,"Calling Sequence: PUBPLOT, X, Y, [,XERR=xerr, "+ $
		     "YERR=yerr, OUT=out, INP=inp]"
	return
endIF

;These steps are necessary because variables can not be both parameters and 
;in common blocks.  Also, if the user supplies multiple vectors rather than
;a single array, use "prepubplot" to combine them into a single array.

	IF (n_params(0) gt 2) THEN BEGIN
		PrePubPlot,x,xx,x1,x2,x3,x4,x5,x6,x7,x8,x9
		PrePubPlot,y,yy,y1,y2,y3,y4,y5,y6,y7,y8,y9
	endIF ELSE BEGIN
		x = xx
		y = yy
	endELSE
	XErrSet = keyword_set(XERROR)
	YErrSet = keyword_set(YERROR)
	if (XerrSet) then xerr = xerror 
	if (YerrSet) then yerr = yerror

	szy = size(y)
	szx = size(x)
	IF (szy(0) eq 2) THEN OrdNum = szy(2) else Ordnum = 1
	IF ((OrdNum gt 1) AND (szx(0) eq 1)) THEN BEGIN
		xtmp = Y
		FOR i=0, OrdNum -1 DO xtmp(*,i) = X
		X = xtmp
	endIF
	szxerr= size(xerr)
	IF ((OrdNum gt 1) AND (szxerr(0) eq 1)) THEN BEGIN
		xtmp = Y
		FOR i=0, OrdNum -1 DO xtmp(*,i) = xerr
		xerr = xtmp
	endIF
	szyerr= size(yerr)
	IF ((OrdNum gt 1) AND (szyerr(0) eq 1)) THEN BEGIN
		xtmp = Y
		FOR i=0, OrdNum -1 DO xtmp(*,i) = yerr
		yerr = xtmp
	endIF

;
;	Define Plot structures.  IF a structure is supplied as the third
;	argument, derive structures from there.
;	Note that the default graphic for Ordinate is a solid line.  To 
;	Change it to, say, diamonds without a line, the user must select both 
;	the diamond shape and also "none" linestyle.  If none is selected for
;	both the shape and the line style, that vector will not be plotted.
;
szf = size(ff)
IF keyword_set(ff) and (szf(szf(0)+1) eq 8) THEN BEGIN
	Ordinate = ff.Ordinate
	Annotation = ff.Annotation
	Plots = ff.Plots
	ordcount = n_elements(y(0,*)) - n_elements(ordinate)
	IF (ordcount gt 0) THEN $
		Ordinate = [Ordinate,replicate(Ordinate(0),ordcount)]
endIF ELSE BEGIN
	Ordinate = replicate({OrdStruc,PSYM:0, Linestyle:0, SymSize:1.0,$
			      Thick:1.0, ErrBType:0, ErrNum:0.d0, Hat: 0b, $
			      HatLength:2, HatThick:1, XErrBType:0, $
			      XErrNum:0.d0, XHat: 0b, XHatLength:2, $
			      XHatThick:1, LegendItem:1b, Title:""}, OrdNum)
	Annotation = {AnnotStruc, X:0.0, Y:0.0, Text:"", CharSize:1.0,$
		      CharThick:1.0, Font:0, Orientation:0.0}
	Plots = {PlotStruc, Title:"",Xtitle:"",Ytitle:"",$
		Xrange:float([min(x),max(x)]),Yrange:float([min(y),max(y)]), $
		Type:0b, $
		XStyle:1, YStyle:1, PlotDate:0, Landscape: 0b, PSSize:[7.,7.],$
		PSUnits: 0b, $
		AxisTypes:[0,0],AxisStyles:[0,0], CharSize:1.0, CharThick:1.0,$
		ACharSize:1.0, ACharThick:1.0, LegendOn: 0, LegendSize: 1.0, $
		LegendBox: 1, LegendX: 0.85, $
		LegendY: 0.50, LegendDelChar: "" }
endELSE

;Type: 0 for lin-lin, 1 for lin-log, 2 for log-lin, 3 for log-log

;SET_ARCH will check the !VERSION.ARCH keyword and properly set the
;items in the PUBPLOTARCH common block
	SET_ARCH

;Qbase is the overall base widget
	qbase = widget_base(title="Publication Plotting",/Row,/frame)

;Pbase is the base widget for all of the "global" stuff.
	pbase = widget_base(Qbase,/Column)


;Obase is the base widget containing everything other than the text 
;
	obase = widget_base(pbase,/Row,/Frame)

;Done/ PostScript start buttons
	base = widget_base(obase,/column,/frame)
	buttonbase = widget_base(base,/column,/frame )
	done = widget_button(buttonbase,value='DONE',FONT=WFont)
	plotps = widget_button(buttonbase,value='PLOT to PostScript',$
			       FONT=WFont)

;Axis-Type buttons
	axisbase = widget_base(base,/column,/frame,title="Axis Types")
	LinLin = widget_button(axisbase,value='LINEAR - LINEAR',FONT=WFont)
	LinLog = widget_button(axisbase,value='LINEAR - LOG',FONT=WFont)
	LogLin = widget_button(axisbase,value='LOG - LINEAR',FONT=WFont)
	LogLog = widget_button(axisbase,value='LOG - LOG',FONT=WFont)

;Plot range slider widgets
	base2 = widget_base(base,/column)
	label1 = widget_label(base2,value='AXIS RANGES',FONT=WFont)
	rbase = widget_base(base2,/row)
	rangebase1 = widget_base(rbase,/column)
	rangebase2 = widget_base(rbase,/column)
	xuu = 2.*max(x) - total(x)/n_elements(x)
	xll = 2.*min(x) - total(x)/n_elements(x)
 	yuu = 2.*max(y) - total(y)/n_elements(y)
	yll = 2.*min(y) - total(y)/n_elements(y)
	slopex = 100./(xuu-xll)
	slopey = 100./(yuu-yll)

	ylowerbase = widget_base(rangebase1,/FRAME)
	ylowertxt = widget_text(ylowerbase, VALUE=string(min(y),f='(e9.2E3)'),$
			/EDITABLE, $
			XSIZE=9,/FRAME, YOFFSET=RangeOff0,XOFFSET=XRangeOff(0))
	ylower = widget_slider(ylowerbase, MAXIMUM= 100,MINIMUM=0, $
			VALUE=slopey*(min(y)-yll),FONT=WFont,$
			/SUPPRESS_VALUE, $
			YOFFSET=RangeOff1, XOFFSET=XRangeOff(1))
	Bottomlabel = widget_label(ylowerbase,value="   BOTTOM",FONT=WFont, $
			YOFFSET=RangeOff2, XOFFSET=XRangeOff(2))

	yupperbase = widget_base(rangebase2,/FRAME)
	yuppertxt = widget_text(yupperbase, VALUE=string(max(y),f='(e9.2E3)'),$
			/EDITABLE, $
			XSIZE=9,/FRAME, YOFFSET=RangeOff0,XOFFSET=XRangeOff(0))
	yupper = widget_slider(yupperbase, MAXIMUM= 100, MINIMUM= 0, $
			VALUE=slopey*(max(y)-yll),FONT=WFont, $
			/SUPPRESS_VALUE, $
			YOFFSET=RangeOff1, XOFFSET=XRangeOff(1))
	Toplabel = widget_label(yupperbase,value="      TOP",FONT=WFont, $
			YOFFSET=RangeOff2,XOFFSET=XRangeOff(2))

	rangedummy1= widget_label(rangebase1,value=" ",FONT=WFont)
	rangedummy2= widget_label(rangebase2,value=" ",FONT=WFont)

	xlowerbase = widget_base(rangebase1,/FRAME)
	xlowertxt = widget_text(xlowerbase, VALUE=string(min(x),f='(e9.2E3)'),$
			/EDITABLE, $
			XSIZE=9,/FRAME, YOFFSET=RangeOff0,XOFFSET=XRangeOff(0))
	xlower = widget_slider(xlowerbase,MAXIMUM = 100, MINIMUM = 0, $
			VALUE=slopex*(min(x)-xll), FONT=WFont, $
			/SUPPRESS_VALUE, $
			YOFFSET=RangeOff1, XOFFSET=XRangeOff(1))
	leftlabel = widget_label(xlowerbase,value="      LEFT",FONT=WFont, $
			YOFFSET=RangeOff2,XOFFSET=XrangeOff(2))

	xupperbase = widget_base(rangebase2, /FRAME)
	xuppertxt = widget_text(xupperbase, VALUE=string(max(x),f='(e9.2E3)'),$
			/EDITABLE, $
			XSIZE=9,/FRAME, XOFFSET = XRangeOff(0))
	xupper = widget_Slider(xupperbase,MAXIMUM = 100, MINIMUM = 0,$
			VALUE=slopex*(max(x)-xll),FONT=WFont,/SUPPRESS_VALUE, $
			YOFFSET=RangeOff1, XOFFSET=XRangeOff(1))
	rightlabel = widget_label(xupperbase,value="    RIGHT",FONT=WFont, $
			YOFFSET=RangeOff2, XOFFSET=XRangeOff(2))

;Annotation widgets
	base3 = widget_base(obase,/column,/frame)
	xyoutbase = widget_base(base3,/column,/frame)
	XYOutsOn = widget_button(xyoutbase,value="ADD ANNOTATION",FONT=WFont)
	XYOutsCs = widget_button(xyoutbase,value="CUSTOM ANNOTATION",$
				 FONT=WFont)
	XYOutsRm = widget_button(xyoutbase,value="REMOVE ANNOTATION",$
				 FONT=WFont)
	xysizebase1 = widget_base(xyoutbase,/row)
	xysizebase2 = widget_base(xyoutbase,/row)
	XYOutLarger = widget_button(xysizebase1,value= "  LARGER  ",FONT=WFont)
	XYOutSmaller = widget_button(xysizebase1,value="  SMALLER ",FONT=WFont)
	XYOutThicker = widget_button(xysizebase2,value="  THICKER ",FONT=WFont)
	XYOutNarrower = widget_button(xysizebase2,value="  THINNER ",$
				      FONT=WFont)
;Plots.ACharThick starts at the minimum useful value (1.0), so the 
;"Annotation Narrower" button should start insensitive
	widget_control,XYOutNarrower, SENSITIVE = 0
;Since there are no annotation strings to remove r customize, "CUSTOMIZE" and
;"REMOVE" buttons should start insensitive
	widget_control,XYOutsCs, SENSITIVE = 0
	widget_control,XYOutsRM, SENSITIVE = 0

;Create buttons to activate error bar customization routines
	ErrorBarBase = widget_base(base3,/column)
	ErrorBarOn = widget_button(ErrorBarBase,value="ERROR BARS",FONT=WFont)
	LegendBase = widget_base(base3,/column)
	LegendOn = widget_button(LegendBase,value="LEGEND",FONT=WFont)
	PlotDateBase = widget_base(base3,/column,/nonexclusive)
	PlotDateOn = widget_button(PlotDateBase,value="ADD PLOT DATE",FONT=WFont)

;Axis style widgets
	stylebase = widget_base(base3,/column,/FRAME)
	stylelabel = widget_label(stylebase,value="AXIS STYLES",FONT=WFont)
	base31 = widget_base(stylebase)
	StyleLabelx = widget_label(base31,value='X',xoffset=AxisStyX,$
				   FONT=WFont)
	StyleLabelY = widget_label(base31,value='Y',xoffset=AxisStyY,$
				   FONT=WFont)
	base4 = widget_base(base31,/row)		
	base4t = widget_base(base4)
	base4x = widget_base(base4,/column,/NONEXCLUSIVE,xsize=AxisStySz)
	base4y = widget_base(base4,/column,/NONEXCLUSIVE,ysize=AxisStySz)
	ExtendLabel = widget_label(base4t,value="EXTENDED RANGE",FONT=WFont, $
				   YOFFSET = AxisStyL1)
	SuppressLabel = widget_label(base4t,value="SUPPRESS AXES",FONT=WFont, $
				   YOFFSET = AxisStyL2)
	BoxLabel = widget_label(base4t,value="SUPPRESS BOX-STYLE",FONT=WFont, $
				   YOFFSET = AxisStyL3)
	ExtendedX = widget_button(base4x,value=" ",FONT=WFont)
	SuppressX = widget_button(base4x,value=" ",FONT=WFont)
	BoxX = widget_button(base4x,value=" ",FONT=WFont)
	ExtendedY = widget_button(base4y,value=" ",FONT=WFont)
	SuppressY = widget_button(base4y,value=" ",FONT=WFont)
	BoxY = widget_button(base4y,value=" ",FONT=WFont)

	
;Create Text widgets for plot titles
	titlesbase = widget_base(pbase,/column,/frame)
	textbase = widget_base(titlesbase,/row)
	textbase1 = widget_base(textbase)
	textbase2 = widget_base(textbase,/column)
	tbuttonsbase1 = widget_base(textbase,/column)
	tbuttonsbase2 = widget_base(textbase,/column)
	 tlabel = widget_label(textbase1,value='TITLE: ',yoffset=TitleOffset,$
			       FONT=WFont)
 	 ttext = widget_text(textbase2,/EDITABLE,FONT=WFont,/FRAME)
	 xtlabel = widget_label(textbase1,value='X TITLE:',$
				yoffset=XTitleOffset,FONT=WFont)
	 xttext = widget_text(textbase2,/EDITABLE,FONT=WFont,/FRAME)
	 ytlabel = widget_label(textbase1, value='Y TITLE:',$
				yoffset=YTitleOffset,FONT=WFont)
	 yttext = widget_text(textbase2,/EDITABLE,FONT=WFont,/FRAME)
	 TitleLarger = widget_button(tbuttonsbase1,value="TITLES LARGER  ",$
				     FONT=WFont)
	 TitleSmaller= widget_button(tbuttonsbase2,value="TITLES SMALLER ",$
				     FONT=WFont)
 	 TitleThicker = widget_button(tbuttonsbase1,value="TITLES THICKER ",$
				     FONT=WFont)
	 TitleNarrower = widget_button(tbuttonsbase2,value="TITLES THINNER ",$
				     FONT=WFont)
	
;Plots.CharThick starts at the minimum useful value (1.0), so the "TITLES
;Narrower" button widget should start insensitive.
	widget_control,TitleNarrower,SENSITIVE = 0


;Lists for plot symbols and line types.
;Since there can be an arbitrary number of ordinates, we will provide room for
;an arbitrary number of symbol/line lists, and we will extend the widget as 
;needed.
	BigOrdBase = widget_base(qbase,/row,/scroll,x_scroll_size=BigOrdScrX,$
				y_scroll_size=BigOrdScrY)
	symbollist = lonarr(OrdNum)
	symbollarger= symbollist
	Symbolsmaller = symbollist
	Linelist = symbollist
	LineThicker = symbollist
	LineThinner= Symbollist
	ordinatebase = symbollist
	Ordsizebase1 = symbollist
	OrdSizeBase2 = symbollist
	ordlabel = symbollist
	symbolbase = lonarr(float(OrdNum)/3. + .9) ;Number of bases = number
						   ;of ordinates/3, rounded up

	symbols = ['None','Plus','Asterisk','Dot','Diamond','Triangle',$
		   'Square','X','Histogram','Circle','Filled Circle',  $
		   'Filled Diamond', 'Filled Triangle', 'Filled Square', $
		   'Star','Filled Star','Pentagon','Filled Pentagon', $
		   'Hexagon','Filled Hexagon']

	linestyles = ['None','Solid','Dotted','Dashed','Dash-Dot',$
		      'Dash-Dot-Dot-Dot','Long Dashes']
	
	FOR I=0, Ordnum-1 do BEGIN
		IF (i/3. eq fix(i/3.) ) then $
		     symbolbase(i/3) = widget_base(BigOrdBase,/column)
		Ordinate(i).Title = string(i,f='("Data Set #",i3)')
		ordlabel(i) = widget_label(symbolbase(i/3),value=$
			Ordinate(i).Title, FONT=WFont)
		OrdinateBase(i) = widget_base(symbolbase(i/3),/Row)
		OrdSizeBase1(i) = widget_base(symbolbase(i/3),/Row)
		OrdSizeBase2(i) = widget_base(symbolbase(i/3),/Row)
		symbollist(i) = widget_list(OrdinateBase(i),value=symbols,$
						ysize=5,FONT=WFont)
		linelist(i) = widget_list(Ordinatebase(i),value=linestyles,$
						ysize=5,FONT=WFont)
		symbollarger(i)= widget_button(OrdSizeBase1(i),$
						value=" LARGER ",FONT=WFont)
		symbolsmaller(i)=widget_button(OrdSizeBase1(i),$
						value=" SMALLER",FONT=WFont)
		linethicker(i) = widget_button(OrdSizeBase2(i),$
						value=" THICKER",FONT=WFont)
		LineThinner(i) = widget_button(OrdSizeBase2(i),$
						value=" THINNER",FONT=WFont)
;Since the initial value for Ordinate.Thick is already at its minimum useful
;value (1.0), start with " THINNER" button deactivated.
		widget_control,LineThinner(i),SENSITIVE=0
	endFOR
;
;...LET THERE BE WIDGETS
;
	widget_control,/REALIZE,pbase

	IF !D.Window eq -1 then BEGIN
		WINDOW,/FREE
	endIF

	MakePubPlot, X, Y, Plots, Ordinate, Annotation, $
		     XERROR = Xerr, YERROR = Yerr
	
	xmanager,'PubPlot',qbase,event_handler='PUBPLOT_EVENT',/MODAL
	
	OUTPUT =  {Plots:plots, Ordinate:ordinate, Annotation:annotation}

end

