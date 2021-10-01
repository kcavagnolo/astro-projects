PRO SCATTER3D, x, y, z, xmin, xmax, ymin, ymax, zmin, zmax, PostScript=postscript

; Set the PostScript keyword to send draw the plot in a PostScript file
; instead of on the display.

IF Keyword_Set(postscript) THEN BEGIN
   thisDevice = !D.Name
   keywords = PSConfig(Cancel=cancelled)
   IF cancelled THEN RETURN
   Set_Plot, 'PS'
   Device, _Extra=keywords
ENDIF

;   ; Create the random data. Set the seed so you see what I see.
IF (N_ELEMENTS(x) EQ 0) THEN BEGIN
    seed = 1L
    x = RANDOMU(seed, 32)
    y = RANDOMU(seed, 32)
    z = EXP(-3 * ((x - 0.5)^2 + (y - 0.5)^2))
    xmin = min(x)-0.5*min(x)
    xmax = max(x)+0.5*max(x)
    ymin = min(y)-0.5*min(x)
    ymax = max(y)+0.5*max(x)
    zmin = min(z)-0.5*min(x)
    zmax = max(z)+0.5*max(x)
ENDIF

   ; Load a color table and create colors for the scatterplot.
LOADCT, 39
zcolors = BYTSCL(z, TOP=!D.TABLE_SIZE-2)
IF !D.NAME NE 'PS' THEN BEGIN
   TVLCT, 0, 0, 0, !D.TABLE_SIZE-1
   TVLCT, 200, 200, 200, 0
   DEVICE, DECOMPOSED=0, GET_DECOMPOSED=currentState
ENDIF

   ; Set the 3D coordinate space with axes.
SURFACE, DIST(15), /NODATA, /SAVE, XRANGE=[xmin,xmax], $
   YRANGE=[ymin,ymax], ZRANGE=[zmin, zmax], XSTYLE=1, $
   YSTYLE=1, ZSTYLE=1, CHARSIZE=1.5, $
   POSITION=[0.1, 0.1, 0.95, 0.95, 0.1, 0.95], $
   XTICKLEN=1, YTICKLEN=1, XGRIDSTYLE=1, YGRIDSTYLE=1
AXIS, XAXIS=1, /T3D, CHARSIZE=1.5
AXIS, YAXIS=1, /T3D, CHARSIZE=1.5

   ; Plot the random points in 3D space with a filled circle shape.
phi = Findgen(32) * (!PI * 2 / 32.)
phi = [ phi, phi(0) ]
UserSym, Cos(phi), Sin(phi), /Fill
PLOTS, x, y, z, PSYM=8, COLOR=zcolors, SYMSIZE=2.5, /T3D

   ; Connect the data points to the XY plane of the plot.
FOR j=0,n_elements(x)-1 DO PLOTS, [x(j), x(j)], [y(j), y(j)], [0, z(j)], $
   COLOR=zcolors(j), /T3D

   ; Close the PostScript file and clean-up, if required.
IF Keyword_Set(postscript) THEN BEGIN
   Device, /Close_File
   Set_Plot, thisDevice
ENDIF

   ; Set color decomposition back to default.
IF !D.NAME NE 'PS' THEN BEGIN
   DEVICE, DECOMPOSED=currentState
ENDIF

END
