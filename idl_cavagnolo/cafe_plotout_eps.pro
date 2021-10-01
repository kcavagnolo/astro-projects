PRO cafe_plotout_eps, env, file,                      $
                      color=color,                    $
                      landscape=landscape,            $
                      a4=a4,                          $
                      apj1col=apj1col,aa1col=aa1col,  $
                      aa2col=aa2col,aa14cm=aa14cm,    $
                      times=times,                    $
                      fontsize=fontsize,              $
                      fonttype=fonttype,              $
                      xsize=xsize,ysize=ysize,        $
                      aspect=aspect,scale=scale,      $
                      open=open, close=close,         $ 
                      quiet=quiet,                    $
                      help=help, shorthelp=shorthelp
;+
; NAME:
;           plotout_eps
;
; PURPOSE:
;           Plot out to encapsulated postscript file.
;
; CATEGORY:
;           cafe
;
; SUBCATEGORY:
;           plotout
;
; PLOT FORMAT:
;           Uses current plot settings to print out to encapsulated
;           postscript file. Start color will be set at 0, so
;           foreground color will become black, because background
;           usually is white.  
;
;
; KEYWORDS:
;             fontsize  - Size of font to use (in pt).
;             fonttype  - Which type of font to
;                           -1 - vector drawn fonts
;                            0 - device (system) fonts
;                            1 - truetype fonts.
;                         Default is 0 which gives best results
;                         except when rotation is required (3d-View)
;             xsize     - Size in X-direction in cm.
;             ysize     - Size in Y-direction in cm.
;             aspect    - ratio of y/x. Default is 0.75.
;             scale     - Plots could be scaled up. Default is 1.
;             
; OPTIONS:
;             color     - plot out colorized.
;             landscape - plot out in landscape instead portrait
;                         format.
;             a4        - Plot ut in DIN A4 format. Landscape only.
;             apj1col   - Plot out for 1 column of ApJ. 
;             aa1col    - Plot out for 1 column of A&A. 
;             aa2col    - Plot out for 2 columns of A&A. 
;             aa14cm    - Plot out for full field of  A&A. 
;             times     - Use times font. Default is helvetia.
;             quiet     - Do not actually plot out but open/store file
;                         and parameters only.
;             
; SETUP:
;           All keywords/options may be set with the "set"
;           command. The command prefix is "plotout_eps".
;               
; SIDE EFFECTS:
;           Plots current view into postscript file.
;
;           If the file is not changed multiple plot images are saved
;           into a numbered files. 
;
; EXAMPLE:
;
;               > plotout, graph.eps[color]
;               -> plots to file "graph.eps", using standard postscript
;               format, and colors. Repeating plotout will yield
;               "graph.eps", "graph1.eps", "graph2.eps"....
;
; HISTORY:
;           $Id: cafe_plotout_eps.pro,v 1.1 2007-09-24 20:04:20 cavagnolo Exp $
;             
;-
;
; $Log: cafe_plotout_eps.pro,v $
; Revision 1.1  2007-09-24 20:04:20  cavagnolo
; *** empty log message ***
;
; Revision 1.11  2004/03/24 13:50:32  goehler
; quiet setplot
;
; Revision 1.10  2004/03/23 17:49:44  goehler
; fix: allow font type setting
;
; Revision 1.9  2004/01/02 05:31:41  goehler
; fix: actually not encapsulated flag set
;
; Revision 1.8  2003/10/28 13:48:15  goehler
; fix: do not use default start color 0 when startcolor set
;
; Revision 1.7  2003/09/16 07:37:43  goehler
; Update for setup documentation, minor errors corrected.
;
; Revision 1.6  2003/07/25 18:52:45  goehler
; fix: now dots allowed in filenames (extension is the last part after dot)
;
; Revision 1.5  2003/06/18 13:23:07  goehler
; fix: propagate setup properly through cafegetparam by using command line options
;
; Revision 1.4  2003/06/17 15:40:58  goehler
; added possibility to preset the plotout options
;
; Revision 1.3  2003/05/28 10:02:00  goehler
; added quiet option which does not actually plot
;
; Revision 1.2  2003/05/16 18:47:47  goehler
; updated name according common convention (task_subtask)
;
; Revision 1.1  2003/03/11 14:36:56  goehler
; initial version for plotout of eps files (to different files)
;
;

    ;; command name of this source (needed for automatic help)
    name="plotout_eps"

    ;; ------------------------------------------------------------
    ;; HELP
    ;; ------------------------------------------------------------
    ;; if help given -> print the specification above (from this file)
    IF keyword_set(help) THEN BEGIN
        cafe_help,env, name
        return
    ENDIF 


    ;; ------------------------------------------------------------
    ;; SHORT HELP
    ;; ------------------------------------------------------------
    IF keyword_set(shorthelp) THEN BEGIN  
        print, "eps      - encapsulated postscript plot driver"
        return
    ENDIF

    ;; ------------------------------------------------------------
    ;; FILE SETUP
    ;; ------------------------------------------------------------

    ;; extraction. expect extension (otherwise garbage..)
    fileitems=stregex(file,"(.*)(\.[^.]*)",/extract,/subexpr)
    filechunk = fileitems[1]
    fileext   = fileitems[2]



    ;; ------------------------------------------------------------
    ;; CLOSE FILE - NOTHING TO DO
    ;; ------------------------------------------------------------

    IF keyword_set(close) THEN return 
    

    ;; ------------------------------------------------------------
    ;; OPEN FILE - REMOVE FORMER SET
    ;; ------------------------------------------------------------

    IF keyword_set(open) THEN BEGIN ;; remove former list of files:

        ;; list of filename plus number:
        filelst1 = findfile(filechunk+"???"+fileext)

        ;; file existing -> must delete it:
        IF (findfile(file))[0] NE "" THEN BEGIN 
            IF filelst1[0] NE "" THEN   $ ; also numbered files found ->
              filelst = [file,filelst1] $ ; add them to list
            ELSE                        $
              filelst = file              ; otherwise sole filename
        ENDIF ELSE BEGIN                  ; file not found -> use numbered files only
            filelst = filelst1
        ENDELSE 

        ;; some found -> delete them:
        IF filelst[0]  NE "" THEN BEGIN 
            file_delete, filelst
        ENDIF 
    ENDIF         


    ;; ------------------------------------------------------------
    ;; CREATE FILE NAME 
    ;; ------------------------------------------------------------

    ;; create nominal file:
    filestr = file

    ;; create new file name:
    i = 0
    WHILE (findfile(filestr))[0] NE "" DO BEGIN 
        filestr = filechunk+string(i,format="(I3.3)")+fileext
        i = i+1
    ENDWHILE

    ;; ------------------------------------------------------------
    ;; SETUP
    ;; ------------------------------------------------------------


    cafegetparam, landscape, "plotout_eps:landscape",  (*env).setup, landscape
    cafegetparam, color,     "plotout_eps:color",      (*env).setup, color
    cafegetparam, xsize,     "plotout_eps:xsize",      (*env).setup, xsize
    cafegetparam, ysize,     "plotout_eps:ysize",      (*env).setup, ysize
    cafegetparam, aspect,    "plotout_eps:aspect",     (*env).setup, aspect
    cafegetparam, scale,     "plotout_eps:scale",      (*env).setup, scale
    cafegetparam, a4,        "plotout_eps:a4",         (*env).setup, a4
    cafegetparam, fontsize,  "plotout_eps:fontsize",   (*env).setup, fontsize
    cafegetparam, fonttype,  "plotout_ps:fonttype",   (*env).setup, fonttype
    cafegetparam, times,     "plotout_eps:times",      (*env).setup, times
    cafegetparam, apj1col,   "plotout_eps:apj1col",    (*env).setup, apj1col
    cafegetparam, aa1col,    "plotout_eps:aa1col",     (*env).setup, aa1col
    cafegetparam, aa2col,    "plotout_eps:aa2col",     (*env).setup, aa2col
    cafegetparam, aa14cm,    "plotout_eps:aa14cm",     (*env).setup, aa14cm


    ;; set defaults:
    IF n_elements(landscape) EQ 0 THEN landscape = 0
    IF n_elements(xsize) EQ 0 THEN xsize = 17.78
    IF n_elements(scale) EQ 0 THEN scale =1.0
    IF n_elements(fontsize) EQ 0 THEN fontsize = 12
    IF n_elements(fonttype) EQ 0 THEN fonttype = 0


    ;; save current plot options:
    plotstore=(*env).plot

    ;; save current font:
    savefont = !P.FONT

    ;; save current device:
    savedevice = !D.NAME
    
    ;; set default first color -> 0:
    IF keyword_set(color) THEN BEGIN 
        FOR panel = 0,n_elements((*env).plot.panels)-1 DO $
          cafe_setplot,env,"startcolor="+string(fix(cafegetplotparam(env,"startcolor",panel,0))),/quiet
    ENDIF ELSE BEGIN 
        cafe_setplot,env,"startcolor=0",/quiet
    ENDELSE
    
    ;; no color option -> do not change anything:
    IF NOT keyword_set(color) THEN cafe_setplot,env,"deltacolor=0",/quiet

        
    ;; default y size:
    IF (n_elements(ysize) EQ 0 AND n_elements(aspect) EQ 0) THEN BEGIN 
        aspect=12.700/17.780 ;; default IDL value
    ENDIF 

    ;; a4 is tricky -> must set explizitely:
    IF (keyword_set(a4)) THEN BEGIN 
        
        ;; compute absolute size of a4:
        a4_xsize=29.8 
        aspect=21.1/a4_xsize

        ;; always landscape (?)
        landscape=1

        ;; add margins:
        margin = 2.             ;cm
        xsize = a4_xsize - 2.*margin      

        ;; shift this properly (IDL can't do it itself)
        xoffset=aspect*margin
        yoffset= a4_xsize-margin ;-)      
    ENDIF 

    
    ;; default aspect ratio is almost 1:sqrt(2)
    IF (n_elements(aspect) EQ 0) THEN aspect=0.75


    ;; settings for apj single column:
    IF (keyword_set(apj1col)) THEN BEGIN 
        xsize=8 & times=1
        fontsize=10.
        scale=xsize*12./fontsize
    ENDIF 

    ;; A&A, 1column
    IF (keyword_set(aa1col)) THEN BEGIN 
        xsize=8.8 & times=1
        fontsize=9. ;; same size as caption

        scale=12./fontsize
    ENDIF 

    ;; A&A, 2column
    IF (keyword_set(aa2col)) THEN BEGIN 
        xsize=17. & times=1
        fontsize=9.
        
        scale=12./fontsize
    ENDIF 

    ;; A&A 14cm wide plot, caption at low right corner
    IF (keyword_set(aa14cm)) THEN BEGIN 
        xsize=14. & times=1
        fontsize=9.
        scale=12./fontsize
    ENDIF 
    
    
    
    ;; ------------------------------------------------------------
    ;; OPEN PLOT DEVICE
    ;; ------------------------------------------------------------


    ;; write to postscript file anyway:
    set_plot,'PS'               


    ;; adjust device:
    device,landscape=landscape, color=color,                      $
           xsize=scale*xsize,ysize=scale*aspect*xsize,            $
           bits_per_pixel=8,                                      $
           filename=filestr,times=times,inches=0,helvet=helvetica,$
           xoffset=xoffset, yoffset=yoffset,font_size=fontsize,   $
           encapsulated=1
    
    
    ;; set iso latin character set -> allow angstroem etc. 
    device,/isolatin1
           
    ;; use system font (helvetica or times)
    !p.font=fonttype
    

    ;; ------------------------------------------------------------
    ;; PERFORM PLOTTING 
    ;; ------------------------------------------------------------
    
    ;; plot out what is shown
    IF NOT keyword_set(quiet) THEN $
      cafe_plot,env,/quiet
        

    ;; ------------------------------------------------------------
    ;; RESTORE FORMER STATE 
    ;; ------------------------------------------------------------
    
    set_plot,'PS'               
    device,/close

    ;; now use former device
    set_plot, savedevice
    
    ;; restore plot options:
    (*env).plot = plotstore
        
    ;; restore font:
    !P.FONT=savefont

    ;; ------------------------------------------------------------
    ;; INFORMATION
    ;; ------------------------------------------------------------

    cafereport,env, "Saved plot in: "+filestr
END 

