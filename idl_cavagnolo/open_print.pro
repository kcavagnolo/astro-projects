pro open_print,file, color=color,a4=a4,postscript=postscript, $
               apj1col=apj1col,aa1col=aa1col,aa2col=aa2col,aa14cm=aa14cm, $
               aa12cm=aa12cm,aspect=aspect,scale=scale,fontsize=fontsize, $
               xsize=xsize,ysize=ysize,times=times,printing=printing
;+
; NAME:
;
;
;
; PURPOSE: Procedure to set up plotting to ps-files
;
;
; CATEGORY: misc
;
;
;
; CALLING SEQUENCE:
; open_print,file, color=color,a4=a4,postscript=postscript, $
;               apj1col=apj1col,aa1col=aa1col,aa2col=aa2col,aa14cm=aa14cm, $
;               aa12cm=aa12cm,aspect=aspect,scale=scale,fontsize=fontsize, $
;               xsize=xsize,ysize=ysize,times=times,printing=printing
;
;
; INPUTS:
;       file: name of the eps-file to be generated (default: idl.ps)
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;        color: use color
;        a4: output to A4 paper
;        postscript: generate postscript, not encapsulated postscript
;        apj1col: scale output appropriate for 1column figures in
;                 the Astrophysical Journal (and similar journals)
;        aa1col : dito, for Astronomy and Astrophysics
;        aa2col : output for Astronomy and Astrophysics 2column
;                 figures
;        aa12cm : output for 12cm wide A&A figures (these are the ones
;                 with a caption on the side of the figure in the new
;                 Editions de Physique format)
;        aspect=: define the aspect ratio of the figure, if one of the
;                 aa1col, aa2col, aa12cm, or apj1col keywords is set
;                 (the aspect ratio is the ratio y-size/x-size)
;        scale  : scale fonts etc. do not use except when you know
;                 what you are doing
;        fontsize: set the fontsize (in pt). Do NOT use with the
;                 aa and apj options!
;        xsize,ysize: size of the eps. Do NOT use with the AA and Apj
;                 related options. Use aspect instead.
;        times : if set, use Times Roman font (default is helvetica,
;                except for the AA and Apj related options).
;        printing: if this keyword is given, then open_print is only
;                  executed if printing ne 0 (to simplify writing
;                  programs that go either on the screen or on a
;                  file, depending on how a switch is set)
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;            the plot device is switched over to the PS device
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;           see code
;
;
; EXAMPLE:
;          printing=1
;          if keyword_set(printing) then open_print,'fig1.eps',/aa1col
;          plot,[0,1],[0,1]
;          if keyword_set(printing) then close_print,/ghost
;
;
; MODIFICATION HISTORY:
; J.W., 1994,1996
; 1997.08.27: added a4 keyword
; 1998.01.21: now device,encapsulated=0 when turning on postscript
;             (perhaps that helps when switching back and forth
;             between PS and EPS...)
; 2001.06.17: switch to 8 bits per pixel for color figures
; 2002.02.27: complete rewrite; added various formats for astronomy
;             and astrophysics 
; 2002.03.06: fixed problem with color printing, don't understand why
;             the old way of J.W. doesn't work
; $Log: open_print.pro,v $
; Revision 1.1  2007-09-04 16:57:21  cavagnolo
; *** empty log message ***
;
; Revision 1.12  2005/09/24 16:15:07  wilms
; added printing option
;
; Revision 1.11  2005/06/02 21:14:46  wilms
; added aa12cm option
;
; Revision 1.10  2004/09/24 13:50:27  wilms
; added Doc Header
;
; Revision 1.9  2002/08/06 08:46:55  goehler
; Changed x/ysize setting for /a4 style, added explicit
; x/yoffset settings which are necessary for landscape
; in general and the /a4 style in special.
;
; Revision 1.8  2002/07/16 18:07:51  wilms
; automatic logging needs dollars, not percent signs
;
;-

  common plotstuff, filename,savefont

  IF (n_elements(printing) NE 0) THEN BEGIN 
      IF (printing EQ 0) THEN return
  ENDIF 

  savefont=!p.font

  IF (n_elements(file) EQ 0) THEN file='idl.ps'
  filename=file

  set_plot,'PS'                    ; create output for postscript

  encapsulated=1
  IF (keyword_set(postscript)) THEN encapsulated=0

  landscape=0

  IF (file_exist(file)) THEN BEGIN 
      spawn,['/bin/rm',file],/noshell
  ENDIF 


  IF (n_elements(scale) EQ 0) THEN scale=1.

  IF (keyword_set(a4)) THEN BEGIN 
      
      ;; compute absolute size of a4:
      a4_xsize=29.8 
      aspect=21.1/a4_xsize

      ;; always landscape (?)
      landscape=1

      ;; add margins:
      margin = 2.;cm
      xsize = a4_xsize - 2.*margin      

      ;; shift this properly (IDL can't do it itself)
      xoffset=aspect*margin
      yoffset= a4_xsize-margin ;-)      
  ENDIF 
  
  IF (keyword_set(color)) THEN color=1 ELSE color=0

  ;; aspect ratio
  IF (n_elements(aspect) EQ 0) THEN aspect=0.75

  IF (n_elements(fontsize) EQ 0) THEN fontsize=12.
  IF (n_elements(times) EQ 0) THEN times=0

  IF (times EQ 0) THEN helvetica=1

  
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

  ;; A&A, 1column
  IF (keyword_set(aa2col)) THEN BEGIN 
      xsize=17. & times=1
      fontsize=9.

      scale=12./fontsize
  ENDIF 

  ;; A&A 14cm wide plot, caption at low right corner
  IF (keyword_set(aa14cm)) THEN BEGIN 
      message,'WARNING: THE AA14CM FORMAT IS DEPRECATED, USE AA12CM',/info
      xsize=14. & times=1
      fontsize=9.

      scale=12./fontsize
  ENDIF 

  ;; A&A 12cm wide plot, caption at low right corner
  IF (keyword_set(aa12cm)) THEN BEGIN 
      xsize=12. & times=1
      fontsize=9.

      scale=12./fontsize
  ENDIF 

  IF (n_elements(xsize) EQ 0) THEN BEGIN 
      xsize=17.78 ;; idl default value
      IF (n_elements(ysize) EQ 0 AND n_elements(aspect) EQ 0) THEN BEGIN 
          aspect=12.700/17.780 ;; default IDL value
      ENDIF 
  ENDIF 

  device,encapsulated=encapsulated,landscape=landscape, $
    xsize=scale*xsize,ysize=scale*aspect*xsize,color=color,bits_per_pixel=8, $
    filename=file,times=times,inches=0,helvet=helvetica,                     $
    xoffset=xoffset, yoffset=yoffset
  device,/isolatin1
  !p.font=0                     ; use system font (helvetica or times)


  ;; setting the font size is more difficult
  ;; the following is how it should work...
  ;; default values for 12pt font -- cannot use !d structure
  ;; because that might have been changed from a previous
  ;; call to open_print
  ;  wi=222. ;x_ch_size
  ;  he=352. ;y_ch_size
  ;  device,set_character_size=[wi,he]*fontsize/12.
  ;..the problem is that I never got it to work --> see the cludge
  ;above in the A&A section to get 10pt fonts...
  
END 
