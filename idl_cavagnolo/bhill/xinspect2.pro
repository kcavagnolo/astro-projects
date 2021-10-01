;+
;   NAME:                   XINSPECT2
;
;   PURPOSE:   This procedure allows the user to browse 2 big FITS images
;              without having to read the whole of either one in.
;
;   CALLING SEQUENCE:
;       XINSPECT2, Fitsfile, Fitsf2
;
;   POSITIONAL PARAMETER:
;       Fitsfile       String, name of FITS file.
;       Fitsf2         String, name of FITS file.
;
;   KEYWORD PARAMETER:
;       RETAIN         Window backup storage parameter (default=2)
;
;   METHOD:   Each big image is read in and subsampled for display.
;             The user moves a panning cursor around on one of the
;             big images by pressing down the mouse button and dragging.
;             When the user lets go of the button, the subimage 
;             corresponding to the pan cursor is read from both big
;             images at full resolution and displayed in two other windows.
;             These windows have their own pan cursors controlling 
;             real-time update of zoomed subimages.
;
;   NOTES:
;      The latest version and supporting procedures are 
;      available in http://idlastro.gsfc.nasa.gov/ftp/contrib/bhill/
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, RITSS, 3 Oct 2000
;      Retain=2; doc message.  RSH, 11 Dec 2001
;-
PRO XINSPECT2_CLEANUP, Tlb

widget_control, tlb, get_uvalue=info
IF obj_valid((*info).dsp) THEN obj_destroy, (*info).dsp
IF obj_valid((*info).dsp2) THEN obj_destroy, (*info).dsp2
IF ptr_valid((*info).fitsfile) THEN ptr_free, (*info).fitsfile
IF ptr_valid((*info).fitsf2) THEN ptr_free, (*info).fitsf2

IF ptr_valid(info) THEN ptr_free, info

RETURN
END

PRO EXAMDISPLAY::XINSPECT2_BIGPAN, Event

;  This routine draws a box on the panning window containing a
;  smushed version of the whole big image.  It is set up to be
;  used with the "XY handler" interface of the examdisplay object.

widget_control, event.top, get_uvalue=info

CASE event.type OF
   0:  BEGIN
       step = (*info).step
       xdim = (*info).xdim
       ydim = (*info).ydim
       (*info).dsp2->size, xsize2, ysize2
       xdim2 = (*info).xdim2
       ydim2 = (*info).ydim2
       (*info).dsp->refresh
       (*info).dsp3->refresh
       width = round(float(xsize2)/step)
       halfwidth = round(width*0.5)
       x0 = (event.x - halfwidth) > 0
       y0 = (event.y - halfwidth) > 0
       x0b = x0*step
       y0b = y0*step
       x1b = (x0b + xsize2 - 1) < (xdim-1)
       y1b = (y0b + ysize2 - 1) < (ydim-1)
       x1 = round(float(x1b)/step) < (xdim2-1)
       y1 = round(float(y1b)/step) < (ydim2-1)
       (*info).dsp->id, drawid
       widget_control, drawid, get_value=gwin
       wset, gwin
       (*info).dsp2->size, boxcolor=boxcolor
       plots, [x0, x0, x1, x1, x0], [y0, y1, y1, y0, y0], /device, $
           color=boxcolor
       (*info).dsp3->id, drawid
       widget_control, drawid, get_value=gwin
       wset, gwin
       (*info).dsp4->size, boxcolor=boxcolor
       plots, [x0, x0, x1, x1, x0], [y0, y1, y1, y0, y0], /device, $
           color=boxcolor
       widget_control, (*info).xlab, $
           set_value='X:  '+ strn(x0b) + ' - ' + strn(x1b)
       widget_control, (*info).ylab, $
           set_value='Y:  '+ strn(y0b) + ' - ' + strn(y1b)
       fxread, *(*info).fitsfile, dsub, hsub, x0b, x1b, y0b, y1b, 1
       (*info).dsp2->draw, dsub, hsub
       fxread, *(*info).fitsf2, dsub2, hsub2, x0b, x1b, y0b, y1b, 1
       (*info).dsp4->draw, dsub2, hsub2
   END
   ELSE:
ENDCASE
RETURN
END

PRO XINSPECT2, Fitsfile, Fitsf2, RETAIN=retain
    
IF n_params(0) LT 1 THEN BEGIN
    print,'CALLING SEQUENCE:  XINSPECT2, FitsFile1, FitsFile2'
    print,'KEYWORD PARAMETER:  RETAIN' 
    RETURN
ENDIF

fits_read, fitsfile, 0, h, /header_only
xdim = sxpar(h, 'NAXIS1')
ydim = sxpar(h, 'NAXIS2')

step = ceil(ydim/400.) > ceil(xdim/400.)

fxread, fitsfile, d2, h2, -1, -1, -1, -1, step
sz2 = size(d2)
xdim2 = sz2[1] & ydim2 = sz2[2]
hrebin, h2, outsize=[xdim2,ydim2]

fxread, fitsf2, dx2, hx2, -1, -1, -1, -1, step

IF n_elements(retain) LT 1 THEN retain=2
device, retain=retain

tlb = widget_base(title='XINSPECT2',col=1,group_leader=group_leader)

row1 = widget_base(tlb,row=1)

dsp = obj_new('EXAMDISPLAY', row1, xsize=xdim2, ysize=ydim2, $
                         /kill_button, $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=0, $
                         panwin=0,readout=0, $
                         refresh=1, /noexamine)

;  Subsidiary pan windows are chained together

dsp2 = obj_new('GANGEXAMINE', row1, xsize=200, ysize=200, $
                         log_tag='xinspect2a', $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=150, $
                         panwin=0,readout=1, vertical=0, $
                         refresh=0)

row2 = widget_base(tlb,row=1)

dsp3 = obj_new('EXAMDISPLAY', row2, xsize=xdim2, ysize=ydim2, $
                         kill_button=0, $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=0, $
                         panwin=0,readout=0, $
                         refresh=1,/noexamine)

dsp4 = obj_new('GANGEXAMINE', row2, dsp2, dsp2, xsize=200, ysize=200, $
                         log_tag='xinspect2b', $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=150, $
                         panwin=0,readout=1, vertical=0, $
                         /sync, /blink, refresh=0)

row3 = widget_base(tlb,row=1)

xlab = widget_label(row3, value='X:  ', xsize=300)
ylab = widget_label(row3, value='Y:  ', xsize=300)

widget_control, tlb, /realize
;
;  Get graphics window IDs.
dsp2->id, draw_wid1
dsp4->id, draw_wid2
widget_control, draw_wid1, get_value=gwin1
widget_control, draw_wid2, get_value=gwin2
widget_control, tlb, tlb_set_title='Windows '+strn(gwin1) + $
                                   ' and '+strn(gwin2)

dsp->draw, d2
dsp3->draw, dx2

dsp->imgdisplay::xy_sensitive, 'xinspect2_bigpan', /repeat_events
dsp3->imgdisplay::xy_sensitive, 'xinspect2_bigpan', /repeat_events

info = ptr_new({tlb:tlb, dsp:dsp, dsp2:dsp2, $
                dsp3:dsp3, dsp4:dsp4, $
                xlab:xlab, ylab:ylab, $
                step:step, xdim:xdim, ydim:ydim, $
                xdim2:xdim2, ydim2:ydim2, $
                fitsfile:ptr_new(fitsfile), $
                fitsf2:ptr_new(fitsf2)})

widget_control, tlb, set_uvalue=info

xmanager, 'xinspect2', tlb, /no_block, cleanup='xinspect2_cleanup'

RETURN
END
