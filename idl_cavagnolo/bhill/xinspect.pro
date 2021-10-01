;+
;   NAME:                   XINSPECT
;
;   PURPOSE:   This procedure allows the user to browse a big FITS image
;              without having to read the whole thing in.
;
;   CALLING SEQUENCE:
;       XINSPECT, Fitsfile
;
;   POSITIONAL PARAMETER:
;       Fitsfile       String, name of FITS file.
;
;   KEYWORD PARAMETER:
;       RETAIN         Window backup storage parameter (default=2)
;
;   METHOD:   The big image is read in and subsampled for display.
;             The user moves a panning cursor around on the big image
;             by pressing down the mouse button and dragging.
;             When the user lets go of the button, the subimage 
;             corresponding to the pan cursor is read in at full
;             resolution and displayed in another window.  This
;             window has its own pan cursor controlling real-time update
;             of a zoomed subimage.
;      
;   NOTES:
;      The latest version and supporting procedures are 
;      available in http://idlastro.gsfc.nasa.gov/ftp/contrib/bhill/
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, RITSS, 3 Oct 2000
;      Retain=2; doc message.  RSH, 11 Dec 2001
;-
PRO XINSPECT_CLEANUP, Tlb

widget_control, tlb, get_uvalue=info
IF obj_valid((*info).dsp) THEN obj_destroy, (*info).dsp
IF obj_valid((*info).dsp2) THEN obj_destroy, (*info).dsp2
IF ptr_valid((*info).fitsfile) THEN ptr_free, (*info).fitsfile

IF ptr_valid(info) THEN ptr_free, info

RETURN
END

PRO IMGDISPLAY::XINSPECT_BIGPAN, Event

widget_control, event.top, get_uvalue=info

CASE event.type OF
   0:  BEGIN
       step = (*info).step
       xdim = (*info).xdim
       ydim = (*info).ydim
       (*info).dsp2->size, xsize2, ysize2
       xdim2 = (*info).xdim2
       ydim2 = (*info).ydim2
       self->refresh
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
       widget_control, self.maindisp, get_value=gwin
       wset, gwin
       (*info).dsp2->size, boxcolor=boxcolor
       plots, [x0, x0, x1, x1, x0], [y0, y1, y1, y0, y0], /device, $
           color=boxcolor
       widget_control, (*info).xlab, $
           set_value='X:  '+ strn(x0b) + ' - ' + strn(x1b)
       widget_control, (*info).ylab, $
           set_value='Y:  '+ strn(y0b) + ' - ' + strn(y1b)
       fxread, *(*info).fitsfile, dsub, hsub, x0b, x1b, y0b, y1b, 1
       (*info).dsp2->draw, dsub, hsub
   END
   ELSE:
ENDCASE
RETURN
END

PRO XINSPECT, Fitsfile, RETAIN=retain
    
IF n_params(0) LT 1 THEN BEGIN
    print,'CALLING SEQUENCE:  XINSPECT, FitsFile'
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

IF n_elements(retain) LT 1 THEN retain=2
device, retain=retain

tlb = widget_base(title='XINSPECT',col=1,group_leader=group_leader)

row1 = widget_base(tlb,row=1)

col1a = widget_base(row1, col=1)
col1b = widget_base(row1, col=1)

dsp = obj_new('EXAMDISPLAY', col1a, xsize=xdim2, ysize=ydim2, $
                         /kill_button, $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=0, $
                         panwin=0,readout=0, $
                         refresh=1, /noexamine)

xlab = widget_label(col1a, value='X:  ', xsize=300)
ylab = widget_label(col1a, value='Y:  ', xsize=300)

dsp2 = obj_new('EXAMDISPLAY', col1b, xsize=400, ysize=400, $
                         log_tag='xinspect', $
                         x_scroll_size=0, y_scroll_size=0, $
                         /ps_dialog,zoomwin=150, $
                         panwin=0,readout=1, $
                         refresh=0)

widget_control, tlb, /realize
;
;  Get graphics window IDs.
dsp2->id, draw_wid2
widget_control, draw_wid2, get_value=gwin2
widget_control, tlb, tlb_set_title='Window '+strn(gwin2)

dsp->draw, d2

dsp->imgdisplay::xy_sensitive, 'xinspect_bigpan', /repeat_events

info = ptr_new({tlb:tlb, dsp:dsp, dsp2:dsp2, $
                xlab:xlab, ylab:ylab, $
                step:step, xdim:xdim, ydim:ydim, $
                xdim2:xdim2, ydim2:ydim2, $
                fitsfile:ptr_new(fitsfile)})

widget_control, tlb, set_uvalue=info

xmanager, 'xinspect', tlb, /no_block, cleanup='xinspect_cleanup'

RETURN
END
