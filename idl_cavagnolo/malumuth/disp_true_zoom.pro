;+
;                      Zoom True Color Image
;*NAME:	disp_true_zoom.pro
;
;*PURPOSE:  
;           Widget tool for displaying the True Color image within 
;           the TCTOOL zoom window
;
;
;*CATEGORY:  Called by ZOOMWIND
;           
;
;*CALLING SEQUENCE:  disp_true_zoom,zoomfact
;           
;
;*OUTPUTS:     NONE
;       
;
;*KEYWORD PARAMETERS:   NONE
;           
;
;
;
;*EXAMPLES:
;
;*PROCEDURE:  TCTOOL does all that is needed ;          
;
;*SUPPORT PROCEDURES:  NONE
;
;
;*HISTORY:
;	6/99	E Malumuth/RITSS
;-
pro disp_true_zoom,zoomfact
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON timages,tredimage,tgreenimage,tblueimage
   COMMON DRAW51_Comm, DRAW51_Id
;
;
sr=size(tredimage)
sg=size(tgreenimage)
sb=size(tblueimage)
  redimage=rebin(tredimage,sr(1)*zoomfact,sr(2)*zoomfact)
  greenimage=rebin(tgreenimage,sg(1)*zoomfact,sg(2)*zoomfact)
  blueimage=rebin(tblueimage,sb(1)*zoomfact,sb(2)*zoomfact)

wset, DRAW51_Id
tv,redimage,channel=1
tv,greenimage,channel=2
tv,blueimage,channel=3


return
end


