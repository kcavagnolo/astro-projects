pro open,style,outfile
;
;  This procedure produces and prints a landscape plot using ALL
;

set_plot,'PS'

case style of
'/lcolor'    : begin
                 device,/landscape,/inches,xsize=9.0,ysize=6.5,font_size=10
                 device,/color
	       end
'/pcolor'    : begin
                 device,/portrait,/inches,xsize=6.5,ysize=9.0,font_size=10
                 device,yoffset=2.,xoffset=2.
                 device,/color
               end
'/ocolor'    : begin
                 device,/landscape,/inches,xsize=9.0,ysize=6.5,font_size=10
                 device,/color,/inches,xoffset=0.75,yoffset=9.0
	       end
'/landscape' : begin
                 device,/landscape,/inches,xsize=9.0,ysize=6.5,font_size=10
                 device,/color
	       end
'/portrait'  : begin
                 device,/portrait,/inches,xsize=6.5,ysize=9.0,font_size=10
                 device,yoffset=2.,xoffset=2.
                 device,/color
               end
'/lsquare'   : begin
                 device,/landscape,/inches,xsize=7.0,ysize=6.5,font_size=10
                 device,/color
	       end
'/psquare'   : begin
                 device,/portrait,/inches,xsize=7.0,ysize=6.5,font_size=10
                 device,yoffset=7.0,xoffset=1.5
                 device,/color
	       end
'/bigsquare' : begin
                 device,/landscape,/inches,xsize=9.0,ysize=8.5, $
                        font_size=10,xoffset=-0.2
                 device,/color
	       end
'/encapl'    : begin
                 device,/encapsulated,/landscape,/color,font_size=10
	       end
'/encapp'    : begin
                 device,/encapsulated,/portrait,/color,font_size=10
	       end
else: begin
      print,'Your options are: '
      print,'    /lcolor     -- for LW17 landscape color postscript output'
      print,'    /pcolor     -- for LW17 portrait color postscript output'
      print,'    /ocolor     -- for CLP color postscript output'
      print,'    /landscape  -- for landscape orientation'
      print,'    /portrait   -- for portrait orientation'
      print,'    /lsquare    -- square plot with landscape orientation'
      print,'    /psquare    -- square plot with portrait orientation'
      print,'    /bigsquare  -- larger version of /lsquare'
      print,'    /encapl     -- encapsulated landscape file'   
      print,'    /encapp     -- encapsulated portrait file'   
      end
endcase

device,filename=outfile

return
end
