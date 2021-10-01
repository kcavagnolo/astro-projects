pro background, imname, ri, rf, ps=ps

!p.thick = 2.5
!p.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5

if (N_params() EQ 0) then begin
   print, 'Syntax - background, imagename, startradius, finalradius,/ps'
   return
endif

if keyword_set(ps) then begin
   p = 1
   goto, PLOT
endif else begin
   p = 0
endelse

im = mrdfits(imname,1,h)
s = size(im, /dimensions)
N = [s[0],s[1]]

print,''
outfile= ' '
read, 'Enter Output Filename: ', outfile
openw,1,outfile,/append
printf,1,'; 1. Radius From Target (px)'
printf,1,'; 2. Mean'
printf,1,'; 3. Median'
printf,1,'; 4. Mode'	
printf,1,'; 5. Standard Deviation'
printf,1
close,1	

print,''
print, 'Pointing at the Target...'
read, 'Enter Target X: ', cx
read, 'Enter Target Y: ', cy

for rad = ri, rf, 25 do begin
   meant = 0
   mediant = 0
   modet = 0
   stdt = 0
   for theta = 0, 340, 20 do begin
      dx = rad * cos(theta)
      dy = rad * sin(theta)
      x = cx + dx
      y = cy + dy
      dist_circle, circ, N, x, y	
      good = where(circ lt 25)
      subim = im[good]
      image_statistics, subim, mean=mean, stddev=std, count=count
      void = max(histogram(subim,omin=mn),mxpos)
      mode = mn + mxpos
      std = (sqrt(count) / count) * mode
      meant = meant + mean
      modet = modet + mode
      stdt = stdt + std^2
   endfor
   meanf = meant / 18
   medianf = mediant / 18
   modef = modet / 18
   stdf = sqrt(stdt) / 18
   openw, 1, outfile, /append
   printf, 1,rad,meanf,modef,stdf
   close, 1
endfor

PLOT:
if (p eq 1) then begin
   filename = ' '
   read, filename, prompt = 'Enter Background File Name: '
   readcol,filename,rad,me,mo,s
   ymax = max(mo)
   ymin = min(mo)
   xmin = ri - 20
   xmax = rf + 20
   erase
   plot, rad, mo, psym=symcat(16), xtitle='Radius from Target (px)', ytitle='Intensity (counts)',yrange=[ymin-0.5,ymax+0.5],/ystyle,xrange=[xmin,xmax],/xstyle
   oploterror, rad, mo, s
   set_plot, 'ps'
   device, filename= 'back.ps', /color,/times, xs=12, ys=11,/bold,/encapsulated
   loadct, 13
   plot, rad, mo, psym=symcat(16),symsize=0.5, xtitle='Radius from Target (px)', ytitle='Intensity (counts)',yrange=[ymin-0.5,ymax+0.5],/ystyle,xrange=[xmin,xmax],/xstyle
   oploterror, rad, mo, s
   set_plot, 'x'
endif
end
