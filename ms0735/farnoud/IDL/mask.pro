pro mask

im = readfits ('taufim.fits',h)
s = size(im, /dimensions)
N = [s[0],s[1]]

readcol,'mask', xl,yl,xh,yh
l = size(xh,/dimensions)
l = l[0]
for m = 0,(l-1),1 do begin
	for x = xl[m],xh[m],1 do begin
		for y = yl[m],yh[m],1 do begin
			im[x,y] = 0
		endfor
	endfor
endfor

writefits, 'taufim1.fits', im, h
end
