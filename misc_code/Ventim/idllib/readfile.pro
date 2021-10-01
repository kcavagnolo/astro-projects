pro clusters

; .run mapread.pro
; mapread,1,xy,yz,xz
; contour,alog(xy),/fill,nlevels=20,title='Cluster',xrange=[0,250],yrange=[0,250]
; .run clustinfo.pro

  nrecords =  0L
  recnum = 1L
  openr, lun, '/Gadget/ClusterProjections/cluster_info.global', /get_lun
  while(eof(lun) ne 1) do begin
;     on_ioerror, bad_rec
;     error = 1
    readf, lun, x
    print, recnum
;     error = 0
    nrecords = nrecords + 1
    recnum = recnum + 1
  endwhile
;   bad_rec:
;   if(error eq 1) then print, 'Bad data at record '
  print, nrecords
  close, lun
  return

end
