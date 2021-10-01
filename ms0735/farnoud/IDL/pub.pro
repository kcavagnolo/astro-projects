pro pub

!p.thick=2.5
!P.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5
!p.charsize = 1

set_plot,'x'
set_plot,'ps'
device, filename='pjprad.eps', /encapsulated, /times, /color, xs=10, ys=9
loadct, 13

readcol,'radio',name,lx,pj,pjle,pjue,prad,format='a,d,d,d,d,d'

plot,prad/1E38,pj/1E38,psym=symcat(9), ytitle='P!Dcav!N (10!E38!N erg s!E-1!N)', xtitle='P!Drad,1400!N (10!E38!N erg s!E-1!N)',symsize =0.4, /xlog, /ylog, yrange=[1E1,1E7],/ystyle
oploterror,prad/1E38,pj/1E38,pjue/1E38,/hibar, psym=3
oploterror,prad/1E38,pj/1E38,pjle/1E38,/lobar, psym=3
sx = [prad[0],prad[5]]/1E38
sy = [pj[0], pj[5]]/1E38
ssx = [prad[18],prad[19],prad[20],prad[21]]/1E38
ssy = [pj[18],pj[19],pj[20],pj[21]]/1E38
oplot,sx,sy,psym=symcat(16),symsize =0.4
oplot,ssx,ssy,psym=symcat(14),symsize =0.6
x=[1E-4,0.001,0.01,0.1,1,10,100,1000,10000]
y=10*x
oplot,x,y,linestyle=2,thick=1
xyouts,1.5,20,'10:1', orientation =55,charsize=0.4
y1=100*x
oplot,x,y1,linestyle=2,thick=1
xyouts, 0.15,20,'100:1', orientation =55,charsize=0.4
y2=1000*x
oplot,x,y2,linestyle=2,thick=1
xyouts, 0.015,20,'1000:1', orientation =55,charsize=0.4
y3=10000*x
oplot,x,y3,linestyle=2,thick=1
xyouts, 0.0015,20,'10000:1', orientation =55,charsize=0.4
y4=100000*x
oplot,x,y4,linestyle=2,thick=1
xyouts, 0.00015,20,'100000:1', orientation =55,charsize=0.4
oplot, [0.0002], [6E6],psym=symcat(9),symsize=0.4
xyouts, 0.00024,5.5E6,'Cat. Source',charsize=0.5	
oplot, [0.0002], [4E6],psym=symcat(16),symsize=0.4
xyouts, 0.00024,3.6E6,'Meas. Source',charsize=0.5	
oplot, [0.0002], [2.7E6],psym=symcat(14),symsize=0.5
xyouts, 0.00024,2.4E6,'Meas. Lobes',charsize=0.5	



;readcol,'data',name,lx,pj,pjle,pjue,pjlx,pjlxe,abmk,abmke,nk,nke,fk,fke,sfrn,sfrne,sfrf,sfrfe,mdot,mdotle,mdotue,format='a,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d'

;plot,sfrn,mdot,psym=symcat(16), ytitle='M!DBH!N (M!9!Dn!N!3 yr!E-1!N)', xtitle='SFR!DRe!N (M!9!Dn!N!3 yr!E-1!N)',symsize =0.3, /xlog, /ylog
;oploterror,sfrn,mdot,sfrne,mdotue,/hibar, psym=3,/nohat
;oploterror,sfrn,mdot,sfrne,mdotle,/lobar, psym=3,/nohat
;oplot,sfrf,mdot,psym=symcat(16),symsize=0.3,color=255
;oploterror,sfrf,mdot,sfrfe,mdotue,/hibar, psym=3,/nohat,errcolor=255
;oploterror,sfrf,mdot,sfrfe,mdotle,/lobar, psym=3,/nohat,errcolor=255
;x= [0.001,0.01,0.1,1]
;y = (0.1/0.9)*1.4E-3*x
;xyouts, 0.0012, 2.1E-7,'!7e!3=0.1', orientation =25,charsize=0.4
;y1 = (0.4/0.6)*1.4E-3*x
;xyouts, 0.0012, 1.21E-6,'!7e!3=0.4', orientation =25,charsize=0.4
;y2 = (0.8/0.2)*1.4E-3*x
;xyouts, 0.0012, 8E-6,'!7e!3=0.8', orientation =25,charsize=0.4
;oplot, x,y,linestyle=2,thick=1
;oplot, x,y1,linestyle=2,thick=1
;oplot, x,y2,linestyle=2,thick=1
;oplot, [0.0015], [3E-2],psym=symcat(16),symsize=0.3
;xyouts, 0.0016,2.7E-2,'Near-UV',charsize=0.5	
;oplot, [0.0015], [2E-2],psym=symcat(16),symsize=0.3,color=255
;xyouts, 0.0016,1.8E-2,'Far-UV',charsize=0.5	
;arrow, 0.036,2.35E-3, 0.031,2.35E-3, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.436,2.03E-4,0.38,2.03E-4, fill=0, /data, shaft=0.1, width=2
;arrow, 0.209,2.03E-4,0.18,2.03E-4, fill=0, /data, shaft=0.1, width=2,color=255
;xyouts, 0.000215,1.18E-5, '.'

;plot,sfrn,nk,psym=symcat(16), ytitle='(UV-K)!DRe!N', xtitle='SFR!DRe!N (M!9!Dn!N!3 yr!E-1!N)',symsize =0.3, /xlog, yrange=[-5,6],/ystyle
;oploterror,sfrn,nk,sfrne,nke,psym=3,/nohat
;oplot,sfrf,fk,psym=symcat(16),symsize=0.3,color=255
;oploterror,sfrf,fk,sfrfe,fke,psym=3,/nohat,errcolor=255
;oplot, [0.0015], [-4],psym=symcat(16),symsize=0.3
;xyouts, 0.0016,-4.1,'Near-UV',charsize=0.5	
;oplot, [0.0015], [-4.5],psym=symcat(16),symsize=0.3,color=255
;xyouts, 0.0016,-4.6,'Far-UV',charsize=0.5	
;arrow, 0.036,4.12, 0.036,4.42, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.036,4.12, 0.032,4.12, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.436,1.6,0.436,1.9, fill=0, /data, shaft=0.1, width=2
;arrow, 0.436,1.6,0.38,1.6, fill=0, /data, shaft=0.1, width=2
;arrow, 0.209,4.04,0.209,4.34, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.209,4.04,0.18,4.04, fill=0, /data, shaft=0.1, width=2, color=255

;plot,sfrn,lx/1E38,psym=symcat(16), ytitle='L!DX!N (10!E38!N erg s!E-1!N)', xtitle='SFR!DRe!N (M!9!Dn!N!3 yr!E-1!N)',symsize =0.3, /xlog, /ylog, yrange=[1,1E6],/ystyle
;s = size(lx,/dimensions)
;zero=indgen(s)
;zero=zero*0
;oploterror,sfrn,lx/1E38,sfrne,zero,psym=3,/nohat
;oplot,sfrf,lx/1E38,psym=symcat(16),symsize=0.3,color=255
;oploterror,sfrf,lx/1E38,sfrfe,zero,psym=3,/nohat,errcolor=255
;oplot, [0.0015], [5E5],psym=symcat(16),symsize=0.3
;xyouts, 0.0016,4.7E5,'Near-UV',charsize=0.5	
;oplot, [0.0015], [3E5],psym=symcat(16),symsize=0.3,color=255
;xyouts, 0.0016,2.7E5,'Far-UV',charsize=0.5	
;arrow, 0.036,6.02E4, 0.031,6.02E4, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.436,1.39E4,0.38,1.39E4, fill=0, /data, shaft=0.1, width=2
;arrow, 0.209,1.39E4,0.18,1.39E4, fill=0, /data, shaft=0.1, width=2, color=255

;plot,sfrn,pj/1E38,psym=symcat(16), ytitle='P!Dcav!N (10!E38!N erg s!E-1!N)', xtitle='SFR!DRe!N (M!9!Dn!N!3 yr!E-1!N)',symsize =0.3, /xlog, /ylog, yrange=[1,1E7],/ystyle
;oploterror,sfrn,pj/1E38,sfrne,pjue/1E38,/hibar, psym=3,/nohat
;oploterror,sfrn,pj/1E38,sfrne,pjle/1E38,/lobar, psym=3,/nohat
;oplot,sfrf,pj/1E38,psym=symcat(16),symsize=0.3,color=255
;oploterror,sfrf,pj/1E38,sfrfe,pjue/1E38,/hibar, psym=3,/nohat,errcolor=255
;oploterror,sfrf,pj/1E38,sfrfe,pjle/1E38,/lobar, psym=3,/nohat,errcolor=255
;oplot, [0.0015], [5E6],psym=symcat(16),symsize=0.3
;xyouts, 0.0016,4.7E6,'Near-UV',charsize=0.5	
;oplot, [0.0015], [3E6],psym=symcat(16),symsize=0.3,color=255
;xyouts, 0.0016,2.7E6,'Far-UV',charsize=0.5	
;arrow, 0.036,1.46E5, 0.032,1.46E5, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.436,1.26E4,0.38,1.26E4, fill=0, /data, shaft=0.1, width=2
;arrow, 0.209,1.26E4,0.19,1.26E4, fill=0, /data, shaft=0.1, width=2, color=255

;plot,pjlx,sfrn,psym=symcat(16), ytitle='SFR!DRe!N (M!9!Dn!N!3 yr!E-1!N)', xtitle='P!Dcav!N / L!DX!N',symsize =0.3, /xlog, /ylog
;oploterror,pjlx,sfrn,sfrne,psym=3,/nohat
;oplot,pjlx,sfrf,psym=symcat(16),symsize=0.3,color=255
;oploterror,pjlx,sfrf,sfrfe,psym=3,/nohat, errcolor=255
;x = [1,1,1,1]
;y = [0.001,0.01,0.1,1]
;oplot, x,y,linestyle=2,thick=1
;oplot, [0.15], [0.002],psym=symcat(16),symsize=0.3
;xyouts, 0.16,0.0019,'Near-UV',charsize=0.5	
;oplot, [0.15], [0.0015],psym=symcat(16),symsize=0.3,color=255
;xyouts, 0.16,0.0014,'Far-UV',charsize=0.5	
;arrow, 2.43,0.036, 2.43,0.029, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, 0.9096,0.436, 0.9096,0.36, fill=0, /data, shaft=0.1, width=2
;arrow, 0.9096,0.209, 0.9096,0.17, fill=0, /data, shaft=0.1, width=2, color=255

;plot,abmk,nk,psym=symcat(16), ytitle='(UV-K)!DRe!N', xtitle='M!DK!N',symsize =0.3, xrange=[-18,-27],/xstyle, yrange=[-5,6],/ystyle
;oploterror,abmk,nk,abmke,nke,psym=3,/nohat
;oplot,abmk,fk,psym=symcat(16),symsize=0.3,color=255
;oploterror,abmk,fk,abmke,fke,psym=3,/nohat, errcolor=255
;oplot, x,y,linestyle=2,thick=1
;oplot, [-25.5], [-3.5],psym=symcat(16),symsize=0.3
;xyouts, -25.6,-3.6,'Near-UV',charsize=0.5	
;oplot, [-25.5], [-4],psym=symcat(16),symsize=0.3,color=255
;xyouts, -25.6,-4.1,'Far-UV',charsize=0.5	
;arrow, -24.81,4.12, -24.81,4.52, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, -26.55,4.04, -26.55,4.44, fill=0, /data, shaft=0.1, width=2, color=255
;arrow, -26.55,1.6, -26.55,2, fill=0, /data, shaft=0.1, width=2

;plot, lx/1E38,pj/1E38, psym=symcat(9), ytitle='P!Dcav!N (10!E38!N erg s!E-1!N)', xtitle='L!DX!N (10!E38!N erg s!E-1!N)',symsize =0.3,/xlog,/ylog, yrange=[1, 1E7],/ystyle, xrange=[1,1E7],/xstyle
;oploterror,lx/1E38,pj/1E38,pjue/1E38,/hibar, psym=3
;oploterror, lx/1E38,pj/1E38,pjle/1E38,/lobar, psym=3
;x = [1, 10, 100, 1000, 10000,1E5,1E6,1E7]
;y = x
;oplot, x, y, linestyle=2,thick=1

device, /close
set_plot,'x'
end
