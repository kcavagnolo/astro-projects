PRO pval, infile

ksigma = 3.0
sig = 0.05
fsig = 0.01
vfsig = 0.1
myhome = GETENV('HOME')
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
readcol,infile,FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
        cluster,obsids,x,y,rmax,mincts,z,nh,tx,fe,lbol,chip,eobs,diff,robs,loc

GET_LUN, OUT
OPENW, OUT, 'pval.info'
printf, OUT, format='(A-20,7A15,A5,A15,A5,A10,4A15)',$
        '#Cluster','Ipval_K0','Ipval_Plaw','Ratio',$
        'Fpval_K0','Fpval_Plaw','Ratio','Ipdiff','A/R',$
        'Fpdiff','A/R','Check','IFval','FFval','vIFval','vFFval'
iimprov = 0.
fimprov = 0.
viimprov = 0.
vfimprov = 0.
k0better = 0.
plawbetter = 0.
const = 0.
both = 0.
plaw = 0.
poor = 0.
total = 0.
numplaw = 0.
idiff = 0.
fdiff = 0.
prevname = 'safsdfsdf'
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   IF name EQ prevname THEN GOTO,SKIP
   ord = where(cluster EQ name)
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      temp = obsids[ord]
      FOR j=1,n_elements(temp)-1 DO BEGIN
         obsid = obsid+'_'+strcompress(temp[j],/remove_all)
      ENDFOR
   ENDELSE

   ;# check for file existance
   file = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file,count=count2)
   IF (count2 NE 1) THEN BEGIN
      print,'Missing ',file
      GOTO,SKIP
   ENDIF

   ;# read in data
   datafit = read_ascii(file, template = s_resultstemplate)
   p0=datafit.prob[0]
   p1=datafit.prob[1]
   p2=datafit.prob[2]
   p3=datafit.prob[3]

   d0=datafit.dof[0]
   d1=datafit.dof[1]
   d2=datafit.dof[2]
   d3=datafit.dof[3]

   c0=datafit.chisq[0]
   c1=datafit.chisq[1]
   c2=datafit.chisq[2]
   c3=datafit.chisq[3]
   IF c3 LE c2 THEN print, '>>> ',name
   IF c1 LE c0 THEN print, '>>> ',name

   f1 = ((c1-c0)/(d1-d0)) / (c0/d0)
   f2 = ((c3-c2)/(d3-d2)) / (c2/d2)
   fval1 = mpftest(f1, d1-d0, d0)
   fval2 = mpftest(f2, d3-d2, d2)
   vf1 = (c1/d1)/(c0/d0)
   vf2 = (c2/d2)/(c3/d3)
   vfval1 = mpftest(vf1, d1-d0, d0)
   vfval2 = mpftest(vf2, d3-d2, d2)

   dci = abs(d0*c0-d1*c1)
   dofdci = d0
   dcf = abs(d2*c2-d3*c3)
   dofdcf = d2

   k0=datafit.k0[0]
   k1=datafit.k0[1]
   k2=datafit.k0[2]
   k3=datafit.k0[3]

   ke0=datafit.k0err[0]
   ke1=datafit.k0err[1]
   ke2=datafit.k0err[2]
   ke3=datafit.k0err[3]

   pval0 = mpchitest(c0*d0,d0,/slevel)
   pval1 = mpchitest(c1*d1,d1,/slevel)
   pval2 = mpchitest(c2*d2,d2,/slevel)
   pval3 = mpchitest(c3*d3,d3,/slevel)
   pval4 = mpchitest(dci,dofdci,/slevel)
   pval5 = mpchitest(dcf,dofdcf,/slevel)

;   IF k2 GT 50. THEN GOTO, SKIP
;   IF k2 LT 50. THEN GOTO, SKIP
;   IF k2 LT 4. THEN GOTO, SKIP
   total++
   IF (pval2 LE sig AND pval3 LE sig) THEN BEGIN
       print,'Unacceptable fits ',name,' ',obsid
       poor++
   ENDIF
   IF (pval2 GT sig AND pval3 GT sig) THEN both++
   IF (pval2 GT sig) THEN const++
   IF (pval3 GT sig) THEN plaw++ 
   IF (pval2 GT pval3) THEN k0better++
   IF (pval3 GT pval2) THEN BEGIN
       print,'Plaw better than K0+plaw ',name,' ',obsid
       plawbetter++
   ENDIF
   IF pval4 GT sig THEN ilabel = 'A' ELSE ilabel = 'R'
   IF pval4 GT sig THEN idiff++
   IF pval5 GT sig THEN flabel = 'A' ELSE flabel = 'R'
   IF pval5 GT sig THEN fdiff++
   IF (k2-ksigma*ke2 LE 0.) THEN numplaw++
   IF fval1 LT fsig THEN iimprov++
   IF fval2 LT fsig THEN fimprov++
   IF vfval1 LT vfsig THEN viimprov++
   IF vfval2 LT vfsig THEN vfimprov++
   rat1 = pval0/pval1
   rat2 = pval2/pval3
   IF rat2 LT 100. THEN check = '******' ELSE check = '---'
   printf, OUT, format='(A-20,7E15.3,A5,E15.3,A5,A10,4E15.3)',$
           name,pval0,pval1,rat1,pval2,pval3,rat2,pval4,ilabel,pval5,flabel,check,fval1,fval2,vfval1,vfval2
   push, names, name
   push, obs, obsid
   push, prob0, pval0
   push, prob1, pval1
   push, prob2, pval2
   push, prob3, pval3
   push, allk, k2
   push, allks, k2/ke2
    
SKIP:
prevname = name
ENDFOR

printf, OUT, format='(A-10,5A10)', '# Total','PLAW','Const','Both','Neither','K0=0'
printf, OUT, format='(I-10,5I10)', total, plaw, const, both, poor, numplaw
printf, OUT, format='(A-50,F10.4)', '# Frac well-rep by SINGLE power-law: ',plaw/total
printf, OUT, format='(A-50,F10.4)', '# Frac well-rep by K0+power-law: ',const/total
printf, OUT, format='(A-50,F10.4)', '# Frac well-rep by both: ',both/total
printf, OUT, format='(A-50,F10.4)', '# Frac NOT well-rep by either: ',poor/total
printf, OUT, format='(A-50,F10.4)', '# Frac w/ K0+plaw better than plaw: ',k0better/total
printf, OUT, format='(A-50,F10.4)', '# Frac w/ plaw better than K0+plaw: ',plawbetter/total
printf, OUT, format='(A-50,F10.4)', '# Frac w/ K0=0: ',numplaw/total
printf, OUT, format='(A-50,F10.4)', '# Interp null accepted: ',idiff/total
printf, OUT, format='(A-50,F10.4)', '# Flat null accepted: ',fdiff/total
printf, OUT, format='(A-50,F10.4)', '# Frac improv by add params @ '+num2str(fsig)+': ',fimprov/total
printf, OUT, format='(A-50,F10.4)', '# Frac improv by add params (var-type) @ '+num2str(vfsig)+': ',vfimprov/total
FREE_LUN,OUT

print, format='(A-10,5A10)', '# Total','PLAW','Const','Both','Neither','K0=0'
print, format='(I-10,5I10)', total, plaw, const, both, poor, numplaw
print, format='(A-50,F10.4)', '# Frac well-rep by SINGLE power-law: ',plaw/total
print, format='(A-50,F10.4)', '# Frac well-rep by K0+power-law: ',const/total
print, format='(A-50,F10.4)', '# Frac well-rep by both: ',both/total
print, format='(A-50,F10.4)', '# Frac NOT well-rep by either: ',poor/total
print, format='(A-50,F10.4)', '# Frac w/ K0+plaw better than plaw: ',k0better/total
print, format='(A-50,F10.4)', '# Frac w/ plaw better than K0+plaw: ',plawbetter/total
print, format='(A-50,F10.4)', '# Frac w/ K0=0 @ '+num2str(ksigma)+': ',numplaw/total
print, format='(A-50,F10.4)', '# Interp null accepted: ',idiff/total
print, format='(A-50,F10.4)', '# Flat null accepted: ',fdiff/total
print, format='(A-50,F10.4)', '# Frac improv by add params @ '+num2str(fsig)+': ',fimprov/total
print, format='(A-50,F10.4)', '# Frac improv by add params (var-type) @ '+num2str(vfsig)+': ',vfimprov/total

set_plot, 'PS'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
fnames  = ['itpl.eps','flat.eps']
xtitles = [textoidl('Pval K0 not = 0'), $
           textoidl('Pval K0 not = 0')]
ytitles = [textoidl('Pval K0 = 0'), $
           textoidl('Pval K0 = 0')]
x = [ptr_new(prob0), ptr_new(prob2)]
y = [ptr_new(prob1), ptr_new(prob3)]
FOR jj=0,n_elements(fnames)-1 DO BEGIN
   px = *x[jj]
   py = *y[jj]
   device, filename=fnames[jj], $
           /encapsulated, $
           /portrait, $
           /helvetica
   plotsym, 0, 0.4, /fill
   xmin = 0.8*min(px)
   xmax = 1.2*max(px)
   ymin = 0.8*min(py)
   ymax = 1.1*max(py)
   plot, px, py, $
     xtitle = xtitles[jj], $
     ytitle = ytitles[jj], $
     xran = [xmin,xmax], $
     yran = [ymin,ymax], $
     /xsty, /ysty, $
     /XLOG, /ylog, $
     psym = 8, $
     charsize = 1.0
   device,/close
ENDFOR
END
