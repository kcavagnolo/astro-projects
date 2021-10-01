;;;;;;;;;;;;;
; READ RESP ;
;;;;;;;;;;;;;

pro read_resp,rmf,arf,inegrid,outegrid,resp,channel,verbose=verbose,$
              native=native,chmin=chmin,chmax=chmax,ermin=ermin,ermax=ermax

;
; Sebastian Heinz, 12/15/2008
;
; read in a generic XSPEC response on the input energy range grid inegrid and 
; the output energy grid outegrid
;
; input:
;
;        rmf:       response file name
;
;        arf:       ancilliary response
;
;        inegrid:   input energy grid (this is the axis of the calculated
;                   spectrum)
;
;        outegrid:  output energy grid (this is the output spectral axis of the
;                   convolved spectrum)
;
; output:
;
;        resp:      structure containing the response matrix
;
; keywords:
;
;        verbose:   set to print out progress information
;
;        native:    reset energy grid to instrument channel map and return
;                   response
;
;        chmin:     returns minimum channel number of response file
;
;        chmax:     returns maximum channel bumber in response file
;        
;        ermin:     returns minimum energy of energy grid
;
;        ermax:     returns maximum energy of energy grid        
;


if keyword_set(verbose) then verbose=verbose else verbose=1
if keyword_set(native) then native=native else native=0

ienum=n_elements(inegrid)-1L
oenum=n_elements(outegrid)-1L

if rmf eq "dummy" then begin

    ; dummy? Then make diagonal matrix
    resp=create_struct('n1',ienum,'n2',oenum,'egrid1',inegrid,$
                       'egrid2',outegrid)
    for i=0L,ienum-1L do begin
        regrid,[1.0],[inegrid(i),inegrid(i+1)],matrix,outegrid
        inner=where(matrix gt 0.0)
        if inner(0) ne -1 then begin $
          resp=create_struct(resp,'elements'+$
                             strtrim(string(format='(i9.9)',i),2),$
                             inner,$
                             'response'+strtrim(string(format='(i9.9)',i),2),$
                             matrix(inner))
        endif else begin
          resp=create_struct(resp,'elements'+$
                             strtrim(string(format='(i9.9)',i),2),[0],$
                             'response'+$
                             strtrim(string(format='(i9.9)',i),2),[0])
        endelse
    endfor
    ermin=min(inegrid)
    ermax=max(inegrid)

endif else begin

    ; read in channel information
    fits_info,rmf,N_ext=nn,/silent
    if nn lt 1 or nn gt 2 then stop,'Error: Response file has wrong dimensions'
    extn=strarr(nn)
    for ih=1L,nn do extn(ih-1L)=sxpar(headfits(rmf,EXTEN=ih),'EXTNAME')
    mastrn=where(strmatch(extn,'*MATRIX*') eq 1)
    if mastrn eq -1 then $
      stop,'Error: Response file does not contain response matrix'
    ebstrn=where(strmatch(extn,'*EBOUNDS*') eq 1)
    if ebstrn eq -1 then ebstrn = 1 + ((mastrn+1) MOD 2) else ebstrn+=1
    
    fxbopen,lu,rmf,ebstrn
    fxbread,lu,emin,'E_MIN'
    fxbread,lu,emax,'E_MAX'
    fxbread,lu,chn,'CHANNEL'
    h1=fxbheader(lu)
    ch1=sxpar(h1,'TLMIN1',count=ct)
    if ct ne 0 then ch1=ch1(0) else ch1=1
    fxbclose,lu

    ermin=min(emin)
    ermax=max(emax)

    chmin=min(chn)
    chmax=max(chn)

    if (arf ne "" and file_test(arf) and (not file_test(arf,/directory))) $
        then begin
        fxbopen,lu,arf,"SPECRESP"
        fxbread,lu,amat,'SPECRESP'
        fxbclose,lu
    endif else begin
        amat=replicate(1.e,n_elements(emin))
    endelse
    
    if (arf eq "" or (not file_test(arf)) or file_test(arf,/directory)) $
      then begin
        fits_info,rmf,N_ext=nn,/silent
        if nn lt 1 then stop,'Error: Response file has zero extensions'
        extn=strarr(nn)
        for ih=1,nn do extn(ih-1)=sxpar(headfits(rmf,EXTEN=ih),'EXTNAME')
        mastrn=where(strmatch(extn,'*MATRIX*') eq 1)
        if mastrn eq -1 then $
          stop,'Error: Response file does not contain response matrix' else $
          mstr=extn(mastrn)
    endif else begin
        mstr="MATRIX"
    endelse
    
    if (native eq 0) then $
      channel=interpol([chn,max(chn)+1],[emin,max(emax)],outegrid)

    fxbopen,lu,rmf,mstr
    fxbread,lu,elo,'ENERG_LO'
    fxbread,lu,ehi,'ENERG_HI'
    fxbread,lu,ngp,'N_GRP'
    h2=fxbheader(lu)
    ch2=sxpar(h2,'TLMIN4',count=ct)
    if ct ne 0 then ch2=ch2(0) else ch2=1
    
    if keyword_set(native) then begin
        inner=where(emax gt min(outegrid) and emin lt max(outegrid))
        iinner=where(ehi gt min(inegrid) and elo lt max(inegrid))
        if inner(0) eq -1 then begin
            outegrid=[min(outegrid),max(outegrid)]
            channel=[0]
            print,'WARNING: output energy grid has '+$
              'no overlap with response matrix'
        endif else begin
            outegrid=[emin(inner(0)),emax(inner)]
            channel=[chn(inner)-1,chn(max(inner))]
        endelse
        if iinner(0) eq -1 then begin
            inegrid=[min(inegrid),max(inegrid)]
            print,'WARNING: input energy grid has '+$
              'no overlap with response matrix'
        endif else begin
            inegrid=[elo(iinner(0)),ehi(iinner)]
        endelse
        ienum=n_elements(inegrid)-1
        oenum=n_elements(outegrid)-1
    endif

    nmat1=n_elements(emin)
    nmat2=n_elements(elo)
    resp=create_struct('n1',ienum,'n2',oenum,$
                       'egrid1',inegrid,'egrid2',outegrid)

    if verbose eq 2 then $
      print,'Reading response file '+rmf+', '+arf
    for j=0l,long(ienum)-1l do begin
        ; find input energy bins that overlap with spectral energy bin
        overlap=where(elo le inegrid(j+1) and ehi ge inegrid(j))
        if overlap(0) eq -1 then goto,empty
        minn=min(overlap)
        maxn=max(overlap)

        ; now load in channel information and find all output channels
        ; initialize assuming no channels
        minc=max(chn)-ch2
        maxc=min(chn)-ch2
        for i=minn,maxn do begin
            if ngp(i) gt 0 then begin
                matrix=fltarr(nmat2)
                fxbread,lu,fchn,'F_CHAN',i+1
                fxbread,lu,nchn,'N_CHAN',i+1
                ; any channels lower/higher than current bounds?
                minc=max([0,min([fchn(0)-ch2,minc])])
                maxc=max([maxc,fchn(ngp(i)-1) - ch2 + $
                          nchn(ngp(i)-1)-ch2])
            endif else begin
                goto,empty
            endelse
        endfor
        if minc gt maxc then goto,empty
        
        ; find the corrosponding energy channels so we can regrid;
        ; the following channels must have overlap with bins;
        ; use them as boundaries for energy sub-grid;
        ; remember that inegrid is face centered
        overlap2=where(outegrid(1:oenum) ge emin(minc) and $
                       outegrid(0:oenum-1) le emax(maxc))
        if overlap2(0) eq -1 then goto,empty
        egpmin=min(overlap2)
        egpmax=max(overlap2)
        submat=fltarr(maxn-minn+1,egpmax-egpmin+1)

        ; now read in the response matrix for overlapping channels
        for i=minn,maxn do begin
            matrix=fltarr(maxc-minc+1)
            fxbread,lu,fchn,'F_CHAN',i+1
            fxbread,lu,nchn,'N_CHAN',i+1
            fxbread,lu,mat,'MATRIX',i+1
            spos=0
            ; make sure there are elements in the matrix
            if ngp(0) gt 0 then begin
                for k=0l,long(ngp(i)-1) do begin
                ; populate matrix, taking nchn offset into account
                    matrix(fchn(k) - ch2 - minc:fchn(k)-1 + $
                           nchn(k) - ch2 - minc)=$
                      mat(spos:spos+nchn(k)-1)
                    spos=spos+nchn(k)
                endfor
                ; now regrid to output energy resolution (y-axis)
                regrid,matrix,[emin(minc:maxc),emax(maxc)],mat,$
                  outegrid(egpmin:egpmax+1),1
                ; and write response from channel i into submat(i,*)
                submat(i-minn,*)=reform(mat,1,egpmax-egpmin+1)*$
                  replicate(amat(i),egpmax-egpmin+1)
            endif else begin
                ; else fill it with zeros
                submat(i-minn,*)=replicate(0,egpmax+1-egpmin)
            endelse
        endfor
        ; now regrid the entire submat into one input energy bin
        regrid,submat,[elo(minn:maxn),ehi(maxn)],$
          matrix,[inegrid(j),inegrid(j+1)],1,/average
 
        ; and stote the output in the response structure
        resp=create_struct(resp,$
                           'elements'+strtrim(string(format='(i9.9)',j),2),$
                           egpmin+indgen(egpmax-egpmin+1),$
                           'response'+strtrim(string(format='(i9.9)',j),2),$
                           matrix)
        goto,nextloop

        empty:resp=create_struct(resp,'elements'+$
                                 strtrim(string(format='(i9.9)',j),2),[0],$
                                 'response'+$
                                 strtrim(string(format='(i9.9)',j),2),[0])
        
        nextloop:done=1
        
        if (verbose ne 0 and j eq 0) then $
          print,format='($,"Reading response...... 000.00% complete")'
        if verbose ne 0 then $
          print,format='($,A16,f6.2,"% complete")',string(replicate(8b,16)),$
          float(j)/float(ienum)*100.e
        
    endfor
    if verbose ne 0 then $
      print,format='(A16,f6.2,"% complete")',string(replicate(8b,16)),100.0
    
    fxbclose,lu
    free_lun,lu

endelse 
end
