;+
; NAME:
;   PROREND
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Render a PRODIS abstract syntax tree into IDL Language Text
;
; CALLING SEQUENCE:
;   PROREND, TREE, TEXT, [ /INIT ]
;
; DESCRIPTION: 
;
;   PROREND converts an abstract syntax tree as returned by PRODIS,
;   into a human-readable form, written in the IDL programming
;   language.  The abstract syntax tree format is a set of linked data
;   structures, and is derived from the raw data on disk.  The human
;   readable form is returned as an array of strings that can be
;   printed to the console or a file.
;
;   The abstract syntax tree is generated by PRODIS, an external
;   procedure in the same library.  The standard approach is to use
;   the following steps:
;
;       1. Use PRODIS to convert raw bytes to abstract syntax tree
;       2. Use PROREND to convert abstract syntax tree to IDL language
;
;   The external routine PROTRANS does the end-to-end conversion steps
;   of both PRODIS and PROREND for you.
;
;   At the moment there is relatively little flexibility in how the
;   IDL code is rendered to text.  For example, all reserved keywords
;   and variables appear in upper-case letters, and array indexing
;   syntax is expressed with round ()'s instead of square []'s.
;   Suggestions on how to achieve this are solicited.
;
;   PROREND does not free the TREE structure.  The user is responsible
;   to do this using the PROFREE procedure.
;   
;
; COMPATIBILITY:
;
;   -- File Format --
;
;   PROREND accepts any tree provided by PRODIS.  PRODIS cannot
;   examine compressed save files.  It is able to read and translate
;   SAVE files produced by IDL 4, and IDL versions 5.0 through 5.5.
;   The output of PROREND should be compatible with IDL 4 and 5.
;
;   This procedure is part of the CMSVLIB SAVE library for IDL by
;   Craig Markwardt.  You must have the full CMSVLIB core package
;   installed in order for this procedure to function properly.
;
; INPUTS:
;
;   TREE - the abstract syntax tree, as returned by PRODIS.  This
;          structure is unmodified by PROREND.
;
;   TEXT - upon output, the IDL code is placed in as an array of
;          strings in TEXT.  By default, any new IDL code will be
;          *appended* to TEXT.  Use the /INIT keyword to overwrite the
;          existing contents of TEXT.
;
;
; KEYWORDS:
;
;   INIT - if set, then overwrite the TEXT array with the new IDL
;          code.  By default (INIT not set), any new IDL code is
;          *appended* to TEXT.
;  
; EXAMPLE:
;
;   This example compiles a test function, saves it to a file called
;   test_pro.sav, and then disassembles the save file into a syntax
;   tree using PRODIS.  Finally, the syntax tree is converted to IDL
;   text, which is printed to the console.
;
;     IDL> .comp 
;     - pro test_pro, x
;     -   x = x + 1
;     -   return
;     - end
;     % Compiled module: TEST_PRO.
;     IDL> save, 'test_pro', /routine, file='test_pro.sav'
;     IDL> prodis, 'test_pro.sav', prodecl, tree
;     IDL> prorend, tree, text
;     IDL> print, text, format='(A)'
;     PRO TEST_PRO, X
;       ;; Beginning of code
;       X = X+1
;       RETURN
;     END
;
;
; SEE ALSO:
;
;   PRODIS, PROREND, CMSAVEDIR, CMSVLIB
;
; MODIFICATION HISTORY:
;   Written, 2000-2002, CM
;   Documented, 19 Mar 2002, CM
;   Added PRN_STRCAT, to avoid an internal library function, 22 Mar
;     2002, CM
;   
;
; $Id: prorend.pro,v 1.2 2009-06-19 22:21:48 cavagnolo Exp $
;
;-
; Copyright (C) 2000-2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Utility function: concatenate string array
function prn_strcat, strings, joinstring=joinstring

  if n_elements(strings) EQ 0 then return, ''
  if n_elements(strings) EQ 1 then return, strings(0)

  n_strings = n_elements(strings)
  
  fmt = '('+strtrim(n_strings,2)+'(A,:))'
  mystrings = strings
  if n_strings GT 1 AND n_elements(joinstring) EQ 1 then $
    mystrings(0:n_strings-2) = mystrings(0:n_strings-2) + joinstring(0)

  return, (string(mystrings, format=fmt))(0)
end

;; Utility function: push a value onto the stack
pro prn_push, stack, val, nstack=nstack, template=node0
  nvals = n_elements(val)
  if n_elements(nstack) EQ 0 then nstack = n_elements(stack)
  if n_elements(val) EQ 0 then return

  n2 = nvals + nstack
  if n_elements(stack) LT n2 then begin
      if n_elements(node0) EQ 0 then node0 = ''
      if n_elements(stack) EQ 0 then begin
          stack = replicate(node0(0), (n_elements(val)*2) > 10)
      endif else begin
          stack = [stack, replicate(node0(0),((n2-nstack)*5) > 10) ]
      endelse
  endif
  
  stack(nstack) = val
  nstack = n2
  return
end

;; Utility function: extract operand from tree
function prn_opn, prodecl, ptr, type=type, embed=embed, last_operation=lastop

  if n_elements(ptr) EQ 0 then return, ''

  sz = size(ptr)
  if sz(sz(0)+1) EQ 10 then begin
      if ptr_valid(ptr(0)) EQ 0 then return, ''
      x = *ptr
  endif else if sz(sz(0)+1) EQ 8 then begin
      x = ptr
  endif else begin
      return, ''
  endelse

  ntext1 = 0L
  prn_parse, prodecl, x, text1, nstack=ntext1, last_operation=lastop

  type = x(0).type
  op = text1(ntext1-1)

  if keyword_set(embed) then begin
      if (type AND 7) EQ 0 then op = '('+op+')'
  endif
    
  return, op
end

;; NODE: 'RETURN'
function prn_return, prodecl, tree

  str = 'RETURN'

  if ptr_valid(tree(0).operands(0)) then $
    str = str + ', ' + prn_opn(prodecl, tree(0).operands(0))
  
  return, str
end

;; NODE: 'UNOP' or 'BINOP'
function prn_ubop, prodecl, tree, binop=binop, unop=unop

  opn0 = prn_opn(prodecl, tree(0).operands(0), type=type0, /embed)
  op = tree(0).value

  ;; Spaces between operands and operation
  sp = ''
  b = (byte(op(0)))(0)
  ;; For alpha op, add space
  if b GE '41'xb AND b LE '7a'xb then sp = ' ' 

  if keyword_set(binop) then begin
      opn1 = prn_opn(prodecl, tree(0).operands(1), type=type1, /embed)

      if sp EQ '' AND (op EQ '+' OR op EQ '-') then begin
          l = strmid(opn0, strlen(opn0)-1, 1)
          ;; Trailing D or E must not be confused with coming + or -
          if l EQ 'D' OR l EQ 'E' then sp = ' '
      endif

      return, opn0+sp+op+sp+opn1
  endif else if keyword_set(unop) then begin
      return, op+sp+opn0
  endif

  return, ''
end

;; NODE: 'ASSIGN'
pro prn_assign, prodecl, tree, text, nstack=ntext, prefix=prefix
  if n_elements(prefix) EQ 0 then prefix = ''
  if (tree(0).type AND 8) NE 0 then pf = prefix else pf = ''

  opn0 = prn_opn(prodecl, tree(0).operands(0), type=type0)
  opn1 = prn_opn(prodecl, tree(0).operands(1), type=type1)

  ;; Nested assignments must be protected
  if (type1 AND 8) NE 0 then $
    opn1 = '('+opn1+')'

  prn_push, text, pf+opn0+' = '+opn1, nstack=ntext
  return
end

;; NODE: 'SUBSCRIPT' (subscripted variable)
function prn_subscript, prodecl, tree, text, nstack=ntext
  lval = prn_opn(prodecl, tree(0).operands(0), type=type0, /embed)
  ;; Protect against functions being subscripted
  if (type0 AND 4) NE 0 then lval = '('+lval+')'

  ndims = long(tree(0).value)
  dims = *(tree(0).operands(1))

  dimstr = strarr(ndims)

  for i = 0, ndims-1 do begin
      v = dims(i).value
      case dims(i).op of
          'SUB': begin      ;; Single value '*'
              if v EQ 'ALL' then dimstr(i) = '*'
          end
          'SUBRANGE': begin ;; Range of values, either A:B or A:*
              opn1 = prn_opn(prodecl, dims(i).operands(0))
              if (*dims(i).operands(1)).value EQ 'END' then $
                opn2 = '*' $
              else $
                opn2 = prn_opn(prodecl, dims(i).operands(1))
              dimstr(i) = opn1+':'+opn2
          end
          ELSE: begin       ;; Single value A
              dimstr(i) = prn_opn(prodecl, dims(i))
          end
      endcase
  endfor

  ;; Compose lval with dimensions
  return, lval+'('+prn_strcat(dimstr, join=',')+')'
end

;; NODE: 'PROCALL' or 'METHCALL'
pro prn_procall, prodecl, tree, text, nstack=ntext, prefix=prefix, $
                 method=meth, funct=funct, statement=stmt

  if n_elements(prefix) EQ 0 then prefix = ''

  ;; Basics about the function or procedure
  proname = tree(0).value
  protype = tree(0).type
  funct = (protype AND 4) NE 0
  stmt = (funct EQ 0)

  ;; Handle case of a class method
  if keyword_set(meth) then begin
      dest = prn_opn(prodecl, tree(0).operands(0), type=dtype)
      if (dtype AND 3) EQ 0 then dest = '('+dest+')'
      proname = dest + '->' + proname
      args = tree(0).operands(1)
  endif else begin
      args = tree(0).operands(0)
  endelse

  ;; Append arguments, both positional and keyword ones
  argstr = ''
  nargs = n_elements(*(args))
  if nargs GT 0 then begin
      args = *(args)
      argstr = strarr(nargs)
      for i = 0, nargs-1 do begin
          if args(i).op EQ 'KEYWORD' then begin
              kword = args(i).value
              argstr(i) = kword+'='+prn_opn(prodecl, args(i).operands(0))
          endif else begin
              argstr(i) = prn_opn(prodecl, args(i))
          endelse
      endfor
      argstr = prn_strcat(argstr, join=', ')
  endif

  ;; Express as the form as a function or a procedure
  if funct then begin
      proval = proname+'('+argstr+')'
  endif else begin
      if argstr EQ '' then $
        proval = proname $
      else $
        proval = proname+', '+argstr
  endelse

  if stmt then proval = prefix+proval
  prn_push, text, proval, nstack=ntext
end

;; NODE: 'ARRAY'  (square-brackets style array)
function prn_array, prodecl, tree
  elts = tree(0).operands(0)
  if n_elements(*elts) EQ 0 then return, '[]'
  nelts = n_elements(*elts)
  elts = *elts
  eltstr = strarr(nelts)
  
  for i = 0, nelts-1 do begin
      eltstr(i) = prn_opn(prodecl, elts(i))
  endfor

  return, '['+prn_strcat(eltstr,join=', ')+']'
end

;; NODE: 'PDEREF' (pointer dereference)
function prn_pderef, prodecl, tree
  opn = prn_opn(prodecl, tree(0).operands(0), type=type0)
  if (type0 AND 2) EQ 0 then opn = '('+opn+')'
  return, '*'+opn
end

;; NODE: 'FOR' (for-loop construct)
pro prn_for, prodecl, tree, text, nstack=ntext, prefix=prefix
  if n_elements(prefix) EQ 0 then prefix = ''
  
  lval = prn_opn(prodecl, tree(0).operands(0))

  ;; Parse START, STOP, STEP range values
  rng  = *(tree(0).operands(1))
  nrng = n_elements(rng)
  rngstr = strarr(nrng)
  for i = 0, nrng-1 do $
    rngstr(i) = prn_opn(prodecl, rng(i))
  rngstr = prn_strcat(rngstr, join=', ')

  ;; Parse body of FOR loop
  body = *(tree(0).operands(2))
  prn_parse, prodecl, body, bodytext, nstack=nbodytext, prefix=''

  ;; Choose long or short form for FOR loop
  if nbodytext EQ 1 then begin
      prn_push, text, nstack=ntext, $
        prefix+'FOR '+lval+' = '+rngstr+' DO '+bodytext(0)
  endif else begin
      prn_push, text, nstack=ntext, $
        prefix+'FOR '+lval+' = '+rngstr+' DO BEGIN'
      if nbodytext GT 0 then $
        prn_push, text, nstack=ntext, $
        prefix+'    '+bodytext(0:nbodytext-1)
      prn_push, text, nstack=ntext, $
        prefix+'ENDFOR'
  endelse
      
  return
end

;; NODE: 'IF' (if-then-else construct)
pro prn_if, prodecl, tree, text, nstack=ntext, prefix=prefix
  if n_elements(prefix) EQ 0 then prefix = ''
  expr = prn_opn(prodecl, tree(0).operands(0))

  ;; Extract body of the IF clause
  if ptr_valid(tree(0).operands(1)) GT 0 then begin
      if n_elements(*(tree(0).operands(1))) EQ 0 then $
        goto, NOIFBODY
      ifbody = *(tree(0).operands(1))
      prn_parse, prodecl, ifbody, ifbodytext, nstack=nifbodytext, prefix=''
  endif else begin
      NOIFBODY:
      ifbodytext = ''
      nifbodytext = 0L
  endelse

  ;; Extract body of the ELSE clause
  if ptr_valid(tree(0).operands(2)) GT 0 then begin
      if n_elements(*(tree(0).operands(2))) EQ 0 then $
        goto, NOELBODY
      elbody = *(tree(0).operands(2))
      prn_parse, prodecl, elbody, elbodytext, nstack=nelbodytext, prefix=''
  endif else begin
      NOELBODY:
      elbodytext = ''
      nelbodytext = 0L
  endelse

  if nifbodytext EQ 1 AND nelbodytext LE 1 then begin
      ;; Case of "IF expr THEN stmt ELSE stmt"
      ifstr = 'IF '+expr+' THEN '+ifbodytext(0)
      if nelbodytext GT 0 then $
        ifstr = ifstr + '   ELSE   '+elbodytext(0)
      prn_push, text, nstack=ntext, $
        prefix+ifstr
  endif else begin
      ;; Case of "IF epxr THEN BEGIN & stmts & ENDIF ..."
      prn_push, text, nstack=ntext, $
        prefix+'IF '+expr+' THEN BEGIN'
      if nifbodytext GT 0 then $
        prn_push, text, nstack=ntext, $
        prefix+'    '+ifbodytext(0:nifbodytext-1)
      if nelbodytext EQ 0 then begin
          prn_push, text, nstack=ntext, $
            prefix+'ENDIF'
      endif else begin
          ;; "... ELSE BEGIN & stmts & ENDELSE"
          prn_push, text, nstack=ntext, $
            prefix+'ENDIF ELSE BEGIN'
          prn_push, text, nstack=ntext, $
            prefix+'    '+elbodytext(0:nelbodytext-1)
          prn_push, text, nstack=ntext, $
            prefix+'ENDELSE'
      endelse
  endelse          

  return
end

;; NODE: 'WHILE' (while-loop construct)
pro prn_while, prodecl, tree, text, nstack=ntext, prefix=prefix
  if n_elements(prefix) EQ 0 then prefix = ''
  expr = prn_opn(prodecl, tree(0).operands(0))

  ;; Parse body of WHILE loop
  body = *(tree(0).operands(1))
  prn_parse, prodecl, body, bodytext, nstack=nbodytext, prefix=''

  ;; Choose either long or short form for the WHILE loop
  if nbodytext EQ 1 then begin
      prn_push, text, nstack=ntext, $
        prefix+'WHILE '+expr+' DO '+bodytext(0)
  endif else begin
      prn_push, text, nstack=ntext, $
        prefix+'WHILE '+expr+' DO BEGIN'
      if nbodytext GT 0 then $
        prn_push, text, nstack=ntext, $
        prefix+'    '+bodytext(0:nbodytext-1)
      prn_push, text, nstack=ntext, $
        prefix+'ENDWHILE'
  endelse

  return
end

;; NODE: 'TRICOND' (triple condition of the form TEST ? A : B)
function prn_tricond, prodecl, tree
  expr   = prn_opn(prodecl, tree(0).operands(0), type=type0, /embed)
  ifstmt = prn_opn(prodecl, tree(0).operands(1), type=type1, /embed)
  elstmt = prn_opn(prodecl, tree(0).operands(2), type=type2, /embed)

  return, expr+'?'+ifstmt+':'+elstmt
end

;; NODE: 'ON_IOERROR'
function prn_onioerror, prodecl, tree
  mark = 'MARK$'+strtrim(tree(0).value)
  if tree(0).value EQ '0' then mark = 'NULL'
  return, 'ON_IOERROR, '+mark
end

;; NODE: 'STRUCTREF' (reference to a structure
function prn_structref, prodecl, tree
  lval = prn_opn(prodecl, tree(0).operands(0), type=type0, /embed)
  ;; Protect against functions being structref'd
  if (type0 AND 4) NE 0 then lval = '('+lval+')'

  ;; Error checking
  tags = tree(0).operands(1)
  if ptr_valid(tags) EQ 0 then return, lval
  if n_elements(*tags) EQ 0 then return, lval

  ;; Chain the structure references together, using PRN_OPN to
  ;; recurse.
  ntags = long(tree(0).value)
  tags = *tags
  tagstr = strarr(ntags)
  for i = 0, ntags-1 do begin
      tag = tags(i)
      tagval = prn_opn(prodecl, tags(i), last_op=lastop)
      if lastop NE 'TAGNAME' AND lastop NE 'TAGSUBSCRIPT' then $
        tagval = '('+tagval+')'

      tagstr(i) = tagval
  endfor

  return, prn_strcat([lval, tagstr], join='.')
end

;; NODE: 'STRUCT' (structure definition)
function prn_struct, prodecl, tree

  ;; Extract the tree associated with this structure definition
  if ptr_valid(tree(0).operands(0)) then $
    if n_elements(*tree(0).operands(0)) GT 0 then begin
      tagvals = *tree(0).operands(0)
      ntags = n_elements(tagvals)

      ;; Scan through the branches of the tree, looking for the three
      ;; kinds.
      sstr = strarr(ntags)
      for i = 0, ntags-1 do begin
          case tagvals(i).op of
              'TAGNAME': sstr(i) = tagvals(i).value+':'
              'TAGVAL': begin
                  if tagvals(i).value NE '' then $
                    sstr(i) = tagvals(i).value+': '
                  sstr(i) = sstr(i) + prn_opn(prodecl, tagvals(i).operands(0))
              end
              'INHERITS': sstr(i) = 'INHERITS '+tagvals(i).value
          endcase
      endfor
  endif

  ;; Add the structure name if it is not an anonymous structure
  if tree(0).value NE '' then begin
      if n_elements(sstr) GT 0 then begin
          sstr = [tree(0).value, sstr]
      endif else begin
          sstr = [tree(0).value]
      endelse
  endif
  if n_elements(sstr) EQ 0 then sstr = ''

  return, '{'+prn_strcat(sstr, join=', ')+'}'
end

;; NODE: 'CASE' (case-of construct)
pro prn_case, prodecl, tree, text, nstack=ntext, prefix=prefix
  if n_elements(prefix) EQ 0 then prefix = ''

  ;; Extract test expression
  expr = prn_opn(prodecl, tree(0).operands(0), /embed)
  prn_push, text, nstack=ntext, $
    prefix+'CASE '+expr+' OF'

  ;; Scan through possible branches of the CASE statement
  ncases = long(tree(0).value)
  if ncases GT 0 AND ptr_valid(tree(0).operands(1)) EQ 1 then begin
      cases = *(tree(0).operands(1))
      for i = 0, ncases-1 do begin

          nbodytext = 0L
          ibody = 1L
          if cases(i).op EQ 'CASEVAL' then begin
              ;; Standard branch
              caseval  = prn_opn(prodecl, cases(i).operands(0), /embed)
          endif else if cases(i).op EQ 'CASEELSE' then begin
              ;; The ELSE (default) branch
              caseval = 'ELSE'
              ibody = 0
          endif else begin
              caseval = ''
          endelse

          ;; Extract the block of code associated with this branch
          if ptr_valid(cases(i).operands(ibody)) then $
            if n_elements(*cases(i).operands(ibody)) GT 0 then $
            prn_parse, prodecl, *cases(i).operands(ibody), $
            bodytext, nstack=nbodytext, prefix=''
          if nbodytext EQ 0 then bodytext = ''

          ;; Render the text
          if nbodytext GT 1 then begin
              prn_push, text, nstack=ntext, $
                prefix+'  '+caseval+': BEGIN'
              prn_push, text, nstack=ntext, $
                prefix+'    '+bodytext(0:nbodytext-1)
              prn_push, text, nstack=ntext, $
                prefix+'  END'
          endif else begin
              prn_push, text, nstack=ntext, $
                prefix+'  '+caseval+': '+bodytext(0)
          endelse
      endfor
  endif

  prn_push, text, nstack=ntext, $
    prefix+'ENDCASE'

  return              
end

;; Main parse loop of PROREND (called recursively!)
pro prn_parse, prodecl, tree, text, nstack=ntext, prefix=prefix, $
               last_operation=lastop
  if n_elements(prefix) EQ 0 then prefix = ''

  lastop = ''
  for i = 0L, n_elements(tree)-1 do begin
      case tree(i).op of
          'ARRAY': prn_push, text, nstack=ntext, $
            prn_array(prodecl, tree(i))
          'ASSIGN': prn_assign, prodecl, tree(i), text, nstack=ntext, $
            prefix=prefix
          'BINOP': prn_push, text, nstack=ntext, $
            prn_ubop(prodecl, tree(i), /binop)
          'CASE': prn_case, prodecl, tree(i), text, nstack=ntext, prefix=prefix
          'FOR': prn_for, prodecl, tree(i), text, nstack=ntext, prefix=prefix
          'GOTO': prn_push, text, nstack=ntext, $
            prefix+'GOTO, MARK$'+strtrim(tree(i).value)
          'IF': prn_if, prodecl, tree(i), text, nstack=ntext, prefix=prefix
          'IMM': prn_push, text, tree(i).value, nstack=ntext
          'LINE': 
          'LVAL': prn_push, text, tree(i).value, nstack=ntext
          'MARK': prn_push, text, nstack=ntext, $
            strmid(prefix,0,strlen(prefix)-1)+'MARK$'+tree(i).value+':'
          'METHCALL': prn_procall, prodecl, tree(i), text, nstack=ntext, $
            prefix=prefix, /method
          'ON_IOERROR': prn_push, text, nstack=ntext, $
            prefix+prn_onioerror(prodecl, tree(i))
          'PDEREF': prn_push, text, nstack=ntext, $
            prn_pderef(prodecl, tree(i))
          'PROCALL': prn_procall, prodecl, tree(i), text, nstack=ntext, $
            prefix=prefix
          'RETURN': prn_push, text, nstack=ntext, $
            prefix+prn_return(prodecl, tree(i))
          'STOP': prn_push, text, prefix+'STOP', nstack=ntext
          'STRUCT': prn_push, text, nstack=ntext, $
            prn_struct(prodecl, tree(i))
          'STRUCTREF': prn_push, text, nstack=ntext, $
            prn_structref(prodecl, tree(i))
          'SUBSCRIPT': prn_push, text, nstack=ntext, $
            prn_subscript(prodecl, tree(i))
          'TAGNAME': prn_push, text, tree(i).value, nstack=ntext
          'TAGSUBSCRIPT': prn_push, text, nstack=ntext, $
            prn_subscript(prodecl, tree(i))
          'TRICOND': prn_push, text, nstack=ntext, $
            prn_tricond(prodecl, tree(i))
          'UNOP': prn_push, text, nstack=ntext, $
            prn_ubop(prodecl, tree(i), /unop)
          'WHILE': prn_while, prodecl, tree(i), text, nstack=ntext, $
            prefix=prefix
          ELSE: print, 'WARNING: unknown type '+tree(i).op
      endcase
      lastop = tree(i).op
  endfor
end

;; Entry point for PROREND
pro prorend, tree0, text, init=init, mangle=mangle
  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, '  PROREND, TREE, TEXT, /INIT', /info
      return
  endif

  if keyword_set(init) then begin
      text = 0 & dummy = temporary(text)
  endif

  ;; Error checking
  if tag_names(tree0, /structure_name) NE 'PDS_NODE' then begin
      message, 'ERROR: TREE must be an abstract syntax tree'
      return
  endif

  if tree0.op NE 'PRODEF' then begin
      message, 'ERROR: head of TREE must be PRODEF node'
      return
  endif

  pnode = *(tree0.operands(0))
  if pnode.op NE 'PRODECL' then begin
      NO_PRODECL:
      message, 'ERROR: TREE has no PRODECL node'
      return
  endif

  if ptr_valid(pnode.operands(0)) then $
    if n_elements(*(pnode.operands(0))) GT 0 then $
    prodecl = *(pnode.operands(0))
  if n_elements(prodecl) EQ 0 then goto, NO_PRODECL

  ;; Assume everything is hunky-dorey from here on out
  tree = *(tree0.operands(1))

  ntext = n_elements(text)
  n_symbols = prodecl.n_syms
  n_args = prodecl.n_args

  if n_symbols GT 0 then symbols = prodecl.symbols

  ;; Generate the declaration of the procedure
  if keyword_set(mangle) then mang_str = '_' else mang_str = ''
  decl = prodecl.type + mang_str + ' ' + prodecl.proname
  if n_args GT 0 then begin
      args = prodecl.args
      
      ;; Positional arguments
      wh = where(args EQ '', n_pos)
      if n_pos GT 0 AND prodecl.is_method AND $
        symbols(wh(0)>0).name EQ 'SELF' then begin
          ;; Methods have a hidden argument named SELF
          n_pos = n_pos - 1
          if n_pos GT 0 then wh = wh(1:*)
      endif
      if n_pos GT 0 then begin
          decl = decl + ', '
          fmt = '('+strtrim(n_pos,2)+'(A,:,", "))'
          decl = decl + string(symbols(wh).name, format=fmt)
      endif

      ;; Keyword arguments
      wh = where(args NE '', n_key)
      if n_key GT 0 then begin
          decl = decl + ', '
          fmt = '('+strtrim(n_key,2)+'(A,:,", "))'
          decl = decl + string(args(wh)+'='+symbols(wh).name, format=fmt)
      endif
  endif
  prn_push, text, decl, nstack=ntext

  ;; Declare any common blocks to be used
  if prodecl.n_commons GT 0 then begin
      comstr = strarr(prodecl.n_commons)
      for i = 0L, prodecl.n_commons-1 do begin
          wh = where(symbols.values(0) EQ (i+1),ct)
          if ct GT 0 then begin
              ss = sort(symbols(wh).values(2))
              cnames = symbols(wh(ss)).name
              comstr(i) = '  COMMON ' + prodecl.commons(i).name + ', ' + $
                prn_strcat(cnames, join=',')
          endif
      endfor
      comstr = ['','  ;; Declaration of common blocks',comstr,'']
  endif

  prn_push, text, comstr, nstack=ntext
  prn_push, text, '  ;; Beginning of code', nstack=ntext
  prn_parse, prodecl, tree, text, nstack=ntext, prefix='  '
  prn_push, text, 'END', nstack=ntext

  text = text(0:ntext-1)
  return
end
