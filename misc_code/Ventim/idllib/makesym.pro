pro MakeSym, symbol, scale
;+
;  PROCEDURE:
;	MAKESYM
;  PURPOSE:
;	Call USERSYM to create one of the below plot symbols.
;  CALLING SEQUENCE:
;  	Makesym, SYMBOL
;  INPUTS:
;	SYMBOL - number indicating which symbol you want.  If not within the 
;	range of available symbols, nothing is done.
;	9  - Circle
;	10  - Filled Circle
;	11 - Filled Diamond
;	12 - Filled Triangle
;	13 - Filled Square
;	14 - 5-point star
;	15 - Filled 5-point star
;	16 - Pentagon
;	17 - Filled pentagon
;	18 - Hexagon
;	19 - Filled Hexagon
;  OUTPUTS:
;	None.
;	USERSYM is called, which sets the default user plot symbol.
;  Restrictions:
;	Does not account for the histogram plotting style.
;  History:
;	Joel D. Offenberg, Hughes STX, 21 Jan 1993
;	Added symbols 14-19.  JDO, March 1994
;-

scale = n_elements(scale) GT 0 ? scale : 1.0

Case SYMBOL of
9:	BEGIN	;Circle
		a = findgen(25)*2*!PI/24.
		USERSYM, scale*1.0*cos(a), scale*1.0*sin(a), thick = 4
	END
10:	BEGIN	;Filled Circle
		a = findgen(25)*2*!PI/24.
		USERSYM, scale*1.0*cos(a), scale*1.0*sin(a),/FILL
	END
11:	USERSYM, scale*[0,1,0,-1],scale*[-1,0,1,0],/FILL, THICK=3    ;Filled Diamond
12:	USERSYM, scale*[1.5,0,-1.5, 1.5],scale*[-1.5,1.5,-1.5, -1.5], /FILL, THICK = 4     ;Filled Triangle
22:	USERSYM, scale*[1.5,0,-1.5, 1.5],scale*[-1.5,1.5,-1.5, -1.5], THICK = 4     ;Triangle
13:	USERSYM, scale*[1,1,-1,-1],scale*[-1,1,1,-1],/Fill, THICK = 4	       ;Filled Square
14:	USERSYM, scale*[.5,.85,0,1,.15,.5]-1,scale*[1,0,.6,.6,0,1]-1, THICK = 2  ; 5-point star
15:	USERSYM, scale*[.5,.85,0,1,.15,.5]-1,scale*[1,0,.6,.6,0,1]-1, /fill, THICK = 2
		        			; Filled 5-point star
16:	USERSYM, scale*[.5,1,.85,.15,0,.5]-1,scale*[1,.6,0,0,.6,1]-1, THICK = 4;Pentagon
17:	USERSYM, scale*[.5,1,.85,.15,0,.5]-1,scale*[1,.6,0,0,.6,1]-1, /FILL, THICK = 4
		 				;Filled pent.
18:	USERSYM, scale*[1,.75,.25,0,.25,.75,1]-1,scale*[.5,1,1,.5,0,0,.5]-1, THICK = 2;Hexagon
19:	USERSYM, scale*[1,.75,.25,0,.25,.75,1]-1,scale*[.5,1,1,.5,0,0,.5]-1, /fill, THICK = 2
		        				;Filled Hexagon
20:	USERSYM, scale*[1.5,1.5,-1.5,-1.5, 1.5],[-1.5,1.5,1.5,-1.5, -1.5], THICK = 4	       ;Square
21:	BEGIN	;Filled Circle
		a = findgen(25)*2*!PI/24.
		USERSYM, scale*0.5*cos(a), scale*0.5*sin(a),/FILL
	END
else:	BEGIN
		;do nothing
	endELSE
endCASE

end
