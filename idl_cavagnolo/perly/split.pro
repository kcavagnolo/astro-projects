; out = split(',', 'gimme,some,candy!')
; print, out
;   gimme some candy!

FUNCTION split, delimiter, string
  array  = ['x']
  length = strlen(string)
  last   = 0
  WHILE last LT length DO BEGIN
      pos = STRPOS(string, delimiter, last)
      IF pos EQ -1 THEN pos = length
      array = [ array, STRMID(string, last, pos-last) ]
      IF delimiter EQ ' ' THEN WHILE StrMid(string,pos+1,1) EQ ' ' DO pos=pos+1
      last = pos+1
  ENDWHILE
  RETURN, array[1:*]
END
