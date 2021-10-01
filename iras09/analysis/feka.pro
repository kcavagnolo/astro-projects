PRO feka

;# [09, me99, i01-corr, i01]
x =   [1, 2, 3, 4]
y =   [279, 685, 762, 1100]
yhi = [171, 482, 624, 900]
ylo = [176, 480, 347, 500]
plot, x, y, $
       psym = 2, $
       xran = [0, 5], $
       yran = [10, 2500], $
       /xsty, /ysty
oploterror, x, y, yhi, /hibar, psym=2
oploterror, x, y, ylo, /lobar, psym=2

END
