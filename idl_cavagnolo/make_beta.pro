PRO make_beta, inim, outim, dim, params

a = mrdfits(inim,0,hdr)
x = findgen(dim)#replicate(1.,dim)
y = transpose(x)
r = sqrt((x-(dim/2))^2.+(y-(dim/2))^2.)
s0   = double(params[0])
rc   = double(params[1])
beta = double(params[2])
bgd  = double(params[3])
image = s0*(1.0+(r/rc)^2.0)^(-3.0*beta+0.5)+bgd
mwrfits, image, outim, hdr, /create

END
