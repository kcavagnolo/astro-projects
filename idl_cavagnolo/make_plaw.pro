PRO make_plaw, inim, outim, dim, norm, alpha

a = mrdfits(inim,0,hdr)
x = findgen(dim)#replicate(1.,dim)
y = transpose(x)
r = (x-(dim/2))^2.+(y-(dim/2))^2.
image = norm*r^(alpha)
ord=where(image EQ max(image))
image[ord] = norm
mwrfits, image, outim, hdr, /create

END
