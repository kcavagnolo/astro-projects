PRO calclum

band = ['F606W', 'F814W', 'GFUV', 'GNUV', 'UVW1', 'UVM2', 'B', 'H', 'J', 'K']

;# RBS 797
z = replicate(0.354, n_elements(band))
appm = [19.26, 18.21, 20.68, 21.86, 20.91, 19.63, 19.2, 15.28, 16.22, 14.61]

;# IRAS 19297-0406
;band = ['H', 'J', 'K']
;z = replicate(0.08573, n_elements(band))
;appm = [13.14, 13.27, 13.01]

;# IRAS 09140+4109
;band = ['H', 'J', 'K']
;z = replicate(0.442, n_elements(band))
;appm = [15.87, 16.83, 14.77]

appmerr = [0.69, 0.56, 0.23, 0.35, 0.39, 0.0, 0.1, 0.13, 0.12, 0.10]
galext = [0.114, 0.069, 0.3259, 0.3669, 0.2319, 0.3505, 0.159, 0.022, 0.035, 0.0135]
kcorr = [1.080, 0.236, 1.9195, 1.9195, 1.9195, 1.9195, 1.569, -0.087, -0.009, -0.241]
ecorr = [-0.438, -0.381, -0.5458, -0.5458, -0.5458, -0.5458, -0.487, -0.362, -0.366, -0.359]

mcorr = appm-galext-kcorr-ecorr
mcorrerr = mcorr*abs(appmerr/appm)

mag2lum, mcorr, z, band, err=mcorrerr

END

