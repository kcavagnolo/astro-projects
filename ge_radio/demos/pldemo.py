# Got this stuff from
# http://matplotlib.sourceforge.net/users_guide_0.98.3.pdf
# STM 2008-08-20

# bring up a frame
pl.clf()

# draw axes
ax = pl.subplot(221)

# some stuff to plot
x = [1000001,1000002,1000003.5,1000005]
y = [0.00001,0.000011,0.000015,0.000021]

# simple plot
pl.title('default')
pl.plot(x,y)

# Note that x axis has sucky formatting

# Lets try to fix that
ax = pl.subplot(222)

majorFormatter = pl.FormatStrFormatter('%10.1f')
ax.xaxis.set_major_formatter(majorFormatter)
pl.title('format %10.1f')
pl.plot(x,y)

# Better but too crowded
ax = pl.subplot(223)

majorLocator = pl.MultipleLocator(2)
ax.xaxis.set_major_formatter(majorFormatter)
ax.xaxis.set_major_locator(majorLocator)
pl.title('locate 2 format %10.1f')
pl.plot(x,y)

# Better, probably will need an algorithm to set these
