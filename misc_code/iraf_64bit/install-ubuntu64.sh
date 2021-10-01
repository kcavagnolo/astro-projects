#!/bin/bash

# Variables to use
IRAF=/iraf
SUDO=sudo
LBIN=/usr/local/bin
DEPS="tcsh sharutils lib32ncurses5 libc6-dev gfortran python-pmw libf2c2 python-urwid ipython python-numpy python-tk python-pyfits python-dev libx11-dev ia32-libs libc-dev-amd64"

# Stop if there are any errors.
set -e

ubuntu_sudo()	{
    echo ''
    echo '		Administrator Password:'
    echo ''
    $SUDO true
}

iraf_deb()	{
    echo ''
    echo '		Installing dependencies.'
    echo ''
    $SUDO apt-get install -y $DEPS
    $SUDO dpkg -i --force-architecture ldso_1.9.11-15_i386.deb
    $SUDO dpkg -i --force-architecture libc5_5.4.46-15_i386.deb 
    $SUDO dpkg -i --force-architecture termcap-compat_1.2.3_i386.deb 
}

iraf_user()	{
    echo ''
    echo '		Creating IRAF user.'
    echo ''
    $SUDO adduser --home /iraf/iraf/local --shell /bin/tcsh --disabled-password --quiet --gecos "Iraf Administrator" --no-create-home iraf
}

iraf_untar()	{
    echo ''
    echo '		Creating target directory.'
    echo ''
    $SUDO mkdir -p /iraf
    echo ''
    echo '		Unpacking files.'
    echo ''
    $SUDO wget http://iraf.noao.edu/iraf/ftp/iraf/v214/PCIX/as.pcix.gen.gz
    $SUDO tar -xpzf as.pcix.gen.gz -C $IRAF
    $SUDO chmod 755 $IRAF
    $SUDO chown iraf.iraf -R $IRAF
}

iraf_x11()	{
    cd $IRAF/extern/x11iraf
    $SUDO sh install-x11iraf.sh
}

iraf_pre()	{
    cd /iraf/iraf/unix/hlib/
    $SUDO su -c "setenv iraf /iraf/iraf/ && source /iraf/iraf/unix/hlib/irafuser.csh" - iraf
}

iraf_install()	{
    echo ''
    echo '		Postinstall and configuration.'
    echo ''
    $SUDO touch /etc/redhat-release
    cd $IRAF/iraf/unix/hlib/
    $SUDO $IRAF/iraf/unix/hlib/install << END_OF_IRAF
$IRAF/iraf
$IRAF/imdirs
$lbin

no
END_OF_IRAF
    cd /iraf/iraf/local
    $SUDO su -c "source .login" - iraf
    $SUDO su -c "rehash" - iraf
}

iraf_pyraf()	{
    cd $IRAF/extern/pyraf
    $SUDO sh install-pyraf.sh
}

iraf_telarchive()    {
    cd $IRAF/extern/telarchive
    $SUDO sh install-telarchive.sh
}

iraf_post()	{
    $SUDO rm /etc/redhat-release
    echo ''
    echo ''
    echo ''
    echo '		IRAF install has finished.'
    echo ''
    echo '		Before using IRAF for the first time, '
    echo '		you have to execute the command:'
    echo ''
    echo '			mkiraf'
    echo ''
    echo '		from your home directory and pick'
    echo '		the xgterm terminal.'
    echo ''
    echo '		A script called iraf has been installed'
    echo '		in /usr/local/bin/ which executes'
    echo '		the xgterm terminal running ecl, and'
    echo '		the ds9 viewer.'
    echo ''
}

# Actions to follow.
ubuntu_sudo
iraf_deb
iraf_user
iraf_untar
iraf_x11
iraf_pre
iraf_install
iraf_pyraf
iraf_telarchive
iraf_post
exit
