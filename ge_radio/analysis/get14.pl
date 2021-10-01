#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"radlum_nvss_withz.dat");
open(B,">fixed_1.4radio.dat");
printf B "%-20s %10s %15s %15s %15s %15s %15s\n",
    "# Name", "type", "NED_z", "D_L", "flux", "err", "nu_0";
printf B "%-20s %10s %15s %15s %15s %15s %15s\n",
    "# --", "--", "--", "Mpc", "Jy", "Jy", "Hz";
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line = split;
    $dl = 0.0;
    $fre = 1.4e+09;
    printf B "%-20s %10s %15.6f %15.4e %15.7f %15.7f %15.3e\n",
    $line[0], $line[2], $line[3], $dl, $line[4], $line[5], $fre;
}
close A;
close B;
exit 0;
