#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"HST_10491_41_ACS_WFC_F606W_sexphot_trm.cat");
open(B,">606_sexcat.reg");
#open(A,"HST_10875_40_ACS_WFC_F814W_sexphot_trm.cat");
#open(B,">814_sexcat.reg");
printf B "# Region file format: DS9 version 4.1\n";
printf B "global color=green dashlist=8 3 width=1 font=\"helvetica 8 normal\" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1\n";
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line = split;
    $ra = $line[0];
    $dec = $line[1];
    $mag = $line[25];
    $magerr = $line[26];
    $x = $line[52]-$line[50];
    $y = $line[53]-$line[51];
    $r = $x if ($x ge $y);
    $r = $y if ($y gt $x);
#    printf B "circle(${ra}, ${dec}, ${r}) \# text=\{${mag} +/- ${magerr}\}\n";
    printf B "ellipse(${ra}, ${dec}, ${x}, ${y}) \# text=\{${mag} +/- ${magerr}\}\n";
}
close A;
close B;
exit 0;
