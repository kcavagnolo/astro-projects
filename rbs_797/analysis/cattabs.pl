#! /usr/bin/perl -w
@tables = qw(pvout1.tex);
open(OUT,">cavities.tex");
print OUT
    '\begin{deluxetable*}{cccccccccc}[ht]'."\n",
    '\tabletypesize{}'."\n",
    '\tablewidth{\linewidth}'."\n",
    '\tablecaption{Cavity Properties.\label{tab:cavities}}'."\n",
    '\tablehead{'."\n",
    '\colhead{ID} & \colhead{$a$} & \colhead{$b$} & \colhead{$c$} & \colhead{$D$} & \colhead{\tsonic} & \colhead{\tbuoy} & \colhead{\trefill} & \colhead{\ecav} & \colhead{\pcav}\\\\'."\n",
    '\colhead{-} & \colhead{kpc} & \colhead{kpc} & \colhead{kpc} & \colhead{kpc} & \colhead{Myr} & \colhead{Myr} & \colhead{Myr} & \colhead{$10^{60}$ erg} & \colhead{$10^{45}$ erg s$^{-1}$}\\\\'."\n",
    '\colhead{(1)} & \colhead{(2)} & \colhead{(3)} & \colhead{(4)} & \colhead{(5)} & \colhead{(6)} & \colhead{(7)} & \colhead{(8)} & \colhead{(9)} & \colhead{(10)}}'."\n",
    '\startdata'."\n";
foreach $tab (@tables) {
    open(A,"$tab");
    while(<A>){
	chomp;
	if (/^\#/) {
	    next;
	}
	next if (/^$/);
	$b = $_;
	$b =~ s/\s+//;
	$b =~ s/\n//;
	$b =~ s/\&/ \& /;
	$b =~ s/\\pm/ \\pm /;
	$b =~ s/\\\\/\\\\\n/;
	print OUT $b;
    }
    close A;
}
print OUT
    '\enddata'."\n",
    '\tablecomments{'."\n",
    'Col. (1) Cavity identification;'."\n",
    'Col. (2) Semi-major axis;'."\n",
    'Col. (3) Semi-minor axis;'."\n",
    'Col. (4) Semi-polar axis;'."\n",
    'Col. (5) Distance from AGN;'."\n",
    'Col. (6) Sound speed age;'."\n",
    'Col. (7) Buoyancy age;'."\n",
    'Col. (8) Refill age;'."\n",
    'Col. (9) Cavity energy assuming $\gamma = 4/3$;'."\n",
    'Col. (10) Cavity power using \tbuoy.}'."\n",
    '\end{deluxetable*}'."\n";
close OUT;

############################
############################
############################

@tables = qw(pvout2.tex);
@params = ('\ecav', '\pcav', '\mbh', '\macc', '\dmacc', '\dmbh', '\ddmbh', '\dmedd', '\dmbon', '\dmacc/\dmedd', '\dmacc/\dmbon');
@units = ('$10^{60}$ erg', '$10^{45}$ \lum', '$10^9$ \msol', '$10^6$ \msol', '\msolpy', '$10^6$ \msol', '\msolpy', '\msolpy', '$10^{-4}$ \msolpy', '$10^{-3}$', '-');
open(OUT,">totals.tex");
print OUT
    '\begin{deluxetable*}{cccc}[ht]'."\n",
    '\tabletypesize{}'."\n",
    '\tablecaption{Cavity Derived Accretion Properties.\label{tab:totals}}'."\n",
    '\tablehead{\colhead{Row} & \colhead{Param.} & \colhead{Value} & \colhead{Units}}'."\n",
    '\startdata'."\n";
foreach $tab (@tables) {
    open(A,"$tab");
    while(<A>){
	chomp;
	if (/^\#/) {
	    next;
	}
	next if (/^$/);
	$b = $_;
	$b =~ s/\s+//;
	$b =~ s/\n//;
	$b =~ s/E\-0/\\times 10\^\{-/;
	$b =~ s/E\+0/\\times 10\^\{/;
	$b =~ s/\\\\//;
	push @data, $b;
    }
    close A;
}

$a = join("",@data);
@data2 = split(/&/,$a);
$i = 0;
foreach $b (@data2) {
    $num = $i+1;
    if ($b =~ m/times/g) {
	$b =~ s/\\pm/\} \\pm /;
	$b =~ s/\$$/\}\$/;
    }
    print OUT "($num) \& $params[$i] & $b \& $units[$i]\\\\\n";
    $i++;
}

print OUT
    '\enddata'."\n",
    '\tablecomments{'."\n",
    'Row (1) Total cavity energy;'."\n",
    'Row (2) Total cavity power;'."\n",
    'Row (3) Black hole mass;'."\n",
    'Row (4) Mass accreted;'."\n",
    'Row (5) Mass accretion rate;'."\n",
    'Row (6) Change in black hole mass;'."\n",
    'Row (7) Rate of chage for Row (6);'."\n",
    'Row (8) Eddington accretion rate;'."\n",
    'Row (9) Bondi accretion rate;'."\n",
    'Row (10) Eddington accretion ratio;'."\n",
    'Row (11) Bondi accretion ratio.}'."\n",
    '\end{deluxetable*}'."\n";
close OUT;
exit 0;
