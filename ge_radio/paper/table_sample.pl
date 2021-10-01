#!/usr/bin/perl -w

# This script takes the XSPEC fit results .dat file and makes a latex
# table. To use this script:
# [linux%] perl table_sample.pl ../pf_info/sample_info.dat ../mc_ge/summaryb_loZ sample.tex

# useful settings for this table
# @items = ("cluster","obsid","ra","dec","exp","mode","z","nh","tx","fe","chip","lbol","eobs","rmax","robs");
@items = ("cluster","ra","dec","los","lbol");
@tabcolumns = ('l');
foreach (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);
$caption = "Summary of Sample" . '\label{tab:sample}';
@tabcomments = (
    'Source name',
    'R.A. of source centroid',
    'Decl. of source centroid',
    'Co-moving distance',
    'Bolometric (0.01-100 keV) luminosity'
    );

%tabnotes = (
    );

@tabrefs = (
    );
    
# read in file
open(INFILE,$ARGV[0]) || die "Cannot open $ARGV[0] for reading\n";
open(LXFILE,">$ARGV[1]") || die "Cannot open $ARGV[1] for writing\n";
open(OUTFILE,">$ARGV[2]") || die "Cannot open $ARGV[2] for writing\n";

# print header information
@lines = <INFILE>;
print_header();
$oldcluster = 'dfhksdjfhksd';
foreach $line (@lines) {
    next if ($line =~ /^\#/);
    next if ($line =~ /^$/);
    ($rawcluster,$obsid,$x,$y,$rmax,$mincts,$z,$nh,$tx,$fe,$lbol,$lbollo,
     $lbolhi,$chip,$eobs,$diff,$robs,$ra,$dec,$exp,$mode,$r12,$r45,$r67,
     $rat12,$rat45,$rat67) = split(" ",$line);
    $rawcluster =~ s/\_dag$//;
    $cluster = $rawcluster;
    $cluster =~ s/\_00/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/|| $cluster =~ /Zw/ || $cluster =~ /ZW/);
    $cluster =~ s/\_0/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/ || $cluster =~ /Zw/ || $cluster =~ /ZW/);
    $cluster =~ s/\_/ J/g if ($cluster =~ /MS/);
    $cluster =~ s/JJ/J/g;
    $cluster =~ s/\_/ /g;
    $cluster =~ s/BELL/bell/g;
    $cluster =~ s/YGNUS/ygnus/g;
    $cluster =~ s/YDRA/ydra/g;
    $cluster =~ s/ENTAURUS/entaurus/g;
    $cluster =~ s/ERCULES/ercules/g;
    $cluster =~ s/PHIUCHUS/phiuchus/g;
    $cluster =~ s/WICKY/wicky/g;
    $cluster =~ s/ERSIC/ersic/g;
    $obsid = sprintf("%4s",$obsid);
    $x = sprintf("%4i",$x);
    $y = sprintf("%4i",$y);
    $rmax = sprintf("%3i",$rmax);
    $mincts = sprintf("%6i",$mincts);
    $z = sprintf("%5.4f",$z);
    $nh = sprintf("%3.2f",$nh);
    $tx = sprintf("%3.2f",$tx);
    $fe = sprintf("%3.2f",$fe);
    $lbol = sprintf("%5.2f",$lbol);
    $lbollo = sprintf("%5.2f",$lbollo);
    $lbolhi = sprintf("%5.2f",$lbolhi);
    $chip =~ s/s/S/g;
    $chip =~ s/i/I/g;
    $chip = sprintf("%2s",$chip);
    $eobs = sprintf("%3.2f",$eobs);
    $diff = sprintf("%3s",$diff);
    $robs = sprintf("%3i",$robs);
    $ra =  sprintf("%10s",$ra);
    $dec = sprintf("%10s",$dec);
    $exp = sprintf("%3.1f",$exp);
    $mode =~ s/^FAINT/F/g;
    $mode =~ s/^VFAINT/VF/g;
    $mode = sprintf("%2s",$mode);
    $r12 = sprintf("%5.4f",$r12);
    $r45 = sprintf("%5.4f",$r45);
    $r67 = sprintf("%5.4f",$r67);
    $rat12 = sprintf("%5.4f",$rat12);
    $rat45 = sprintf("%5.4f",$rat45);
    $rat67 = sprintf("%5.4f",$rat67);

    # write all IDL commands to a file and then run
    open(PROFILE,">temp.pro");
    print PROFILE "\!quiet=1\n";
    print PROFILE "cosmology,'$z',result,/silent\n";
    print PROFILE "los = result[0]\n";
    print PROFILE "openw,1,'temp.log'\n";
    print PROFILE "printf,1,strcompress(los,/remove_all)\n";
    print PROFILE "close,1\n";
    print PROFILE "exit \n";
    close PROFILE;
    system("\$IDL_DIR/bin/idl temp.pro");
    open(FILE,"temp.log");
    while(<FILE>) {
	chomp;
	next if (/^\#/);
	next if (/^$/);
	s/^\s+//;  # trim leading whitespace
	s/\s+$//;  # trim trailing whitespace
	@vals = split;
	$los = $vals[0];
    }
    close FILE;
    unlink("temp.log");
    unlink("temp.pro");

    # format
    $los = sprintf("%.1f",$los);

    # don't repeat entries for the same cluster
    if ($rawcluster eq $oldcluster) {
	$cluster = '';
	$ra = '\nodata';
	$dec = '\nodata';
	$z = '\nodata';
	$los = '\nodata';
	$lbol = '\nodata';
	$tx = '\nodata';
    }

    # make hash to hold values
    %values = (
	"cluster"=> $cluster,
	"obsid"   => $obsid,
	"x"=> $x,
	"y"=> $y,
	"rmax"=> $rmax,
	"mincts"=> $mincts,
	"z"=> $z,
	"los" => $los,
	"nh"=> $nh,
	"tx"=> $tx,
	"fe"=> $fe,
	"lbol"=> $lbol,
	"lbolo"=> $lbollo,
	"lbolhi"=> $lbolhi,
	"chip"=> $chip,
	"eobs"=> $eobs,
	"diff"=> $diff,
	"robs"=> $robs,
	"ra"=> $ra,
	"dec"=> $dec,
	"exp"=> $exp,
	"mode"=> $mode,
	"r12"=> $r12,
	"r45"=> $r45,
	"r67"=> $r67,
	"rat12"=> $rat12,
	"rat45"=> $rat45,
	"rat67"=> $rat67
	);
    @out = @values{@items};
    print OUTFILE join(" & ",@out),"\\\\\n";
    $oldcluster = $rawcluster;
}

# print the footer
print_footer();

# exit cleanly
exit 0;

############################################
############################################

sub print_colhead {
    my ($line,$out);
    @out = @_;
    foreach $out (@out) {
	$out = '\colhead{'.$out.'}';
    }
    $line = join(' & ',@out);
    return $line;
}

############################################
############################################

sub print_header {

    # define some useful hashes
    %colhead = (
	"cluster"=> "Source",
	"obsid"   => "Obs. ID",
	"x"=> "Phys. X",
	"y"=> "Phys. Y",
	"rmax"=> "\$R\_{max}\$",
	"mincts"=> "Min. Cts.",
	"z"=> "\$z\$",
	"los"=> "\$D_C\$",
	"nh"=> "\$N\_{HI}\$",
	"tx"=> "\$kT\_{X}\$",
	"fe"=> "Z",
	"lbol"=> "\$L\_{bol.}\$",
	"chip"=> "ACIS",
	"eobs"=> "\$\E_{obs}\$",
	"diff"=> "Diffuse",
	"robs"=> "\$R\_{break}\$",
	"ra"=> "R.A.",
	"dec"=> "Decl.",
	"exp"=> "Exposure Time",
	"mode"=> "Mode",
	"r12"=> "R12",
	"r45"=> "R45",
	"r67"=> "R67",
	"rat12"=> "R12\/Src",
	"rat45"=> "R45\/Src",
	"rat67"=> "R67\/Src"
	);
    %units = (
	"cluster"=> "-",
	"obsid"   => "-",
	"x"=> "pix",
	"y"=> "pix",
	"rmax"=> "pix",
	"mincts"=> "counts",
	"z"=> "-",
	"los"=> "Mpc",
	"nh"=> "\$10^{20} cm^{-2}\$",
	"tx"=> "keV",
	"fe"=> "Z\$\_\{\\sun\}\$",
	"lbol"=> "\$10^{44} \\lum\$",
	"chip"=> "-",
	"eobs"=> "keV",
	"diff"=> "-",
	"robs"=> "pix",
	"ra"=> "hr\:min\:sec",
	"dec"=> "\$\\mydeg\:\\arcm\:\\arcs\$",
	"exp"=> "ksec",
	"mode"=> "-",
	"r12"=> "\\ctps",
	"r45"=> "\\ctps",
	"r67"=> "\\ctps",
	"rat12"=> "-",
	"rat45"=> "-",
	"rat67"=> "-",
	"notes"=> "-"
	);

    # make first header row
    @header1 = @colhead{@items};
    @header2 = @units{@items};

    # make row of column numbers
    $nhead = @header1;
    @header3 = map { '{(' . $_ . ')}'} (1..$nhead);

    # print the table header
    print OUTFILE (
	'\begin{deluxetable}{' . $tabcolumns . '}',"\n",
	'\tablewidth{0pt}',"\n",
	'\tabletypesize{\scriptsize}',"\n",
	'\tablecaption{' . $caption . '}',"\n",
	'\tablehead{',
	print_colhead(@header1),'\\\\',"\n",
	print_colhead(@header2),'\\\\',"\n",
	print_colhead(@header3),'}',"\n",
	'\startdata',"\n"
	);
}

############################################
############################################

sub print_footer{
    print OUTFILE '\enddata',"\n";
    my $num = 1;
    if ($#tabcomments > 0) {
	foreach $item (@tabcomments) {
	    $num = sprintf("%i",$num);
	    push @comments, 'Col. ('.$num.')'.' '.$item;
	    $num++;
	}
	$comments = join("; ",@comments);
	print OUTFILE '\tablecomments{'.$comments.'.}'."\n";
    }
    if (scalar keys %tabnotes > 0) {
	foreach $key (sort keys %tabnotes) {
	    my $val = $tabnotes{$key};
	    print OUTFILE '\tablenotetext{'.$key.'}{'.$val.'}'."\n";
	}
    }
    if (scalar keys %tabrefs > 0) {
	$num = 1;
	foreach $item (@tabrefs) {
	    $num = sprintf("%i",$num);
	    push @refs, '('.$num.')'.' '.$item;
	    $num++;
	}
	$refs = join("; ",@refs);
	print OUTFILE '\tablerefs{'.$refs.'.}'."\n";
    }
    print OUTFILE '\end{deluxetable}'."\n",
}

############################################
############################################
