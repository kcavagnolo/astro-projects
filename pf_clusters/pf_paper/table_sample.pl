#!/usr/bin/perl -w

# This script takes the XSPEC fit results .dat file and makes a latex
# table. To use this script:
# [linux%] perl table_sample.pl ../pf_info/sample_info.dat sample.tex

# useful settings for this table
#@items = ("cluster","obsid","ra","dec","exp","mode","z","nh","tx","fe","chip","lbol","eobs","rmax","robs");

# read in the master table
open(A,"../pf_fits/master.table");
while(<A>){
    chomp;
    @line = split;
    next if (/^\#/);
    next if (/^$/);
    $master{$line[0]} = $_;
}
close A;

# now read in the items to print out
#@items = ("cluster","obsid","ra","dec","exp","chip","z","tx","lbol","notes");
@items = ("cluster","obsid","ra","dec","exp","chip","z","tx","notes");
@tabcolumns = ('l');
foreach (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);
$caption = "Summary of Sample" . '\label{tab:sample}';
@tabcomments = (
    'Cluster name',
    'CXC CDA Observation Identification Number',
    'R.A. of cluster center',
    'Decl. of cluster center',
    'exposure time',
#    'observing mode',
    'CCD location of cluster center',
    'redshift',
    'average cluster temperature',
    'best-fit core entropy measured in this work',
#    'cluster bolometric luminosity',
    'assigned notes: `a\' Clusters analyzed using the best-fit $\beta$-model for the surface brightness profiles (discussed in \S\ref{sec:dene}); `b\' Clusters with complex surface brightness of which only the central regions were used in fitting $K(r)$; `c\' Clusters only used during analysis of the \hifl\ sub-sample (discussed in \S\ref{sec:hifl}); `d\' Clusters with central AGN removed during analysis (discussed in \S\ref{sec:centsrc}); `e\' Clusters with central compact source removed during analysis (discussed in \S\ref{sec:centsrc}); `f\' Clusters with central bin ignored during fitting (discussed in \S\ref{sec:centsrc}).'
    );

%tabnotes = (
    );

@tabrefs = (
    );
    
# read in file
open(INFILE,$ARGV[0]) || die "Can't open $ARGV[0] for reading\n";
open(OUTFILE,">$ARGV[1]") || die "Can't open $ARGV[1] for writing\n";
@lines = <INFILE>;

# print header information
print_header();
$oldcluster = 'dfhksdjfhksd';
foreach $line (@lines) {
    next if ($line =~ /^\#/);
    next if ($line =~ /^$/);
    ($rawcluster,$obsid,$x,$y,$rmax,$mincts,$z,$nh,$tx,$fe,$lbol,$lbollo,
     $lbolhi,$chip,$eobs,$diff,$robs,$ra,$dec,$exp,$mode,$r12,$r45,$r67,
     $rat12,$rat45,$rat67) = split(" ",$line);
    $rawcluster =~ s/\_dag$//;

    # read from master table
    if (exists $master{$rawcluster}) {
	@data = split(/\s+/,$master{$rawcluster});
	$k0 = sprintf("%8.1f",$data[5]);
	$k0err = sprintf("%8.1f",$data[6]);
	$k100 = sprintf("%8.1f",$data[7]);
	$k100err = sprintf("%8.1f",$data[8]);
	$alpha = sprintf("%6.2f",$data[9]);
	$aerr = sprintf("%6.2f",$data[10]);
	$lbol = $data[11]/1e44;
	$lbol = sprintf("%6.2f",$lbol);
	$lha = $data[12]/1e40;
	if ($lha <= 0.0) {
	    $lha = sprintf("%6s","n/a");
	} else {
	    $lha = sprintf("%6.2f",$lha);
	}
	$lrad = $data[14]/1e40;
	if ($lrad <= 0.0) {
	    $lrad = sprintf("%6s","n/a");
	} else {
	    $lrad = sprintf("%6.2f",$lrad);
	}
    } else {
	$k0 = sprintf("%",-1.00);
	$k0err = sprintf("%",-1.00);
	$k100 = sprintf("%",-1.00);
	$k100err = sprintf("%",-1.00);
	$alpha = sprintf("%",-1.00);
	$aerr = sprintf("%",-1.00);
	$lbol = sprintf("%",-1.00);
	$lha = sprintf("%",-1.00);
	$lrad = sprintf("%",-1.00);
    }

    # format the name
    %beta = ("ABELL_1060"=>"", "ABELL_2256"=>"", "ABELL_0119"=>"",
	     "ABELL_0160"=>"", "ABELL_0193"=>"", "ABELL_0400"=>"",
	     "ABELL_1240"=>"", "ABELL_1736"=>"", "ABELL_2125"=>"",
	     "ABELL_2255"=>"", "ABELL_2319"=>"", "ABELL_2462"=>"",
	     "ABELL_2631"=>"", "ABELL_3376"=>"", "ABELL_3391"=>"",
	     "ABELL_3395"=>"", "MKW_08"=>"", "RBS_0461"=>"");
    %dbeta = ("ABELL_0539"=>"", "ABELL_1644"=>"", "ABELL_2107"=>"",
	      "ABELL_2199"=>"", "AWM7"=>"", "CENTAURUS"=>"",
	      "ESO_3060170"=>"", "MKW_08"=>"", "MS_J1157.3+5531"=>"",
	      "RX_J1302.2+3308"=>"" );
    %hifl = ("M49"=>"","NGC_4636"=>"","NGC_5044"=>"","NGC_0507"=>"",
	     "NGC_5813"=>"","NGC_5846"=>"");
    %asrc = ("3C_295"=>"", "3C_388"=>"", "4C_55.16"=>"",
	     "ABELL_0426"=>"", "ABELL_2052"=>"", "ABELL_3112"=>"",
	     "ABELL_3581"=>"", "CYGNUS_A"=>"", "HYDRA_A"=>"",
	     "M87"=>"", "RBS_0797"=>"",
	     "ZWICKY_1742"=>"");
    %csrc = ("ABELL_0119"=>"",
	     "ABELL_0160"=>"", "ABELL_0193"=>"", "ABELL_0223"=>"",
	     "ABELL_0400"=>"", "ABELL_0539"=>"", "ABELL_0562"=>"",
	     "ABELL_0576"=>"", "ABELL_0611"=>"", "ABELL_0744"=>"",
	     "ABELL_1060"=>"", "ABELL_2151"=>"", "ABELL_2670"=>"",
	     "ABELL_2717"=>"", "ABELL_3558"=>"", "ABELL_3822"=>"",
	     "ABELL_1736"=>"", "ABELL_2462"=>"", "ABELL_3391"=>"",
	     "ABELL_3395"=>"", "MACS_J0547.0-3904"=>"",
	     "MACS_J1931.8-2634"=>"", "RX_J1320.2+3308"=>"",
	     "ZwCl_0857.9+2107"=>"", "RBS_0461"=>"");
    %ibin = ("2PIGG_J0011.5-2850"=>"",
	     "ABELL_1060"=>"",
	     "ABELL_2034"=>"",
	     "ABELL_3558"=>"",
	     "RX_J1022.1+3830"=>"",
	     "SC_1327-312"=>"");

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
    undef @notes;
    push @notes, "a" if (exists $beta{$rawcluster});
    push @notes, "b" if (exists $dbeta{$rawcluster});
    push @notes, "c" if (exists $hifl{$rawcluster});
    push @notes, "d" if (exists $asrc{$rawcluster});
    push @notes, "e" if (exists $csrc{$rawcluster});
    push @notes, "f" if (exists $ibin{$rawcluster});
    if (@notes) {
	$notes = join(",",@notes);
    } else {
	$notes = '\nodata';
    }
    $notes = sprintf("%6s",$notes);
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
    $k0 = $k0 . ' $\pm$ ' . $k0err;
    $k100 = $k100 . ' $\pm$ ' . $k100err;
    $alpha = $alpha . ' $\pm$ ' . $aerr;

    # don't repeat entries for the same cluster
    if ($rawcluster eq $oldcluster) {
	$cluster = '';
	$ra = '\nodata';
	$dec = '\nodata';
	$z = '\nodata';
	$lbol = '\nodata';
	$tx = '\nodata';
	$notes = '\nodata';
	$k0 = '\nodata';
	$k100 = '\nodata';
	$alpha = '\nodata';
	$lha = '\nodata';
	$lrad = '\nodata';
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
	"rat67"=> $rat67,
	"notes"=> $notes,
	"k0"=> $k0,
	"k100"=> $k100,
	"alpha"=> $alpha,
	"lha"=> $lha,
	"lrad"=> $lrad
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
	"cluster"=> "Cluster",
	"obsid"   => "Obs. ID",
	"x"=> "Phys. X",
	"y"=> "Phys. Y",
	"rmax"=> "\$R\_{max}\$",
	"mincts"=> "Min. Cts.",
	"z"=> "\$z\$",
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
	"rat67"=> "R67\/Src",
	"notes"=> "Notes",
	"k0"=> "\\kna",
	"k100"=> "\\khun",
	"alpha"=> "\$\\alpha\$",
	"lha"=> "\$L\_{\\halpha}\$",
	"lrad"=> "\$\\nu L\_{\\nu}\$"
	);
    %units = (
	"cluster"=> "",
	"obsid"   => "",
	"x"=> "pix",
	"y"=> "pix",
	"rmax"=> "pix",
	"mincts"=> "counts",
	"z"=> "",
	"nh"=> "\$10^{20} cm^{-2}\$",
	"tx"=> "keV",
	"fe"=> "Z\$\_\{\\sun\}\$",
	"lbol"=> "\$10^{44} \\lum\$",
	"chip"=> "",
	"eobs"=> "keV",
	"diff"=> "",
	"robs"=> "pix",
	"ra"=> "hr\:min\:sec",
	"dec"=> "\$\\mydeg\:\\arcm\:\\arcs\$",
	"exp"=> "ksec",
	"mode"=> "",
	"r12"=> "\\ctps",
	"r45"=> "\\ctps",
	"r67"=> "\\ctps",
	"rat12"=> "",
	"rat45"=> "",
	"rat67"=> "",
	"notes"=> "",
	"k0"=> "\\ent",
	"k100"=> "\\ent",
	"alpha"=> "",
	"lha"=> "\$10^{40} \\lum\$",
	"lrad"=> "\$10^{40} \\lum\$"
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
