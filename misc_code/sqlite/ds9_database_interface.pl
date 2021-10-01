#! /usr/bin/perl -w
# (c) 2007 Chris Waters <watersc1@pa.msu.edu>

$catalog = '/orion3/final_gclf/data/catalog.db';

use DBI;
use DBD::SQLite;
use XPA;

@keys = ('RA','DEC','V','dV','I','dI','Color','dColor',
	 'instr_V','instr_I','instr_dV','instr_dI',
	 'apcor_V','apcor_I','complete_V','complete_I','completeness',
	 'elongation_v','elongation_i','isoarea_v','isoarea_i',
	 'halffluxradius_v','halffluxradius_i',
	 'maxmu_v','maxmu_i','meanmu_v','meanmu_i',
	 'EXCLUDE','quality',
	 'wfpc2_V','wfpc2_dV','wfpc2_I','wfpc2_dI',
	 'wfpc2_merge','wfpc2_completeness',
	 'mass','Lv',
	 'sk_concentration','sk_rtidal','sk_x0','sk_y0','sk_xp','sk_yp',
	 'sk_mag','sk_bkg','sk_sig','sk_chi',
	 'acsvcs_xray_IAU_name','acsvcs_xray_cps','acsvcs_xray_luminosity',
	 'acsvcs_xray_hardness21','acsvcs_xray_hardness31');



$dbhandle = DBI->connect("dbi:SQLite:dbname=$catalog","","") ||
    die "Unable to open catalog database $catalog: $DBI::errstr\n";

$x = 10;
while ($x > 0) {
    ($x,$y) = XPA::get_location();
    %best = ();
    $R = 25;
    if (($x < 0)||($x =~ /NaN/)) {
	last;
    }
    $sth = $dbhandle->prepare(qq{SELECT * from clusters WHERE abs(Xi - $x) < 25 AND abs(Yi - $y) < 25});
    $sth->execute();
    while ($hr = $sth->fetchrow_hashref) {
	%row = %{ $hr };
	
	$r = sqrt( ($row{Xi} - $x)**2 + ($row{Yi} - $y)**2);
	if ($r < $R) {
	    $R = $r;
	    %best = %row;
	}
    }
    $sth->finish;
    
    foreach $k (@keys) {
	unless ((exists($best{$k}))&&(defined($best{$k}))) {
	    $best{$k} = ' ';
	}
    }
    write;
}

$dbhandle->disconnect;



format STDOUT =
===============================================================================
@||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
$catalog
@<<<<<<<< @<<<<<<<< ==> @<<<<<<<< @<<<<<<<<      R = @<<<<<<<<<
$x,$y,$best{Xi},$best{Yi},$R
RA: @<<<<<<<<<<<< DEC: @<<<<<<<<<<<<             merge = @<<<<<<<<<
$best{RA},$best{DEC},$best{merge_sep}

                      V                    I                        
MAGNITUDES:  @<<<<<< +/- @<<<<<<  @<<<<<< +/- @<<<<<<  @<<<<<< +/- @<<<<<<
$best{V},$best{dV},$best{I},$best{dI},$best{Color},$best{dColor}
INSTRUMENT:  @<<<<<< +/- @<<<<<<  @<<<<<< +/- @<<<<<<  
$best{instr_V},$best{instr_dV},$best{instr_I},$best{instr_dI}
APCOR:       @<<<<<<              @<<<<<<
$best{apcor_V},$best{apcor_I}
COMPLETE:    @<<<<<<              @<<<<<<              @<<<<<<
$best{complete_V},$best{complete_I},$best{completeness}

ELONGATION:  @<<<<<<              @<<<<<<
$best{elongation_v},$best{elongation_i}
ISOAREA:     @<<<<<<              @<<<<<<
$best{isoarea_v},$best{isoarea_i}
R_HALF_SEX:  @<<<<<<              @<<<<<<
$best{halffluxradius_v},$best{halffluxradius_i}
MAXMU:       @<<<<<<              @<<<<<<
$best{maxmu_v},$best{maxmu_i}
MEANMU_SEX:  @<<<<<<              @<<<<<<
$best{meanmu_v},$best{meanmu_i}

EXCLUDE:     @<<<<<<              QUALITY:    @<<<<<<
"---",$best{quality}

WFPC2_MAG:   @<<<<<< +/- @<<<<<<  @<<<<<< +/- @<<<<<<
$best{wfpc2_V},$best{wfpc2_dV},$best{wfpc2_I},$best{wfpc2_dI}
WFPC2_SEP:   @<<<<<<              WFPC2_COMP: @<<<<<<
$best{wfpc2_merge},$best{wfpc2_completeness}

MASS:        @<<<<<<<<<           Lv:         @<<<<<<<<<
$best{mass},$best{Lv}

SK_C:        @<<<<<<              SK_Rt:      @<<<<<<
$best{sk_concentration},$best{sk_rtidal}
SK_X0:       @<<<<<<              SK_Y0:      @<<<<<<
$best{sk_x0},$best{sk_y0}
SK_XP:       @<<<<<<              SK_YP:      @<<<<<<
$best{sk_xp},$best{sk_yp}
SK_MAG:      @<<<<<<              SK_BKG:     @<<<<<<
$best{sk_mag},$best{sk_bkg}
SK_SIG:      @<<<<<<              SK_CHI:     @<<<<<<
$best{sk_sig},$best{sk_chi}

ACSVCS_XRAY_IAU_NAME : @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$best{acsvcs_xray_IAU_name}
CPS:         @<<<<<<<             LUMINOSITY: @<<<<<<<
$best{acsvcs_xray_cps},$best{acsvcs_xray_luminosity}
HARDNESS21:  @<<<<<<<             HARDNESS31: @<<<<<<<
$best{acsvcs_xray_hardness21},$best{acsvcs_xray_hardness31}
===============================================================================


.
