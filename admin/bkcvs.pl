#! /usr/bin/perl -w

@directories = qq{ /usr/local/ /mnt/DROBO/accept };
$drive = '/mnt/ABYSS';
$destination = "${drive}/";

# Mount disk
$unmounted = 1;
$fail = 0;

open(MTAB,"/etc/mtab") || die "No /etc/mtab, WTF!\n";
while (<MTAB>) {
    chomp;
    if ($_ =~ /$drive/) {
	print STDERR "Drive already mounted. Skipping the hard stuff...\n";
	$unmounted = -1;
	last;
    }
}
close(MTAB);    

while ($unmounted > 0) {
    system("mount $drive");
    if (($? >> 8) != 0) {
	warn "Drive didn't mount right...we'll assume it's sleeping...\n";
	sleep(15);
	$fail++;
    }
    else {
	print STDERR "Drive mounted successfully!\n";
	$unmounted = 0;
    }
    if (($fail >= 4)&&($unmounted == 1)) {
	die "Drive didn't mount after $fail tries. Aborting.\n";
    }
}

# Run Rsync
$logfile = "/home/cavagnolo/admin/abyss_rsync.log";
open(A,">$logfile");
print A "Started back-up at local time: ",scalar(localtime),"\n";
close A;
foreach $d (@directories) {
    print STDERR "Backing up $d to $destination\n";
    $pid = system("rsync -avE $d $destination >> $logfile");
    waitpid($pid,0);
    print STDERR ".\n";
}
open(A,">>$logfile");
print A "Finished back-up at local time: ",scalar(localtime),"\n";
close A;

# Umount disk
if ($unmounted == 0) {
    system("umount $drive");
}
