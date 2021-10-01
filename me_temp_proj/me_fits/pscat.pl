#!/usr/bin/perl

$output = pop @ARGV;
$num = pop @ARGV;

@line = @ARGV;
foreach (@line) {
    push(@new,$_ . " ");
    if($_ eq $output) {
	print "Your output is your input!\n";
	exit;
    }
}

if (@ARGV == 0) {
    while (<>) {
	chomp;
	push(@new,$_ . " ");
	if($_ eq $output) {
	    print "Your output is your input!\n";
	    exit;
	}
    }
}

system "cat @new > temp.1234";
print "Now repaginating ${output}.\n";
system "ps2ps temp.1234 temp.2345";
system "psnup -${num} temp.2345 ${output}";
unlink "temp.1234";
unlink "temp.2345";
