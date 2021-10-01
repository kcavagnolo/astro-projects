#! /usr/bin/perl -w

open(A,$ARGV[0]);
open(B,$ARGV[1]);
while(<A>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @line=split;
    $info{$line[1]} = $line[1];
}

while(<B>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @line=split;
    $id=$line[1];
    unless (exists $info{$id}) {
	print "$id not found in $ARGV[0]\n";
    }
}
