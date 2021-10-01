#! /usr/bin/perl -w

open(A,"r5000.tex");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    next if (/^\\/);
    next if (/^\}/);
    @line=split("&");
    $line[0] =~ s/\$\\dagger\$//g;
    $line[0] =~ s/^\s+//;
    $line[0] =~ s/\s+$//;
    $line[0] =~ s/ /_/;
    $a{$line[0]} = $line[0];
}
close A;

open(A,"r2500.tex");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    next if (/^\\/);
    next if (/^\}/);
    @line=split("&");
    $line[0] =~ s/\$\\dagger\$//g;
    $line[0] =~ s/^\s+//;
    $line[0] =~ s/\s+$//;
    $line[0] =~ s/ /_/;
    print "$line[0]\n" unless (exists $a{$line[0]});
}
close A;

exit 0;
