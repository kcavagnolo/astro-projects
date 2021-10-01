#! /usr/bin/perl -w

system("rm -f all_results.log");
system("ls -1 *_results.log > junk");
open(A,"junk");
open(B,">temp");
while(<A>){
    chomp;
    @a = split;
    $file = $a[0];
    open(C,$file);
    while(<C>){
	chomp;
	next if (/\#/);
	next if (/^$/);
	print B "$_\n";
    }
    close C;
}
close A;
close B;
system("~/research/redux/scripts/sorter.pl temp all_results.log");
system("rm -f junk temp");
exit 0;
