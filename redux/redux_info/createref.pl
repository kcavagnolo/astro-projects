#! /usr/bin/perl -w

$file = "../../me_temp_proj/me_fits/dat/c2fits_final_r2500-50_fefree_7-7.dat";
open(A,"$file");
while(<A>){
  chomp;
  next if (/^\#/);
  next if (/^$/);
  @line=split;
  unless (exists $r2{$line[0]}) {
    $r2{$line[0]}=$_;
  } else {
    print "$line[0] already found in $file\n";
  }
}
close A;

$file = "../../me_temp_proj/me_fits/dat/c2fits_final_r5000-50_fefree_7-7.dat";
open(A,"$file");
while(<A>){
  chomp;
  next if (/^\#/);
  next if (/^$/);
  @line=split;
  unless (exists $r5{$line[0]}) {
    $r5{$line[0]}=$_;
  } else {
    print "$line[0] already found in $file\n";
  }
}
close A;

$file = "unused_ref.list";
open(A,"$file");
#open(B,">out");
#open(C,">defunct_me.list");
while(<A>){
  chomp;
  next if (/^\#/);
  next if (/^$/);
  @line=split;
  if (exists $r2{$line[0]}){
      print "$line[0] found in r2500-50\n";
#    print B "$_"."    BOTH\n";
  } elsif (exists $r5{$line[0]}){
      print "$line[0] found in r5000-50\n";
#    print B "$_"."    R5ONLY\n";
  } else {
#    print C "$_\n";
    $count++;
  }
}
print "$count\n";
close A;
#close B;
#close C;
exit 0;
