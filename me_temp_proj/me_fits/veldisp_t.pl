#! /usr/bin/perl -w

open(A,">out");
printf A "%-25s %6s %6s %6s %6s %8s %8s %8s\n","#Name","Obsid","tx","tlo","thi","sigma","siglo","sighi";
open(AGU,"../me_info/aguerri.table");
#open(REF,"dat/tsm_r2500-50_7-7.dat");
open(REF,"dat/final_r2500-50_fefree_7-7.dat");

while(<AGU>){
  chomp;
  next if (/^\#/); # skip comment lines
  next if (/^$/);  # skip blank lines
  s/^\s+//;        # trim leading whitespace
  s/\s+$//;        # trim trailing whitespace
  @data = split;
  $name = $data[0];
  $info{$name} = $_;
}
close AGU;

while(<REF>){
  chomp;
  next if (/^\#/); # skip comment lines
  next if (/^$/);  # skip blank lines
  s/^\s+//;        # trim leading whitespace
  s/\s+$//;        # trim trailing whitespace
  @line=split;
  if (exists $info{$line[0]}) {
    @temp = split(/\s+/,$info{$line[0]});
    printf A "%-25s %6s %6.2f %6.2f %6.2f %8.2f %8.2f %8.2f\n",
      $line[0],$line[1],$line[7],$line[8],$line[9],$temp[6],$temp[8],$temp[7];
  }
}
close REF;
close A;
