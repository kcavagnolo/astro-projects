#! /usr/bin/perl -w

$option = "mau";

open(A,">out");
printf A "%-25s %6s %6s %6s %6s %6s\n","#Name","Obsid","Mdot","M500","tcool","ref";
if ($option eq "chen") {
  open(CHEN1,"../me_info/chen12.table");
  open(CHEN2,"../me_info/chen45.table");
  open(REF,"../me_info/reference.list");
}
if ($option eq "bau") {
  open(BAU,"../me_info/bauer.table");
  open(REF,"tcool_mdot_m.dat");
}
if ($option eq "mau") {
  open(MAU,"../me_info/maughan.table");
  open(REF,"tcool_mdot_m.dat");
}

if ($option eq "chen") {
  while(<CHEN1>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = $data[0];
    $info1{$name} = $_;
  }
  close CHEN1;
  while(<CHEN2>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = $data[0];
    $info2{$name} = $_;
  }
  close CHEN2;
}

if ($option eq "bau") {
  while(<BAU>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = $data[0];
    $info3{$name} = $_;
  }
  close BAU;
}

if ($option eq "mau") {
  while(<MAU>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = $data[0];
    $info4{$name} = $_;
  }
  close MAU;
}

if ($option eq "chen") {
  while(<REF>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @line=split;
    if (exists $info1{$line[0]}) {
      @temp1 = split(/\s+/,$info1{$line[0]});
      @temp2 = split(/\s+/,$info2{$line[0]});
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],$temp1[17],$temp2[6],$temp1[14]*10.0,"c";
    } else {
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],-1,-1.00,-1.00,"z";
    }
  }
  close REF;
  close A;
}

if ($option eq "bau") {
  while(<REF>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @line=split;
    if (exists $info3{$line[0]} && $line[5] ne "c") {
      @temp3 = split(/\s+/,$info3{$line[0]});
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],$line[2],$line[3],$temp3[11],"b";
    } else {
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],$line[2],$line[3],$line[4],$line[5];
    }
  }
  close REF;
  close A;
}

if ($option eq "mau") {
  while(<REF>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @line=split;
    if (exists $info4{$line[0]} && $line[5] ne "c") {
      @temp4 = split(/\s+/,$info4{$line[0]});
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],$line[2],$temp4[21]/10,$line[4],"m";
    } else {
      printf A "%-25s %6s %6.0f %6.2f %6.2f %6s\n",$line[0],$line[1],$line[2],$line[3],$line[4],$line[5];
    }
  }
  close REF;
  close A;
}
