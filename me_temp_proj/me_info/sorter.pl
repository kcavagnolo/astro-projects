#! /usr/bin/perl -w
$i=0;
while (<>) {
    chomp;
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    push @hdr,"$_\n" if (/^\#/);
    next if (/^\#/);
    @data = split;
#    $name = join "_", $data[0],$i;
    $name = join "_", $data[0],$data[1],$i;
    $info{$name} = $_;
    $i++;
}

open(LOG,">out");
print LOG "@hdr";
foreach $key (sort keys %info) {
    print LOG "$info{$key}\n";
}
close LOG;
