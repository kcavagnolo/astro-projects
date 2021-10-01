#!/usr/bin/perl
# Perl script to display XMMS song info on Torsmo
# Requires XMMS::InfoPipe module and xmms-infopipe plugin

use XMMS::InfoPipe;

my $numArgs = $#ARGV+1;

if($numArgs != 1) {
    print "Usage error: xmms.pl [keyname]\n";
    exit;
}

else {
    my $key = $ARGV[0];
    my $xmms = XMMS::InfoPipe->new();
    my $playing = $xmms->is_playing();
    my $paused = $xmms->is_paused();

   # display info since XMMS is playing/paused
    if($playing == 1 || $paused == 1) {

      # extract song artist
	if($key eq "Artist") {
	    my $title = $xmms->{info}->{Title};
	    @song = split(/ - /, $title);
	    my $artist = $song[0];

         #truncate long artist names
	    if(length($artist) >= 30) {
		my $temp = substr($artist, 0, 30);
		$artist = $temp . "...";
	    }
	    print $artist , "\n";
	}

      # extract song title
	elsif($key eq "Title") {
	    my $title = $xmms->{info}->{Title};
	    @song = split(/ - /, $title);
	    print $song[1] , "\n";      
	}

      # join position and length times, ie 2:04 / 4:50
	elsif($key eq "Time") {
	    my $length = $xmms->{info}->{Time};
	    my $position = $xmms->{info}->{Position};
	    print $position , " / " , $length , "\n";
	}

      # format bitrate, ie 192 kbps
	elsif($key eq "Bitrate") {
	    my $bitrate = $xmms->{info}->{"Current bitrate"};
	    print $bitrate/1000 , " kbps\n";
	}

      # number of tunes in playlist
	elsif($key eq "Playlist") {
	    my $tunes = $xmms->{info}->{"Tunes in playlist"};
	    print $tunes , " tunes\n";
	}

      # print out info for other keys (case sensitive, 'cat /tmp/xmms-info')
	else {
	    print $xmms->{info}->{$key} , "\n";
	}
    }

   # display status only since XMMS is not playing/paused
    else {
	if($key eq "Status") {
	    print $xmms->{info}->{Status} , "\n";
	}
    }
}
