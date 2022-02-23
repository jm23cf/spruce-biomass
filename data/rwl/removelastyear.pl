#!/usr/bin/perl
#
# Copyright (c) 2011 Matthew Emmett <matthew@emmett.ca>
#
# Permission to use, copy, modify, and distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF MIND, USE,
# DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
# OR PERFORMANCE OF THIS SOFTWARE.
#

use List::Util qw[min max];

use Getopt::Std;
use Pod::Usage;

######################################################################
# routines

sub print_row {
    ($transect, $year, $n, @measurements) = @_;

    print OUT sprintf("%-6s  %4d ", $transect, $year);
    @chunk = splice(@measurements, 0, $n);
    print OUT join(" ", map { sprintf("%5d", $_ ) } @chunk);
    print OUT "\n";
}

sub sort_samples {
    $a =~ /.*?[LD](\d+)(.*)/;
    $a_num = $1;
    $a_lex = $2;

    $b =~ /.*?[LD](\d+)(.*)/;
    $b_num = $1;
    $b_lex = $2;

    return 1 if ($a_num > $b_num);
    return -1 if ($a_num < $b_num);
    return $a_lex <=> $b_lex;
}

######################################################################
# main

while (my $file = <*/*.raw */*.rwl>) {
    next if $file =~ /dating/;

    print "$file\n";

    # read input and parse
    open(RAW, "<$file") or die("Error opening input file '$file': $!.\n");
    @raw = <RAW>;
    close(RAW);

    %samples = ();
    foreach (@raw) {
	@cols = split;
	$sample = $cols[0];
	$year   = $cols[1];
	@widths = @cols[2..$#cols];

	$w = 0;
	foreach (@widths) {
	    #print "$sample $year $w $_\n";
	    $samples{$sample}{$year+$w} = $_;
	    $w ++;
	}
    }

    system("cp $file $file.bak");

    open(OUT, ">$file") or die("Error writing to output file '$file': $!.\n");
    @keys = sort sort_samples keys %samples;
    foreach (@keys) {
    	$fyog = min(keys %{$samples{$_}});
    	$lyog = max(keys %{$samples{$_}});
	
    	# rip off the last year
    	$samples{$_}{$lyog-1} = $samples{$_}{$lyog};
    	$lyog = $lyog - 1;

    	@measurements = ();
    	for ($i=$fyog; $i<=$lyog; $i++) {
    	    push(@measurements, $samples{$_}{$i});
    	}

    	$first_decade = ( int($fyog/10) + 1 ) * 10;
    	print_row($_, $fyog, $first_decade-$fyog, @measurements);
    	for ($i=$first_decade; $i<=$lyog; $i+=10) {
    	    print_row($_, $i, 10, @measurements);
    	}
    }
    close(OUT);
}
