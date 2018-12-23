#!/usr/bin/perl -w -- 

use strict;

while (<>) {
    chomp;
    print STDERR "got '$_'\n";
    if (/#?([A-Z0-9]{2})([A-Z0-9]{2})([A-Z0-9]{2})/gi) {
#        my @vars = unpack 'H2H2H2', $1;
        print STDERR "vars = $1 $2 $3\n";
        printf STDERR "[ %.2f, %.2f, %.2f, ]\n", hex($1) / 255, hex($2) / 255, hex($3) / 255;
    }
    else {
        print STDERR "no match\n";
    }
}
