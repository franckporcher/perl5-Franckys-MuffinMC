#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Franckys::MuffinMC' ) || print "Bail out!\n";
}

diag( "Testing Franckys::MuffinMC $Franckys::MuffinMC::VERSION, Perl $], $^X" );
