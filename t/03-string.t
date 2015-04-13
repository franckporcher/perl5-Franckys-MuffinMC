#!perl -T
use 5.16.0;
use strict;
use warnings;
use Test::Most;

use Franckys::MuffinMC;
use Franckys::Error;
use Franckys::Trace;

##
# Global cumulative error object
#
Franckys::Error::def_error(EVAR            => 'Undefined variable: %s');
Franckys::Error::def_error(EFUNC           => 'Undefined function: %s');
Franckys::Error::def_error(EMATHS          => 'Math error: %s');

##
# Error checking
#
my $ERROR = Franckys::Error::Error();
sub no_error {
    is( $ERROR->nb_errors(), 0, "Check error" )
        or do { diag("Trapped error: ($_)") foreach $ERROR->msgs() };
}


##
# Test list expressions
#
sub should {
    my ($test, $expr, $ref) = @_;

    my $final = muffin_eval($expr, $ERROR);
    is_deeply( $final,
               $ref,
               "$test",
    ) or diag explain $final;

    no_error();
}

should('string', '=(digits  0 1 2 3 4 5 6 7 8 9)',                      [0..9]            );
should('string', '=(letters a b c d e f)',                              ['a' .. 'f']       );
should('string', '=(hex     $(digits) $(letters))',                     [0..9, 'a' .. 'f'] );

my @hexdigits = (0..9, 'a' .. 'f');
my ($a,$b,$c);
my @bytes
    = map { 
        $a = $_;
        map { 
            $b = $_;
            "${a}${b}"
        } @hexdigits
      } @hexdigits;
my $i = 1;

should('string', '=(bytes   "$(hex)$(hex)")', \@bytes);
foreach my $byte (@bytes) {
    should('string', "\@(bytes $i)", [ $byte ]);
    $i++;
}


# The end
no_error();
done_testing();

__END__


@($(L) 1 1 2 3 3)   -> 0 0 1 2 2
@($(L) 0 -1 -2 -3)  -> 9 8 7 6
