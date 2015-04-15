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
my $ERROR = Franckys::Error::Error();

##
# Globals
#
my ($var, $final, $final_2, $p, @p, $ref);

##
# MuffinMC Variables
#
my %muffin_vars = (
    digits_1        => '0 1 2 3 4',
    digits_2        => '5 6 7 8 9',
    hex_letters_1   => 'a b c',
    hex_letters_2   => 'd e f',
    hex_digits_A    => '$(digits_1) $(digits_2) $(hex_letters_1) $(hex_letters_2)',
    hex_digits_B    => '$(digits_1 digits_2 hex_letters_1 hex_letters_2)',
);

my @varlist=( qw(
    digits_1
    digits_2
    hex_letters_1
    hex_letters_2
    hex_digits_A
    hex_digits_B
));


##
# Referential
#
my %ref_vars = (
    digits_1        => [0 .. 4],
    digits_2        => [5 .. 9],
    hex_letters_1   => [qw(a b c)],
    hex_letters_2   => [qw(d e f)],
);
$ref_vars{hex_digits_A}
    = [ map { @{$ref_vars{$_}} } qw(digits_1 digits_2 hex_letters_1 hex_letters_2) ];
$ref_vars{hex_digits_B} = $ref_vars{hex_digits_A};


##
# Define MuffinMC variables thru API
#
foreach $var (@varlist) {
    $final = muffin_setvar($var, muffin_eval($muffin_vars{$var}, $ERROR));

    is_deeply( $final,
               $ref_vars{$var},
               "Define var: $var"
    ) or diag explain $final;
}


##
# Access MuffinMC variables thru API
#
foreach $var (@varlist) {
    $final = muffin_getval($var, $ERROR);

    is_deeply( $final,
               $ref_vars{$var},
               "Access var: $var"
    ) or diag explain $final;
}


##
# Combinations
#
# On vérifie que :  '$(var1 $var2 ...)' = '$(var1) $(var2) ...' = [ @$var1 @$var2... ]
#
# pour toute combinaison des variables.
#

# perm(\@RES, n, @E, @P(E))
sub perm {
	my ($res, $n) = splice @_, 0, 2;

	if ( $n > 1 ) {
		my @E   = splice @_, 0, $n;
        my @P_E = @_;
		perm(   $res,	
                $n - 1,
				@E[0 .. ($_-1), ($_+1) .. $#E],
				$E[$_],
                @P_E,
			) foreach (0 .. $#E);
	} else {
	    push $res, \@_;
	}
    return $res;
}

foreach my $p ( @{ perm([], scalar(@varlist), @varlist) } ) {    # $p = ['varname'...]
    my @p     = @$p;
    $final    = muffin_eval(sprintf('$(%s)', "@p"), $ERROR);
    $final_2  = muffin_eval("@{[  map{ sprintf('$(%s)', $_) } @p  ]}");
    my $ref = [];
    push $ref, @{ $ref_vars{$_} } foreach @p;

    is_deeply( $final,
               $ref,
               "Combine vars: \$(@p)"
    ) or diag explain $final;

    is_deeply( $final_2,
               $ref,
               "Combine vars: \$(@p)"
    ) or diag explain $final;
}

# Test the Error
is( $ERROR->nb_errors(), 0, "Errors should be 0" )
    or do{ diag($_) foreach $ERROR->msgs() };


# The end
done_testing();
