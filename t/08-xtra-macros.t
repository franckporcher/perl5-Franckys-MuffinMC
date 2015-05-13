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
    $ERROR->nb_errors() == 0
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

#trace_on();
should('test 1',    '"(!(!:quote                           ) a b c)',   ['abc']     );
should('test 2',    '"(!(        :join-with " "            ) a b c)',   ['"a b c"'] );
should('test 3',    '"(!(!:quote :join-with " "            ) a b c)',   ['a b c']   );
should('test 3',    '"(!(!:quote :join-with :              ) a b c)',   ['a:b:c']   );
should('test 3',    '"(!(!:quote :quote-with | :join-with :) a b c)',   ['a:b:c']   );
should('test 3',    '"(!(        :quote-with | :join-with :) a b c)',   ['|a:b:c|'] );
should('test 3',    '"(!(        :quote-left < :quote-right > :join-with :) a b c)',   ['<a:b:c>'] );


# The end
done_testing();
