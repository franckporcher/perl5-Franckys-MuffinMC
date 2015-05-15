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

should('test 1',    '"(!(!:quote                           ) a b c)',   ['abc']     );
should('test 2',    '"(!(        :join-with " "            ) a b c)',   ['"a b c"'] );
should('test 3',    '"(!(!:quote :join-with " "            ) a b c)',   ['a b c']   );
should('test 4',    '"(!(!:quote :join-with :              ) a b c)',   ['a:b:c']   );
should('test 5',    '"(!(!:quote :quote-with | :join-with :) a b c)',   ['a:b:c']   );
should('test 6',    '"(!(        :quote-with | :join-with :) a b c)',   ['|a:b:c|'] );
should('test 7',    '"(!(        :quote-left < :quote-right > :join-with :) a b c)',   ['<a:b:c>'] );
should('test 8',
    '=(* x.1.PID "" x.1.N un x.10.PID 1 x.10.N dix x.100.PID 10 x.100.N cent d \'(?($(_1) "#(self $("x.$(_1).PID")) "( !(!:quote :join-with "; ") $("x.$(_1).N"))" "NA")))',
    [qw( x.1.PID x.1.N x.10.PID x.10.N x.100.PID x.100.N d)]
);
should('test 9',    '#(d 100)',  ['NA un dix cent']);
should('test 10',
    '=(* 1.PID "" 10.PID 1 100.PID 10 d \'( ?($(_1) #(self $("$(_1).PID")) NOPID)))',
    [qw( 1.PID 10.PID 100.PID d)]
);
should('test 11', '#(d 100)', [ 'NOPID' ]);
should('test 12', '=(1.PID)', []);
should('test 13', '#(d 100)', [ 'NOPID' ]);
#trace_on();


# The end
done_testing();
