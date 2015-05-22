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

should('test 1',    '"(                                      a b c)',   ['a b c']   );
should('test 2',    '"(!(:quote                            ) a b c)',   ['"a b c"'] );
should('test 3',    '"(!(        :nospace                  ) a b c)',   ['abc']     );
should('test 4',    '"(!(:quote  :nospace                  ) a b c)',   ['"abc"']   );
should('test 5',    '"(!(                      :join-with :) a b c)',   ['a:b:c']   );
should('test 6',    '"(!(:quote  :quote-with | :join-with :) a b c)',   ['|a:b:c|'] );
should('test 7',    '"(!(:quote  :quote-left < :quote-right > :join-with :) a b c)',   ['<a:b:c>'] );
should('test 8',
    '=(* x.1.PID "" x.1.N un x.10.PID 1 x.10.N dix x.100.PID 10 x.100.N cent d \'(?($(_1) "#(self $("x.$(_1).PID")) "( !(:join-with "; ") $("x.$(_1).N"))" "NA")))',
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
should('test 14', 
        'P(
             =(* L     "A(MODEL= Coconut Asphalte Peace-and-Love) A(SIZE= S M L XL) A(COLOR= red blue yellow green)"
                 LA    L( MODEL "Modèle: " SIZE "Taille: " COLOR "Couleur: ")
                 mcont \'( "#(assoc $(_1) $(LA)) $(_2)" ) 
                 ccont \'( "( !( :join-with ",, ") $(@) ) )
             )

             ~( !( :c-continuation ccont :m-continuation mcont) L (MODEL,)=* (SIZE,)=* (COLOR,)=* )
         )',
        [
        'Modèle:  Coconut, Taille:  S, Couleur:  red',
        'Modèle:  Coconut, Taille:  S, Couleur:  blue',
        'Modèle:  Coconut, Taille:  S, Couleur:  yellow',
        'Modèle:  Coconut, Taille:  S, Couleur:  green',
        'Modèle:  Coconut, Taille:  M, Couleur:  red',
        'Modèle:  Coconut, Taille:  M, Couleur:  blue',
        'Modèle:  Coconut, Taille:  M, Couleur:  yellow',
        'Modèle:  Coconut, Taille:  M, Couleur:  green',
        'Modèle:  Coconut, Taille:  L, Couleur:  red',
        'Modèle:  Coconut, Taille:  L, Couleur:  blue',
        'Modèle:  Coconut, Taille:  L, Couleur:  yellow',
        'Modèle:  Coconut, Taille:  L, Couleur:  green',
        'Modèle:  Coconut, Taille:  XL, Couleur:  red',
        'Modèle:  Coconut, Taille:  XL, Couleur:  blue',
        'Modèle:  Coconut, Taille:  XL, Couleur:  yellow',
        'Modèle:  Coconut, Taille:  XL, Couleur:  green',
        'Modèle:  Asphalte, Taille:  S, Couleur:  red',
        'Modèle:  Asphalte, Taille:  S, Couleur:  blue',
        'Modèle:  Asphalte, Taille:  S, Couleur:  yellow',
        'Modèle:  Asphalte, Taille:  S, Couleur:  green',
        'Modèle:  Asphalte, Taille:  M, Couleur:  red',
        'Modèle:  Asphalte, Taille:  M, Couleur:  blue',
        'Modèle:  Asphalte, Taille:  M, Couleur:  yellow',
        'Modèle:  Asphalte, Taille:  M, Couleur:  green',
        'Modèle:  Asphalte, Taille:  L, Couleur:  red',
        'Modèle:  Asphalte, Taille:  L, Couleur:  blue',
        'Modèle:  Asphalte, Taille:  L, Couleur:  yellow',
        'Modèle:  Asphalte, Taille:  L, Couleur:  green',
        'Modèle:  Asphalte, Taille:  XL, Couleur:  red',
        'Modèle:  Asphalte, Taille:  XL, Couleur:  blue',
        'Modèle:  Asphalte, Taille:  XL, Couleur:  yellow',
        'Modèle:  Asphalte, Taille:  XL, Couleur:  green',
        'Modèle:  Peace-and-Love, Taille:  S, Couleur:  red',
        'Modèle:  Peace-and-Love, Taille:  S, Couleur:  blue',
        'Modèle:  Peace-and-Love, Taille:  S, Couleur:  yellow',
        'Modèle:  Peace-and-Love, Taille:  S, Couleur:  green',
        'Modèle:  Peace-and-Love, Taille:  M, Couleur:  red',
        'Modèle:  Peace-and-Love, Taille:  M, Couleur:  blue',
        'Modèle:  Peace-and-Love, Taille:  M, Couleur:  yellow',
        'Modèle:  Peace-and-Love, Taille:  M, Couleur:  green',
        'Modèle:  Peace-and-Love, Taille:  L, Couleur:  red',
        'Modèle:  Peace-and-Love, Taille:  L, Couleur:  blue',
        'Modèle:  Peace-and-Love, Taille:  L, Couleur:  yellow',
        'Modèle:  Peace-and-Love, Taille:  L, Couleur:  green',
        'Modèle:  Peace-and-Love, Taille:  XL, Couleur:  red',
        'Modèle:  Peace-and-Love, Taille:  XL, Couleur:  blue',
        'Modèle:  Peace-and-Love, Taille:  XL, Couleur:  yellow',
        'Modèle:  Peace-and-Love, Taille:  XL, Couleur:  green'
        ]
);

#trace_on();


# The end
done_testing();
