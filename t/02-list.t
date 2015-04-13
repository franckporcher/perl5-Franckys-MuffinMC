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

# Def vars
should('test 1', '=(l 0 1 2 3 4 5 6 7 8 9)',                            [0..9]                                          );
should('test 2', '=(index_impairs 2 4 6 8 10)',                         [2,4,6,8,10]                                    );
should('test 3', '=(index_pairs 1 3 5 7 9)',                            [1,3,5,7,9]                                     );
should('test 4', '=(reverse 10 9 8 7 6 5 4 3 2 1)',                     [ reverse 1..10 ]                               );
should('test 5', '=(vars l index_impairs index_pairs reverse vars)',    [qw(l index_impairs index_pairs reverse vars )] );

should('test 6', '@(l)',                                            [0..9]                                          );
should('test 7', '@(index_impairs)',                                [2,4,6,8,10]                                    );
should('test 8', '@(index_pairs)',                                  [1,3,5,7,9]                                     );
should('test 9', '@(reverse)',                                      [ reverse 1..10 ]                               );
should('test 10', '@(vars)',                                        [qw(l index_impairs index_pairs reverse vars )] );

should('test 11', '@($(l))',                                        [0..9]                                          );
should('test 12', '@($(index_impairs))',                            [2,4,6,8,10]                                    );
should('test 13', '@($(index_pairs))',                              [1,3,5,7,9]                                     );
should('test 14', '@($(reverse))',                                  [ reverse 1..10 ]                               );
should('test 15', '@($(vars))',                                     [qw(l index_impairs index_pairs reverse vars )] );

should('test 16', '@(l $(index_pairs))',                            [0,2,4,6,8]                                     );
should('test 17', '@(l $(index_impairs))',                          [1,3,5,7,9]                                     );
should('test 18', '@(l $(reverse))',                                [ reverse 0..9 ]                                );
should('test 19', '@(l $(index_impairs) $(index_pairs) $(reverse))',[1,3,5,7,9,0,2,4,6,8, reverse 0..9]             );
should('test 20', '@(l $(index_impairs index_pairs reverse))',      [1,3,5,7,9,0,2,4,6,8, reverse 0..9]             );

should('test 21', '#(l)',                                           [10]                                            );
should('test 22', '#(l l l)',                                       ['l']                                           );
should('test 23', '=(ll l l l)',                                    [qw(l l l)]                                     );
should('test 24', '=(ll #(l))',                                     [10]                                            );
should('test 25', '$(ll)',                                          [10]                                            );
should('test 26', '#(# #(.. -10      -1 )     )',                   [10]                                            );
should('test 27', '#(# #(.. "-$(ll)" -1 )     )',                   [10]                                            );
should('test 28', '#(# #(.. "-#(l)"  -1 )     )',                   [10]                                            );
should('test 29', '#(# #(.. -1       -10)     )',                   [10]                                            );
should('test 30', '#(# #(.. -1      "-$(ll)") )',                   [10]                                            );
should('test 31', '#(# #(.. -1      "-#(l)")  )',                   [10]                                            );

should('test 32',  '#(.. -10      -1      )',                       [-10 .. -1]                                     );
should('test 33',  '#(.. "-$(ll)" -1      )',                       [-10 .. -1]                                     );
should('test 34',  '#(.. -1       -10     )',                       [reverse -10 .. -1]                             );
should('test 35',  '#(.. -1      "-$(ll)" )',                       [reverse -10 .. -1]                             );

should('test 36', '@(l -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)',            [reverse 0..9]                                  );
should('test 37', '@(l #(.. -1   -10     )           )',            [reverse 0..9]                                  );
should('test 38', '@(l #(.. -1  "-#(l)"  )           )',            [reverse 0..9]                                  );
should('test 39', '@(l #(.. -1  "-$(ll)" )           )',            [reverse 0..9]                                  );
should('test 40', '@(l -10 -9 -8 -7 -6 -5 -4 -3 -2 -1)',            [0..9]                                          );
should('test 41', '@(l #(..  -10     -1  )           )',            [0..9]                                          );
should('test 42', '@(l #(.. "-#(l)"  -1  )           )',            [0..9]                                          );
should('test 43', '@(l #(.. "-$(ll)" -1  )           )',            [0..9]                                          );

should('list 44', '@(a b c d e f)',                                 [qw( a b c d e f)]                              );
should('test 45', '@(l $(l))',                                      [0..8]                                          );
should('test 46', '@("foo bar" 1 1)',                               ['foo bar', 'foo bar']                          );


should('test 47', '=(ll l l l)',                                    [qw(l l l)]                                     );
should('test 48', '#($(ll))',                                       [10, 10, 10]                                    );
# The end
done_testing();

__END__


@($(L) 1 1 2 3 3)   -> 0 0 1 2 2
@($(L) 0 -1 -2 -3)  -> 9 8 7 6
