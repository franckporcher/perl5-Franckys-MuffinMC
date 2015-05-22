#!perl -T
use 5.16.0;
use strict;
use warnings;
use Test::Most;

use Franckys::MuffinMC;
use Franckys::Error;
use Franckys::Trace;
use Data::Dumper;
$Data::Dumper::Indent    = 0;
$Data::Dumper::Quotekeys = 0;
$Data::Dumper::Sortkeys  = 1;
$Data::Dumper::Pad       = '';


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
sub m_explain {
    my ($test, $expr, $ref) = @_;

    my $obj = muffin_explain($expr, $ERROR);
    my $str = Dumper( $obj );


    is_deeply( $str,
               $str,
               "$test",
    );
    diag explain $obj;

    no_error();
}

m_explain('test 1',    '=(foo foo bar)',                               ['foo', 'bar']);
m_explain('test 2',    '$(foo)',                                       ['foo', 'bar']  );
m_explain('test 3',    '"$(foo)"',                                     ['foo', 'bar']);
m_explain('test 4',    '"1 L(a "$(foo)" b) 2"',                        [ '1 a 2', '1 foo 2', '1 bar 2', '1 b 2']);
m_explain('test 5',    '"1 L(a  $(foo)  b) 2"',                        [ '1 a 2', '1 foo 2', '1 bar 2', '1 b 2']);
m_explain('test 6',    '"1 #(. L(a " $(foo) " b)) 2"',                 [ '1 a foo  bar b 2' ]);
m_explain('test 7',    '"1 #(. a " $(foo) " b) 2"',                    [ '1 a foo  bar b 2' ]);
m_explain('test 8',    '"1 $(foo) 2"',                                 [ '1 foo 2', '1 bar 2']);
m_explain('test 9',    '"1 #(.$(foo)) 2"',                             [ '1 foobar 2']);
m_explain('test 10',   '"1 #(." $(foo) ") 2"',                         [ '1  foo  bar  2']);
m_explain('test 11',   '"1 \L(a "$(foo)" b) 2"',                       ['1 L(a "$(foo)" b) 2']);
m_explain('test 12',   '"1 \L(a  $(foo)  b) 2"',                       ['1 L(a  $(foo)  b) 2']);

m_explain('test 13',   '#(+ 0 1 2 3 4 5 6 7 8 9)',                     [45]);
m_explain('test 14',   q{ #(map '(a) 0 1 2 3 4 5 6 7 8 9)},            [qw(a a a a a a a a a a)]);
m_explain('test 15',   '#(map "a b" 1 2 3 4 5)',                       ['a b', 'a b', 'a b', 'a b', 'a b']);
m_explain('test 16',   q{#(map '("a b") 1 2 3 4 5)},                   ['a b', 'a b', 'a b', 'a b', 'a b']);

m_explain('test 17',   '=(foo a b)',                                   ['a', 'b']);
m_explain('test 18',   '#(map $(foo) 1 2 3 4 5)',                      ['a','a','a','a','a','b','b','b','b','b']);
m_explain('test 19',   q{#(map '($(foo)) 1 2 3 4 5)},                  ['a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b']);
muffin_eval(        q{=(foo '("a b"))});
m_explain('test 20',   '#(map $(foo) 1 2 3 4 5)',                      ['a b', 'a b', 'a b', 'a b', 'a b']);

muffin_eval(        q{=(square '(#(* $(_1) $(_1))))});
m_explain('test 21',   '#(map $(square) #(.. 1 10))',                  [qw(1 4 9 16 25 36 49 64 81 100)]);
m_explain('test 22',   q{#(map '(#(* $(_1) $(_1)))  #(.. 1 10))},      [qw(1 4 9 16 25 36 49 64 81 100)]);

m_explain('test 23',   '=(consonnes  c g m)',                          [qw(c g m)] );
m_explain('test 24',   '=(voyelles   a e i o u y)',                    [qw(a e i o u y)]);

muffin_eval(        q{ =(cv '( #(. @(consonnes $(_1)) \$(voyelles) ) )) });
m_explain('test 25',   '#(map $(cv) #(.. 1 #(consonnes)))',                           ['c$(voyelles)', 'g$(voyelles)', 'm$(voyelles)']);
m_explain('test 26',   '#(. #(map $(cv) #(.. 1 #(consonnes))))',                      ['c$(voyelles)g$(voyelles)m$(voyelles)']);
m_explain('test 27',   '=(formule "( !(:quote :nospace) #(. #(map $(cv) #(.. 1 #(consonnes))))))',       ['"c$(voyelles)g$(voyelles)m$(voyelles)"']);

my $acronymes = ['cagama','cagame','cagami','cagamo','cagamu','cagamy','cagema','cageme','cagemi','cagemo','cagemu','cagemy','cagima','cagime','cagimi','cagimo','cagimu','cagimy','cagoma','cagome','cagomi','cagomo','cagomu','cagomy','caguma','cagume','cagumi','cagumo','cagumu','cagumy','cagyma','cagyme','cagymi','cagymo','cagymu','cagymy','cegama','cegame','cegami','cegamo','cegamu','cegamy','cegema','cegeme','cegemi','cegemo','cegemu','cegemy','cegima','cegime','cegimi','cegimo','cegimu','cegimy','cegoma','cegome','cegomi','cegomo','cegomu','cegomy','ceguma','cegume','cegumi','cegumo','cegumu','cegumy','cegyma','cegyme','cegymi','cegymo','cegymu','cegymy','cigama','cigame','cigami','cigamo','cigamu','cigamy','cigema','cigeme','cigemi','cigemo','cigemu','cigemy','cigima','cigime','cigimi','cigimo','cigimu','cigimy','cigoma','cigome','cigomi','cigomo','cigomu','cigomy','ciguma','cigume','cigumi','cigumo','cigumu','cigumy','cigyma','cigyme','cigymi','cigymo','cigymu','cigymy','cogama','cogame','cogami','cogamo','cogamu','cogamy','cogema','cogeme','cogemi','cogemo','cogemu','cogemy','cogima','cogime','cogimi','cogimo','cogimu','cogimy','cogoma','cogome','cogomi','cogomo','cogomu','cogomy','coguma','cogume','cogumi','cogumo','cogumu','cogumy','cogyma','cogyme','cogymi','cogymo','cogymu','cogymy','cugama','cugame','cugami','cugamo','cugamu','cugamy','cugema','cugeme','cugemi','cugemo','cugemu','cugemy','cugima','cugime','cugimi','cugimo','cugimu','cugimy','cugoma','cugome','cugomi','cugomo','cugomu','cugomy','cuguma','cugume','cugumi','cugumo','cugumu','cugumy','cugyma','cugyme','cugymi','cugymo','cugymu','cugymy','cygama','cygame','cygami','cygamo','cygamu','cygamy','cygema','cygeme','cygemi','cygemo','cygemu','cygemy','cygima','cygime','cygimi','cygimo','cygimu','cygimy','cygoma','cygome','cygomi','cygomo','cygomu','cygomy','cyguma','cygume','cygumi','cygumo','cygumu','cygumy','cygyma','cygyme','cygymi','cygymo','cygymu','cygymy'];

m_explain('test 28a',  '#(eval $(formule))',                                         $acronymes);
m_explain('test 28b',  'M($(formule))',                                              $acronymes);

# THE GOOD VERSION HERE
m_explain('test 29a',  q{  #(eval "( !(:quote :nospace) #(map   '(#(. @(consonnes $(_1)) \$(voyelles)))   #(.. 1 #(consonnes)))) ) }, $acronymes);
m_explain('test 29b',  q{  M( "( !(:quote :nospace) #(map   '(#(. @(consonnes $(_1)) \$(voyelles)))   #(.. 1 #(consonnes)))) ) }, $acronymes);


m_explain('test 30', q{ #(a b c d)},                    ['a']                   );
m_explain('test 31', '=("f o o" f o o)',                ['f', 'o', 'o']         );
m_explain('test 32', q{ #("f o o")},                    [3]                     );
m_explain('test 33', q{ #("f o o" c d)},                ['f o o']               );
m_explain('test 34', q{ #(+ 1 2 3 4 5)},                [15]                    );
m_explain('test 35', q{ =("+ - * /" + - * /)},          ['+', '-', '*', '/']    );
m_explain('test 36', q{ #("+ - * /" 1 2 3 4 5)},        ["+ - * /"]             );
m_explain('test 37', q{ #("+ - * /")},                  [4]                     );
m_explain('test 38', q{ #($("+ - * /") 1 2 4 8)},       [15, -13, 64, 0.015625] );
m_explain('test 39', q{ #($("+ - * /") 1 2 4 8)},       [15, -13, 64, 0.015625] );

m_explain('gcd',
q{ 
    =(gcd '(    #(cmp? $(_1) 
                       $(_2) 
                       #(gcd $(_1) #(- $(_2) $(_1))) 
                       $(_1) 
                       #(gcd #(- $(_1) $(_2)) $(_2)))
           )
    )
}, []);

m_explain('call gcd',
q{ 
    =(gcd '(    ?(
                    #(== $(_2) 0)
                    $(_1) 
                    #(self $(_2) #(mod $(_1) $(_2))))
           )
    )
},
[]);

muffin_eval(q{ =(gcd225 '( #(gcd 225 $(_1)) ) )});
m_explain('test 40', '#(gcd 225 3)',                    [3]);
m_explain('test 41', '#(gcd 225 5)',                    [5]);
m_explain('test 42', '#(gcd 225 9)',                    [9]);
m_explain('test 43', '#(gcd 225 15)',                   [15]);
m_explain('test 44', '#(gcd 225 25)',                   [25]);
m_explain('test 45', '#(gcd 225 27)',                   [9]);
m_explain('test 46', '#(gcd 225 45)',                   [45]);
m_explain('test 47', '#(gcd 225 75)',                   [75]);
m_explain('test 48', '#(gcd 225 125)',                  [25]);
m_explain('test 49', '#(gcd 225 81)',                   [9]);
m_explain('test 50', '#(gcd 225 135)',                  [45]);
m_explain('test 51', '#(gcd 225 225)',                  [225]);
m_explain('test 52', '#(gcd 225 625)',                  [25]);
m_explain('test 53', q{ #(map gcd225 3 5 9 15 25 27 45 75 125 81 135 225 625) }, [3, 5, 9, 15, 25, 9, 45, 75, 25, 9, 45, 225, 25]);

m_explain('fact', q{ =(fact '(?(#(le $(_1) 1) 1 #(* $(_1) #(fact #(1- $(_1))))))) }, []);
m_explain('test 54', '#(fact 0)',                       [1]);
m_explain('test 55', '#(fact 1)',                       [1]);
m_explain('test 56', '#(fact 2)',                       [2]);
m_explain('test 57', '#(fact 3)',                       [6]);
m_explain('test 58', '#(fact 4)',                       [24]);
m_explain('test 59', '#(fact 5)',                       [120]);
m_explain('test 60', '#(fact 6)',                       [720]);
m_explain('fact',q{ 
    =(fact '( ?( #(le $(_1) 1) 
                 1 
                 #(* $(_1) 
                     #(self #(1- $(_1)))))
            )
      )
},[]);
m_explain('test 61', q{ #(map fact #(.. 1 10))},        [ 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800 ]);


m_explain('test 62',  q{ =(* x 0 y 1 fibit   I( #(prog1 $(y) =(* x $(y) y #(+$(x)$(y)))))) }, ['x', 'y', 'fibit']);
m_explain('test 63', '#(fibit 1)',                       [1]);
m_explain('test 64', '#(fibit 1)',                       [1]);
m_explain('test 65', '#(fibit 1)',                       [2]);
m_explain('test 66', '#(fibit 1)',                       [3]);
m_explain('test 67', '#(fibit 2)',                       [5,8]);
m_explain('test 68', '#(fibit 6)',                       [13,21,34,55,89,144]);

muffin_eval(q{
    =(reset-fibit '( #(progn  =(* x 0 y 1) 
                              #(fibit $(_1)))
                   )
     )
});
m_explain('test 69a', '#(reset-fibit 15)',                [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]);

muffin_eval(q{
    =(reset-fibit-2 '( P( =(* x 0 y 1) 
                          #(fibit $(_1)))
                   )
     )
});
m_explain('test 69b', '#(reset-fibit-2 15)',              [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]);

m_explain('test 69c', q{ =(* x 0 y 1 fibit2  I( 1($(y) =(* x $(y) y #(+$(x)$(y)))))) }, ['x', 'y', 'fibit2']);
muffin_eval(q{
    =(reset-fibit-3 '( P( =(* x 0 y 1) 
                          #(fibit2 $(_1)))
                   )
     )
});
m_explain('test 69d', '#(reset-fibit-3 15)',              [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]);

# Egalité de deux expressions
muffin_eval(q{
    =(fib-fun
        '(
            #(==
                #(progn =(* x 0 y 1)
                        #(+ #(map fibit #(.. 1 $(_1)))))
                #(progn =(* x 0 y 1)
                        #(+ #(fibit #(+ #(.. 1 $(_1))))))
             )
         )
    )
});
m_explain('test 70a', '#(map fib-fun #(.. 1 20))',         [map{1}1..20]);
m_explain('test 71', '#(map 1 #(.. 1 20))',               [map{1}1..20]);

muffin_eval(q{
    =(fib-fun-2
        '(
            #(==
                P( =(* x 0 y 1)
                   #(+ #(map fibit #(.. 1 $(_1)))))
                P( =(* x 0 y 1)
                   #(+ #(fibit #(+ #(.. 1 $(_1))))))
             )
         )
    )
});
m_explain('test 70b', '#(map fib-fun-2 #(.. 1 20))',         [map{1}1..20]);

muffin_eval(q{
    =(it I( *(3 "_") N))
});
m_explain('test 72', '#(it 3)', ['_','_','_']);
m_explain('test 73', '#(it 3)', ['N','_','_']);
m_explain('test 74', '#(it 3)', ['_','N','_']);
m_explain('test 75', '#(it 3)', ['_','_','N']);
m_explain('test 76', '#(it 3)', ['_','_','_']);

muffin_eval(q{
    =(* x 3 it I( *($(x) "_") N))
});
m_explain('test 77', '#(it 3)', ['_','_','_']);
m_explain('test 78', '#(it 3)', ['N','_','_']);
m_explain('test 79', '#(it 3)', ['_','N','_']);
m_explain('test 80', '#(it 3)', ['_','_','N']);
m_explain('test 81', '#(it 3)', ['_','_','_']);

muffin_eval(q{
    =(x 5)
    =(* y #(2- $(x))
        I I(*($(y)"_")N))
    ))
});
m_explain(
    'test 82', 
    q{ "N#(map '( "#(. #(I $(y)))" ) #(.. 1 $(x)))N" },
    [
        'N___N',
        'NN__N',
        'N_N_N',
        'N__NN',
        'N___N'
]);

m_explain('ascii_art',
q{
    =(ascii-art-1 '(
            =(* x   #(2- $(_1))
                I   I(*($(x)" ")N)
                foo '( #(. #(I $(x)))))
            #(say ?(#(== $(_1) 1) N "N#(map foo #(.. 1 $(_1)))N"))
    ))
},[]);

m_explain('test 83', '#(ascii-art-1 1)', [
    'x',
    'I',
    'foo',
    'N',
]);
m_explain('test 84', '#(ascii-art-1 3)', [
    'x',
    'I',
    'foo',
    'N N',
    'NNN',
    'N N',
]);
m_explain('test 85', '#(ascii-art-1 5)', [
    'x',
    'I',
    'foo',
    'N   N',
    'NN  N',
    'N N N',
    'N  NN',
    'N   N',
]);
m_explain('test 86', '#(ascii-art-1 7)', [
    'x',
    'I',
    'foo',
    'N     N',
    'NN    N',
    'N N   N',
    'N  N  N',
    'N   N N',
    'N    NN',
    'N     N',
]);
m_explain('test 87', '#(ascii-art-1 9)', [
    'x',
    'I',
    'foo',
    'N       N',
    'NN      N',
    'N N     N',
    'N  N    N',
    'N   N   N',
    'N    N  N',
    'N     N N',
    'N      NN',
    'N       N',
]);

m_explain('test 88', q{#(map '( #(ascii-art-1 $(_1))) 1 3 5 7 9)}, [
    'x',
    'I',
    'foo',
    'N',
    'x',
    'I',
    'foo',
    'N N',
    'NNN',
    'N N',
    'x',
    'I',
    'foo',
    'N   N',
    'NN  N',
    'N N N',
    'N  NN',
    'N   N',
    'x',
    'I',
    'foo',
    'N     N',
    'NN    N',
    'N N   N',
    'N  N  N',
    'N   N N',
    'N    NN',
    'N     N',
    'x',
    'I',
    'foo',
    'N       N',
    'NN      N',
    'N N     N',
    'N  N    N',
    'N   N   N',
    'N    N  N',
    'N     N N',
    'N      NN',
    'N       N',
]);

m_explain('test 89', '=(* muffin Muffin remember-muffin 2015) #(say "$(muffin) was born in $(remember-muffin) out of necessity ;)" )',
        [
            'muffin',
            'remember-muffin',
            "Muffin was born in 2015 out of necessity ;)"
]);

m_explain('test 90', '#(say "MuffinMC was born in 2015 out of necessity !" )',
        [
            'MuffinMC was born in 2015 out of necessity !'
]);

m_explain('test 99', '@(L() 1 2 3)', [] );


#muffin_eval(q{
#    =(ascii-art
#        '( #(say "N    #(map #(I(*(#(1- $(_1))" ")N) $(_1)) #(.. 1 $(_1)))     N" ) )
#     )
#});
#m_explain('test 72', '#(ascii-art 3)', []);


#trace_on();

# The end
done_testing();

__END__


@($(L) 1 1 2 3 3)   -> 0 0 1 2 2
@($(L) 0 -1 -2 -3)  -> 9 8 7 6
