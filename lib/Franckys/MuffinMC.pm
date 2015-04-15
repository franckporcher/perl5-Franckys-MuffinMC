#===============================================================================
#
#         FILE: Franckys/MuffinMC.pm
#
#        USAGE: Use Franckys::MuffinMC;
#
#  DESCRIPTION: MUTINY Flexible Functional INline Macro Command language
#
#      OPTIONS: 
# REQUIREMENTS: See doc below
#         BUGS: None so far
#        NOTES: Developed for MUTINY Tahiti Project
#
#       AUTHOR: Franck Porcher, Ph.D. - franck.porcher@franckys.com
# ORGANIZATION: Franckys
#      VERSION: v0.11
#      CREATED: Dim  1 fev 2015 23:11:48 PST
#
# Copyright (C) 1995-2015 - Franck Porcher, Ph.D 
# www.franckys.com
# Tous droits réservés - All rights reserved
#===============================================================================
package Franckys::MuffinMC;
use 5.16.0;                                    ## no critic (ValuesAndExpressions::ProhibitVersionStrings)
use strict;
use warnings;
no warnings     qw(recursion);
use autodie;
use feature             qw( switch say unicode_strings );

use Const::Fast;
use Scalar::Util qw( blessed looks_like_number );
use Franckys::Trace;
use Franckys::Error;


#----------------------------------------------------------------------------
# UTF8 STUFF
#----------------------------------------------------------------------------
use utf8;
use warnings            FATAL => 'utf8';
use charnames           qw( :full :short );
use Encode              qw( encode decode );
use Unicode::Collate;
use Unicode::Normalize  qw( NFD NFC );


#----------------------------------------------------------------------------
# I/O
#----------------------------------------------------------------------------
use open qw( :encoding(UTF-8) :std );


#----------------------------------------------------------------------------
# EXPORTED STUFF
#----------------------------------------------------------------------------
use Perl6::Export::Attrs;


#----------------------------------------------------------------------------
# POD
#----------------------------------------------------------------------------
=pod

=head1 NAME

Franckys::MuffinMC - MUtiny Flexible Functionnal Macro Command Language


=head1 VERSION

Version 0.11

=cut

use version; our $VERSION = qv('0.1.1');           # Keep on same line


=pod

=head1 SYNOPSIS


=head1 DESCRIPTION

This modules provides a versatile, flexible, extensible functional simple macro command
language API.

It was mainly developed for the MUTINY Tahiti Project, where a PrestaShop-based e-marchand site
is using a front-end spreadsheet to pilot the store's catalog inventory operations.

Franckys::MuffinMC is initially designed to be used as an underlying engine to empower
such spreadsheet with user-definable meta operations over the store inventory.

=head1 EXPORT

The following functions are exported by default :

=over 4

=item . B<muffin_eval()>

=item . B<muffin_setvar()>

=item . B<muffin_getval()>

=item . B<muffin_exists_var()>

=back


=head1 ERRORS

This module throws the following errors and expect
them to be nicely trapped/defined through custom Franckys::Error 
comprehensive messages.

=over 4

=item . B<EVAR> -- Undefined variable : foo

 Franckys::Error::def_error(EVAR  => 'Undefined variable: %s.');

=item . B<EFUNC> -- Undefined function : bar

 Franckys::Error::def_error(EVAR  => 'Undefined function: %s.');

=item . B<EVAR> -- Arithmetic error : msg

 Franckys::Error::def_error(EMATHS  => 'Arithmetic error: %s.');

=back

=cut


#----------------------------------------------------------------------------
# CONSTANTS & GLOBALS
#----------------------------------------------------------------------------
const my $STRING_MARKER     => '"';
const my $ENDTOKEN_MARKER   => ')',
const my $SEP               => "\0";

my %TOKEN_MARKER = ( # For the parser only
    $STRING_MARKER      => 'S',
    $ENDTOKEN_MARKER    => 'E',
    #  "M("             => 'M' 
);

my %CLEANSTRING = ( # For the parser only - to clean strings after parsing
    $TOKEN_MARKER{$STRING_MARKER}   =>  $STRING_MARKER,     # S -> "
    $TOKEN_MARKER{$ENDTOKEN_MARKER} =>  $ENDTOKEN_MARKER,   # E -> )
    #   Internal_marker             =>  external_marker(
    #   M                           =>  M(
);

my %DEFMACRO = (
    $TOKEN_MARKER{$STRING_MARKER} => \&eval_string,       # S
    # internal_marker             => sub
    #   M                         => \&macro_M
);

# external -> internal
my %MACROS = ();

my $cleanstring_keys = join '', keys %CLEANSTRING;
my $defmacro_keys    = join '', keys %DEFMACRO;
my $macro_keys       = join '', keys %MACROS;

my %REGEX = (
    leading_spaces          => qr/^[[:space:]]+/s,
    trailing_spaces         => qr/[[:space:]]+$/s,
    spaces                  => qr/[[:space:]]+/s,
    eval_token              => qr/^($SEP(\d+)([$defmacro_keys])(.*?)$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})$/s,
    split_token             => qr/($SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})/s,
    cleanstring             => qr/$SEP\d+([$cleanstring_keys])/s,
    eval_string_token       => qr/(?<value>$SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})|(?<value>[^$SEP]+)/s,
    token_marker            => qr/([\\]*)([$macro_keys]\(|[${STRING_MARKER}${ENDTOKEN_MARKER}])/,
);

# def_macro($macro, $macro_handler)
sub def_macro {
    my ($macro, $macro_handler, $imacro) = @_;
    $imacro = $macro unless defined $imacro;

    die "[def_macro] 1-CHAR-MACRO SUB - Invalid arguments:[@_]"
        unless defined($macro) 
               && defined($macro_handler);

    die "[def_macro] invalid macro:[$macro]. Must be one character long only"
        unless length($macro) == 1;

    die "[def_macro] Macro reserved or already defined [$macro]."
        if exists $CLEANSTRING{$imacro};

    die "[def_macro] Invalid macro_handler:[$macro_handler]. Must be a subroutine"
        unless ref($macro_handler) eq 'CODE';

    $MACROS{        $macro   } = $imacro;
    $DEFMACRO{      $imacro  } = $macro_handler;
    $CLEANSTRING{   $imacro  } = "$macro(";             # Idem
    $TOKEN_MARKER{ "$macro(" } = $imacro;               # For the parser / tokenizer

    # Update engine regexes 
    $cleanstring_keys = join '', keys %CLEANSTRING;
    $defmacro_keys    = join '', keys %DEFMACRO;
    $macro_keys       = join '', keys %MACROS;

    $REGEX{ eval_token         } = qr/^($SEP(\d+)([$defmacro_keys])(.*?)$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})$/s;
    $REGEX{ split_token        } = qr/($SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})/s;
    $REGEX{ cleanstring        } = qr/$SEP\d+([$cleanstring_keys])/s;
    $REGEX{ eval_string_token  } = qr/(?<value>$SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})|(?<value>[^$SEP]+)/s;
    $REGEX{ token_marker       } = qr/([\\]*)([$macro_keys]\(|[${STRING_MARKER}${ENDTOKEN_MARKER}])/;
}

{
    ##
    # PSGI / Plack apps do not run INIT blocks :(((((
    #
    my $init_done = 0;

    sub _init {
        return if $init_done;

        def_macro(  '='  => \&macro_set_var            );
        def_macro(  '$'  => \&macro_eval_var           );
        def_macro(   L   => \&macro_mklist             );
        def_macro(  '@'  => \&macro_access_list        );
        def_macro(  '#'  => \&macro_eval_func          );
        def_macro(  '?'  => \&macro_eval_cond          );
        def_macro(  '*'  => \&macro_repeat             );
        def_macro(   I   => \&macro_set_iterator       );
        def_macro(   Q   => \&macro_set_lazy           );
        def_macro( q(")  => \&macro_qquote             );
        def_macro(   W   => \&macro_warning            );
        def_macro( q(')  => \&macro_quote              );
        def_macro(   M   => \&macro_meta_eval          );
        def_macro(   P   => \&macro_progn              );
        def_macro(   1   => \&macro_prog1         ,'p' );

        $init_done = 1;
    }
}

#----------------------------------------------------------------------------
# SUBROUTINES/METHODS
#----------------------------------------------------------------------------
=head1 SUBROUTINES/METHODS

=cut

###
### muffin_error()
###
=pod

=head2 muffin_error() [INTERNAL]

 my $error_msg = muffin_error(err_tag, $param, $client_obj);

When defined, the client object should handle the method 
C<Error(TAG, $varname)>  which should return an object that 
handles the method <as_string()> to get the error message as a valid value.

By default, return our own error as a string literal

=cut

sub muffin_error {
    my ($tag, $param, $o) = @_;

    my $error;

    $error = $o->Error($tag, $param)
        if $o && ref($o) && $o->can('Error');

    $error
        =  ( $error
            && ref($error)
            && $error->can( 'as_string' )
           )
           ? $error->as_string()
           : Error($tag, $param)->as_string();
           ;

    return [ $error ];
}

#---------------------------------------------------------------------------r
# MuffinMC VARIABLES HANDLING
#----------------------------------------------------------------------------
my %MUFFIN_VARS  = (
    self    => [],
    '@'     => [],
    '#'     => [ 0 ],
);

###
### muffin_setvar()
###
=pod

=head2 muffin_setvar() - Set a MuffinMC variable [PUBLIC]

 my $value = muffin_setvar($var_name, $var_value)

MuffinMC variables can be set in 3 different ways :

=item B<Final-Value>

A MuffinMC B<Final-Value> holds the final immutable values of the variable. 

A MuffinMC B<Final-Value> is always represented by a Perl array reference.

When that array reference holds only one value, we speak about a scalar-MuffinMC 
variable.

When that array reference holds more than one value, we speak about a muti-valued-MuffinMC 
variable.

MuffinMC variables can be muldi-dimensional.

=item B<String-literal>

Think of a MuffinMC variable set with a B<String-literal> as a kind of lazy-variable whose value is dynamic and 
computed anew as needed.

Exemple: be I a variable bound to an iterator that ranges the interval 'a'..'f' C< =(It I( #(.. a f) ))>

We could define a variable VI to access the subsequent values of that iterator with :

  C<muffin_setvar('VI', "E(I)">

The first time VI is evaluated will return 'a', the second time 'b', etc.

=item <Custom-subroutine>

A MuffinMC variable defined by a subroutine is called a functional-muffinMC-variablei ; it is the equivalent of a Perl tie,
a mechanism by which a function is accessed through a variable.

Exemple : 
  C<muffin_setvar('localtime', sub { return scalar(localtime) })>

Each time we access the variable, we get a different value.

=back

=cut

sub muffin_setvar :Export(:DEFAULT) {
    my ($varname, $value) = @_;
    &tracein;

    $MUFFIN_VARS{ $varname } = $value;

    traceout($value);
    return $value;
}


###
### muffin_getval()
### 
=pod

=head2 muffin_getval() [PUBLIC]

 my $final_value = muffin_getval(varname, @client_params)

 (type) LITERAL -> FVALUE+

Access the value of a MuffinMC variable.

Always return a final value.

=cut

sub muffin_getval :Export(:DEFAULT) {
    my ($varname, @client_params) = @_;

    tracein($varname);

    # Undefined variable
    return muffin_error('EVAR', $varname)
        unless exists $MUFFIN_VARS{ $varname };

    my $value = $MUFFIN_VARS{ $varname };

    # Undefined value
    return []
        unless defined $value;

    # Expand functional-variable
    $value = $value->( @client_params )
        if ref($value) eq 'CODE';

    # Expand lazy-variable
    $value =  muffin_eval( $value, @client_params )
        unless ref($value) eq 'ARRAY';

    traceout( $value );

    return $value;
}

###
### muffin_exists_var()
###
=pod

=head2 muffin_exists_var() [PUBLIC]

 my $vardef | undef = muffin_exists_var(varname)

Check the existence of a MuffinMC variable.

=cut

sub muffin_exists_var {
    &tracein;
    my $varname = shift;

    my $value = exists $MUFFIN_VARS{ $varname }
        ? $MUFFIN_VARS{ $varname }
        : undef
        ;

    traceout($value);
    return $value;
}


#----------------------------------------------------------------------------
# FUNCTIONS
#----------------------------------------------------------------------------
my %FUNCS = (
    '+'     => \&_f_add,
    '*'     => \&_f_times,
    '**'    => \&_f_exp,
    '-'     => \&_f_minus,
    '1-'    => \&_f_minus1,
    '2-'    => \&_f_minus2,
    '/'     => \&_f_divide,
    div     => \&_f_div,
    mod     => \&_f_mod,
    'x'     => \&_f_strtimes,
    '.'     => \&_f_stradd,
    '..'    => \&_f_range,
    range   => \&_f_range,
    cardinal=> \&_f_cardinal,
    card    => \&_f_cardinal,
    length  => \&_f_cardinal,
    len     => \&_f_cardinal,
    '#'     => \&_f_cardinal,
    map     => \&_f_map,
    grep    => \&_f_grep,
    zip     => \&_f_zip,
    prog1   => \&_f_prog1,
    prog2   => \&_f_prog2,
    prog3   => \&_f_prog3,
    progn   => \&_f_progn,
    do      => \&_f_progn,
    eval    => \&_f_eval,
    '=='    => \&_f_eq,
    eq      => \&_f_eq,
    '!='    => \&_f_ne,
    ne      => \&_f_ne,
    '>'     => \&_f_gt,
    gt      => \&_f_gt,
    '>='    => \&_f_ge,
    ge      => \&_f_ge,
    '<'     => \&_f_lt,
    lt      => \&_f_lt,
    '<='    => \&_f_le,
    le      => \&_f_le,
    cmp     => \&_f_cmp,
    '<=>'   => \&_f_cmp,
    'cmp?'  => \&_f_progcmp,
    '<?>'   => \&_f_progcmp,
    say     => \&_f_say,
);

sub get_func {
    &tracein;
    my $funcname = shift;
    my $res = exists $FUNCS{ $funcname }  ? $FUNCS{ $funcname } : undef;
    traceout( $res );
    return $res;
}

sub set_func {
    my ($funcname, $def) = @_;
    return $FUNCS{ $funcname } = $def;
}

#----------------------------------------------------------------------------
# FUNCTIONS MINIMAL LIBRARY
# 
# Prototype : f( \@client_params, function_arg...)
#----------------------------------------------------------------------------
sub _f_add {
    my $client_params = shift;
    my $n = 0;
    $n += $_ foreach map { @{ muffin_eval_token($_, @$client_params) } } @_;
    return [ $n ];
}

sub _f_times {
    my $client_params = shift;
    my $n = 1;
    $n *= $_ foreach map { @{ muffin_eval_token($_, @$client_params) } } @_;
    return [ $n ];
}

sub _f_exp {
    my $client_params = shift;
    my ($n, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n **= $_ foreach @values;
    $n = 1 unless defined $n;
    return [ $n ];
}

sub _f_minus {
    my $client_params = shift;
    my ($n, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n -= $_ foreach @values;
    $n = 0 unless defined $n;
    return [ $n ];
}

sub _f_minus1 {
    my $client_params = shift;
    my ($n) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n = 0 unless defined $n;
    return [ $n - 1 ];
}

sub _f_minus2 {
    my $client_params = shift;
    my ($n) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n = 0 unless defined $n;
    return [ $n - 2 ];
}

sub _f_divide {
    my $client_params = shift;
    my ($n, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n /= $_ foreach @values;
    $n = 1 unless defined $n;
    return [ $n ];
}

sub _f_div {
    my $client_params = shift;
    my ($n, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n = int($n / $_) foreach @values;
    $n = 1 unless defined $n;
    return [ $n ];
}

sub _f_mod {
    my $client_params = shift;
    my ($n, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n %= $_ foreach @values;
    $n = 0 unless defined $n;
    return [ $n ];
}

sub _f_strtimes {
    my $client_params = shift;
    my ($s, @values) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $s = '' unless defined $s;
    $s x= $_ foreach @values;
    return [ $s ];
}

sub _f_stradd {
    my $client_params = shift;
    my $s = '';
    $s .= $_ foreach map { @{ muffin_eval_token($_, @$client_params) } } @_;
    return [ $s ];
}

sub _f_range {
    my $client_params = shift;

    my ($min, $max) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final;

    if ( looks_like_number($min)
         && looks_like_number($max)
    ) {
        if ($max < $min) {
            $final = [ reverse $max .. $min ]
        }
        else {
            $final = [ $min .. $max ]
        }
    }
    elsif ( "$max" lt "$min" ) {
            $final = [ reverse "$max" .. "$min" ]
    }
    else {
            $final = [ "$min" .. "$max" ]
    }

    return $final;
}

sub _f_cardinal {
    my $client_params = shift;
    my $n = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    return [ $n ];
}

#  #(map FUNC list)
sub _f_map {
    my ($client_params, $func, @tokens,
        ####
        @funcs, @final, @args, $f) = @_;

    tracein($func, @tokens);

    @funcs = is_token($func) ? @{ muffin_eval_token($func, @$client_params) } : $func;
    @args  = map { @{ muffin_eval_token($_, @$client_params) } } @tokens;
    @final
        = map { 
            $f = $_;
            map {
                @{ apply_func($client_params, $f, 1, $_) }
            } @args;
        } @funcs;

    traceout(\@final);
    return(\@final);
}

##
# MuffinMC meta evaluation
#(eval muffinMC-expr)
sub _f_eval {
    my $client_params = shift;
    &tracein;

    my @forms
        =  map { 
                @{ muffin_eval_token($_, @$client_params) } 
           } @_;

    my $final = muffin_eval( "@forms" );

    traceout($final);
    return $final;
}

#(progn form...)
sub _f_progn {
    my $client_params = shift;
    &tracein;

    # Evaluate progn tokens
    my $final = [];
    ($final = muffin_eval_token($_, @$client_params))
        foreach @_;

    traceout($final);
    return $final;
}

#(prog1 form...)
sub _f_prog1 {
    my ($client_params, $expr, @exprs) = @_;

    # Evaluate first expr
    my $final = [];
    $final = muffin_eval_token($expr, @$client_params)
        if $expr;

    # Evaluate remaining exprs
    muffin_eval_token($_, @$client_params)
        foreach @exprs;

    traceout($final);
    return $final;
}

#(prog2 form...)
sub _f_prog2 {
    my $client_params = shift;

    return [] unless @_;
    muffin_eval_token((shift @_), @$client_params);

    return [] unless @_;
    my $final =  muffin_eval_token((shift @_), @$client_params);

    # Evaluate progn tokens
    muffin_eval_token($_, @$client_params)
        foreach @_;

    traceout($final);
    return $final;
}

#(prog3 form...)
sub _f_prog3 {
    my $client_params = shift;

    return [] unless @_;
    muffin_eval_token((shift @_), @$client_params);

    return [] unless @_;
    muffin_eval_token((shift @_), @$client_params);

    return [] unless @_;
    my $final =  muffin_eval_token((shift @_), @$client_params);

    # Evaluate progn tokens
    muffin_eval_token($_, @$client_params)
        foreach @_;

    traceout($final);
    return $final;
}

    #grep    => \&_f_grep,
    #zip     => \&_f_zip,

#(== a b)
sub _f_eq {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a == $b)
        : ($a eq $b)
        ;

    return [$final];
}

#(!= a b)
sub _f_ne {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a != $b)
        : ($a ne $b)
        ;

    return [$final];
}

#(< a b)
sub _f_lt {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a < $b)
        : ($a lt $b)
        ;

    return [$final];
}

#(<= a b)
sub _f_le {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a <= $b)
        : ($a le $b)
        ;

    return [$final];
}

#(> a b)
sub _f_gt {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a > $b)
        : ($a gt $b)
        ;

    return [$final];
}

#(>= a b)
sub _f_ge {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a >= $b)
        : ($a ge $b)
        ;

    return [$final];
}

#(<=> a b)
sub _f_cmp {
    my $client_params = shift;

    my ($a, $b) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $final 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a <=> $b)
        : ($a cmp $b)
        ;

    return [$final];
}

#(<?> a b lt_bloc eq_bloc gt_bloc)
sub _f_progcmp {
    my $client_params = shift;

    my ($a, $b) = @{ muffin_eval_token(shift, @$client_params) };

    unless( defined $b ) {
        ($b) = @{ muffin_eval_token(shift, @$client_params) };
    }

    my $cmp 
        = looks_like_number($a) || looks_like_number($b)
        ? ($a <=> $b)
        : ($a cmp $b)
        ;

    my $bloc_code = $_[$cmp + 1];

    my $final 
        = defined $bloc_code
            ? muffin_eval_token($bloc_code, @$client_params)
            : []
            ;

    return $final;
}

#(say args...)
sub _f_say {
    my $client_params = shift;

    my @final = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    say $_ foreach @final;

    return \@final;
}

#----------------------------------------------------------------------------
# MUFFIN EVALUATOR
#
# Types
#   STR_LIT         : chaine literale externe, représentant une expression MuffinMC
#   STR_BLANK       : chaine literale vide ou composée seulement de caractères
#                     d'espacement
#   LITERAL         : chaine sans espace, représentant un simple littéral (sans token)
#   TOKEN           : <TOKSTART>STR_TOK<TOKSTOP>
#   STR_TOKEN       : chaine tokenisée (LITERAL|TOKEN)*, séquence littérale de tokens et de litéraux
#
#   FVALUE          : Valeur finale après évaluation
#
#   <TYPE>+         : Liste de 1 ou plusieurs TYPE
#   <TYPE>*         : Liste de 0 ou plusieurs TYPE
#   (TYPE1 | TYPE2) : <TYPE1> ou <TYPE2>
#----------------------------------------------------------------------------
# $final = muffin_eval(field_string);
#
# STR_LIT ---> FVALUE*
#
# Point d'entrée de toute évaluation externe
sub muffin_eval :Export(:DEFAULT) {
    my ($string, @client_params) = @_;
    tracein($string);

    # INIT
    _init();

    my $final = muffin_eval_tokenstring( tokenize($string), @client_params );

    traceout($final);
    return $final;
}

##
# $final = muffin_eval_tokenstring(token_str);
#
# STR_TOKEN ---> FVALUE*
#
sub muffin_eval_tokenstring {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @token_values = map { @{ muffin_eval_token($_, @client_params) } } muffin_split_tokenstring($tokenstring);

    traceout( \@token_values );
    return \@token_values;
}

##
# $final = muffin_eval_token(token);
#
# TOKEN | LITERAL ---> FVALUE*
#


sub muffin_eval_token {
    my ($token, @client_params) = @_;
    tracein($token);

    my $final
        = ( $token =~ m/$REGEX{eval_token}/ )
            ? $DEFMACRO{$3}->($4, @client_params)   # TOKENSTRING
            : [$token]                              # LITERAL
            ;

    traceout($final);
    return $final;
}

##
# my @tokens = muffin_split_tokenstring($token_string);
#
# STR_TOKEN  ---> (LITERAL | TOKEN)*
#
# Les chaines vides ou blanches sont supprimées
sub muffin_split_tokenstring {
    &tracein;
    my $tokenstring = shift;

    my @tokens = ();
    my $str_lit;

    my $p = pos($tokenstring) = 0;    # Just to play safe, reset \G to 0
    while ( $tokenstring =~ m/$REGEX{split_token}/gc ) {
        if ( $-[0] > $p ) {
            # LITERAL* initial ou entre deux TOKEN
            $str_lit = substr $tokenstring, $p, $-[0] - $p;
            #trace("str_lit:         ($str_lit)");

            $str_lit = cleanstring( $str_lit );
            #trace("str_lit_cleaned: ($str_lit)

            push @tokens, split /$REGEX{spaces}/s, $str_lit
                if length($str_lit) > 0;
            # trace( sprintf('==> [%d+%d : (%s)', $p, $-[0] - $p, $tokens[-1]) );
        }
        # TOKEN
        $p = $+[0];
        $tokens[@tokens] = substr $tokenstring, $-[0], $p - $-[0];
        #trace( sprintf('==> [%d+%d : (%s)', $-[0], $p - $-[0], $tokens[-1]) );
    }
    # Last LITERAL*
    if ( $p < length $tokenstring ) {
        $str_lit = substr $tokenstring, $p ;
        #trace("FINAL str_lit:         ($str_lit)");

        $str_lit = cleanstring( $str_lit );
        #trace("FINAL str_lit_cleaned: ($str_lit)");

        push @tokens, split /$REGEX{spaces}/s, $str_lit
            if length $str_lit > 0;
        # trace( sprintf('==> [%d-FIN : (%s)', $p, $tokens[-1]) );
    }
    #say "FINAL: ($_)" foreach @tokens;

    traceout(@tokens);
    return @tokens;
}


##
# $bool = is_token($string);
sub is_token {
    return substr($_[0], 0, 1) eq $SEP;
}

##
# $bool = is_tokenstring($string);
sub is_tokenstring {
    return index($_[0], $SEP) > -1;
}

sub cleanstring {
    my $s = shift;
    $s =~ s/$REGEX{cleanstring}/$CLEANSTRING{$1}/sg;
    $s =~ s/$REGEX{leading_spaces}//s;
    $s =~ s/$REGEX{trailing_spaces}//s;
    return $s;
}


#=============================================================================
#
# MuffinMC MACRO DEFINITIONS
#
#=============================================================================


# SPECIALIZED muffin_eval_tokenstrings
#   - Strings         "..."
#   - Var access      $(varname... )
#   - List access     @(LISTE index...)
#   - Functions       #(op param... )
#   - Var creation    =(varname value)
#   - List builder    L(a b c ...)            => [ a b c ... ]
#   - Conditional     ?(cond vtrue vfalse)    => vtrue | vfalse
#   - Warning         W(text)                 => signal a warning to the parser
#   - Lazy evaluation Q(text)                 => Lazy evaluation
#   - Iterator        I(values)               => Iterator
#   - RepeatSequence  *(n e...)               => Iterator
#   - EvalLazy        E(lazy facteur)         => list
#=============================================================================
##
# '(muffin_expr) - quote form -> the expr in tokenized form
#
# my $final = macro_quote($tokenstring);
#
# Useful for #(map 'FUNC ...)
#
sub macro_quote {
    my $tokenstring = shift;
    tracein($tokenstring);

    my $final = [$tokenstring];

    traceout($final);
    return $final;
}

##
# "(muffin_expr) - qquote a form after evaluation
#
# my $final = macro_qquote($tokenstring);
#
# Useful for meta-evaluation of a MuffinMC expression #(eval 'FUNC ...)
#
sub macro_qquote {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    local $" ='';
    my $final = [ sprintf('"%s"', "@{ muffin_eval_tokenstring($tokenstring, @client_params) }") ];

    traceout($final);
    return $final;
}

##
# Q(muffin_expr) - Delayed evaluation
#
# my $value_ar = macro_set_lazy($tokenstring);
#
# Returns a final closure for later evaluation via E(...)
#
# E(Q(expr)) == Expr
# 
sub macro_set_lazy {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final
        = [
                bless
                    sub { return muffin_eval_tokenstring($tokenstring, @client_params) }, # delayed evaluation
                    'Franckys::MuffinMC::Lazy'
          ];

    traceout($final);
    return $final;
}

# undef | Lazy::sub = muffin_isa_lazy($value)
sub muffin_isa_lazy {
    my $final = shift;
    $final = $final->[0] if ref($final) eq 'ARRAY';
    return ((blessed $final) || '') eq 'Franckys::MuffinMC::Lazy'
        ? $final
        : undef
        ;
}

## 
# I(muffin_expr) - Iterator
#    
# my $value_ar = macro_set_iterator($tokenstring);
#
# Creates an infinite iterator over a finite cycle of values
# for 3rd phase evaluation
#
sub macro_set_iterator {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @tokens = muffin_split_tokenstring($tokenstring);    # FIFO
    my @buffer = ();                                        # FIFO

    my $final
        = @tokens < 1
          ? []
          : [
                bless
                    sub {
                        my $want = shift || 1;
                        tracein("(ITERATOR) ==> WANT:$want  TOKENS:@tokens  BUFFER:@buffer");

                        my @final = ();

                        while ($want > 0) {
                            if (@buffer) {
                                push @final, shift @buffer;
                                $want--;
                            }
                            else {
                                my $token = shift @tokens;
                                push @tokens, $token;

                                push @buffer, @{ apply_func(\@client_params, $token, 0) };
                            }
                        }

                        traceout("(ITERATOR) <== (@final) [WANT:$want TOKENS:@tokens  BUFFER:@buffer]");

                        return \@final;
                    },
                    'Franckys::MuffinMC::Iterator'
            ];

    traceout($final);
    return $final;
}

# undef | Iterator::sub = muffin_isa_iterator($final)
sub muffin_isa_iterator {
    my $final = shift;
    $final = $final->[0] if ref($final) eq 'ARRAY';
    return ((blessed $final) || '') eq 'Franckys::MuffinMC::Iterator'
        ? $final
        : undef
        ;
}


##
# values = "a $(l1) b $(l2)"
#
# my $final = eval_string($tokenstring);
#
# Cardinalité: 1 -> N x M x P x... (Produit cartésien)
#
# 1. La chaine est décomposée en ses composants propres:
#    litéraux et tokens.
#
# 2. Si un composant est un token, celui-ci est expansé
# de manière usuelle pour produire l'ensemble-valeur de valeurs
# associée à ce token. Dans le cas contraire, le composant
# est l'unique élément de l'ensemble-valeur associé
# au composant.
#
# Le résultat final est une liste de chaines résultant
# du produit cartésien entre tous les ensembles-valeurs
# issus de cette décomposition.
#
# Ex. $(op) = [+ - * / ]
#     " a $(op) b = c "
#       | |     | 
#       | |     `--->  R={' b = c '}   1 élément
#       | `--------->  Q={+,-,*/}      4 éléments
#       `----------->  P={' a '}       1 élément
#
# SOL = Recombinaison(P x Q x R) --> 1 x 4 x 1 éléments = 4 éléments
# SOL = {" a + b = c "  " a - b = c "  " a * b = c "  " a / b = c "}
#
# Remarquer comment les espaces sont conservés de manière précise
# au niveau des litéraux.
#
sub eval_string {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Split the string into subset-values, each of one susceptible to be
    # multi-valued, for further recombination.
    # A substring makes a 1-value set.
    # A token is expanded as usual.
    my @sets_of_values =  ();

    pos($tokenstring) = 0;     # Just to play safe, reset \G to 0
    while ( $tokenstring =~ m/$REGEX{eval_string_token}/g ) {
        my $substring = $+{value};

        push @sets_of_values,
                is_token($substring)
                ? muffin_eval_token($substring, @client_params)
                : [$substring]
                ;
    }

    my @values = get_produit_cartesien(@sets_of_values);
    my $final = \@values;

    traceout($final);
    return $final ;
}

# my $values_ar = get_produit_cartesien(@set_of_values);
# Bouge plus vite à droite
sub get_produit_cartesien {
    &tracein;

    if ( @_ == 0) {
        traceout('');
        return '';
    }
    else {
        my $one_set = shift;
        my @right_values   = get_produit_cartesien( @_ );
        my @values
            = map {
                my $left_value = $_;
                map { $left_value . $_ } @right_values
            } @$one_set;

        traceout( @values );
        return @values;
    }
}


###
#
# $(varname...) - Access to variable(s) value(s)
#
# STR_TOKEN -> FVALUE+
#
# my $final = macro_eval_var($tokenstring, @client_params);
#
# Sémantique.
#
# La tokenstring est évaluée pour ramener un ou plusieurs
# noms de variables.
#
# Puis chaque variable est évaluée à son tour.
#
# Les valeurs ramenées sont enfin concaténées entre elles afin
# de produire une valeur finale.
sub macro_eval_var {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @varnames_final = @{ muffin_eval_tokenstring($tokenstring, @client_params) };

    my @values = map { @{ muffin_getval($_, @client_params) } } @varnames_final;

    my $final = \@values;
    traceout($final);
    return $final;
}


##
#
# #(func arg...) - Function evaluation
#
# my $final = macro_eval_func($tokenstring)
#
sub macro_eval_func {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # 1. Split the token into operator and operands
    my ($func, @fargs, $f, $final) =  muffin_split_tokenstring($tokenstring);

    # 2. Eval op if needed
    my @funcs = is_token($func) ? @{ muffin_eval_token($func, @client_params) } : $func;

    # 3. Apply for each $func and gather collected results
    my @final
        = map { @{ apply_func(\@client_params, $_, 0, @fargs) } }
            @funcs;

    traceout(\@final);
    return \@final;
}

sub apply_func {
    my ($client_params, $func, $args_already_evaled, @fargs,
         ####
         $f, $final,
    ) = @_;
    tracein($func, @fargs);

    my $redoing    = 0;
    my $saved_func = $func;

  REDO:
    # Builtin function
    if ( $f = get_func($func) ) {
        unless (
            eval {
                $final 
                    = $f->(
                        $client_params,
                        $args_already_evaled 
                            ? map {sprintf('"%s"', $_)} @fargs  # Protect args from evaluation
                            : @fargs
                      );
            }
        ) {
            $final = muffin_error('EFUNC', "$@")
                if $@;
        }
    }

    # Iterator   
    elsif ( $f = muffin_isa_iterator($func) ) {
        my ($factor) 
            = @fargs == 0                   ? 1
            : $args_already_evaled          ? $fargs[0] 
            :                                 @{ muffin_eval_token($fargs[0], @$client_params) }
            ;

        $final = $f->( looks_like_number($factor) ? $factor : 1 );
    }
    
    # Lazy-form   
    elsif ( $f = muffin_isa_lazy($func) ) {
        $final = $f->();
    }

    # Custom function
    elsif ( is_tokenstring($func) ){
        my %old_bindings = (
            self => muffin_getval('self'),    # Function, to allow anonymous recursive calls
            '@'  => muffin_getval('@'),       # Arguments list
            '#'  => muffin_getval('#'),       # Number of arguments
        );

        # Save old bindings (dynamic scope - shallow binding)
        @fargs = map { @{ muffin_eval_token($_, @$client_params) } } @fargs
            unless $args_already_evaled;
        muffin_setvar( 'self', [ $func ]       );         
        muffin_setvar( '@',    \@fargs         );
        muffin_setvar( '#',    [scalar @fargs] );

        my $i = 1;
        foreach my $arg (@fargs) {
            $old_bindings{ "_$i" } = muffin_getval("_$i")
                if muffin_exists_var( "_$i" );
            muffin_setvar("_$i", [ $arg ] );
            $i++;
        }

        # Evaluate custom function
        unless (
            eval {
                $final = muffin_eval_tokenstring($func, @$client_params);
            }
        ) {
            $final = muffin_error('EFUNC', "$@")
                if $@;
        }

        # Restore old bindings
        while ( my($var, $val) = each %old_bindings ) {
            muffin_setvar($var, $val);
        }
    }

    # Redo ?
    elsif ( $redoing ) {
        $final = [ 1 ]
            if @fargs == 0;
    }

    # Variable fonctionnelle 
    elsif ( muffin_exists_var($func) ) {
        my $val = muffin_getval($func, @$client_params);    # Variable final value [val...]
        
        # Potentially a functional variable...
        if ( @$val == 1 ) {
            $func    = $val->[0];
            $redoing = 1;
            goto REDO;
        }

        # Length of a variable
        elsif ( @fargs == 0 ) {
            $final = [ scalar @$val ];
        }

    }

    # Forme dégénérée #(a) -> a
    $final = [$saved_func]
        unless defined $final;

    traceout($final);
    return $final;
}


##
# @(LIST index...) - List indexed access
#
# Sémantique:
#
# . La forme @(L index...) dans laquelle L est une variable valide
#   est identique à la forme : @( $(L) index...)
#
#   @(L 2 1) --> @( $(L) 2 1) --> (L[2] L[1])
#
# . La forme @(T index...) dans laquelle T représente une 
#   forme fonctionnelle évaluable
#   est identique à la forme : @( =(_X T) index...)
#
#   T est évaluée et le résultat sert de liste sur laquelle
#   les index sont appliqués.
#
# . La forme @(L index...) dans laquelle L n'est pas une variable valide
#   retourne la liste résultant de l'évaluation de l'ensemble
#   des éléments d'entrée, soit l'équivalent de =(_X L index...)
#   sans que la variable _X ne soit jamais définie.
#
# my $final = macro_access_list($tokenstring)
#
# Attention:
#   . Indexes start at 1 (not 0)
#   . Invalid indexes are discarded
#
sub macro_access_list {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Split between list (1st composant) and indexes (the remaining # composants)
    my ($toklist,
        @indices,
        ##
        $list,
        $final,
        $list_found) = muffin_split_tokenstring($tokenstring);

    # First argument must provide the list -- even reduced to 1 element
    #trace( sprintf('LISTE: %s', $toklist ) );
    #trace("INDEXES: ", @indices);
    if ( is_token($toklist) ) {
        $list = muffin_eval_token($toklist, @client_params);
    }
    elsif ( muffin_exists_var($toklist) ) {
        $list = muffin_getval($toklist, @client_params);
    }

    if ( defined $list ) { # Found a list to apply indexes
        my @indexes = map { @{ muffin_eval_token($_, @client_params) } } @indices;

        if (@indexes) {
            my $llength = @$list;
            my @sublist
                = map {
                    !looks_like_number($_)          ? ()
                    : ($_ > 0 && $_ <= $llength)    ? $list->[ $_ - 1 ]     # Mutiny external indexes start at 1
                    : ($_ < 0 && $_ >= -$llength)   ? $list->[ $_     ]     # Parcours de droite à gauche avec indexes négatifs
                    : ()
                } @indexes;
            $final = \@sublist;
        }
        else {
            $final = $list
        }
    }
    else { # in @(L...)
           # L is neither a functional form nor
           # a valid variable.
           # We return the list of all args evaled
        $final = muffin_eval_tokenstring($tokenstring, @client_params);
    }

    traceout($final);
    return $final;
}


###
# W(text) - signal a warning to the parser, returns nothing
#
# my $value_ar = macro_warning($token);
sub macro_warning {
    my ($token, @client_params) = @_;
    tracein($token);
    traceout( [ $token ] );
    return [ $token ];
}

###
# value  =(varname value) - Variable creation
# value  =(// v1 val1 v2 val2...) - affectation en //
#
# my $value_ar = macro_set_var($token);
sub macro_set_var {
    my ($tokenstring, @client_params,
        ####
        @tokens, $op, @rest ) = @_;
    tracein($tokenstring);

    my @final = ();

    # Lookup for // indication in 1st position
    @tokens = muffin_split_tokenstring($tokenstring);
    ($op, @rest) = @{ muffin_eval_token(shift @tokens, @client_params) };

    if (defined $op) {
        if ($op eq '*') {
            my %bindings = ();
            while (@tokens) {
                my ($var, @values) = map { @{ muffin_eval_token($_, @client_params)} } splice @tokens, 0, 2;
                if (defined $var) {
                    $bindings{$var} = \@values;
                    push @final, $var;
                }
            }
            muffin_setvar($_, $bindings{$_})
                foreach @final;
        }
        else {
            # Regular affectation
            @final = (@rest, map { @{ muffin_eval_token($_, @client_params)} } @tokens);
            muffin_setvar($op, \@final);
        }
    }

    traceout(\@final);
    return \@final;
}

###
# L("a b" c "d e" ...) - make list from parameters
#
# To be used in constructs like "a L(1 2 3) c" 
#
#   -> { 'a 1 c' 'a 2 c' 'a 3 c' }
#
# my $value_ar = macro_mklist($token);
sub macro_mklist {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = muffin_eval_tokenstring($tokenstring, @client_params);

    traceout($final);
    return $final;
}

##
# ?(cond vtrue vfalse) - Conditional
#
# Lazy-evaluation : only vtrue or vfalse are evaluated
#
# my $value_ar = macro_eval_cond($token);
sub macro_eval_cond {
    my ($token, @client_params) = @_;
    tracein($token);

    my ($cond_tok, $true_tok, $false_tok) = muffin_split_tokenstring($token);

    my ($cond) = @{ muffin_eval_token($cond_tok, @client_params) };

    my $final = [];

    if ($cond) {
        $final = muffin_eval_token($true_tok, @client_params)
            if defined $true_tok;
    }
    else {
        $final = muffin_eval_token($false_tok, @client_params)
            if defined $false_tok;
    }

    traceout($final);
    return $final;
}

##
# *(facteur_repetitif e1 ... en) - Repeat sequence
#
# my $final = macro_repeat($tokenstring);
sub macro_repeat {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my ($facteur, @values) = @{ muffin_eval_tokenstring($tokenstring, @client_params) };
    my @final
        = 
            !looks_like_number($facteur) ? ( $facteur, @values ) # !!! => ()?
            : $facteur < 1               ? ()
            : $facteur == 1              ? ( @values )
            :                              ( map { @values } 1..$facteur )
            ;

    traceout(\@final);
    return \@final;
}

##
# M(muffinMC-expr) - Meta-evaluation
#
sub macro_meta_eval {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @forms = @{ muffin_eval_tokenstring($tokenstring, @client_params) };
    my $final = muffin_eval( "@forms" );

    traceout($final);
    return $final;
}

##
# P(expr...) - Progn returns the value of the latest expr
sub macro_progn {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [];
    ($final = muffin_eval_token($_, @client_params))
        foreach muffin_split_tokenstring($tokenstring);

    traceout($final);
    return $final;
}

# 1(expr...) - Prog1 - Returns the value of the 1st expr
sub macro_prog1 {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my ($expr, @exprs) = muffin_split_tokenstring($tokenstring);

    my $final = [];
    $final    = muffin_eval_token($expr, @client_params)
        if defined $expr;

    muffin_eval_token($_, @client_params)
        foreach @exprs;

    traceout($final);
    return $final;
}


#----------------------------------------------------------------------------
# PARSER & TOKENIZER
#----------------------------------------------------------------------------
# TOKENIZER -> Tokenize a string
# my $token_string = tokenize($string);
sub tokenize {
    my $s  = shift;

    my $section_level   = 0;
    my $section_escaped = 0;
    my $section_quoted  = [-1];

    $s =~ s{$REGEX{token_marker}}{ rec_descend($1, $2, \$section_level, \$section_escaped, $section_quoted) }ge;

    # Suppress \\\\
    $s =~ s{([\\]+)}{'\\' x (length($1) / 2)}sgeo;

    return $s;
}


sub rec_descend {
    my( $escape_seq,
        $marker,
        $section_level,
        $section_escaped,
        $section_quoted,
    ) = @_;
    &tracein;

    my $rewrite;
    my $escape_flag = 0;
    my $decr_flag;

    # Rewrite escape sequence (2n '\'  -> n '\')
    if ( $escape_seq ) {
        my $n = length $escape_seq;
        $escape_flag = $n % 2 == 1;
        $escape_seq = substr($escape_seq,0,1) x ($n - $escape_flag);
    }

    # STRING MARKER
    if ( $marker eq $STRING_MARKER ) {

        # \"...
        goto DONE
            if $escape_flag;

        if ( $section_quoted->[-1] < $$section_level ) {
            # CASE 1. New string section within a quoted section
            #         ...\$(  "..." )  => no new token
            #                 ^
            # CASE 2. Regular new string section "..." 
            #                                    ^   
            #         => Start token                           
            $$section_level++;
            push @$section_quoted, $$section_level;

            goto DONE
                if $$section_escaped;   # CASE 1
        }
        else {
            # CASE 1. End string section within a quoted section
            #         ...\$(  "..." )  => no new token
            #                     ^
            # CASE 2. Regular new string section "..." 
            #                                        ^   
            #         => End token                           
            pop @$section_quoted;
            $decr_flag = 1;

            goto DONE
                if $$section_escaped;       # CASE 1
            $marker  = $ENDTOKEN_MARKER;    # CASE 2
        }
    }
   
    # ENDTOKEN MARKER
    elsif ( $marker eq $ENDTOKEN_MARKER ) {
        # Ignorer  ... \)
        #               ^
        # ou       "  )  " 
        #             ^
        goto DONE
            if $escape_flag
               || $$section_level < 1
               || $$section_level == $section_quoted->[-1];

        $decr_flag = 1;

        # TOKEN's END marker # Decrement level last
        if ( $$section_escaped ) {
            # Fin de section quotée \$(...) ?
            #                             ^ 
            $$section_escaped = 0
                if $$section_escaped == $$section_level;
            goto DONE;
        }
        # else {
        # Fin de section regulière $(...)
        #                               ^ 
        # }
    }

    # NEW MACRO SECTION 
    else {
        $$section_level++;

        goto DONE
            if $$section_escaped;

        if ( $escape_flag ) {
            # beginning of quoted section  \$(...
            #                               ^                           
            $$section_escaped = $$section_level;
            goto DONE;
        }
    }

    # TOKEN
    $rewrite = ${escape_seq} . ${SEP} . ${$section_level} . $TOKEN_MARKER{$marker};
    goto RETURN;

    DONE:
    # NO TOKEN
    $rewrite = ${escape_seq} . ${marker};

    RETURN:
    $$section_level-- if $decr_flag;
    traceout($rewrite);
    return $rewrite;

}

#-----
1;
__END__

#----------------------------------------------------------------------------
# DOCUMENTATION
#----------------------------------------------------------------------------
=pod 

=head1 DEPENDENCIES

=over 4

=item B<Perl6::Export::Attrs>

=item B<Const::Fast>

=item B<Scalar::Util>

=item B<Franckys::Trace>

=back

=head1 AUTHOR

Franck PORCHER,PhD, C<< <franck.porcher at franckys.com> >>

=head1 DIAGNOSTICS

Successfully tested against Perl 5.14, 5.16, 5.18 and 5.20.

=head1 CONFIGURATION AND ENVIRONMENT

Successfully tested on FreeBSD (9.0+), Debian Linux, and Apple OS X Mountain Lion.

=head1 BUGS AND LIMITATIONS

Please report any bugs or feature requests to C<bug-franckys-error at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Franckys-Error>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.


=head1 INCOMPATIBILITIES

This code is guaranteed to work with Perl 5.16 or higher.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Franckys::MuffinMC


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Franckys-MuffinMC>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Franckys-MuffinMC>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Franckys-MuffinMC>

=item * Search CPAN

L<http://search.cpan.org/dist/Franckys-MuffinMC/>

=back


=head1 LICENSE AND COPYRIGHT

Copyright (C) 2015 - Franck Porcher, Ph.D - All rights reserved

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut
