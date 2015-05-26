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
no warnings             qw(recursion);
use autodie;
use feature             qw( switch say unicode_strings );

use Const::Fast;
use Scalar::Util        qw( blessed looks_like_number );
use Franckys::Trace;
use Franckys::Error;
use File::Temp          qw( tempfile );

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

=item . B<muffin_explain()>

=item . B<muffin_rewrite()>

=item . B<muffin_setvar()>

=item . B<muffin_getval()>

=item . B<muffin_exists_var()>

=item . B<muffin_dump_vars()>

=item . B<muffin_reset_vars()>

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
const my $BS                => ',';

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

my %MACROTYPE = ( # For the syntactic colorization
    $TOKEN_MARKER{$STRING_MARKER}   =>  'string',
    #   M                           =>  'txxx',
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
    split_pragma            => qr/^[[:space:]]*($SEP(\d+)([$defmacro_keys])(.*?)$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})(.*)$/s,
    split_token             => qr/($SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})/s,
    cleanstring             => qr/$SEP\d+([$cleanstring_keys])/s,
    eval_string_token       => qr/(?<value>$SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})|(?<value>[^$SEP]+)/s,
    token_marker            => qr/([$BS]*)([$macro_keys]\(|[${STRING_MARKER}${ENDTOKEN_MARKER}])/,
);

# def_macro($macro, $macro_handler)
sub def_macro {
    my ($macro_type, $macro, $macro_handler, $imacro) = @_;
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
    $MACROTYPE{     $imacro  } = $macro_type;
    $CLEANSTRING{   $imacro  } = "$macro(";             # Idem
    $TOKEN_MARKER{ "$macro(" } = $imacro;               # For the parser / tokenizer

    # Update engine regexes 
    $cleanstring_keys = join '', keys %CLEANSTRING;
    $defmacro_keys    = join '', keys %DEFMACRO;
    $macro_keys       = join '', keys %MACROS;

    $REGEX{ eval_token         } = qr/^($SEP(\d+)([$defmacro_keys])(.*?)$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})$/s;
    $REGEX{ split_token        } = qr/($SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})/s;
    $REGEX{ split_pragma       } = qr/^[[:space:]]*($SEP(\d+)([$defmacro_keys])(.*?)$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})(.*)$/s;
    $REGEX{ cleanstring        } = qr/$SEP\d+([$cleanstring_keys])/s;
    $REGEX{ eval_string_token  } = qr/(?<value>$SEP(\d+)[$defmacro_keys].*?$SEP\2$TOKEN_MARKER{$ENDTOKEN_MARKER})|(?<value>[^$SEP]+)/s;
    $REGEX{ token_marker       } = qr/([$BS]*)([$macro_keys]\(|[${STRING_MARKER}${ENDTOKEN_MARKER}])/;
}

{
    ##
    # PSGI / Plack apps do not run INIT blocks :(((((
    #
    my $init_done = 0;

    sub _init {
        return if $init_done;

        def_macro( 'defvar',     '='  => \&macro_set_var            );
        def_macro( 'getval',     '$'  => \&macro_eval_var           );
        def_macro( 'deflist',     L   => \&macro_mk_list            );
        def_macro( 'defvector',  'V'  => \&macro_mk_vector          );
        def_macro( 'defun',      'F'  => \&macro_defun              );
        def_macro( 'listval',    '@'  => \&macro_access_list        );
        def_macro( 'func',       '#'  => \&macro_eval_func          );
        def_macro( 'cond',       '?'  => \&macro_eval_cond          );
        def_macro( 'multseq',    '*'  => \&macro_repeat             );
        def_macro( 'iterator',    I   => \&macro_set_iterator       );
        def_macro( 'lazy',        Q   => \&macro_set_lazy           );
        def_macro( 'qquote',    q(")  => \&macro_qquote             );
        def_macro( 'warning',     W   => \&macro_warning            );
        def_macro( 'quote',     q(')  => \&macro_quote              );
        def_macro( 'evalmeta',    M   => \&macro_meta_eval          );
        def_macro( 'progn',       P   => \&macro_progn              );
        def_macro( 'prog1',       1   => \&macro_prog1         , 'p');
        def_macro( 'splitval',    S   => \&macro_split_var     , 's');
        def_macro( 'associer',    A   => \&macro_associer           );
        def_macro( 'pragma',     '!'  => \&macro_pragma             );
        def_macro( 'matching',   '~'  => \&macro_pattern_matching   );
        def_macro( 'trace',      'T'  => \&macro_trace              );

        Franckys::Error::def_error(EVAR            => 'Variable indéfinie:[%s].');
        Franckys::Error::def_error(EFUNC           => 'Evaluation fonctionnelle:[%s].');

        $init_done = 1;
    }
}

#----------------------------------------------------------------------------
# PRAGMA
#
#   . Expose pragmas as var $(:pragma) or $(:@) or $(:#)
#   . Pragmas can also be dynamically set with =(:pragma values...)
#
#   Pragmas have 'local' dynamic semantics :
#   - pragmas explicitely sets join the global pragmas stack
#     which can be modified everywhere in the scope and
#     survive the macro scope.
#     Explicit pragmas have the Perl 'local' semantics : global scope + 
#     lexical alteration.
#   - pragmas non explicitely sets live in the local pragmas
#     stack : local scope and local alteration, like the my semantics
#
#----------------------------------------------------------------------------
our %G_PRAGMA = ();
our $E_PRAGMA = {};
our $L_PRAGMA = {};

sub muffin_exists_pragma {
    my $p = shift;
    return  ($p eq '#')
            || ($p eq '@')
            || exists($G_PRAGMA{$p}) || exists($L_PRAGMA->{$p})
            ;

}

sub muffin_get_pragma {
    my $p = shift;
    return  
               ($p eq '#')            ? [ scalar(keys %G_PRAGMA) + scalar(keys %$L_PRAGMA) ]
             : ($p eq '@')            ? [ 
                                           (map { [ $_, @{ $G_PRAGMA{$_} }   ] } sort keys %G_PRAGMA  ),
                                           (map { [ $_, @{ $L_PRAGMA->{$_} } ] } sort keys %$L_PRAGMA ),
                                        ]
             : exists $E_PRAGMA->{$p} ? $G_PRAGMA{$p}
             : exists $L_PRAGMA->{$p} ? $L_PRAGMA->{$p}
             :                          []
    ;
}

sub muffin_set_pragma {
    my ($p, $v) = @_;

    (exists $E_PRAGMA->{$p}  ? \%G_PRAGMA : $L_PRAGMA)->{$p} = $v
        unless ($p eq '#') || ($p eq '@');

    return $v;
}

# [INTERNAL] Return value for internal use => removed from sublist
sub muffin_iget_pragma {
    return "@{muffin_get_pragma(shift)}"
}

# [INTERNAL] Set pragma new $values
sub muffin_iset_pragma {
    my $p = shift;
    muffin_set_pragma($p, [ @_ ]);
}

# [INTERNAL] Return values for internal use of a pragma
sub muffin_lget_pragma {
    return @{muffin_get_pragma(shift)}
}

# [INTERNAL] (void) set default value for pragma
sub set_default_pragma {
    my ($p, $default, $replacement, $cond, $forced) = @_;

    if ($cond) {
        muffin_set_pragma($p, [ $forced ])
    }
    elsif ( ! ( exists($E_PRAGMA->{$p}) || exists($L_PRAGMA->{$p}) ) ) {
        muffin_set_pragma($p, [ defined $default ? $default : () ])
    }
    elsif ( defined $replacement ) {
        muffin_set_pragma($p, [$replacement])
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
C<set_error(TAG, $param...)>  which should return an object that 
handles the method <as_string()> to get the error message as a valid value.

By default, return our own error as a string literal

=cut

sub muffin_error {
    my ($tag, $param, $o, @client_params) = @_;

    my $error;

    $error = $o->set_error($tag, $param, @client_params)
        if $o && ref($o) && $o->can('set_error');

    $error
        =  ( $error
            && ref($error)
            && $error->can( 'as_string' )
           )
           ? $error->as_string()
           : Error($tag, $param, @client_params)->as_string();
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
    NIL     => [],
    nil     => [],
    TRUE    => [1],
    true    => [1],
    VRAI    => [1],
    vrai    => [1],
    FALSE   => [0],
    false   => [0],
    FAUX    => [0],
    faux    => [0],
    TAB     => ["\t"],
    tab     => ["\t"],
    NL      => ["\n"],
    nl      => ["\n"],
    LF      => ["\n"],
    lf      => ["\n"],
    CR      => ["\r"],
    cr      => ["\r"],
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

    # Pseudo argument variable :: $(_i)
    if ( ($varname =~ /^_(\d+)$/) && ($1 > 0) ) {
        my $arglist = $MUFFIN_VARS{'@'};
        $arglist->[$1 - 1] = $value;

        # Initializing missing values in $(@)
        if ($1 > @$arglist) {
            for (my $i = @$arglist; $i < $1 - 1; $i++) {
                $arglist->[$i] = [];
            }
            $MUFFIN_VARS{'#'} = @$arglist;
        }
        $MUFFIN_VARS{'@'} = $arglist;
    }

    # Pseudo pragma variable :: $(:pragma)
    #  Pragmas are set by MuffinMC, one
    #  cannot create new ones for they wouldn't be
    #  of any use.
    elsif ( ($varname =~ /^:(.*)$/) && muffin_exists_pragma($1)) {
        muffin_set_pragma($1, $value);
    }

    # Regular var
    else {
        $MUFFIN_VARS{ $varname } = $value;
    }

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

    my $value = [];

    # Pseudo argument variable :: $(_i)
    if ( ($varname =~ /^_(\d+)$/) && ($1 > 0) ) {
        my $arglist = $MUFFIN_VARS{'@'};
        $value = [ $arglist->[$1 - 1] ]
            if $1 <= @$arglist;
    }

    # Pseudo pragma variable :: $(:pragma)
    elsif ( ($varname =~ /^:(.*)$/) && muffin_exists_pragma($1)) {
        $value = muffin_get_pragma($1);
    }

    # Regular variable
    else {

        # Undefined variable
        # return muffin_error('EVAR', $varname, @client_params)
        #    unless exists $MUFFIN_VARS{ $varname };
        #
        goto RETURN_UNDEF
            unless exists $MUFFIN_VARS{ $varname };

        # Undefined value
        unless ( defined($value = $MUFFIN_VARS{ $varname }) ) {
            $value = [];
            goto RETURN_UNDEF
        }

        # Expand functional-variable
        $value = $value->( @client_params )
            if ref($value) eq 'CODE';

        # Expand alias (muffin expr as a string)
        $value =  muffin_eval( $value, @client_params )
            unless ref($value) eq 'ARRAY';
    }

  RETURN_UNDEF:
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

sub muffin_exists_var :Export(:DEFAULT) {
    &tracein;
    my $varname = shift;

    my $value = undef;

    # Pseudo argument variable :: $(_i)
    if ( ($varname =~ /^_(\d+)$/) && ($1 > 0) ) {
        my $arglist = $MUFFIN_VARS{'@'};
        $value = [ $arglist->[$1 - 1] ]
            if $1 <= @$arglist;
    }

    # Regular variable
    elsif ( exists $MUFFIN_VARS{ $varname } ) {
        $value = $MUFFIN_VARS{ $varname }
    }

    traceout($value);
    return $value;
}

###
### muffin_dump_vars()
###
=pod 

=head2 muffin_dump_vars( pattern ) [PUBLIC]

 my $stash = muffin_dump_vars()

Dump the MuffinMC Table symbols as a hash reference

=cut

sub muffin_dump_vars :Export(:DEFAULT) {
    my $client_params = shift;
    &tracein;

    my @final;

    if (@_) {
        my @patterns
            = map {
                s/\*/([^[:space:]]*)/sg;
                qr/$_/si
              } map { 
                    @{ muffin_eval_token($_, @$client_params) } 
              } @_;

        foreach my $var ( keys %MUFFIN_VARS ) {
            foreach my $p ( @patterns ) {
                if ( $var =~ m/$p/si ) {
                    my $val = $MUFFIN_VARS{$var};
                    push @final,
                         [ $var, 
                            ref($val) eq 'ARRAY'
                            ?  @{ $val } 
                            :     $val
                         ];
                    last;
                }
            }
        }

        if ( @final == 1 ) {
            @final = @{ $final[0] }
        }
    }
    else {
        my ($var, $val);
        while( ($var,$val) = each %MUFFIN_VARS) {
            push @final,
                 [ $var, 
                    ref($val) eq 'ARRAY'
                    ?  @{ $val } 
                    :     $val
                 ];
        }
    }

    traceout( \@final );
    return \@final;
}


###
### muffin_reset_vars()
###
=pod 

=head2 muffin_reset_vars() [PUBLIC]

 my $stash = muffin_reset_vars()

Reset the MuffinMC Table symbols

=cut

sub muffin_reset_vars :Export(:DEFAULT) {
    %MUFFIN_VARS  = (
        self    => [],
        '@'     => [],
        '#'     => [ 0 ],
        NIL     => [],
        nil     => [],
        TRUE    => [1],
        true    => [1],
        VRAI    => [1],
        vrai    => [1],
        FALSE   => [0],
        false   => [0],
        FAUX    => [0],
        faux    => [0],
        TAB     => ["\t"],
        tab     => ["\t"],
        NL      => ["\n"],
        nl      => ["\n"],
        LF      => ["\n"],
        lf      => ["\n"],
        CR      => ["\r"],
        cr      => ["\r"],
    );
    return muffin_dump_vars();
}

#----------------------------------------------------------------------------
# FUNCTIONS
#----------------------------------------------------------------------------
my %FUNCS = (
    '+'     => \&_f_add,
    '1+'    => \&_f_add1,
    '2+'    => \&_f_add2,
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
    filter  => \&_f_grep,
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
    'not'   => \&_f_not,
    '!'     => \&_f_not,
    say     => \&_f_say,
    element => \&_f_element,
    membre  => \&_f_element,
    member  => \&_f_element,
    pas_element => \&_f_not_element,
    not_element => \&_f_not_element,
    not_member  => \&_f_not_element,
    assoc       => \&_f_assoc,
    'assoc*'    => \&_f_assoc_split,
    ean13       => \&_f_ean13,
    dumpvars    => \&muffin_dump_vars,
);

sub get_func {
    my $funcname = shift;
    return exists $FUNCS{ $funcname }  ? $FUNCS{ $funcname } : undef;
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

sub _f_add1 {
    my $client_params = shift;
    my ($n) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n = 0 unless defined $n;
    return [ $n + 1 ];
}

sub _f_add2 {
    my $client_params = shift;
    my ($n) = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    $n = 0 unless defined $n;
    return [ $n + 2 ];
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

#  #(grep FUNC list)
sub _f_grep {
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
                my $arg   = $_;
                my $final = apply_func($client_params, $f, 1, $arg);
                ( $final
                   && ( (@$final > 1)
                        || ( @$final == 1 && $final->[0] ) 
                      )
                )
                ? $arg
                : ()
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

# #(element x liste)
# retourne l'indice (>0) dans la liste, ou 0
sub _f_element {
    my $client_params = shift;
    my ($e, @l)       = map { @{ muffin_eval_token($_, @$client_params) } } @_;
    my $is_number     = looks_like_number($e);

    my $i = 1;
    foreach (@l) {
        if ( $is_number ) {
            last if $e == $_ ; 
        }
        else {
            last if "$e" eq "$_"; 
        }
        $i++;
    }
    return [ $i > @l ? 0 : $i ];
}

# #(not_element x liste)
# retourne 1 ou 0
sub _f_not_element {
    my $final = &_f_element;
    return [ $final->[0] ? 0 : 1 ];
}

# #(not x)
sub _f_not {
    my $client_params = shift;
    my @final
        = map {
            $_ ? 0 : 1;
        } map { @{ muffin_eval_token($_, @$client_params) } } @_;
    return \@final;
}

##
# #(assoc E aliste)
# pour chaque e de E
# retourne :
#   . la valeur associée dans la aliste, utilisant e comme clé
#   . rien si e n'est pas une clé de la aliste
#
# La dernière clé de la aliste peut être '*', qui signifie défaut
# Retourner alors la valeur associée
#
sub _f_assoc {
    my ($client_params, $token_key, @token_alist) = @_;

    my @keys  = @{ muffin_eval_token($token_key, @$client_params) };
    my @alist = map { @{ muffin_eval_token($_, @$client_params) } } @token_alist;

    return _f_assoc_common(\@keys, \@alist);
}

##
# #(assoc* E aliste) === #(assoc S(E) aliste)
#
# pour chaque e de E
# retourne :
#   . déstructurer e en mots
#   . la valeur associée dans la aliste, utilisant e comme clé
#   . rien si e n'est pas une clé de la aliste
#
# La dernière clé de la aliste peut être '*', qui signifie défaut
# Retourner alors la valeur associée
#
sub _f_assoc_split {
    my ($client_params, $token_key, @token_alist) = @_;

    my @keys
        = map { split /$REGEX{spaces}/s
          } @{ muffin_eval_token($token_key, @$client_params) };

    my @alist
        = map { @{ muffin_eval_token($_, @$client_params) } 
          } @token_alist;

    return _f_assoc_common(\@keys, \@alist);
}

sub _f_assoc_common {
    my ($keys, $alist) = @_;

    my $n         = @$alist;
    my $n_1       = $n - 1;
    my $n_2       = $n - 2;
    my @final     = ();

    foreach my $key (@$keys) {
        my $i = 0;
        while ( $i < $n_1 ) {
            if ( $key eq $alist->[$i]
                 || ( $i == $n_2 && $alist->[$i] eq '*' )
            ) {
                push @final, $alist->[$i+1];
                last;
            }
            else {
                $i += 2;
            }
        }
    }

    return \@final;
}

## 
# #(ean13  x liste)
# retourne un code ean13 correct, avec le check digit set (13ième digit)
#
sub _f_ean13 {
    my $client_params = shift;

    require Business::Barcode::EAN13;

    # value,pos,ndigits,...
    my @ean13_buffer = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    my @l = map { @{ muffin_eval_token($_, @$client_params) } } @_;

    my ($value, $pos, $len, $rep);

    while (@l >= 3) {
        my ($value, $pos, $len) = splice @l, 0, 3;

        # Consistency check
        # $pos >= 0
        # and $len > 0
        # and $pos + $len <= 12
        # and value in [0, 10^len[
        next if $pos < 0
                || $len <= 0
                || ($pos + $len) > 12
                || $value < 0 
                || $value >= (10 ** $len);
        
        # Build string representation
        $rep   = sprintf("%0${len}d", $value);

        # Stores it in ean13 buffer
        splice @ean13_buffer, $pos, $len, (split //, $rep); 
    }

    # Ean13 body
    my $ean13    = join '', @ean13_buffer;
    $ean13      .= Business::Barcode::EAN13::check_digit( $ean13 );

    return [ $ean13 ];
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

    my $final;

    if ( $token =~ m/$REGEX{eval_token}/ ) {
        # TOKENSTRING
        my $macro            = $3;
        my $data_tokenstring = $4;
        my $e_pragma;

        ($e_pragma, $data_tokenstring) = muffin_extract_pragma($data_tokenstring, @client_params);

        # Set explicit pragma
        my $save_epragma = $E_PRAGMA;
        $E_PRAGMA        = $e_pragma;

        my $save_lpragma = $L_PRAGMA;
        $L_PRAGMA        = {};

        # Save pragmas
        my (%save_gpragma, $p, $v);
        if ( %$e_pragma ) { 

            # Set G_PRAGMA
            while ( ($p, $v) = each %$e_pragma ) {
                $save_gpragma{$p} = $G_PRAGMA{$p}
                    if exists $G_PRAGMA{$p};
                $G_PRAGMA{$p} = $v;
            }

            trace("Set Pragma -> ", \%G_PRAGMA);
        }

        # EVAL TOKEN
        $final = $DEFMACRO{$macro}->($e_pragma, $data_tokenstring, @client_params);

        # Restore pragmas
        if ( %$e_pragma ) { 
            while ( ($p, $v) = each %$e_pragma ) {
                if (exists $save_gpragma{$p}) {
                    $G_PRAGMA{$p} =  $save_gpragma{$p}
                }
                else {
                    delete $G_PRAGMA{$p};
                }
            }

            trace("Restored Pragma -> ", \%G_PRAGMA);
        }

        $E_PRAGMA = $save_epragma;
        $L_PRAGMA = $save_lpragma;
    }
    else {
        # LITERAL
        $final = [$token]
    }

    traceout($final);
    return $final;
}

##
# my ($pragma, $tokenstring) =  muffin_extract_pragma($tokenstring, @client_params);
#
# STR_TOKEN  ---> pragma_href + STR_TOKEN
#
# Extrait et evalue le prgama en première position de la token string
sub  muffin_extract_pragma {
    my ($tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @final;

    if ( $tokenstring =~ m/$REGEX{split_pragma}/
          && token_type($3) eq 'pragma'
    ) {
        my $pragma_tokenstring  = $1;    # Extracted pragma
        my $data_tokenstring    = $5;    # Rest

        my %pragma = @{ muffin_eval_token($pragma_tokenstring, @client_params) };

        # Trick pragma to be compatible with muffin values
        #   that is  { pragma => [ $final ], ... }
        $pragma{$_} = [ $pragma{$_} ] foreach keys %pragma;

        @final = (\%pragma, $data_tokenstring);
    }
    else {
        @final = ({}, $tokenstring);
    }

    traceout(@final);
    return @final;
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
#
# Fast/unprecise lookup
sub is_token {
    return substr($_[0], 0, 1) eq $SEP;
}

##
# $bool = is_tokenstring($string);
sub is_tokenstring {
    return index($_[0], $SEP) > -1;
}

##
# my $token_type | undef = token_type(token|macro)
#
# slow/precise lookup
sub token_type {
    my $s = shift;

    return exists $MACROTYPE{ $s }           ? $MACROTYPE{ $s }
         : (
             ($s =~ m/$REGEX{eval_token}/)
             && (exists $MACROTYPE{ $3 })
           )                                 ? $MACROTYPE{ $3 }
         : undef
         ;
}


sub cleanstring {
    my $s = shift;
    $s =~ s/$REGEX{cleanstring}/$CLEANSTRING{$1}/sg;
    $s =~ s/$REGEX{leading_spaces}//s;
    $s =~ s/$REGEX{trailing_spaces}//s;
    return $s;
}

##
# [explained...] = muffin_explain(muffin-expr);
#
#
sub muffin_explain :Export(:DEFAULT) {
    my ($muffin_expr, @client_params) = @_;
    tracein($muffin_expr);

    # INIT
    _init();

    my $explained = muffin_explain_tokenstring( tokenize($muffin_expr, 1), 0, @client_params );

    traceout($explained);
    return $explained;
}

##
# \@list = muffin_explain_tokenstring( $tokenstring, $depth, @client_params );
#
sub muffin_explain_tokenstring {
    my ($tokenstring, $depth, @client_params) = @_;
    tracein($tokenstring);

    # INIT
    _init();

    my @explained = map { muffin_explain_token($_, $depth, 0, @client_params) } muffin_split_tokenstring( $tokenstring );

    traceout(\@explained);
    return \@explained;
}

##
# $obj = muffin_explain_token( $token, $depth, instring,  @client_params );
#
sub muffin_explain_token {
    my ($token, $depth, $instring, @client_params) = @_;
    tracein($token);

    my $obj;

    if ( $token =~ m/$REGEX{eval_token}/ ) {
        # MACRO OBJECT
        
        my $marker  = $CLEANSTRING{ $3 };
        my $tt_type =   $MACROTYPE{ $3 };

        if ( $marker eq $STRING_MARKER ) {
            # STRING MACRO
            my $tokenstring    = $4;
            my @sets_of_values = ();
            pos($tokenstring)  = 0;     # Just to play safe, reset \G to 0

            # Special split to keep sequence of whitespaces
            while ( $tokenstring =~ m/$REGEX{eval_string_token}/g ) {
                push @sets_of_values, $+{value};
            }

            $obj
                = {
                tt_type     => $tt_type,
                tt_start    => $STRING_MARKER,
                tt_stop     => $STRING_MARKER,
                tt_value    => [ map { muffin_explain_token($_, $depth + 1, 1,  @client_params) } @sets_of_values ],
                tt_depth    => $depth,
              }

        }
        else {
            # OTHER MACRO
            my $explains = muffin_explain_tokenstring( $4, $depth + 1, 0, @client_params );

            if ( @$explains ) {
                my $n = @$explains;

                given ( $tt_type ) {
                    #  =(V ...)
                    when ('defvar') {
                        if ( $explains->[0]{tt_type}  eq 'literal'    
                             && ( $explains->[0]{tt_value} eq '*' || $explains->[0]{tt_value} eq '**')
                        ){
                            # =(* Var Val Var Val...)
                            $explains->[0]{tt_type} = 'literal_inherit';

                            my $i = 1;
                            while ( $i < $n ) {
                                $explains->[$i]{tt_type} = 'literal_var'
                                    if $explains->[$i]{tt_type}  eq 'literal';
                                $i += 2;
                            }
                        }
                        else {
                            # =(V val...)
                            $explains->[0]{tt_type} = 'literal_var'
                                if $explains->[0]{tt_type}  eq 'literal';
                        }
                    }

                    #  $(V ...)
                    when ('getval') {
                        foreach (@$explains) {
                            $_->{tt_type} =  'literal_var'
                                if $_->{tt_type}  eq 'literal';
                        }
                    }
                    
                    #  S(V ...)
                    when ('splitval') {
                        foreach (@$explains) {
                            $_->{tt_type} =  'literal_var'
                                if $_->{tt_type}  eq 'literal';
                        }
                    }

                    #  @(V ...)
                    when ('listval') {
                        $explains->[0]{tt_type} = 'literal_var'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                    #  #(F ...)
                    when ('func') {
                        $explains->[0]{tt_type} = 'literal_func'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                    #  A(form...)
                    when ('associer') {
                        # A(V ...)
                        $explains->[0]{tt_type} = 'literal_var'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                    #  ~(L patterns)
                    when ('matching') {
                        # A(V ...)
                        $explains->[0]{tt_type} = 'literal_var'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                    #  F(form...)
                    when ('defun') {
                        $explains->[0]{tt_type} = 'literal_var'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                    default {
                        $explains->[0]{tt_type} = 'literal_inherit'
                            if $explains->[0]{tt_type} eq 'literal';
                    }

                } # given{...}
            } # if
 
            $obj
                = {
                tt_type     => $tt_type,
                tt_start    => $marker,
                tt_stop     => $ENDTOKEN_MARKER,
                tt_value    => $explains,
                tt_depth    => $depth,
              }
        }
    }
    else {
        # LITERAL OBJECT
        $obj
            = {
                tt_type     => $instring ? 'literal_string': 'literal',
                tt_start    => '',
                tt_stop     => '',
                tt_value    => $token,
                tt_depth    => $depth,
              }
            ;
    }

    traceout($obj);
    return $obj;
}


##
# $rewrite = muffin_rewrite(muffin-expr);
#
#
sub muffin_rewrite :Export(:DEFAULT) {
    my ($muffin_expr, @client_params) = @_;
    tracein($muffin_expr);

    # INIT
    _init();

    my $objlist  = muffin_explain($muffin_expr, @client_params );
    my $rewrites = muffin_rewrite_objlist($objlist);
    my $rewrite  = "@{ $rewrites }";

    traceout($rewrite);
    return $rewrite;
}

##
# [ $rewrite, ... ] = muffin_rewrite_objlist( [ $obj, ... ] );
#
sub muffin_rewrite_objlist {
    my ($objlist) = @_;
    tracein($objlist);

    # INIT
    _init();

    my @rewrites = map { muffin_rewrite_obj($_) } @$objlist;

    traceout(\@rewrites);
    return \@rewrites;
}

##
# $rewrite = muffin_rewrite_obj($obj);
#
sub muffin_rewrite_obj {
    my ($obj) = @_;
    tracein($obj);

    my $rewrite;

    if ( $obj->{tt_type} =~ /^literal/ ) {
        $rewrite = $obj->{tt_value};
    }
    else {
        my $rewrites = muffin_rewrite_objlist( $obj->{tt_value} );
        my $content
            = $obj->{tt_type} eq 'string'
                ? do {
                    local $"='';
                     "@{ $rewrites }";
                  }
                : "@{ $rewrites }"
                ;

        $rewrite 
            = sprintf(
                    '%s%s%s',
                    $obj->{tt_start},
                    $content,
                    $obj->{tt_stop},
                );
    }

    traceout($rewrite);
    return $rewrite;
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
# '(muffin_expr) - return the non evaluated tokenstring, for later evaluation
#
# my $final = macro_quote($tokenstring);
#
# Useful for #(map 'FUNC ...)
#
sub macro_quote {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [$tokenstring];

    traceout($final);
    return $final;
}

##
# F(muffin_expr) - defines a functional body
#
# my $final = macro_defun($tokenstring);
#
# Useful for #(map F(def) ...)
#
# Functionaly equivalent to '(...)
#
sub macro_defun {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [$tokenstring];

    traceout($final);
    return $final;
}

##
# "(muffin_expr) - qquote the result of an evaluation
#
# my $final = macro_qquote($tokenstring);
#
# Useful for meta-evaluation of a MuffinMC expression #(eval 'FUNC ...)
#
# PRAGMAS
#   :quote       <bool>  faux par défaut
#   :quote-with  value   (double quote by default) Defines the left/right quotes. Implies :quote
#   :quote-left  value   (double quote by default). Implies :quote
#   :quote-right value   (double quote by default). Implies :quote
#
#   :nospace     <bool>  faux par défaut. Vrai => join-with ''
#   :join-with   value   (space string by default)
#
sub macro_qquote {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Pragmas
    set_default_pragma('quote',
                        0,
                        undef,
                        exists($pragma->{'quote-left'})
                            || exists($pragma->{'quote-right'})
                            || exists($pragma->{'quote-with'}),
                        1);
    set_default_pragma('quote-with' , '"');
    set_default_pragma('quote-left' , muffin_iget_pragma('quote-with'));
    set_default_pragma('quote-right', muffin_iget_pragma('quote-with'));
    set_default_pragma('nospace'    , 0);
    set_default_pragma('join-with'  , muffin_iget_pragma('nospace') ? '' : ' '); # explicitely overlaps nospace setting

    my $final = muffin_eval_tokenstring($tokenstring, @client_params);

    local $" = muffin_iget_pragma('join-with');
    $final 
        = [ 
            sprintf(    '%s%s%s', 
                        muffin_iget_pragma('quote') ? muffin_iget_pragma('quote-left') : '',
                        "@{$final}",
                        muffin_iget_pragma('quote') ? muffin_iget_pragma('quote-right') : '',
            )
          ];

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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final
        = [
                bless
                    # f(@client_params)
                    sub { return muffin_eval_tokenstring($tokenstring, @_ ? @_ : @client_params) },
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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @tokens = muffin_split_tokenstring($tokenstring);    # FIFO
    my @buffer = ();                                        # FIFO

    my $final
        = @tokens < 1
          ? []
          : [
                bless
                    # f(factor, @client_params)
                    sub {
                        my $want = shift || 1;
                        my @client_params = @_ ? @_ : @client_params;

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
# PRAGMAS:
#   :cross-product  <bool>  (true by default)
#   :quote          <bool>  faux par défaut
#   :quote-with     value   (double quote by default) Defines the left/right quotes. Implies :quote
#   :quote-left     value   (double quote by default). Implies :quote
#   :quote-right    value   (double quote by default). Implies :quote
#
#   :join-multiple-with     value   (space string by default)
#   :quote-multiple         <bool>  faux par défaut
#   :quote-multiple-with    value   (double quote by default) Defines the left/right quotes. Implies :quote
#   :quote-multiple-left    value   (double quote by default). Implies :quote
#   :quote-multiple-right   value   (double quote by default). Implies :quote
#
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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Pragmas
    set_default_pragma('cross-product', 1);
    set_default_pragma('quote',
                        0,
                        undef,
                        exists($pragma->{'quote-left'})
                            || exists($pragma->{'quote-right'})
                            || exists($pragma->{'quote-with'}),
                        1);
    set_default_pragma('quote-with' , '"');
    set_default_pragma('quote-left' , muffin_iget_pragma('quote-with'));
    set_default_pragma('quote-right', muffin_iget_pragma('quote-with'));

    set_default_pragma('join-multiple-with', ' ');
    set_default_pragma('quote-multiple',
                        0,
                        undef,
                        exists($pragma->{'quote-multiple-left'})
                            || exists($pragma->{'quote-multiple-right'})
                            || exists($pragma->{'quote-multiple-with'}),
                        1);
    set_default_pragma('quote-multiple-with' , '"');
    set_default_pragma('quote-multiple-left' , muffin_iget_pragma('quote-multiple-with'));
    set_default_pragma('quote-multiple-right', muffin_iget_pragma('quote-multiple-with'));

    # Split the string into subset-values, each of one susceptible to be
    # multi-valued, for further recombination.
    # A substring makes a 1-value set.
    # A token is expanded as usual.
    my @sets_of_values =  ();
    pos($tokenstring) = 0;     # Just to play safe, reset \G to 0
    while ( $tokenstring =~ m/$REGEX{eval_string_token}/g ) {
        my $substring = $+{value};

        if ( is_token($substring) ) {
            my $final = muffin_eval_token($substring, @client_params);
            # An empty set is the neutral element for
            # the cross-product, and thus need not
            # be added for computation.
            push @sets_of_values, $final
                if @$final > 0;
        }
        else {
            push @sets_of_values, [$substring];
        }
    }

    my (@final, $final, $part);
    if ( muffin_iget_pragma('cross-product') ) {
        @final = get_produit_cartesien(@sets_of_values);
    }
    else {
        $final = '';

        foreach $part (@sets_of_values) {
            if ( @$part > 1 ) {
                local $" = muffin_iget_pragma('join-multiple-with');
                $final
                    = sprintf( '%s%s%s%s', 
                                $final,
                                muffin_iget_pragma('quote-multiple') ? muffin_iget_pragma('quote-multiple-left') : '',
                                "@{$part}",
                                muffin_iget_pragma('quote-multiple') ? muffin_iget_pragma('quote-multiple-right') : '',
                      );
            }
            else {
                $final = $final . $part->[0];
            }
        }
        @final = ($final);
    }

    if ( muffin_iget_pragma('quote') ) {
        my $quote_left  = muffin_iget_pragma('quote-left');
        my $quote_right = muffin_iget_pragma('quote-right');
        @final = map { "${quote_left}${_}${quote_right}" } @final;
    }
    $final = \@final;

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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @varnames_final = @{ muffin_eval_tokenstring($tokenstring, @client_params) };

    my @values = map { @{ muffin_getval($_, @client_params) } } @varnames_final;

    my $final = \@values;
    traceout($final);
    return $final;
}


###
#
# S(varname|value...) - Split variable values or simple values
#                       using spaces by default.
#
#
# STR_TOKEN -> FVALUE+
#
# Pragmas:
#   :split-on pattern
#
sub macro_split_var {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Pragmas
    set_default_pragma('split-on', $REGEX{spaces});

    # Computation
    my @varnames = @{ muffin_eval_tokenstring($tokenstring, @client_params) };

    # Split work
    my $pattern =  muffin_iget_pragma('split-on');
       $pattern = qr/$pattern/s;

    my @values
        = map {
              split /$pattern/s
          } map { 
                muffin_exists_var($_)
                    ? @{ muffin_getval($_, @client_params) } 
                    : $_
            } @varnames;

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
    my ($pragma, $tokenstring, @client_params) = @_;
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
    # Builtin function : give it the  control
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
            $final = muffin_error('EFUNC', "$@", @$client_params)
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

        $final = $f->( looks_like_number($factor) ? $factor : 1, @$client_params );
    }
    
    # Lazy-form & Custom function
    elsif ( is_tokenstring($func) || ($f = muffin_isa_lazy($func)) ){
        # Evaluate function arguments
        @fargs = map { @{ muffin_eval_token($_, @$client_params) } } @fargs
            unless $args_already_evaled;

        # Save actual bindings
        my %save_binding_bloc = (
            self => muffin_getval('self'),    # Function, to allow anonymous recursive calls
            '@'  => muffin_getval('@'),       # Arguments list
            '#'  => muffin_getval('#'),       # Number of arguments
        );

        # Create a shallow binding block (dynamic scope)
        muffin_setvar( 'self', [ $func ]       );         
        muffin_setvar( '@',    \@fargs         );
        muffin_setvar( '#',    [scalar @fargs] );

        # Evaluate custom function
        unless (
            eval {
                # On évalue la lazy dans le contexte de l'appelant, non de la
                # fermeture initiale
                $final = muffin_isa_lazy($func) ? $f->(@$client_params) : muffin_eval_tokenstring($func, @$client_params);
            }
        ) {
            $final = muffin_error('EFUNC', "$@", @$client_params)
                if $@;
        }

        # Restore old bindings
        while ( my($var, $val) = each %save_binding_bloc ) {
            muffin_setvar($var, $val);
        }
    }

    # Redo ?
    elsif ( $redoing ) {
        $final = [ 1 ]      # Count number of elements
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
# @([ MATRIX dims dims ...) - Matrix multidimentional indexed acess
#   At any level, * means : All elements to become
#   new sublists to search for
#
#  PRAGMAS
#  :matrix-mode     <bool>  (false by default)
#
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
#   => constructeur de listes == L(L index...)
#
# my $final = macro_access_list($tokenstring)
#
# Attention:
#   . Indexes start at 1 (not 0)
#   . Invalid indexes are discarded
#
sub macro_access_list {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    #PRAGMAS
    set_default_pragma('matrix-mode', 0);

    # Split between list (1st composant) and indexes (the remaining # composants)
    my ($toklist, @indices) = muffin_split_tokenstring($tokenstring);

    my ( $list,
         $final,
         $list_found,
         $is_matrix_op
    );

    # First argument must provide the list -- even reduced to 1 element
    #trace( sprintf('LISTE: %s', $toklist ) );
    #trace("INDEXES: ", @indices);
    if ( is_token($toklist) ) {
        $list = muffin_eval_token($toklist, @client_params);
    }
    elsif ( muffin_exists_var($toklist) ) {
        $list = muffin_getval($toklist, @client_params);
    }
    
    if ( defined $list
         && (ref $list) eq 'ARRAY'
    ) { # Found a real list to apply indexes to

        if ( @indices == 0 ) {
            $final = $list;
        }

        # Matrix access
        elsif ( muffin_iget_pragma('matrix-mode') ) {
            my @master = ( $list );

            foreach my $token ( @indices ) {
                # indices d'extraction de la dimension
                #    (on peut extraire plusieurs éléments à chaque niveau
                #     y inclus mutliple fois '*')
                my @dims = @{ muffin_eval_token($token, @client_params) };

                # Extraction de l'indice pour chaque vecteur de @master
                # Avec déréférencement automatique à chaque niveau
                @master
                    = map {
                        my $list = $_;

                        map {
                            my $dim = $_;

                              (ref $list) ne 'ARRAY'        ?  $list                # Point fixe : l'élément n'est plus un ensemble
                            : $dim eq '*'                   ? @$list                # On extrait tous les éléments
                            : !looks_like_number($dim)      ? ()                    # On ignore les indices non numériques
                            : ($dim > 0 && $dim <= @$list)  ? $list->[ $dim - 1 ]   # Indice positif > 0 (Mutiny external indexes start at 1)
                            : ($dim < 0 && $dim >= -@$list) ? $list->[ $dim     ]   # Indice négatif pour une parcours de droite à gauche
                            : ()                                                    # On ignore les indices nuls
                            ;
                        } @dims
                    } @master;
            }

            $final = \@master;
        }

        # List access
        else {
            my $n = @$list;

            my @sublist
                = map {
                    !looks_like_number($_)  ? ()
                    : ($_ > 0 && $_ <= $n)  ? $list->[ $_ - 1 ]     # Mutiny external indexes start at 1
                    : ($_ < 0 && $_ >= -$n) ? $list->[ $_     ]     # Parcours de droite à gauche avec indexes négatifs
                    : ()                                            # On ignore les indices nuls
                  } map { 
                    @{ muffin_eval_token($_, @client_params) } 
                  } @indices;

            $final = \@sublist;
        }
    }
    else { # => @( e1... ) or @([ e1 ... )
           # e1 is neither a functional form nor a valid variable.
           # We return the list of all args evaled, like L(...)
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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [ $tokenstring ];

    traceout($final);
    return $final;
}

###
# value  =(varname value) - Variable creation
# value  =(*  v1 val1 v2 val2...) - affectation en //
# value  =(** v1 val1 v2 val2...) - affectation en sequence
#        === =(v1 val1) =(v2 val2) ...
#
# my $value_ar = macro_set_var($token);
sub macro_set_var {
    my ($pragma, $tokenstring, @client_params,
        ####
        @tokens, $buffer, $op, $var, %bindings) = @_;
    tracein($tokenstring);

    # Final values
    my @final = ();

    # Lookup for ** or *  indication in 1st position
    @tokens = muffin_split_tokenstring($tokenstring);

    # Work
    $buffer = @tokens ? muffin_eval_token(shift @tokens, @client_params) : [];
    $op     = shift @$buffer;

    if (defined $op) {
        if ( (my $parallel = $op eq '*')
             || $op eq '**'
        ){
            while (@tokens || @$buffer) {
                # try hard to have at least a var
                $buffer = muffin_eval_token(shift @tokens, @client_params)
                    while @tokens && @$buffer == 0;

                last unless @$buffer;    # no more vars to assign !

                # Extract the variable name
                $var = shift @$buffer;

                # Try hard to have at least one value
                $buffer = muffin_eval_token(shift @tokens, @client_params)
                    if @tokens && @$buffer == 0;

                # Make the assignment
                push @final, $var;
                if ($parallel) {
                   $bindings{$var} = $buffer;
                }
                else {
                    muffin_setvar($var, $buffer);
                }
                $buffer = [];
            }

            if ( $parallel ) {
                muffin_setvar($_, $bindings{$_})
                    foreach @final;
            }
        }
        else {
            # Regular affectation
            @final = (@$buffer, map { @{ muffin_eval_token($_, @client_params)} } @tokens);
            muffin_setvar($op, \@final);
        }
    }

    traceout(\@final);
    return \@final;
}

###
# L(a b c)  - make list from parameters
#
# To be used in constructs like "a L(1 2 3) c" 
#
#   -> { 'a 1 c' 'a 2 c' 'a 3 c' }
#
# my $final = macro_mk_list($token);
sub macro_mk_list {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = muffin_eval_tokenstring($tokenstring, @client_params);

    traceout($final);
    return $final;
}

###
# V( a b c)  - make vector
# V( V(a b c) V(d e f) V( 1 2 3)) - makes a 3x3 matrix list from parameters [[a b c][d e f][1 2 3]]
#
# my [[v...]] = macro_mk_vector($token);
sub macro_mk_vector {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [ muffin_eval_tokenstring($tokenstring, @client_params) ];

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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my ($cond_tok, $true_tok, $false_tok) = muffin_split_tokenstring($tokenstring);

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
    my ($pragma, $tokenstring, @client_params) = @_;
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
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my @forms = @{ muffin_eval_tokenstring($tokenstring, @client_params) };
    my $final = muffin_eval( "@forms" );

    traceout($final);
    return $final;
}


##
# P(expr...) - Progn returns the value of the latest expr
sub macro_progn {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $final = [];
    ($final = muffin_eval_token($_, @client_params))
        foreach muffin_split_tokenstring($tokenstring);

    traceout($final);
    return $final;
}


# 1(expr...) - Prog1 - Returns the value of the 1st expr
sub macro_prog1 {
    my ($pragma, $tokenstring, @client_params) = @_;
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


##
# A(prefix value...) - Associer
#
# A( L(P Q...) "a b" c "d e") -> 
#   ("Pa Pb" Pc "Pd Pe" "Qa Qb" Qc "Qd Qe" ...)
#
# PRAGMAS
#   :quote          <bool>  faux par défaut
#   :quote-with     value   (double quote by default) Defines the left/right quotes. Implies :quote
#   :quote-left     value   (double quote by default). Implies :quote
#   :quote-right    value   (double quote by default). Implies :quote
#   :join-with      value   ('' by default)
#   :join-foo       value   ('' by default)
#   :prefix         value   ('' by default)
#   :suffix         value   ('' by default)
#   :as-prefix      <bool>  (true  by default)
#   :as-suffx       <bool>  (false by default)
#   :split          <bool>  (true by default)
#   :split-on               (\s+  by default)
sub macro_associer {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Pragmas
    set_default_pragma('quote',
                        0,
                        undef,
                        exists($pragma->{'quote-left'})
                            || exists($pragma->{'quote-right'})
                            || exists($pragma->{'quote-with'}),
                        1);
    set_default_pragma('quote-with' , '"');
    set_default_pragma('quote-left' , muffin_iget_pragma('quote-with'));
    set_default_pragma('quote-right', muffin_iget_pragma('quote-with'));
    set_default_pragma('join-with','');
    set_default_pragma('join-foo',undef);
    set_default_pragma('split','1');
    set_default_pragma('split-on','[[:space:]]+');
    set_default_pragma('prefix','');
    set_default_pragma('suffix','');
    set_default_pragma('as-both',0);
    set_default_pragma('as-suffix', muffin_iget_pragma('as-both'));
    set_default_pragma('as-prefix', muffin_iget_pragma('as-both') || !muffin_iget_pragma('as-suffix'));

    my ($tok_prefixes, @tokens) = muffin_split_tokenstring($tokenstring);

    # 1. The glue
    my @glues
        = @{ muffin_eval_token($tok_prefixes, @client_params) };
    muffin_set_pragma('prefix', \@glues)
        if muffin_iget_pragma('as-prefix')
           && !exists($pragma->{'prefix'});
    muffin_set_pragma('suffix', \@glues)
        if muffin_iget_pragma('as-suffix') 
           && !exists($pragma->{'suffix'});

    # 2. The values to be 
    my @values 
        = map { @{ muffin_eval_token($_, @client_params) } }
            @tokens;

    my $join_with = muffin_iget_pragma('join-with');

    my @final
        = map {
            my $prefix = $_;

            map {
                my $split_on  = muffin_iget_pragma('split-on');
                my @subvalues = muffin_iget_pragma('split') ? split /$split_on/ : $_;

                map {
                    my $subvalue = $_;

                    map {
                        my $suffix  = $_;
                        local $"    = muffin_iget_pragma('join-with');

                        my @results = ($prefix, $subvalue, $suffix);
                        my @joiners = muffin_lget_pragma('join-foo');

                        if (@joiners) {
                            # $e        available as $(a) or $(_1)
                            # $pattern  available as $(b) or $(_2)
                            my @args = @results;
                            my $foo;
                            @results 
                                = map { 
                                    $foo = $_;
                                    @{ apply_func(\@client_params, $foo, 1, @args) }
                                } @joiners;
                        }

                        sprintf('%s%s%s', 
                            muffin_iget_pragma('quote') ? muffin_iget_pragma('quote-left') : '',
                            "@results",
                            muffin_iget_pragma('quote') ? muffin_iget_pragma('quote-right') : '',
                        )
                    } muffin_lget_pragma('suffix')
                } @subvalues;
            } @values;
        } muffin_lget_pragma('prefix');

    traceout(\@final);
    return \@final;
}


##
# !( :directive_true !:directive_false :directive value...) - Pragma
#
# => [ pragma value pragma value ... ]
# Removes directive that do not start with a ':'
#
# Pragmas:
#   :true  <value>
#   :false <value>
#
sub macro_pragma {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # Pragmas
    set_default_pragma('true',  1);
    set_default_pragma('false', 0);

    my @values = @{ muffin_eval_tokenstring($tokenstring, @client_params) };

    my $true  = muffin_iget_pragma('true' );
    my $false = muffin_iget_pragma('false');

    my %pragma = ();
    my ($directive, $value);

    while (@values) {
        $directive = shift @values;

        if ( $directive =~ /^!:[^[:space:]]/s ) {
            # Boolean negative directive =>  !:directive
            $directive =~ s/^!://s;
            $pragma{ $directive } = $false;
        }
        elsif ( $directive =~ /^:[^[:space:]]/s ) {
            $directive =~ s/^://s;

            if (@values && ($values[0] =~ /^!?:[^[:space:]]/s)) {
                # 2 consecutive directives  :a :b   ou :a !:b
                # => First is assumed to be a positive boolean directive
                $pragma{ $directive } = $true;
            }
            elsif (@values) {
                # Regular directive => :directive value
                $pragma{ $directive } = shift @values;
            }
            else {
                # positive boolean directive :a (last one)
                $pragma{ $directive } = $true;
            }
        }
        #
        # else  { ignore value , not a directive }
    }

    my $final = [ %pragma ];

    traceout($final);
    return $final;
}


##
# ~( LIST PATTERNS ) - pattern matching
#
# Matching
#   :match-all      <bool>      (true by default)  implie !:match-first
#   :match-n        integer     (false by default)  implie !:match-first !:match-all
#   :match-first    <bool>      (false by default)
#
#   :mode-glob      <bool>      (true  by default) implies !:mode-strict
#   :mode-icase     <bool>      (true  by default)
#   :mode-strict    <bool>      (false by default) implies !:mode-glob
#
# Calcul des résultats
#   :return-match   <bool>      (true)
#   :return-bool    <bool>      (false)
#   :return-element <bool>      (false)
#
#   :match-with     function_name or definition (none by default) Receives elt in a and pattern in b
#   :m-continuation custom continuation function for each match. Receives results as usually, element in a, pattern in b
#   :c-continuation custom continuation function for each cycle match. Receives results as usually, element in a, pattern in b, results in @
#
# Presentation des résultats
#   :quote          <bool>  faux par défaut
#   :quote-with     value   (double quote by default) Defines the left/right quotes
#   :quote-left     value   (double quote by default)
#   :quote-right    value   (double quote by default)
#
#   :join           <bool>  (faux par défaut)
#   :nospace        <bool>  faux par défaut. Vrai => join-with ''
#   :join-with      value   (space string by default. Implies join true)
#
#   :vactor-mode    <bool> (faux)   only works for match-all, where each match
#                                   is presented in its own subcell
#
sub macro_pattern_matching {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    # PRAGMAS : number of matching
    set_default_pragma( 'match-first', 
                        0,
                        undef,
                        exists($pragma->{'match-n'}) && (muffin_iget_pragma('match-n') == 1),
                        1,
    );
    set_default_pragma( 'match-n',
                        0,
                        undef,
                        ( 
                          (exists $pragma->{'match-n'} && (muffin_iget_pragma('match-n') <= 1))
                          || muffin_iget_pragma('match-first')
                        ),
                        0,
    );
    set_default_pragma( 'match-all',
                        undef,
                        undef,
                        1,
                        !(muffin_iget_pragma('match-first') || muffin_iget_pragma('match-n')),
    );


    # PRAGMAS : pattern semantics
    set_default_pragma( 'mode-icase',  1);
    set_default_pragma( 'mode-strict', 0);
    set_default_pragma( 'mode-glob', 
                        1,
                        undef,
                        muffin_iget_pragma('mode-strict'),
                        0,
    );

    # PRAGMAS : results behaviour
    set_default_pragma( 'return-element',  0);
    set_default_pragma( 'return-bool',
                        0,
                        undef,
                        muffin_iget_pragma('return-element'),
                        0,
    );
    set_default_pragma( 'return-match',
                        undef,
                        undef,
                        1,
                        !(muffin_iget_pragma('return-element') || muffin_iget_pragma('return-bool')),
    );

    # PRAGMAS : results presentation
    set_default_pragma('quote',
                        0,
                        undef,
                        exists($pragma->{'quote-left'})
                            || exists($pragma->{'quote-right'})
                            || exists($pragma->{'quote-with'}),
                        1);
    set_default_pragma('quote-with' , '"');
    set_default_pragma('quote-left' , muffin_iget_pragma('quote-with'));
    set_default_pragma('quote-right', muffin_iget_pragma('quote-with'));
    set_default_pragma('join',
                        0,
                        undef,
                        exists($pragma->{'nospace'})
                            || exists($pragma->{'join-with'}),
                        1);
    set_default_pragma('nospace'    , 0);
    set_default_pragma('join-with'  , muffin_iget_pragma('nospace') ? '' : ' '); # explicitely overlaps nospace setting
    set_default_pragma('vactor-mode',
                        0,
                        undef,
                        muffin_iget_pragma('match-first'),
                        0,
    );

    # PRAGMAS : Custom Matchers
    set_default_pragma('match-with', undef);

    # PRAGMAS : EveryMatch custom Continuations
    set_default_pragma('m-continuation', undef);

    # PRAGMAS : EveryCycle custom Continuations
    #   (a cycle runs all patterns against ONE element)
    set_default_pragma('c-continuation', undef);

    # Split elements from patterns
    my ($toklist, @tok_patterns) = muffin_split_tokenstring($tokenstring);

    # Compute elements and patterns
    my @elements;
    if ( is_token($toklist) ) {
        @elements = @{ muffin_eval_token($toklist, @client_params) }
    }
    elsif ( muffin_exists_var($toklist) ) {
        @elements = @{ muffin_getval($toklist, @client_params) }
    }
    else {
        @elements = ($toklist);
    }

    # Compute patterns
    my @patterns 
        = map { @{ muffin_eval_token($_, @client_params) } }
            @tok_patterns;

    # Compile patterns (glob mode / icase mode / strict mode)
    my $mode_glob  = muffin_iget_pragma('mode-glob');
    my $mode_icase = muffin_iget_pragma('mode-icase');
    unless ( muffin_iget_pragma('mode-strict') ) {
        @patterns
            = map {
                my $pattern = $_;
                $pattern =~ s/\*/([^[:space:]]*)/sg
                    if $mode_glob;
                $mode_icase ? qr/$pattern/si : qr/$pattern/s
              } @patterns;
    }

    # Save actual bindings
    my %save_binding_bloc = (
        (muffin_exists_var('a') ? (a => muffin_getval('a')) : ()),
        (muffin_exists_var('b') ? (b => muffin_getval('b')) : ()),
    );

    # PATTERN MATCHING ENGINE
    # A matching cycle is considered for each element turn by turn,
    #   running all patterns against it.
    my @matchers = ();
    my @mcont    = ();
    my @ccont    = ();
    my @final = ();

    foreach my $e (@elements) {
        my @cycle_results = ();

        foreach my $pattern (@patterns) {
            muffin_setvar( 'a', [ "$e" ]);
            muffin_setvar( 'b', [ "$pattern" ]);

            my @results;

            # Core pattern matching : custom or system
            @matchers = muffin_lget_pragma('match-with');
            if (@matchers) {
                # $e        available as $(a) or $(_1)
                # $pattern  available as $(b) or $(_2)
                my @args = ($e, $pattern);
                my $foo;
                @results 
                    = map { 
                        $foo = $_;
                        @{ apply_func(\@client_params, $foo, 1, @args) }
                      } @matchers;
            }
            else {
                @results
                    =   muffin_iget_pragma('mode-strict') 
                        ? ( $e eq $pattern ? $e : () )
                        : ( muffin_iget_pragma('match-all') ? $e =~ m/$pattern/sg : $e =~ m/$pattern/s )
                        ;
            }
            @results = (@results > 0 ? 1 : 0)
                if muffin_iget_pragma('return-bool');
            @results = $e
                if @results && muffin_iget_pragma('return-element');

            # Apply Match Continuation on @results
            # - element in $(a)
            # - pattern in $(b)
            # - matching results in $(@)
            @mcont = muffin_lget_pragma('m-continuation');
            if (@results && @mcont) {
                my @args = @results;
                my $f;
                @results 
                    = map { 
                        $f = $_;
                        @{ apply_func(\@client_params, $f, 1, @args) }
                      } @mcont;
            }

            push @cycle_results, @results;
        }

        # Apply cycle continuation here on @results
        # - element in $(a)
        # - pattern in $(b)
        # - matching results in $(@)
        @ccont = muffin_lget_pragma('c-continuation');
        if (@cycle_results && @ccont) {
            my @args = @cycle_results;
            my $f;
            @cycle_results 
                = map { 
                    $f = $_;
                    @{ apply_func(\@client_params, $f, 1, @args) }
                  } @ccont;
        }

        # Results Presentation
        if ( muffin_iget_pragma('vactor-mode') ) {
            if (@cycle_results) {
                my $final;

                if ( muffin_iget_pragma('join') ) {
                    local $"  = muffin_iget_pragma('join-with');
                    my $quote = muffin_iget_pragma('quote');
                    $final 
                        = sprintf('%s%s%s', 
                                $quote ? muffin_iget_pragma('quote-left')  : '',
                                "@cycle_results",
                                $quote ? muffin_iget_pragma('quote-right') : '',
                          )
                }
                else {
                    $final = \@cycle_results;
                }

                push @final, $final;
            }
        }
        else {
            push @final, @cycle_results;
        }

        # Stop here if match first
        if ( @cycle_results ) {
            goto GAMEOVER 
                if muffin_iget_pragma('match-first');
            if ( my $match_n = muffin_iget_pragma('match-n') ) {
                muffin_iset_pragma('match-n', --$match_n);
                goto GAMEOVER if $match_n == 0;
            }
        }
    }

  GAMEOVER:
    # Restore old bindings
    while ( my($var, $val) = each %save_binding_bloc ) {
        muffin_setvar($var, $val);
    }
  
    # Aggregate results
    if ( muffin_iget_pragma('join') ) {
        local $"  = muffin_iget_pragma('join-with');
        my $quote = muffin_iget_pragma('quote');
        @final = sprintf('%s%s%s', 
                          $quote ? muffin_iget_pragma('quote-left')  : '',
                          "@final",
                          $quote ? muffin_iget_pragma('quote-right') : '',
        );
    }
    
    traceout(\@final);
    return \@final;
}

##
# T(muffinMC) - Capture trace of given expression
#
sub macro_trace {
    my ($pragma, $tokenstring, @client_params) = @_;
    tracein($tokenstring);

    my $save_trace_mode = trace_mode();
    my ($undeffh, $filename) = tempfile('MuffinMC-temp-XXXXXXXXXX', OPEN => 0);
    trace_on(0, $filename) unless $save_trace_mode;

    my $final = muffin_eval_tokenstring($tokenstring, @client_params);

    unless ($save_trace_mode) {
        trace_off();

        if ( open my $fh, '<', $filename ) {
            unshift @$final,
                    ( map { 
                            chomp;
                            1 while ( s/$SEP(\d+)([$defmacro_keys])(.*?)$SEP\1$TOKEN_MARKER{$ENDTOKEN_MARKER}/macro_trace_rewrite($2,$3)/gse );
                            1 while ( s/[\\]000(\d+)[\\]?([$defmacro_keys])(.*?)[\\]000\1$TOKEN_MARKER{$ENDTOKEN_MARKER}/macro_trace_rewrite($2,$3)/gse );
                            $_
                      } <$fh> );
        }
    }

    unlink $filename;

    traceout($final);
    return $final;
}

sub macro_trace_rewrite {
    my ($imacro, $content) = @_;
    my $marker  = $CLEANSTRING{ $imacro };

    return $marker eq $STRING_MARKER ? "\"$content\"" : "${marker}${content})"
}

#----------------------------------------------------------------------------
# PARSER & TOKENIZER
#----------------------------------------------------------------------------
# TOKENIZER -> Tokenize a string
# my $token_string = tokenize($string, $explain_mode);
sub tokenize {
    my ($s, $explain_mode)  = @_;
    &tracein;

    my $section_level   = 0;
    my $section_escaped = 0;
    my $section_quoted  = [-1];

    $s =~ s{$REGEX{token_marker}}{ rec_descend($1, $2, \$section_level, \$section_escaped, $section_quoted, $explain_mode) }ge;

    # Reduces  '\\' -> '\'
    # and      \x   -> x
    #   unless $explain_mode, which should conserve the quoting intact
    $s =~ s{([$BS]+)}{"$BS" x (length($1) / 2)}sge
        unless $explain_mode;

    traceout($s);
    return $s;
}


sub rec_descend {
    my( $escape_seq,
        $marker,
        $section_level,
        $section_escaped,
        $section_quoted,
        $explain_mode
    ) = @_;
    &tracein;

    my $escape_seq_orig = "$escape_seq";
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
    $rewrite 
        = ( $explain_mode ? $escape_seq_orig : $escape_seq )
          . $SEP 
          . ${$section_level} 
          . $TOKEN_MARKER{$marker}
          ;
    goto RETURN;

    DONE:
    # NO TOKEN
    $rewrite = ( $explain_mode ? $escape_seq_orig : $escape_seq ) . $marker;

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
