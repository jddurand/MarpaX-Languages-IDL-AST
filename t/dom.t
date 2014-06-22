#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 1;

BEGIN {
    use_ok( 'MarpaX::Languages::IDL::AST' ) || print "Bail out!\n";
}

my $idl2ast = MarpaX::Languages::IDL::AST->new();
