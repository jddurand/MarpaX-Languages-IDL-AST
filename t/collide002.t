#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
use File::Temp;

our $DATA = do {local $/; <DATA>};
my $tmp = File::Temp->new(UNLINK => 0, SUFFIX => '.idl' );
print $tmp $DATA;
close $tmp;

BEGIN {
    use_ok( 'MarpaX::Languages::IDL::AST' ) || print "Bail out!\n";
}

my $ast = MarpaX::Languages::IDL::AST->new()->parse($tmp->filename)->ast();
ok(defined($ast), 'DOM.idl provides a defined unique AST');

__DATA__
module M {
  typedef short M;
};
