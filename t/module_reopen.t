#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

our $DATA = do {local $/; <DATA>};

BEGIN {
    use_ok( 'MarpaX::Languages::IDL::AST' ) || print "Bail out!\n";
}

my $ast = MarpaX::Languages::IDL::AST->new()->ast(\$DATA);
ok(defined($ast), 'module reopening works and provides a defined unique AST');

__DATA__
module M {
  typedef long myLong;
  typedef short myShort;
  interface B {
    typedef string myString;
  };
};
module M {
  typedef char myChar;
};
module N {
  typedef char myChar;
};
module M {
  typedef char myChar2;
};
