#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
use File::Spec::Functions qw/catfile/;

BEGIN {
    use_ok( 'MarpaX::Languages::IDL::AST2' ) || print "Bail out!\n";
}

my $obj = MarpaX::Languages::IDL::AST2->new();
my $r = $obj->parse(catfile('data', 'dom.idl'));

ok(defined($r), "dom.idl is OK");

use Data::Dumper;
print Dumper($obj->ast);
