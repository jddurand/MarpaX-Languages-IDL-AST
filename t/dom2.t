#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;
use File::Spec::Functions qw/catfile/;
use FindBin;
use Test::File::ShareDir
  -root => "$FindBin::Bin/../",
  -share => {
             -module => { 'MarpaX::Languages::IDL::AST' => 'share' }
  };

BEGIN {
    use_ok( 'MarpaX::Languages::IDL::AST' ) || print "Bail out!\n";
}

my $obj = MarpaX::Languages::IDL::AST->new();
my $r = $obj->parse(catfile('data', 'dom.idl'));
print STDERR $r->output();
ok(defined($r), "dom.idl is OK");
