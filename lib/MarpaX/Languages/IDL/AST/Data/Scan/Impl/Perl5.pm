use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5;

# ABSTRACT: Perl5 default implementation for transpiling IDL

# VERSION

# AUTHORITY

use Moo;
use MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5::_BaseTypes -all;
use Types::Standard -all;
use Types::Common::Numeric -all;

extends 'MarpaX::Languages::IDL::AST::Data::Scan::Impl::_Default';

has main   => (is => 'ro', isa => Str, default => sub { 'IDL' } );
has indent => (is => 'ro', isa => Str, default => sub { '  ' } );

has _lines => (is => 'rw', isa => ArrayRef[Str]);
around trigger_level => sub {
  my ($orig, $self, $level) = @_;

  push(@{$self->_lines}, $self->indent x $self->level);
  return $self->$orig($level)
};
#
# We do not want to pollute perl5's main namespace
#
around globalScope => sub {
  my ($orig, $self) = @_;

  my $globalScope = $self->$orig;

  if ($globalScope =~ /^::/) {
    $globalScope = join('', $self->main, $globalScope)
  } else {
    $globalScope = $globalScope ? join('::', $self->main, $globalScope) : $self->main
  }

  return $globalScope
};

around dsstart => sub {
  my ($orig, $self, $item) = @_;

  $self->_lines([]);

  return $self->$orig($item)
};

around dsopen => sub {
  my ($orig, $self, $item) = @_;

  return $self->$orig($item)
};

around dsread => sub {
  my ($orig, $self, $item) = @_;

  return $self->$orig($item)
};

around dsclose => sub {
  my ($orig, $self, $item) = @_;

  return $self->$orig($item)
};

1;
