use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5;

# ABSTRACT: Perl5 default implementation for transpiling IDL

# VERSION

# AUTHORITY

use Moo;
use MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5::_Perl5Types -all;
use Types::Standard -all;

extends 'MarpaX::Languages::IDL::AST::Data::Scan::Impl::_Default';

has _packages => (is => 'rw', isa => HashRef[Perl5_packageType]);

around dsstart => sub {
  my ($orig, $self) = @_;

  $self->_packages({});

  return $self->$orig()
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

around dsend => sub {
  my ($orig, $self) = @_;

  return $self->$orig()
};

1;
