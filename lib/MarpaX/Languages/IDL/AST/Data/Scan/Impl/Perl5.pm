use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5;

# ABSTRACT: Perl5 default implementation for transpiling IDL

# VERSION

# AUTHORITY

use Moo;
use MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5::_BaseTypes -all;
use Scalar::Util qw/blessed reftype/;
use Types::Standard -all;
use Types::Common::Numeric -all;

use constant {
  LEXEME_START => $[,
  LEXEME_LENGTH => $[ + 1,
  LEXEME_VALUE => $[ + 2,
  SEMICOLON => ';'
};

extends 'MarpaX::Languages::IDL::AST::Data::Scan::Impl::_Default';

has main      => (is => 'ro', isa => Str, default => sub { 'IDL' } );
has indent    => (is => 'ro', isa => Str, default => sub { '  ' } );
has separator => (is => 'ro', isa => Str, default => sub { ' ' } );
has newline   => (is => 'ro', isa => Str, default => sub { "\n" } );

has _lines => (is => 'rw', isa => ArrayRef[ArrayRef[Str]]);

sub _pushLine {
  my ($self) = @_;

  push(@{$self->_lines}, [ $self->indent x $self->level ]);
  return
}
after trigger_level => sub {
  my ($self) = @_;

  $self->_pushLine;
  return
};

sub output {
  my ($self) = @_;

  my $separator = $self->separator;
  return join($self->newline, map { join($separator, @{$_}) } @{$self->_lines});
}

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
  #
  # We identify a token like this:
  # this is a blessed array containing only scalars
  # [ start, length, value ]
  #
  my $blessed = blessed($item) // '';
  my $reftype = reftype($item) // '';
  my $isLexeme = $blessed && $reftype eq 'ARRAY' && scalar(@{$item}) == 3 && ! grep { ref } @{$item};

  if ($isLexeme) {
    my $token = $item->[LEXEME_VALUE];
    push(@{$self->_lines->[-1]}, $token);
    #
    # We want the ';' colon to force a newline
    #
    $self->_pushLine if ($token eq SEMICOLON);
  }

  return $self->$orig($item)
};

around dsclose => sub {
  my ($orig, $self, $item) = @_;

  return $self->$orig($item)
};

1;
