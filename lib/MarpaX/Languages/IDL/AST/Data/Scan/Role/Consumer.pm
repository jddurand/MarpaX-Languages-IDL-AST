use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Role::Consumer;

# ABSTRACT: IDL translation consumer role

# VERSION

# AUTHORITY

use Data::Scan; # 0.003;      # no requires 'output'
use Moo::Role;
use Scalar::Util qw/blessed reftype/;
use Types::Standard -all;
use Types::Common::Numeric -all;
#
# We provide all the required method from Data::Scan::Role::Consumer
#
sub start  {}
sub end    {}
sub sopen  {}
sub sread  { my ($self, $item) = @_; return $item if ($self->isRule($item)); return }
sub sclose {}
#
# We provide these handy method for our consumer
#
sub isRule { my ($self, $item) = @_; return defined($self->_rule)                                             }
sub isL0   { my ($self, $item) = @_; return $self->isRule($item) && $#{$item} == 2 && ! grep { ref } @{$item} }
sub isG1   { my ($self, $item) = @_; return $self->isRule($item) && ! $self->isL0($item)                      }
sub L0     { my ($self, $item) = @_; return $item->[-1] if $self->isL0($item); return                         }
sub G1     { my ($self, $item) = @_; return $item       if $self->isG1($item); return                         }
#
# Internal methods
#
sub _rule {
  my ($self, $item) = @_;

  my $blessed = blessed($item) // '';
  if ($blessed =~ /^IDL::AST::(.+)/ && reftype($item) eq 'ARRAY') {
    return substr($blessed, $-[1], $+[1] - $-[1]);
  }
  return
}

#
# We require an additional output method
#
requires 'output';
#
# Though we totally consume Data::Scan::Role::Consumer
#
with 'Data::Scan::Role::Consumer';

1;
