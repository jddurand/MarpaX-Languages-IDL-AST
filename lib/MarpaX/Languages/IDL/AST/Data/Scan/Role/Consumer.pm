use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: MarpaX::Languages::IDL::AST's Data::Scan Consumer

# VERSION

# AUTHORITY

requires 'specification';

with 'Data::Scan::Role::Consumer';

1;
