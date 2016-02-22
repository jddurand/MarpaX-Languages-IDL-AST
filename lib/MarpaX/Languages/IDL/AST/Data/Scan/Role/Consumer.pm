use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Role::Consumer;
use Moo::Role;

requires 'specification';

with 'Data::Scan::Role::Consumer';

1;
