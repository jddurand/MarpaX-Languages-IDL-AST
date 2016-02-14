use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5::_Perl5Types;

# ABSTRACT: Perl5 Base Types library

# VERSION

# AUTHORITY

use Type::Library
  -base,
  -declare => qw/Perl5_type
                 Perl5_parameterType
                 Perl5_functionType
                 Perl5_packageType/;
use Type::Utils -all;
use Types::Standard -types;

BEGIN { extends 'MarpaX::Languages::IDL::AST::Data::Scan::Impl::Perl5::_BaseTypes' };
#
# We impose that any type is an instance of Type::Tiny
#
declare Perl5_type, as InstanceOf['Type::Tiny'];
#
# And now we declare everything that is used for the transpiling
#
declare Perl5_parameterType, as Dict[type => Perl5_type, name => Str];
declare Perl5_functionType, as Dict[name => Str, signature => Perl5_type, parameters => ArrayRef[Perl5_parameterType]];
declare Perl5_packageType, as Dict[use       => ArrayRef[Str],
                                   vars      => ArrayRef[Str],
                                   with      => ArrayRef[Str],
                                   extends   => ArrayRef[Str],
                                   types     => ArrayRef[Perl5_type],
                                   functions => ArrayRef[Perl5_functionType]
                                   ];

1;
