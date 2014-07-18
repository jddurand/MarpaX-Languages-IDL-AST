#!env perl
package MarpaX::Languages::IDL::AST::MooseX::_BaseTypes;
use MooseX::Types -declare => [
                               qw/
                                   floatingPtType
                                   signedShortInt
                                   signedLongInt
                                   signedLonglongInt
                                   unsignedShortInt
                                   unsignedLongInt
                                   unsignedLonglongInt
                                   charType
                                   wideCharType
                                   stringType
                                   wideStringType
                                   booleanType
                                   octetType
                                   anyType
                                   objectType
                                   valueBaseType
                                 /
                              ];
use MooseX::Types::Moose qw/Int Str Bool Object Any/;

class_type floatingPtType,    { class => 'Math::BigFloat' };
subtype    signedShortInt,      as Int,  where { ($_ >= -2**15) && ($_ <= (2**15 - 1))};
subtype    signedLongInt,       as Int,  where { ($_ >= -2**31) && ($_ <= (2**31 - 1))};
subtype    signedLonglongInt,   as Int,  where { ($_ >= -2**63) && ($_ <= (2**63 - 1))};
subtype    unsignedShortInt,    as Int,  where { ($_ >=      0) && ($_ <= (2**16 - 1))};
subtype    unsignedLongInt,     as Int,  where { ($_ >=      0) && ($_ <= (2**32 - 1))};
subtype    unsignedLonglongInt, as Int,  where { ($_ >=      0) && ($_ <= (2**64 - 1))};
subtype    charType,            as Str,  where { length($_) == 1 && ord(substr($_, 0, 1)) <= 255 };
subtype    wideCharType,        as Str,  where { length($_) == 1 };
subtype    stringType,          as Str,  where { my $str = $_;
                                                 ! grep {ord(substr($str, $_, 1)) > 255} (0..length($_)) };
subtype    wideStringType,      as Str;
subtype    booleanType,         as Bool;
subtype    octetType,           as Int,  where { $_ >= 0 && $_ <= 255 };
subtype    objectType,          as Object;
subtype    valueBaseType,       as Object;
subtype    anyType,             as Any;

1;
