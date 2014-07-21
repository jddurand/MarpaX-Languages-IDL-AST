#!env perl

package MarpaX::Languages::IDL::AST::MooseX::_BaseTypes;
use Exporter 'import';
use Moose;
use Moose::Util::TypeConstraints;
our @_all = qw/$_floatingPtType
               $_signedShortInt
               $_signedLongInt
               $_signedLonglongInt
               $_unsignedShortInt
               $_unsignedLongInt
               $_unsignedLonglongInt
               $_charType
               $_wideCharType
               $_stringType
               $_wideStringType
               $_booleanType
               $_octetType
               $_objectType
               $_valueBaseType
               $_anyType/;
our @EXPORT_OK = @_all;
our %EXPORT_TAGS = ('all' => \@_all);              

our $_floatingPtType      = class_type({ class => 'Math::BigFloat' });
our $_signedShortInt      = subtype({as => 'Int',  where => sub { ($_ >= -2**15) && ($_ <= (2**15 - 1))}});
our $_signedLongInt       = subtype({as => 'Int',  where => sub { ($_ >= -2**31) && ($_ <= (2**31 - 1))}});
our $_signedLonglongInt   = subtype({as => 'Int',  where => sub { ($_ >= -2**63) && ($_ <= (2**63 - 1))}});
our $_unsignedShortInt    = subtype({as => 'Int',  where => sub { ($_ >=      0) && ($_ <= (2**16 - 1))}});
our $_unsignedLongInt     = subtype({as => 'Int',  where => sub { ($_ >=      0) && ($_ <= (2**32 - 1))}});
our $_unsignedLonglongInt = subtype({as => 'Int',  where => sub { ($_ >=      0) && ($_ <= (2**64 - 1))}});
our $_charType            = subtype({as => 'Str',  where => sub { length($_) == 1 && ord(substr($_, 0, 1)) <= 255 }});
our $_wideCharType        = subtype({as => 'Str',  where => sub { length($_) == 1 }});
our $_stringType          = subtype({as => 'Str',  where => sub { my $str = $_;
                                                 ! grep {ord(substr($str, $_, 1)) > 255} (0..length($_)) }});
our $_wideStringType      = subtype({as => 'Str'});
our $_booleanType         = subtype({as => 'Bool'});
our $_octetType           = subtype({as => 'Int',  where => sub { $_ >= 0 && $_ <= 255 }});
our $_objectType          = subtype({as => 'Object'});
our $_valueBaseType       = subtype({as => 'Object'});
our $_anyType             = subtype({as => 'Any'});

1;
