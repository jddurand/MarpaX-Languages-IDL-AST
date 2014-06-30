use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Value;
use Scalar::Util qw/blessed/;

# ABSTRACT: Translate an IDL source to an AST - parse tree value

# VERSION

=head1 DESCRIPTION

This module contain some tools used by IDL to AST

=head2 new()

Instanciate a new object.

=cut

use constant {
  LEXEME_INDEX => 0
};

use constant {
  LEXEME_INDEX_START => 0,
  LEXEME_INDEX_LENGTH => 1,
  LEXEME_INDEX_VALUE => 2
};

sub new {
  my ($class) = @_;

  #
  # "Contents of an entire IDL file, together with the contents of any files referenced by #include statements, forms a naming scope.
  # Definitions that do not appear inside a scope are part of the global scope. There is only a single global scope, irrespective
  # of the number of source files that form a specification."
  #
  # Scopes can be nested.
  #
  my $self = {
  };

  bless($self, $class);

  return $self;
}

sub _scopedName {
  my ($self, $scopedName) = @_;

  #
  # We concatenate identifiers to have a single-like lexeme
  #
  my @scopedNames = ();
  my $firstIdentifier = $scopedName->[0];
  my $lastIdentifier = $scopedName->[-1];
  my $firstLexeme = $firstIdentifier->[LEXEME_INDEX];
  my $lastLexeme = $lastIdentifier->[LEXEME_INDEX];

  my $start = $firstLexeme->[LEXEME_INDEX_START];
  my $length = $lastLexeme->[LEXEME_INDEX_START] + $lastLexeme->[LEXEME_INDEX_LENGTH] - $start;
  map {push(@scopedNames, $_->[LEXEME_INDEX]->[LEXEME_INDEX_VALUE])} @{$scopedName};
  my $value = join('::', @scopedNames);
  my $rc = bless [$start, $length, $value ], blessed($scopedName);

  return $rc;
}

1;
