use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST::Data::Scan::Impl::_Default;

# ABSTRACT: Default implementation consumer of MarpaX::Languages::IDL::AST::Role::Consumer

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module provide and manage general notions of IDL, that are independant of the language generated bindings, like current root, current scope, and scoped name resolution. This module is writen as Data::Scan::Role::Consumer implementation.

=head1 SYNOPSIS

    use MarpaX::Languages::IDL::AST;

    my $idlPath = 'source.idl';
    my $ast = MarpaX::Languages::IDL::AST->new()->parse($idlPath)->validate();

=cut

use Carp qw/croak/;
use Moo;
use MooX::HandlesVia;
use Scalar::Util qw/blessed reftype/;
use Types::Standard -all;
use Types::Common::Numeric -all;

has _root  => (is => 'rw', isa => ArrayRef[Str],
               handles_via => 'Array',
               handles => {
                           _push_root => 'push',
                           _join_root => 'join',
                           _splice_root => 'splice'
                          }
              );
has _scope => (is => 'rw', isa => ArrayRef[Str],
               handles_via => 'Array',
               handles => {
                           _push_scope => 'push',
                           _join_scope => 'join',
                           _splice_scope => 'splice'
                          }
              );
has _context => (is => 'rw', isa => ArrayRef[Any],
                 handles_via => 'Array',
                 handles => {
                             _push_context => 'push',
                             _pop_context => 'pop',
                             _count_context => 'count',
                             _get_context => 'get'
                            }
                );
has _unnamedScopeCounter => (is => 'rw', isa => PositiveOrZeroInt);

sub currentRoot {
  my ($self) = @_;
  return $self->_join_root('')
}

sub currentScope {
  my ($self) = @_;
  return $self->_join_scope('')
}

sub globalName {
  my ($self, $token) = @_;

  return join('', $self->globalScope, $token)
}

sub globalScope {
  my ($self) = @_;

  return join('', $self->currentRoot, $self->currentScope)
}

#
# 5.21.1 Qualified Names
#
# Prior to starting to scan a file containing an IDL specification,
# the name of the current root is initially empty ('') and the
# name of the current scope is initially empty ('').
# Whenever a module keyword is encountered, the string “::” and the
# associated identifier are appended to the name of the current root;
# upon detection of the termination of the module, the trailing '::' and identifier are deleted
# from the name of the current root. Whenever an interface, struct, union, or
# exception keyword is encountered, the string '::' and the associated identifier are appended
# to the name of the current scope; upon detection of the termination of the interface, struct,
# union, or exception, the trailing '::' and identifier
# are deleted from the name of the current scope. Additionally, a new, unnamed,
# scope is entered when the parameters of an
# operation declaration are processed; this allows the parameter names to duplicate other identifiers;
# when parameter processing has completed, the unnamed scope is exited.
#

sub dsstart {
  my ($self) = @_;

  $self->_root([]);
  $self->_scope([]);
  $self->_context([]);
  $self->_unnamedScopeCounter(0);

  return
}

sub dsend {
  my ($self) = @_;

  return
}
#
# In dsopen() and dsclose() we manage root and scope names
# --------------------------------------------------------
#
# 5.21.2 Scoping Rules and Name Resolution
#
# The scope for module, interface, valuetype, struct, exception, eventtype, component, and home begins immediately
# following its opening ‘{‘ and ends immediately preceding its closing ‘}’.
# The scope of an operation begins immediately following its ‘(‘ and ends immediately preceding its closing ‘)’.
# The scope of a union begins immediately following the ‘(‘ following the keyword switch, and ends immediately preceding its closing ‘}’.
# The appearance of the declaration of any of these kinds in any scope, subject to semantic validity of such declaration, opens a nested scope associated with that
# declaration.
#
sub dsopen  {
  my ($self, $item) = @_;
  #
  # We always want to remember the full context.
  # Per def we dsopen() only blessed items, c.f. dsread()
  #
  my $blessed = $self->_blessed($item);
  $self->_push_context($item);
  my $pushed;

  if ($blessed eq 'LCURLY') {
    #
    # <module>                     ::= MODULE <identifier> LCURLY <definitionMany> RCURLY
    #
    if ($self->_count_context >= 2 && $self->_blessed(my $module = $self->_get_context(-2)) eq 'module') {
      my $identifier = $module->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_root('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $interfaceDcl = $self->_get_context(-2)) eq 'interfaceDcl') {
      #
      # <interfaceDcl>               ::= <interfaceHeader> LCURLY <interfaceBody> RCURLY
      # <interface>                  ::= <interfaceDcl>
      #                                | <forwardDcl>
      # <interfaceHeader>            ::= <abstractOrLocalMaybe> INTERFACE <identifier> <interfaceInheritanceSpecMaybe>
      #
      my $interfaceHeader = $interfaceDcl->[0];
      my $identifier = $interfaceHeader->[2];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $valueAbsDcl = $self->_get_context(-2)) eq 'valueAbsDcl') {
      #
      # <valueAbsDcl>              ::= ABSTRACT VALUETYPE <identifier> <valueInheritanceSpecMaybe> LCURLY <exportAny> RCURLY
      #
      my $identifier = $valueAbsDcl->[2];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $valueDcl = $self->_get_context(-2)) eq 'valueDcl') {
      #
      # <valueDcl>                   ::= <valueHeader> LCURLY <valueElementAny> RCURLY
      # <valueHeader>                ::= <customMaybe> VALUETYPE <identifier> <valueInheritanceSpecMaybe>
      #
      my $valueHeader = $valueDcl->[0];
      my $identifier = $valueHeader->[2];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $structType = $self->_get_context(-2)) eq 'structType') {
      #
      # <structType>                 ::= STRUCT <identifier> LCURLY <memberList> RCURLY
      #
      my $identifier = $structType->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $exceptDcl = $self->_get_context(-2)) eq 'exceptDcl') {
      #
      # <exceptDcl>                  ::= EXCEPTION <identifier> LCURLY <memberAny> RCURLY
      #
      my $identifier = $exceptDcl->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $eventAbsDcl = $self->_get_context(-2)) eq 'eventAbsDcl') {
      #
      # <eventAbsDcl>                ::= ABSTRACT EVENTTYPE <identifier> <valueInheritanceSpecMaybe> LCURLY <exportAny> RCURLY
      #
      my $identifier = $eventAbsDcl->[2];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $eventDcl = $self->_get_context(-2)) eq 'eventDcl') {
      #
      # <eventDcl>                   ::= <eventHeader> LCURLY <valueElementAny> RCURLY
      # <eventHeader>                ::= <customMaybe> EVENTTYPE <identifier> <valueInheritanceSpecMaybe>
      #
      my $eventHeader = $eventDcl->[0];
      my $identifier = $eventHeader->[2];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 2 && $self->_blessed(my $componentDcl = $self->_get_context(-2)) eq 'componentDcl') {
      #
      # <componentDcl>               ::= <componentHeader> LCURLY <componentBody> RCURLY
      # <componentHeader>            ::= COMPONENT <identifier> <componentInheritanceSpecMaybe> <supportedInterfaceSpecMaybe>
      #
      my $componentHeader = $componentDcl->[0];
      my $identifier = $componentHeader->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    elsif ($self->_count_context >= 3 && $self->_blessed(my $homeDcl = $self->_get_context(-3)) eq 'homeDcl') {
      #
      # <homeBody>                   ::= LCURLY <homeExportAny> RCURLY
      # <homeDcl>                    ::= <homeHeader> <homeBody>
      # <homeHeader>                 ::= HOME <identifier> <homeInheritanceSpecMaybe> <supportedInterfaceSpecMaybe> MANAGES <scopedName> <primaryKeySpecMaybe>
      #
      my $homeHeader = $homeDcl->[0];
      my $identifier = $homeHeader->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
  }
  elsif ($blessed eq 'LPAREN') {
    if ($self->_count_context >= 2 && $self->_blessed(my $unionType = $self->_get_context(-2)) eq 'unionType') {
      #
      # <unionType>                  ::= UNION <identifier> SWITCH LPAREN <switchTypeSpec> RPAREN LCURLY <switchBody> RCURLY
      #
      my $identifier = $unionType->[1];
      my $IDENTIFIER = $identifier->[0];
      $pushed = $self->_push_scope('::', $self->_token($IDENTIFIER))
    }
    #
    # Unnamed scopes
    #
    elsif (($self->_count_context >= 2) &&
           do {
             my $parentContext = $self->_blessed($self->_get_context(-2));
             grep {$_ eq $parentContext} qw/initDcl primaryExpr parameterDcls factoryDcl finderDcl/
           }
          ) {
      #
      # <initDcl>                    ::= FACTORY <identifier> LPAREN <initParamDeclsMaybe> RPAREN <raisesExprMaybe> SEMICOLON
      # <primaryExpr>                ::= LPAREN <constExp> RPAREN
      # <parameterDcls>              ::= LPAREN <paramDclListMany> RPAREN
      #                              |   LPAREN RPAREN
      # <factoryDcl>                 ::= FACTORY <identifier> LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
      # <finderDcl>                  ::= FINDER <identifier>  LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
      #
      my $unnamedScopeCounter = $self->_unnamedScopeCounter;
      # Make sure unnamed scoped is an invalid identifier
      my $unnamedScope = sprintf('[unnamedScope%d]', $unnamedScopeCounter);
      $self->_unnamedScopeCounter(++$unnamedScopeCounter);
      $pushed = $self->_push_scope('::', $unnamedScope)
    }
  }

  $self->_logger->debugf('%s: %s', $blessed, $self->globalScope) if ($pushed);
  return
}

sub dsclose   {
  my ($self, $item) = @_;

  my $blessed = $self->_blessed($item);
  my $parentContext;
  my $spliced;

  if ($blessed eq 'RCURLY'                                        &&
      $self->_count_context >= 2                                  &&
      ($parentContext = $self->_blessed($self->_get_context(-2))) &&
      grep { $parentContext eq $_} qw/module
                                      interfaceDcl
                                      valueAbsDcl
                                      valueDcl
                                      structType
                                      exceptDcl
                                      eventAbsDcl
                                      eventDcl
                                      componentDcl
                                      homeBody
                                      unionType/
     ) {
    #
    # <module>                     ::= MODULE <identifier> LCURLY <definitionMany> RCURLY
    # <interfaceDcl>               ::= <interfaceHeader> LCURLY <interfaceBody> RCURLY
    # <valueAbsDcl>              ::= ABSTRACT VALUETYPE <identifier> <valueInheritanceSpecMaybe> LCURLY <exportAny> RCURLY
    # <valueDcl>                   ::= <valueHeader> LCURLY <valueElementAny> RCURLY
    # <structType>                 ::= STRUCT <identifier> LCURLY <memberList> RCURLY
    # <exceptDcl>                  ::= EXCEPTION <identifier> LCURLY <memberAny> RCURLY
    # <eventAbsDcl>                ::= ABSTRACT EVENTTYPE <identifier> <valueInheritanceSpecMaybe> LCURLY <exportAny> RCURLY
    # <eventDcl>                   ::= <eventHeader> LCURLY <valueElementAny> RCURLY
    # <componentDcl>               ::= <componentHeader> LCURLY <componentBody> RCURLY
    # <homeBody>                   ::= LCURLY <homeExportAny> RCURLY
    # <unionType>                  ::= UNION <identifier> SWITCH LPAREN <switchTypeSpec> RPAREN LCURLY <switchBody> RCURLY
    #
    $spliced = ($parentContext eq 'module') ? $self->_splice_root(-2, 2) : $self->_splice_scope(-2, 2)
  }
  elsif ($blessed eq 'RPAREN') {
    #
    # Unnamed scopes
    #
    if (($self->_count_context >= 2)                                &&
        ($parentContext = $self->_blessed($self->_get_context(-2))) &&
        grep {$_ eq $parentContext} qw/initDcl primaryExpr parameterDcls factoryDcl finderDcl/
       ) {
      #
      # <initDcl>                    ::= FACTORY <identifier> LPAREN <initParamDeclsMaybe> RPAREN <raisesExprMaybe> SEMICOLON
      # <primaryExpr>                ::= LPAREN <constExp> RPAREN
      # <parameterDcls>              ::= LPAREN <paramDclListMany> RPAREN
      #                              |   LPAREN RPAREN
      # <factoryDcl>                 ::= FACTORY <identifier> LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
      # <finderDcl>                  ::= FINDER <identifier>  LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
      #
      my $unnamedScopeCounter = $self->_unnamedScopeCounter;
      $self->_unnamedScopeCounter(--$unnamedScopeCounter);
      $spliced = $self->_splice_scope(-2, 2)
    }
  }
  $self->_pop_context;
  $self->_logger->debugf('%s: %s', $blessed, $self->globalScope) if ($spliced);
  return
}
sub process { return }
sub output  { return }
#
# We just provide a default dsread that is capable to distinguish lexemes and
# to unfold when necessary
#
sub dsread {
  my ($self, $item) = @_;
  #
  # Item, when blessed, is always an array reference in our case
  #
  my $blessed = $self->_blessed($item);
  my $rc = undef;
  if ($blessed) {
    $rc = $item;
    #
    # Are we subclassed to support this ?
    #
    my $method = $self->can($blessed);
    $self->$method($item) if ($method);
  }

  return $rc;
}

sub _blessed {
  my ($self, $item) = @_;

  my $blessed = blessed($item) // '';
  $blessed =~ s/.*:://;

  return $blessed
}

sub _token {
  my ($self, $item) = @_;

  return $item->[2]
}

with 'Data::Scan::Role::Consumer';
with 'MooX::Role::Logger';

1;
__DATA__
requires 'specification';
requires 'supportedCppCommandAny';
requires 'supportedCppCommand';
requires 'definition';
requires 'module';
requires 'interface';
requires 'interfaceDcl';
requires 'forwardDcl';
requires 'interfaceHeader';
requires 'interfaceBody';
requires 'export';
requires 'interfaceInheritanceSpec';
requires 'interfaceName';
requires 'scopedName';
requires 'value';
requires 'valueForwardDcl';
requires 'valueBoxDcl';
requires 'valueAbsDcl';
requires 'valueDcl';
requires 'valueHeader';
requires 'valueInheritanceSpec';
requires 'valueName';
requires 'valueElement';
requires 'stateMember';
requires 'initDcl';
requires 'initParamDecls';
requires 'initParamDecl';
requires 'initParamAttribute';
requires 'constDcl';
requires 'constType';
requires 'constExp';
requires 'orExpr';
requires 'xorExpr';
requires 'andExpr';
requires 'shiftExpr';
requires 'addExpr';
requires 'multExpr';
requires 'unaryExpr';
requires 'unaryOperator';
requires 'primaryExpr';
requires 'literal';
requires 'booleanLiteral';
requires 'positiveIntConst';
requires 'typeDcl';
requires 'typeDeclarator';
requires 'typeSpec';
requires 'simpleTypeSpec';
requires 'baseTypeSpec';
requires 'templateTypeSpec';
requires 'constrTypeSpec';
requires 'declarators';
requires 'declarator';
requires 'simpleDeclarator';
requires 'complexDeclarator';
requires 'floatingPtType';
requires 'integerType';
requires 'signedInt';
requires 'signedShortInt';
requires 'signedLongInt';
requires 'signedLonglongInt';
requires 'unsignedInt';
requires 'unsignedShortInt';
requires 'unsignedLongInt';
requires 'unsignedLonglongInt';
requires 'charType';
requires 'wideCharType';
requires 'booleanType';
requires 'octetType';
requires 'anyType';
requires 'objectType';
requires 'structType';
requires 'memberList';
requires 'member';
requires 'unionType';
requires 'switchTypeSpec';
requires 'switchBody';
requires 'case';
requires 'caseLabel';
requires 'elementSpec';
requires 'enumType';
requires 'enumerator';
requires 'sequenceType';
requires 'stringType';
requires 'wideStringType';
requires 'arrayDeclarator';
requires 'fixedArraySize';
requires 'attrDcl';
requires 'exceptDcl';
requires 'opDcl';
requires 'opAttribute';
requires 'opTypeSpec';
requires 'parameterDcls';
requires 'paramDcl';
requires 'paramAttribute';
requires 'raisesExpr';
requires 'contextExpr';
requires 'paramTypeSpec';
requires 'fixedPtType';
requires 'fixedPtConstType';
requires 'valueBaseType';
requires 'constrForwardDecl';
requires 'import';
requires 'importedScope';
requires 'typeIdDcl';
requires 'typePrefixDcl';
requires 'readonlyAttrSpec';
requires 'readonlyAttrDeclarator';
requires 'attrSpec';
requires 'attrDeclarator';
requires 'attrRaisesExpr';
requires 'getExcepExpr';
requires 'setExcepExpr';
requires 'exceptionList';
requires 'component';
requires 'componentForwardDcl';
requires 'componentDcl';
requires 'componentHeader';
requires 'supportedInterfaceSpec';
requires 'componentInheritanceSpec';
requires 'componentBody';
requires 'componentExport';
requires 'providesDcl';
requires 'interfaceType';
requires 'usesDcl';
requires 'emitsDcl';
requires 'publishesDcl';
requires 'consumesDcl';
requires 'homeDcl';
requires 'homeHeader';
requires 'homeIinheritanceSpec';
requires 'primaryKeySpec';
requires 'homeBody';
requires 'homeExport';
requires 'factoryDcl';
requires 'finderDcl';
requires 'event';
requires 'eventForwardDcl';
requires 'eventAbsDcl';
requires 'eventDcl';
requires 'eventHeader';
requires 'importAny';
requires 'definitionMany';
requires 'abstractOrLocal';
requires 'abstractOrLocalMaybe';
requires 'abstractOrLocalMaybe';
requires 'interfaceInheritanceSpecMaybe';
requires 'interfaceInheritanceSpecMaybe';
requires 'interfaceNameListMany';
requires 'abstractMaybe';
requires 'abstractMaybe';
requires 'valueInheritanceSpecMaybe';
requires 'valueInheritanceSpecMaybe';
requires 'exportAny';
requires 'valueElementAny';
requires 'customMaybe';
requires 'customMaybe';
requires 'valueNameListMany';
requires 'truncatableMaybe';
requires 'truncatableMaybe';
requires 'valueInheritanceSpec1Values';
requires 'valueInheritanceSpec1ValuesMaybe';
requires 'valueInheritanceSpec1ValuesMaybe';
requires 'valueInheritanceSpec2Interfaces';
requires 'valueInheritanceSpec2InterfacesMaybe';
requires 'valueInheritanceSpec2InterfacesMaybe';
requires 'publicOrPrivate';
requires 'initParamDeclsMaybe';
requires 'initParamDeclsMaybe';
requires 'raisesExprMaybe';
requires 'raisesExprMaybe';
requires 'initParamDeclListMany';
requires 'declaratorListMany';
requires 'caseLabelMany';
requires 'enumeratorListMany';
requires 'fixedArraySizeMany';
requires 'memberAny';
requires 'opAttributeMaybe';
requires 'opAttributeMaybe';
requires 'contextExprMaybe';
requires 'contextExprMaybe';
requires 'paramDclListMany';
requires 'scopedNameListMany';
requires 'stringLiteralListMany';
requires 'simpleDeclaratorListMany';
requires 'setExcepExprMaybe';
requires 'setExcepExprMaybe';
requires 'componentInheritanceSpecMaybe';
requires 'componentInheritanceSpecMaybe';
requires 'supportedInterfaceSpecMaybe';
requires 'supportedInterfaceSpecMaybe';
requires 'multipleMaybe';
requires 'multipleMaybe';
requires 'homeInheritanceSpecMaybe';
requires 'homeInheritanceSpecMaybe';
requires 'primaryKeySpecMaybe';
requires 'primaryKeySpecMaybe';
requires 'homeExportAny';
requires 'comma';
requires 'coloncolon';
requires 'stringLiteral';
requires 'wideStringLiteral';
requires 'integerLiteral';
requires 'identifier';
requires 'characterLiteral';
requires 'wideCharacterLiteral';
requires 'fixedPtLiteral';
requires 'floatingPtLiteral';

with 'Data::Scan::Role::Consumer';

1;