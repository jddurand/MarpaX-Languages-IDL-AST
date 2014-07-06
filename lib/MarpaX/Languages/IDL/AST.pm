use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST;
use MarpaX::Languages::IDL::AST::Value;
use MarpaX::Languages::IDL::AST::Util;
use Scalar::Util qw/blessed/;
use Data::Dumper;
use Template;
use Template::Constants qw/:chomp :debug/;
use File::ShareDir ':ALL';
use constant {
  LEXEME_INDEX => 0
};

use constant {
  LEXEME_INDEX_START => 0,
  LEXEME_INDEX_LENGTH => 1,
  LEXEME_INDEX_VALUE => 2
};

# ABSTRACT: Translate an IDL source to an AST

# VERSION

=head1 DESCRIPTION

This module provide and manage an AST of an IDL file, as per OMG's IDL 3.5 grammar specification.

=head1 SYNOPSIS

    use MarpaX::Languages::IDL::AST;

    my $idlPath = 'source.idl';
    my $ast = MarpaX::Languages::IDL::AST->new()->parse($idlPath)->validate();

=cut

use Carp qw/carp croak/;
use Marpa::R2 qw//;
use File::Basename qw/basename fileparse/;
use File::Spec::Functions qw/case_tolerant/;
use File::Slurp qw/read_file/;

our $BLESS_PACKAGE = 'IDL::AST';
our $DATA = do {local $/; <DATA>};
our $G = Marpa::R2::Scanless::G->new({source => \$DATA, bless_package => $BLESS_PACKAGE});
# Marpa follows Unicode recommendation, i.e. perl's \R, that cannot be in a character class
our $NEWLINE_REGEXP = qr/(?>\x0D\x0A|\v)/;

=head2 $class->new()

Instantiate a new object. Returns a reference to it, denoted $self hereafter.

=cut

sub new {
  my ($class) = @_;

  my $self = {
  };

  bless($self, $class);

  return $self;
}

=head2 $self->parse($path, $hashOptPtr)

Parse the IDL and produce an AST out of it, then a meta-AST that is more useful representation for further processing. Takes as parameters:

=over

=item $path

A required IDL pathname.

=item $hashOptPtr

An optional reference to a hash containing Marpa::R2::Scanless::R() parameters, except the grammar and semantics_package options.

=back

The AST is an exact representation of the parse tree value of the IDL grammar contained in this package, except:

=over

=item scopedName

Original grammar rule is:

 <scopedName> ::= <identifier> | '::' <identifier> | <scopedName> '::' <identifier>

and has been rewriten to:

 <scopedName> ::= <identifier>+ separator => <coloncolon>

A dedicated action rule will concatenate all identifiers into a single string, giving the impression that scopedName is a token (accurate terminology is a lexeme). I.e. the scopedName value in the AST is in the form:

bless([start,totalLength,concatenatedValue], 'IDL::AST::scopedName')

alike the identifier.

=back

This method returns $self.

=cut

sub parse {
    my ($self, $path, $hashOptPtr) = @_;
    #
    # Parameters check
    #
    $hashOptPtr //= {};
    if (ref($hashOptPtr) ne 'HASH') {
	croak '3rd argument must be a pointer to HASH containing any Marpa::R2::Scanles::R option, except the grammar option';
    }
    foreach (qw/grammar semantics_package/) {
      if (exists($hashOptPtr->{$_})) {
        delete($hashOptPtr->{$_});
      }
    }
    #
    # IDL wants the filename to end with .idl
    #
    my ($filename, $directories, $suffix) = fileparse($path, qr/\.[^.]*/);
    if ((  case_tolerant() && (lc($suffix) ne '.idl')) ||
        (! case_tolerant() && (   $suffix  ne '.idl'))) {
      carp "$path does not end with .idl";
    }
    #
    # Load data
    #
    my $data = read_file($path);
    #
    # Regonizer
    #
    my $recce = Marpa::R2::Scanless::R->new({grammar => $G,
                                             # trace_terminals => 1,
                                             semantics_package => 'MarpaX::Languages::IDL::AST::Value',
                                             %{$hashOptPtr}});
    $recce->read(\$data);
    #
    # AST value
    #
    my $value = $recce->value();
    croak 'Undefined AST value' if (! defined($value) || ! defined(${$value}));
    #
    # We want a single value
    #
    my $nextValue = $recce->value();
    croak 'Ambiguous AST' if (defined($nextValue));
    #
    # Let's remember the latest AST
    #
    $self->{_ast} = ${$value};

    return $self->generate();
}

=head2 $self->ast()

Returns the latest AST produced by $self->parse().

=cut

sub ast {
    my ($self) = @_;

    return $self->{_ast};
}

=head2 $self->output()

Returns the latest output produced by $self->generate().

=cut

sub output {
    my ($self) = @_;

    return $self->{_output};
}

=head2 $self->generate($ast, $target, $targetOptionHashp)

Generate files for the given AST $ast.

=over

=item $ast

AST as produced by the method $self->parse(). Default to $self->ast().

=item $target

Target language. Default to 'perl'.
Supported values are those distributed with this package, c.f. the target directory.

=item $targetOptionHashp

Hash reference of options specific to target $target.

=back

This method returns $self.

=cut

sub generate {
    my ($self, $ast, $target, $targetOptionHashp) = @_;

    $ast               //= $self->ast();
    $target            //= 'perl';
    $targetOptionHashp //= {};

    if (ref($targetOptionHashp) ne 'HASH') {
	croak '3rd argument must be a pointer to HASH';
    }

    my $ttOptionHashp = $targetOptionHashp->{tt};
    $ttOptionHashp->{INCLUDE_PATH} //= module_dir(__PACKAGE__);
    $ttOptionHashp->{INTERPOLATE} //= 1;
    $ttOptionHashp->{EVAL_PERL} //= 1;
    $ttOptionHashp->{PRE_CHOMP} //= CHOMP_NONE;
    $ttOptionHashp->{POST_CHOMP} //= CHOMP_NONE;
    my $tt = Template->new($ttOptionHashp) || croak "$Template::ERROR";

    #
    # The semantics for our TT templates is to provide a hash with
    # a reference to a scratchpad hash (free to use) and the AST
    #
    my $ttVarsHashp = $targetOptionHashp->{vars};
    #
    # Data::Dumper explicit support
    #
    $ttVarsHashp->{Dumper} //= sub { print STDERR Dumper(shift); };
    #
    # Scalar::Utils explicit support
    #
    $ttVarsHashp->{blessed} //= sub {return blessed(shift) || ''; };
    $ttVarsHashp->{reftype} //= sub {return reftype(shift) || ''; };
    #
    # TT2 does not like blessed arrays
    #
    $ttVarsHashp->{as_list} //= sub {
	my $arrayp = shift;
	return [ @{$arrayp} ];
    };
    #
    # General hooks
    #
    $ttVarsHashp->{cr}     //= sub {return "\r" x (shift // 0); };
    $ttVarsHashp->{nl}     //= sub {return "\n" x (shift // 0); };
    $ttVarsHashp->{tab}    //= sub {return "\t" x (shift // 0); };
    $ttVarsHashp->{sp}     //= sub {return ' ' x (shift // 0); };
    $ttVarsHashp->{lexeme} //= sub {
	my $item = shift || [];
	my $ref = ref($item);
	if ($ref eq 'ARRAY') {
	    return $item->[2];
	} elsif (! $ref) {
	    return $item;
	} else {
	    return '';
	}
    };
    $ttVarsHashp->{ast} //= $ast;

    my $output = '';
    $tt->process("$target.tt2", $ttVarsHashp, \$self->{_output}) || croak $tt->error();

    return $self;
}


=head1 NOTES

IDL version is 3.5 as of L<OMG IDL3.5 Specification|http://www.omg.org/spec/IDL35/3.5/>.

This specification imposes input to come from a filename, with suffix '.idl'.

Any preprocessing feature is ignored, and eventual multi-line proprocessing directives are likely to cause failure. Since the most expected preprocessing tokens are #include, #ifdef, and al., the user is expected to have already run a preprocessor before using this package.

=head1 SEE ALSO

L<Marpa::R2>

=cut

1;

__DATA__
:default ::= action => [values] bless => ::lhs
lexeme default = action => [ start, length, value ] latm => 1# bless => ::name

:start ::= <specification>

<specification>              ::= <importAny> <definitionMany>
<definition>                 ::= <typeDcl> ';'
                             |   <constDcl> ';'
                             |   <exceptDcl> ';'
                             |   <interface> ';'
                             |   <module> ';'
                             |   <value> ';'
                             |   <typeIdDcl> ';'
                             |   <typePrefixDcl> ';'
                             |   <event> ';'
                             |   <component> ';'
                             |   <homeDcl> ';'
<module>                     ::= 'module' <identifier> '{' <definitionMany> '}'
<interface>                  ::= <interfaceDcl>
                             |   <forwardDcl>
<interfaceDcl>              ::= <interfaceHeader> '{' <interfaceBody> '}'
<forwardDcl>                ::= <abstractOrLocalMaybe> 'interface' <identifier>
<interfaceHeader>           ::= <abstractOrLocalMaybe> 'interface' <identifier> <interfaceInheritanceSpecMaybe>
<interfaceBody>             ::= <export>*
<export>                     ::= <typeDcl> ';'
                             |   <constDcl> ';'
                             |   <exceptDcl> ';'
                             |   <attrDcl> ';'
                             |   <opDcl> ';'
                             |   <typeIdDcl> ';'
                             |   <typePrefixDcl> ';'
<interfaceInheritanceSpec> ::= ':' <interfaceNameListMany>
<interfaceName>             ::= <scopedName>
#<scopedName>                ::= <identifier>
#                             |   '::' <identifier>
#                             |   <scopedName> '::' <identifier>
<scopedName>                ::= <identifier>+ separator => <coloncolon>               action => _scopedName
<value>                      ::= <valueDcl>
                             |   <valueAbsDcl>
                             |   <valueBoxDcl>
                             |   <valueForwardDcl>
<valueForwardDcl>          ::= <abstractMaybe> 'valuetype' <identifier>
<valueBoxDcl>              ::= 'valuetype' <identifier> <typeSpec>
<valueAbsDcl>              ::= 'abstract' 'valuetype' <identifier> <valueInheritanceSpecMaybe> '{' <exportAny> '}'
<valueDcl>                  ::= <valueHeader> '{' <valueElementAny> '}'
<valueHeader>               ::= <customMaybe> 'valuetype' <identifier> <valueInheritanceSpecMaybe>
<valueInheritanceSpec>     ::= <valueInheritanceSpec1ValuesMaybe> <valueInheritanceSpec2InterfacesMaybe>
<valueName>                 ::= <scopedName>
<valueElement>              ::= <export>
                             |   <stateMember>
                             |   <initDcl>
<stateMember>               ::= <publicOrPrivate> <typeSpec> <declarators> ';'
<initDcl>                   ::= 'factory' <identifier> '(' <initParamDeclsMaybe> ')' <raisesExprMaybe> ';'
<initParamDecls>           ::= <initParamDeclListMany>
<initParamDecl>            ::= <initParamAttribute> <paramTypeSpec> <simpleDeclarator>
<initParamAttribute>       ::= 'in'
<constDcl>                  ::= 'const' <constType> <identifier> '=' <constExp>
<constType>                 ::= <integerType>
                             |   <charType>
                             |   <wideCharType>
                             |   <booleanType>
                             |   <floatingPtType>
                             |   <stringType>
                             |   <wideStringType>
                             |   <fixedPtConstType>
                             |   <scopedName>
                             |   <octetType>
<constExp>                  ::= <orExpr>
<orExpr>                    ::= <xorExpr>
                             |   <orExpr> '|' <xorExpr>
<xorExpr>                   ::= <andExpr>
                             |   <xorExpr> '^' <andExpr>
<andExpr>                   ::= <shiftExpr>
                             |   <andExpr> '&' <shiftExpr>
<shiftExpr>                 ::= <addExpr>
                             |   <shiftExpr> '>>' <addExpr>
                             |   <shiftExpr> '<<' <addExpr>
<addExpr>                   ::= <multExpr>
                             |   <addExpr> '+' <multExpr>
                             |   <addExpr> '-' <multExpr>
<multExpr>                  ::= <unaryExpr>
                             |   <multExpr> '*' <unaryExpr>
                             |   <multExpr> '/' <unaryExpr>
                             |   <multExpr> '%' <unaryExpr>
<unaryExpr>                 ::= <unaryOperator> <primaryExpr>
                             | <primaryExpr>
<unaryOperator>             ::= '-'
                             | '+'
                             | '~'
<primaryExpr>               ::= <scopedName>
                             |   <literal>
                             |   '(' <constExp> ')'
<literal>                    ::= <integerLiteral>
                             |   <stringLiteral>
                             |   <wideStringLiteral>
                             |   <characterLiteral>
                             |   <wideCharacterLiteral>
                             |   <fixedPtLiteral>
                             |   <floatingPtLiteral>
                             |   <booleanLiteral>
<booleanLiteral>            ::= 'TRUE'
                             |   'FALSE'
<positiveIntConst>         ::= <constExp>
<typeDcl>                   ::= 'typedef' <typeDeclarator>
                             |   <structType>
                             |   <unionType>
                             |   <enumType>
                             |   'native' <simpleDeclarator>
                             |   <constrForwardDecl>
<typeDeclarator>            ::= <typeSpec> <declarators>
<typeSpec>                  ::= <simpleTypeSpec>
                             |   <constrTypeSpec>
<simpleTypeSpec>           ::= <baseTypeSpec>
                             |   <templateTypeSpec>
                             |   <scopedName>
<baseTypeSpec>             ::= <floatingPtType>
                             |   <integerType>
                             |   <charType>
                             |   <wideCharType>
                             |   <booleanType>
                             |   <octetType>
                             |   <anyType>
                             |   <objectType>
                             |   <valueBaseType>
<templateTypeSpec>         ::= <sequenceType>
                             |   <stringType>
                             |   <wideStringType>
                             |   <fixedPtType>
<constrTypeSpec>           ::= <structType>
                             |   <unionType>
                             |   <enumType>
<declarators>                ::= <declaratorListMany>
<declarator>                 ::= <simpleDeclarator>
                             |   <complexDeclarator>
<simpleDeclarator>          ::= <identifier>
<complexDeclarator>         ::= <arrayDeclarator>
<floatingPtType>           ::= 'float'
                             |   'double'
                             |   'long' 'double'
<integerType>               ::= <signedInt>
                             |   <unsignedInt>
<signedInt>                 ::= <signedShortInt>
                             |   <signedLongInt>
                             |   <signedLonglongInt>
<signedShortInt>           ::= 'short'
<signedLongInt>            ::= 'long'
<signedLonglongInt>        ::= 'long' 'long'
<unsignedInt>               ::= <unsignedShortInt>
                             |   <unsignedLongInt>
                             |   <unsignedLonglongInt>
<unsignedShortInt>         ::= 'unsigned' 'short'
<unsignedLongInt>          ::= 'unsigned' 'long'
<unsignedLonglongInt>      ::= 'unsigned' 'long' 'long'
<charType>                  ::= 'char'
<wideCharType>             ::= 'wchar'
<booleanType>               ::= 'boolean'
<octetType>                 ::= 'octet'
<anyType>                   ::= 'any'
<objectType>                ::= 'Object'
<structType>                ::= 'struct' <identifier> '{' <memberList> '}'
<memberList>                ::= <member>+
<member>                     ::= <typeSpec> <declarators> ';'
<unionType>                 ::= 'union' <identifier> 'switch' '(' <switchTypeSpec> ')' '{' <switchBody> '}'
<switchTypeSpec>           ::=<integerType>
                             | <charType>
                             | <booleanType>
                             | <enumType>
                             | <scopedName>
<switchBody>                ::= <case>+
<case>                       ::= <caseLabelAny> <elementSpec> ';'
<caseLabel>                 ::= 'case' <constExp> ':'
                             | 'default' ':'
<elementSpec>               ::= <typeSpec> <declarator>
<enumType>                  ::= 'enum' <identifier> '{' <enumeratorListMany> '}'
<enumerator>                 ::= <identifier>
<sequenceType>              ::= 'sequence' '<' <simpleTypeSpec> ',' <positiveIntConst> '>'
                             | 'sequence' '<' <simpleTypeSpec> '>'
<stringType>                ::= 'string' '<' <positiveIntConst> '>'
                             | 'string'
<wideStringType>           ::= 'wstring' '<' <positiveIntConst> '>'
                             | 'wstring'
<arrayDeclarator>           ::= <identifier> <fixedArraySizeMany>
<fixedArraySize>           ::= '[' <positiveIntConst> ']'
<attrDcl>                   ::= <readonlyAttrSpec>
                             |   <attrSpec>
<exceptDcl>                 ::= 'exception' <identifier> '{' <memberAny> '}'
<opDcl>                     ::= <opAttributeMaybe> <opTypeSpec> <identifier> <parameterDcls> <raisesExprMaybe> <contextExprMaybe>
<opAttribute>               ::= 'oneway'
<opTypeSpec>               ::= <paramTypeSpec>
                             | 'void'
<parameterDcls>             ::= '(' <paramDclListMany> ')'
                             |   '(' ')'
<paramDcl>                  ::= <paramAttribute> <paramTypeSpec> <simpleDeclarator>
<paramAttribute>            ::='in'
                             |   'out'
                             |   'inout'
<raisesExpr>                ::= 'raises' '(' <scopedNameListMany> ')'
<contextExpr>               ::= 'context' '(' <stringLiteralListMany> ')'
<paramTypeSpec>            ::= <baseTypeSpec>
                             | <stringType>
                             | <wideStringType>
                             | <scopedName>
<fixedPtType>              ::= 'fixed' '<' <positiveIntConst> ',' <positiveIntConst> '>'
<fixedPtConstType>        ::= 'fixed'
<valueBaseType>            ::= 'ValueBase'
<constrForwardDecl>        ::= 'struct' <identifier>
                             |   'union' <identifier>
<import>                     ::= 'import' <importedScope> ';'
<importedScope>             ::= <scopedName>
                             |   <stringLiteral>
<typeIdDcl>                ::= 'typeid' <scopedName> <stringLiteral>
<typePrefixDcl>            ::= 'typeprefix' <scopedName> <stringLiteral>
<readonlyAttrSpec>         ::= 'readonly' 'attribute' <paramTypeSpec> <readonlyAttrDeclarator>
<readonlyAttrDeclarator>   ::= <simpleDeclarator> <raisesExpr>
                             |   <simpleDeclaratorListMany>
<attrSpec>                  ::= 'attribute' <paramTypeSpec> <attrDeclarator>
<attrDeclarator>            ::= <simpleDeclarator> <attrRaisesExpr>
                             |   <simpleDeclaratorListMany>
<attrRaisesExpr>           ::= <getExcepExpr> <setExcepExprMaybe>
                             |   <setExcepExpr>
<getExcepExpr>             ::= 'getraises' <exceptionList>
<setExcepExpr>             ::= 'setraises' <exceptionList>
<exceptionList>             ::= '(' <scopedNameListMany> ')'

# NOTE: Grammar rules 1 through 111 with the exception of the last three lines of rule 2 constitutes the portion of IDL that
# is not related to components.

<component>                  ::= <componentDcl>
                             |   <componentForwardDcl>
<componentForwardDcl>      ::= 'component' <identifier>
<componentDcl>              ::= <componentHeader> '{' <componentBody> '}'
<componentHeader>           ::= 'component' <identifier> <componentInheritanceSpecMaybe> <supportedInterfaceSpecMaybe>
<supportedInterfaceSpec>   ::= 'supports' <scopedNameListMany>
<componentInheritanceSpec> ::= ':' <scopedName>
<componentBody>             ::= <componentExport>*
<componentExport>           ::= <providesDcl> ';'
                             |   <usesDcl> ';'
                             |   <emitsDcl> ';'
                             |   <publishesDcl> ';'
                             |   <consumesDcl> ';'
                             |   <attrDcl> ';'
<providesDcl>               ::= 'provides' <interfaceType> <identifier>
<interfaceType>             ::= <scopedName>
                             |   'Object'
<usesDcl>                   ::= 'uses' <multipleMaybe> <interfaceType> <identifier>
<emitsDcl>                  ::= 'emits' <scopedName> <identifier>
<publishesDcl>              ::= 'publishes' <scopedName> <identifier>
<consumesDcl>               ::= 'consumes' <scopedName> <identifier>
<homeDcl>                   ::= <homeHeader> <homeBody>
<homeHeader>                ::= 'home' <identifier> <homeInheritanceSpecMaybe> <supportedInterfaceSpecMaybe> 'manages' <scopedName> <primaryKeySpecMaybe>
<homeIinheritanceSpec>      ::= ':' <scopedName>
<primaryKeySpec>           ::= 'primarykey' <scopedName>
<homeBody>                  ::= '{' <homeExportAny> '}'
<homeExport>                ::= <export>
                             |   <factoryDcl> ';'
                             |   <finderDcl> ';'
<factoryDcl>                ::= 'factory' <identifier> '(' [ <initParamDecls> ] ')'  <raisesExprMaybe>
<finderDcl>                 ::= 'finder' <identifier>  '(' [ <initParamDecls> ] ')'  <raisesExprMaybe>
<event>                      ::= <eventDcl>
                             |   <eventAbsDcl>
                             |   <eventForwardDcl>
<eventForwardDcl>          ::= <abstractMaybe> 'eventtype' <identifier>
<eventAbsDcl>              ::= 'abstract' 'eventtype' <identifier> <valueInheritanceSpecMaybe> '{' <exportAny> '}'
<eventDcl>                  ::= <eventHeader> '{' <valueElementAny> '}'
<eventHeader>               ::= <customMaybe> 'eventtype' <identifier> <valueInheritanceSpecMaybe>

<importAny> ::= <import>*
<definitionMany> ::= <definition>+
<abstractOrLocal> ::= 'abstract' | 'local'
<abstractOrLocalMaybe> ::= <abstractOrLocal>
<abstractOrLocalMaybe> ::=
<interfaceInheritanceSpecMaybe> ::= <interfaceInheritanceSpec>
<interfaceInheritanceSpecMaybe> ::=
<interfaceNameListMany> ::= <interfaceName>+ separator => <comma>
<abstractMaybe> ::= 'abstract'
<abstractMaybe> ::=
<valueInheritanceSpecMaybe> ::= <valueInheritanceSpec>
<valueInheritanceSpecMaybe> ::=
<exportAny> ::= <export>*
<valueElementAny> ::= <valueElement>*
<customMaybe> ::= 'custom'
<customMaybe> ::=
<valueNameListMany> ::= <valueName>+ separator => <comma>
<valueInheritanceSpec1Values> ::= ':' [ 'truncatable' ] <valueNameListMany>
<valueInheritanceSpec1ValuesMaybe> ::= <valueInheritanceSpec1Values>
<valueInheritanceSpec1ValuesMaybe> ::=
<valueInheritanceSpec2Interfaces>   ::= 'supports' <interfaceNameListMany>
<valueInheritanceSpec2InterfacesMaybe> ::= <valueInheritanceSpec2Interfaces>
<valueInheritanceSpec2InterfacesMaybe> ::=
<publicOrPrivate> ::= 'public' | 'private'
<initParamDeclsMaybe> ::= <initParamDecls>
<initParamDeclsMaybe> ::=
<raisesExprMaybe> ::= <raisesExpr>
<raisesExprMaybe> ::=
<initParamDeclListMany> ::= <initParamDecl>+ separator => <comma>
<declaratorListMany> ::= <declarator>+ separator => <comma>
<caseLabelAny> ::= <caseLabel>+
<enumeratorListMany> ::= <enumerator>+ separator => <comma>
<fixedArraySizeMany> ::= <fixedArraySize>+
<memberAny> ::= <member>*
<opAttributeMaybe> ::= <opAttribute>
<opAttributeMaybe> ::=
<contextExprMaybe> ::= <contextExpr>
<contextExprMaybe> ::=
<paramDclListMany> ::= <paramDcl>+ separator => <comma>
<scopedNameListMany> ::= <scopedName>+ separator => <comma>
<stringLiteralListMany> ::= <stringLiteral>+ separator => <comma>
<simpleDeclaratorListMany> ::= <simpleDeclarator>+ separator => <comma>
<setExcepExprMaybe> ::= <setExcepExpr>
<setExcepExprMaybe> ::=
<componentInheritanceSpecMaybe> ::= <componentInheritanceSpec>
<componentInheritanceSpecMaybe> ::=
<supportedInterfaceSpecMaybe> ::= <supportedInterfaceSpec>
<supportedInterfaceSpecMaybe> ::=
<multipleMaybe> ::= 'multiple'
<multipleMaybe> ::=
<homeInheritanceSpecMaybe> ::= <homeIinheritanceSpec>
<homeInheritanceSpecMaybe> ::=
<primaryKeySpecMaybe> ::= <primaryKeySpec>
<primaryKeySpecMaybe> ::=
<homeExportAny> ::= <homeExport>*
<comma> ::= ','
<coloncolon> ::= '::'


#
# Copied from C language
#
<stringLiteral> ::= STRINGLITERALUNIT+
:lexeme ~ <STRINGLITERALUNIT>
STRING_LITERAL_INSIDE ~ [^"\\\n]
STRING_LITERAL_INSIDE ~ ES
STRING_LITERAL_INSIDE_any ~ STRING_LITERAL_INSIDE*
STRINGLITERALUNIT ~ SP_maybe '"' STRING_LITERAL_INSIDE_any '"' WS_any

<wideStringLiteral> ::= WIDESTRINGLITERALUNIT+
:lexeme ~ <WIDESTRINGLITERALUNIT>
WIDESTRINGLITERALUNIT ~ SP_maybe 'L"' STRING_LITERAL_INSIDE_any '"' WS_any

<integerLiteral> ::= ICONSTANT
:lexeme ~ <ICONSTANT>
ICONSTANT ~ HP H_many IS_maybe
          | BP B_many IS_maybe   # Gcc extension: binary constants
          | NZ D_any IS_maybe
          | '0' O_any IS_maybe
I_CONSTANT_INSIDE ~ [^'\\\n]
I_CONSTANT_INSIDE ~ ES
I_CONSTANT_INSIDE_many ~ I_CONSTANT_INSIDE+

<identifier> ::= IDENTIFIER
:lexeme ~ <IDENTIFIER> priority => -1

IDENTIFIER          ~ L A_any

#
# Original C includes this definition in ICONSTANT
#
<characterLiteral> ::= CHARACTERLITERAL
:lexeme ~ <CHARACTERLITERAL>
CHARACTERLITERAL ~ CP_maybe QUOTE I_CONSTANT_INSIDE_many QUOTE

<wideCharacterLiteral> ::= WIDECHARACTERLITERAL
:lexeme ~ <WIDECHARACTERLITERAL>
WIDECHARACTERLITERAL ~ 'L' QUOTE I_CONSTANT_INSIDE_many QUOTE

dD ~ [dD]
<fixedPtLiteral> ::= FIXEDPTLITERAL
:lexeme ~ <FIXEDPTLITERAL>
FIXEDPTLITERAL ~ D_many '.' D_many dD
                 |        '.' D_many dD
                 | D_many '.'        dD

<floatingPtLiteral> ::= FCONSTANT
:lexeme ~ <FCONSTANT>
FCONSTANT ~ D_many E FS_maybe
          | D_any '.' D_many E_maybe FS_maybe
          | D_many '.' E_maybe FS_maybe
          | HP H_many P FS_maybe
          | HP H_any '.' H_many P FS_maybe
          | HP H_many '.' P FS_maybe

#
# G0 helpers
#
O          ~ [0-7]
O_any      ~ O*
D          ~ [0-9]
D_any      ~ D*
D_many     ~ D+
NZ         ~ [1-9]
L          ~ [a-zA-Z_]
A          ~ [a-zA-Z_0-9]
A_any      ~ A*
H          ~ [a-fA-F0-9]
H_any      ~ H*
H_many     ~ H+
HP         ~ '0' [xX]
B          ~ [0-1]
B_many     ~ B+
BP         ~ '0' [bB]
SIGN_maybe ~ [+-]
SIGN_maybe ~
E          ~ [Ee] SIGN_maybe D_many
E_maybe    ~ E
E_maybe    ~
P          ~ [Pp] SIGN_maybe D_many
FS         ~ [fFlL]
FS_maybe   ~ FS
FS_maybe   ~
LL         ~ 'll' | 'LL' | [lL]
LL_maybe   ~ LL
LL_maybe   ~
U          ~ [uU]
U_maybe    ~ U
U_maybe    ~
IS         ~ U LL_maybe | LL U_maybe
IS_maybe   ~ IS
IS_maybe   ~
CP         ~ [uU]    # L extracted - c.f. WIDECHARACTERLITERAL
CP_maybe   ~ CP
CP_maybe   ~
SP         ~ 'u8' | [uUL]
SP_maybe   ~ SP
SP_maybe   ~
ES_AFTERBS ~ [\'\"\?\\abfnrtv]
           | O
           | O O
           | O O O
           | 'x' H_many
ES         ~ BS ES_AFTERBS
WS         ~ [ \t\v\n\f]
WS_any     ~ WS*
WS_many    ~ WS+
QUOTE     ~ [']
BS        ~ '\'

#
# discards of the C language
#
############################################################################
# Discard of a C comment, c.f. https://gist.github.com/jeffreykegler/5015057
############################################################################
<C style comment> ~ '/*' <comment interior> '*/'
<comment interior> ~
    <optional non stars>
    <optional star prefixed segments>
    <optional pre final stars>
<optional non stars> ~ [^*]*
<optional star prefixed segments> ~ <star prefixed segment>*
<star prefixed segment> ~ <stars> [^/*] <optional star free text>
<stars> ~ [*]+
<optional star free text> ~ [^*]*
<optional pre final stars> ~ [*]*
:discard ~ <C style comment>

##########################
# Discard of a C++ comment
##########################
<Cplusplus style comment> ~ '//' <Cplusplus comment interior>
<Cplusplus comment interior> ~ [^\n]*
:discard ~ <Cplusplus style comment>

###########################
# TAKE CARE: preprocessor commands are IGNORED in this version
# Discard of a Perl comment
###########################
<Perl style comment> ~ '#' <Perl comment interior>
<Perl comment interior> ~ [^\n]*
:discard ~ <Perl style comment>

####################
# WhiteSpace discard
####################
:discard ~ WS_many       # whitespace separates tokens

