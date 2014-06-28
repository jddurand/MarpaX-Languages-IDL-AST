use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST;

# ABSTRACT: Translate an IDL source to an AST

# VERSION

=head1 DESCRIPTION

This module provides an AST of and IDL as per OMG's IDL 3.5 specification.

=head1 SYNOPSIS

    use MarpaX::Languages::IDL::AST;

    my $idlSource =<<IDL;
    module myIdl
    {
      valuetype myType sequence<unsigned short>;
    }
    IDL
    my $ast = MarpaX::Languages::IDL::AST->new()->ast(\$idlSource);

=cut

use Carp qw/croak/;
use Marpa::R2;

our $DATA = do {local $/; <DATA>};
our $G = Marpa::R2::Scanless::G->new({source => \$DATA, bless_package => 'IDL::AST'});

=head2 $class->new()

Instantiate a new $self object.

=cut

sub new {
  my ($class) = @_;

  my $self = {
  };

  bless($self, $class);

  return $self;
}

=head2 $self->ast($datap, $hashOptPtr)

Instantiate a new object. Takes as parameters:

=over

=item $datap

A required scalar reference to the IDL to parse.

=item $hashOptPtr

An optional reference to a hash containing Marpa::R2::Scanless::R() parameters, except the grammar option.

=back

=cut

sub ast {
    my ($self, $datap, $hashOptPtr) = @_;

    if (defined($hashOptPtr) && ref($hashOptPtr) ne 'HASH') {
	croak '3rd argument must be a pointer to HASH containing any Marpa::R2::Scanles::R option, except the grammar option';
    }
    $hashOptPtr //= {};

    my $recce = Marpa::R2::Scanless::R->new({grammar => $G, %{$hashOptPtr}});
    $recce->read($datap);
    my $value = $recce->value();
    croak 'Undefined AST value' if (! defined($value) || ! defined(${$value}));
    #
    # We want a single value
    #
    my $nextValue = $recce->value();
    croak 'Ambiguous AST' if (defined($nextValue));

    return ${$value};
}

=head1 NOTES

IDL version is 3.5 as of L<OMG IDL3.5 Specification|http://www.omg.org/spec/IDL35/3.5/>.

=head1 SEE ALSO

L<Marpa::R2>

=cut

1;

__DATA__
:default ::= action => [values] bless => ::lhs

<specification>              ::= <import any> <definition many>
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
<module>                     ::= 'module' <identifier> '{' <definition many> '}'
<interface>                  ::= <interfaceDcl>
                             |   <forwardDcl>
<interfaceDcl>              ::= <interfaceHeader> '{' <interfaceBody> '}'
<forwardDcl>                ::= <abstract or local maybe> 'interface' <identifier>
<interfaceHeader>           ::= <abstract or local maybe> 'interface' <identifier> <interfaceInheritanceSpecMaybe>
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
<scopedName>                ::= <identifier>
                             |   '::' <identifier>
                             |   <scopedName> '::' <identifier>
<value>                      ::= <valueDcl>
                             |   <valueAbsDcl>
                             |   <valueBoxDcl>
                             |   <valueForwardDcl>
<valueForwardDcl>          ::= <abstract maybe> 'valuetype' <identifier>
<valueBoxDcl>              ::= 'valuetype' <identifier> <typeSpec>
<valueAbsDcl>              ::= 'abstract' 'valuetype' <identifier> <valueInheritanceSpecMaybe> '{' <export any> '}'
<valueDcl>                  ::= <valueHeader> '{' <valueElementAny> '}'
<valueHeader>               ::= <custom maybe> 'valuetype' <identifier> <valueInheritanceSpecMaybe>
<valueInheritanceSpec>     ::= <valueInheritanceSpec1ValuesMaybe> <valueInheritanceSpec2InterfacesMaybe>
<valueName>                 ::= <scopedName>
<valueElement>              ::= <export>
                             |   <stateMember>
                             |   <initDcl>
<stateMember>               ::= <public or private> <typeSpec> <declarators> ';'
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
<declarators>                ::= <declarator list many>
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
<unionType>                 ::='union' <identifier> 'switch' '(' <switchTypeSpec> ')' '{' <switchBody> '}'
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
<enumType>                  ::= 'enum' <identifier> '{' <enumerator list many> '}'
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
<exceptDcl>                 ::= 'exception' <identifier> '{' <member any> '}'
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
<usesDcl>                   ::= 'uses' <multiple maybe> <interfaceType> <identifier>
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
<eventForwardDcl>          ::= <abstract maybe> 'eventtype' <identifier>
<eventAbsDcl>              ::= 'abstract' 'eventtype' <identifier> <valueInheritanceSpecMaybe> '{' <export any> '}'
<eventDcl>                  ::= <eventHeader> '{' <valueElementAny> '}'
<eventHeader>               ::= <custom maybe> 'eventtype' <identifier> <valueInheritanceSpecMaybe>

<import any> ::= <import>*
<definition many> ::= <definition>+
<abstract or local> ::= 'abstract' | 'local'
<abstract or local maybe> ::= <abstract or local>
<abstract or local maybe> ::=
<interfaceInheritanceSpecMaybe> ::= <interfaceInheritanceSpec>
<interfaceInheritanceSpecMaybe> ::=
<interfaceNameListMany> ::= <interfaceName>+ separator => <comma>
<abstract maybe> ::= 'abstract'
<abstract maybe> ::=
<valueInheritanceSpecMaybe> ::= <valueInheritanceSpec>
<valueInheritanceSpecMaybe> ::=
<export any> ::= <export>*
<valueElementAny> ::= <valueElement>*
<custom maybe> ::= 'custom'
<custom maybe> ::=
<valueNameListMany> ::= <valueName>+ separator => <comma>
<valueInheritanceSpec1Values> ::= ':' [ 'truncatable' ] <valueNameListMany>
<valueInheritanceSpec1ValuesMaybe> ::= <valueInheritanceSpec1Values>
<valueInheritanceSpec1ValuesMaybe> ::=
<valueInheritanceSpec2Interfaces>   ::= 'supports' <interfaceNameListMany>
<valueInheritanceSpec2InterfacesMaybe> ::= <valueInheritanceSpec2Interfaces>
<valueInheritanceSpec2InterfacesMaybe> ::=
<public or private> ::= 'public' | 'private'
<initParamDeclsMaybe> ::= <initParamDecls>
<initParamDeclsMaybe> ::=
<raisesExprMaybe> ::= <raisesExpr>
<raisesExprMaybe> ::=
<initParamDeclListMany> ::= <initParamDecl>+ separator => <comma>
<declarator list many> ::= <declarator>+ separator => <comma>
<caseLabelAny> ::= <caseLabel>+
<enumerator list many> ::= <enumerator>+ separator => <comma>
<fixedArraySizeMany> ::= <fixedArraySize>+
<member any> ::= <member>*
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
<multiple maybe> ::= 'multiple'
<multiple maybe> ::=
<homeInheritanceSpecMaybe> ::= <homeIinheritanceSpec>
<homeInheritanceSpecMaybe> ::=
<primaryKeySpecMaybe> ::= <primaryKeySpec>
<primaryKeySpecMaybe> ::=
<homeExportAny> ::= <homeExport>*
<comma> ::= ','


#
# Copied from C language
#
<stringLiteral> ::= STRING_LITERAL_UNIT+
:lexeme ~ <STRING_LITERAL_UNIT>
STRING_LITERAL_INSIDE ~ [^"\\\n]
STRING_LITERAL_INSIDE ~ ES
STRING_LITERAL_INSIDE_any ~ STRING_LITERAL_INSIDE*
STRING_LITERAL_UNIT ~ SP_maybe '"' STRING_LITERAL_INSIDE_any '"' WS_any

<wideStringLiteral> ::= WIDE_STRING_LITERAL_UNIT+
:lexeme ~ <WIDE_STRING_LITERAL_UNIT>
WIDE_STRING_LITERAL_UNIT ~ SP_maybe 'L"' STRING_LITERAL_INSIDE_any '"' WS_any

<integerLiteral> ::= I_CONSTANT
:lexeme ~ <I_CONSTANT>
I_CONSTANT ~ HP H_many IS_maybe
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
# Original C includes this definition in I_CONSTANT
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

<floatingPtLiteral> ::= F_CONSTANT
:lexeme ~ <F_CONSTANT>
F_CONSTANT ~ D_many E FS_maybe
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

