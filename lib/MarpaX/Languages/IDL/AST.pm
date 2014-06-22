use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::IDL::AST;

# ABSTRACT: Translate an IDL source to an AST

use Carp qw/croak/;
use Marpa::R2;

our $DATA = do {local $/; <DATA>};
our $G = Marpa::R2::Scanless::G->new({source => \$DATA});

sub new {
  my ($class) = @_;

  my $self = {
  };

  bless($self, $class);

  return $self;
}

=head1 NOTES

IDL version is 3.5 as of L<OMG IDL3.5 Specification|http://www.omg.org/spec/IDL35/3.5/>.

=head1 SEE ALSO

L<Marpa::R2>

=cut

1;

__DATA__
<specification>              ::= <import any> <definition many>
<definition>                 ::= <type_dcl> ';'
                             |   <const_dcl> ';'
                             |   <except_dcl> ';'
                             |   <interface> ';'
                             |   <module> ';'
                             |   <value> ';'
                             |   <type_id_dcl> ';'
                             |   <type_prefix_dcl> ';'
                             |   <event> ';'
                             |   <component> ';'
                             |   <home_dcl> ';'
<module>                     ::= 'module' <identifier> '{' <definition many> '}'
<interface>                  ::= <interface_dcl>
                             |   <forward_dcl>
<interface_dcl>              ::= <interface_header> '{' <interface_body> '}'
<forward_dcl>                ::= <abstract or local> 'interface' <identifier>
<interface_header>           ::= <abstract or local> 'interface' <identifier> <interface_inheritance_spec maybe>
<interface_body>             ::= <export>*
<export>                     ::= <type_dcl> ';'
                             |   <const_dcl> ';'
                             |   <except_dcl> ';'
                             |   <attr_dcl> ';'
                             |   <op_dcl> ';'
                             |   <type_id_dcl> ';'
                             |   <type_prefix_dcl> ';'
<interface_inheritance_spec> ::= ':' <interface_name list many>
<interface_name>             ::= <scoped_name>
<scoped_name>                ::= <identifier>
                             |   '::' <identifier>
                             |   <scoped_name> '::' <identifier>
<value>                      ::= <value_dcl>
                             |   <value_abs_dcl>
                             |   <value_box_dcl>
                             |   <value_forward_dcl>
<value_forward_dcl>          ::= <abstract maybe> 'valuetype' <identifier>
<value_box_dcl>              ::= 'valuetype' <identifier> <type_spec>
<value_abs_dcl>              ::= 'abstract' 'valuetype' <identifier> <value_inheritance_spec maybe> '{' <export any> '}'
<value_dcl>                  ::= <value_header> '{' <value_element any> '}'
<value_header>               ::= <custom maybe> 'valuetype' <identifier> <value_inheritance_spec maybe>
<value_inheritance_spec>     ::= <value_inheritance_spec 1 values maybe> <value_inheritance_spec 2 interfaces maybe>
<value_name>                 ::= <scoped_name>
<value_element>              ::= <export>
                             |   <state_member>
                             |   <init_dcl>
<state_member>               ::= <public or private> <type_spec> <declarators> ';'
<init_dcl>                   ::= 'factory' <identifier> '(' <init_param_decls maybe> ')' <raises_expr maybe> ';'
<init_param_decls>           ::= <init_param_decl list many>
<init_param_decl>            ::= <init_param_attribute> <param_type_spec> <simple_declarator>
<init_param_attribute>       ::= 'in'
<const_dcl>                  ::= 'const' <const_type> <identifier> '=' <const_exp>
<const_type>                 ::= <integer_type>
                             |   <char_type>
                             |   <wide_char_type>
                             |   <boolean_type>
                             |   <floating_pt_type>
                             |   <string_type>
                             |   <wide_string_type>
                             |   <fixed_pt_const_type>
                             |   <scoped_name>
                             |   <octet_type>
<const_exp>                  ::= <or_expr>
<or_expr>                    ::= <xor_expr>
                             |   <or_expr> '|' <xor_expr>
<xor_expr>                   ::= <and_expr>
                             |   <xor_expr> '^' <and_expr>
<and_expr>                   ::= <shift_expr>
                             |   <and_expr> '&' <shift_expr>
<shift_expr>                 ::= <add_expr>
                             |   <shift_expr> '>>' <add_expr>
                             |   <shift_expr> '<<' <add_expr>
<add_expr>                   ::= <mult_expr>
                             |   <add_expr> '+' <mult_expr>
                             |   <add_expr> '-' <mult_expr>
<mult_expr>                  ::= <unary_expr>
                             |   <mult_expr> '*' <unary_expr>
                             |   <mult_expr> '/' <unary_expr>
                             |   <mult_expr> '%' <unary_expr>
<unary_expr>                 ::= <unary_operator> <primary_expr>
                             | <primary_expr>
<unary_operator>             ::= '-'
                             | '+'
                             | '~'
<primary_expr>               ::= <scoped_name>
                             |   <literal>
                             |   '(' <const_exp> ')'
<literal>                    ::= <integer_literal>
                             |   <string_literal>
                             |   <wide_string_literal>
                             |   <character_literal>
                             |   <wide_character_literal>
                             |   <fixed_pt_literal>
                             |   <floating_pt_literal>
                             |   <boolean_literal>
<boolean_literal>            ::= 'TRUE'
                             |   'FALSE'
<positive_int_const>         ::= <const_exp>
<type_dcl>                   ::= 'typedef' <type_declarator>
                             |   <struct_type>
                             |   <union_type>
                             |   <enum_type>
                             |   'native' <simple_declarator>
                             |   <constr_forward_decl>
<type_declarator>            ::= <type_spec> <declarators>
<type_spec>                  ::= <simple_type_spec>
                             |   <constr_type_spec>
<simple_type_spec>           ::= <base_type_spec>
                             |   <template_type_spec>
                             |   <scoped_name>
<base_type_spec>             ::= <floating_pt_type>
                             |   <integer_type>
                             |   <char_type>
                             |   <wide_char_type>
                             |   <boolean_type>
                             |   <octet_type>
                             |   <any_type>
                             |   <object_type>
                             |   <value_base_type>
<template_type_spec>         ::= <sequence_type>
                             |   <string_type>
                             |   <wide_string_type>
                             |   <fixed_pt_type>
<constr_type_spec>           ::= <struct_type>
                             |   <union_type>
                             |   <enum_type>
<declarators>                ::= <declarator list many>
<declarator>                 ::= <simple_declarator>
                             |   <complex_declarator>
<simple_declarator>          ::= <identifier>
<complex_declarator>         ::= <array_declarator>
<floating_pt_type>           ::= 'float'
                             |   'double'
                             |   'long' 'double'
<integer_type>               ::= <signed_int>
                             |   <unsigned_int>
<signed_int>                 ::= <signed_short_int>
                             |   <signed_long_int>
                             |   <signed_longlong_int>
<signed_short_int>           ::= 'short'
<signed_long_int>            ::= 'long'
<signed_longlong_int>        ::= 'long' 'long'
<unsigned_int>               ::= <unsigned_short_int>
                             |   <unsigned_long_int>
                             |   <unsigned_longlong_int>
<unsigned_short_int>         ::= 'unsigned' 'short'
<unsigned_long_int>          ::= 'unsigned' 'long'
<unsigned_longlong_int>      ::= 'unsigned' 'long' 'long'
<char_type>                  ::= 'char'
<wide_char_type>             ::= 'wchar'
<boolean_type>               ::= 'boolean'
<octet_type>                 ::= 'octet'
<any_type>                   ::= 'any'
<object_type>                ::= 'Object'
<struct_type>                ::= 'struct' <identifier> '{' <member_list> '}'
<member_list>                ::= <member>+
<member>                     ::= <type_spec> <declarators> ';'
<union_type>                 ::='union' <identifier> 'switch' '(' <switch_type_spec> ')' '{' <switch_body> '}'
<switch_type_spec>           ::=<integer_type>
                             | <char_type>
                             | <boolean_type>
                             | <enum_type>
                             | <scoped_name>
<switch_body>                ::= <case>+
<case>                       ::= <case_label any> <element_spec> ';'
<case_label>                 ::= 'case' <const_exp> ':'
                             | 'default' ':'
<element_spec>               ::= <type_spec> <declarator>
<enum_type>                  ::= 'enum' <identifier> '{' <enumerator list many> '}'
<enumerator>                 ::= <identifier>
<sequence_type>              ::= 'sequence' '<' <simple_type_spec> ',' <positive_int_const> '>'
                             | 'sequence' '<' <simple_type_spec> '>'
<string_type>                ::= 'string' '<' <positive_int_const> '>'
                             | 'string'
<wide_string_type>           ::= 'wstring' '<' <positive_int_const> '>'
                             | 'wstring'
<array_declarator>           ::= <identifier> <fixed_array_size many>
<fixed_array_size>           ::= '[' <positive_int_const> ']'
<attr_dcl>                   ::= <readonly_attr_spec>
                             |   <attr_spec>
<except_dcl>                 ::= 'exception' <identifier> '{' <member any> '}'
<op_dcl>                     ::= <op_attribute maybe> <op_type_spec> <identifier> <parameter_dcls> <raises_expr maybe> <context_expr maybe>
<op_attribute>               ::= 'oneway'
<op_type_spec>               ::= <param_type_spec>
                             | 'void'
<parameter_dcls>             ::= '(' <param_dcl list many> ')'
                             |   '(' ')'
<param_dcl>                  ::= <param_attribute> <param_type_spec> <simple_declarator>
<param_attribute>            ::='in'
                             |   'out'
                             |   'inout'
<raises_expr>                ::= 'raises' '(' <scoped_name list many> ')'
<context_expr>               ::= 'context' '(' <string_literal list many> ')'
<param_type_spec>            ::= <base_type_spec>
                             | <string_type>
                             | <wide_string_type>
                             | <scoped_name>
<fixed_pt_type>              ::= 'fixed' '<' <positive_int_const> ',' <positive_int_const> '>'
<fixed_pt_const_type>        ::= 'fixed'
<value_base_type>            ::= 'ValueBase'
<constr_forward_decl>        ::= 'struct' <identifier>
                             |   'union' <identifier>
<import>                     ::= 'import' <imported_scope> ';'
<imported_scope>             ::= <scoped_name>
                             |   <string_literal>
<type_id_dcl>                ::= 'typeid' <scoped_name> <string_literal>
<type_prefix_dcl>            ::= 'typeprefix' <scoped_name> <string_literal>
<readonly_attr_spec>         ::= 'readonly' 'attribute' <param_type_spec> <readonly_attr_declarator>
<readonly_attr_declarator>   ::= <simple_declarator> <raises_expr>
                             |   <simple_declarator list many>
<attr_spec>                  ::= 'attribute' <param_type_spec> <attr_declarator>
<attr_declarator>            ::= <simple_declarator> <attr_raises_expr>
                             |   <simple_declarator list many>
<attr_raises_expr>           ::= <get_excep_expr> <set_excep_expr maybe>
                             |   <set_excep_expr>
<get_excep_expr>             ::= 'getraises' <exception_list>
<set_excep_expr>             ::= 'setraises' <exception_list>
<exception_list>             ::= '(' <scoped_name list many> ')'

# NOTE: Grammar rules 1 through 111 with the exception of the last three lines of rule 2 constitutes the portion of IDL that
# is not related to components.

<component>                  ::= <component_dcl>
                             |   <component_forward_dcl>
<component_forward_dcl>      ::= 'component' <identifier>
<component_dcl>              ::= <component_header> '{' <component_body> '}'
<component_header>           ::= 'component' <identifier> <component_inheritance_spec maybe> <supported_interface_spec maybe>
<supported_interface_spec>   ::= 'supports' <scoped_name list many>
<component_inheritance_spec> ::= ':' <scoped_name>
<component_body>             ::= <component_export>*
<component_export>           ::= <provides_dcl> ';'
                             |   <uses_dcl> ';'
                             |   <emits_dcl> ';'
                             |   <publishes_dcl> ';'
                             |   <consumes_dcl> ';'
                             |   <attr_dcl> ';'
<provides_dcl>               ::= 'provides' <interface_type> <identifier>
<interface_type>             ::= <scoped_name>
                             |   'Object'
<uses_dcl>                   ::= 'uses' <multiple maybe> <interface_type> <identifier>
<emits_dcl>                  ::= 'emits' <scoped_name> <identifier>
<publishes_dcl>              ::= 'publishes' <scoped_name> <identifier>
<consumes_dcl>               ::= 'consumes' <scoped_name> <identifier>
<home_dcl>                   ::= <home_header> <home_body>
<home_header>                ::= 'home' <identifier> <home_inheritance_spec maybe> <supported_interface_spec maybe> 'manages' <scoped_name> <primary_key_spec maybe>
<home_inheritance_spec>      ::= ':' <scoped_name>
<primary_key_spec>           ::= 'primarykey' <scoped_name>
<home_body>                  ::= '{' <home_export any> '}'
<home_export>                ::= <export>
                             |   <factory_dcl> ';'
                             |   <finder_dcl> ';'
<factory_dcl>                ::= 'factory' <identifier> '(' [ <init_param_decls> ] ')'  <raises_expr maybe>
<finder_dcl>                 ::= 'finder' <identifier>  '(' [ <init_param_decls> ] ')'  <raises_expr maybe>
<event>                      ::= <event_dcl>
                             |   <event_abs_dcl>
                             |   <event_forward_dcl>
<event_forward_dcl>          ::= <abstract maybe> 'eventtype' <identifier>
<event_abs_dcl>              ::= 'abstract' 'eventtype' <identifier> <value_inheritance_spec maybe> '{' <export any> '}'
<event_dcl>                  ::= <event_header> '{' <value_element any> '}'
<event_header>               ::= <custom maybe> 'eventtype' <identifier> <value_inheritance_spec maybe>

<import any> ::= <import>*
<definition many> ::= <definition>+
<abstract or local> ::= 'abstract' | 'local'
<interface_inheritance_spec maybe> ::= <interface_inheritance_spec>
<interface_inheritance_spec maybe> ::=
<interface_name list many> ::= <interface_name>+ separator => <comma>
<abstract maybe> ::= 'abstract'
<abstract maybe> ::=
<value_inheritance_spec maybe> ::= <value_inheritance_spec>
<value_inheritance_spec maybe> ::=
<export any> ::= <export>*
<value_element any> ::= <value_element>*
<custom maybe> ::= 'custom'
<custom maybe> ::=
<value_name list many> ::= <value_name>+ separator => <comma>
<value_inheritance_spec 1 values> ::= ':' [ 'truncatable' ] <value_name list many>
<value_inheritance_spec 1 values maybe> ::= <value_inheritance_spec 1 values>
<value_inheritance_spec 1 values maybe> ::=
<value_inheritance_spec 2 interfaces>   ::= 'supports' <interface_name list many>
<value_inheritance_spec 2 interfaces maybe> ::= <value_inheritance_spec 2 interfaces>
<value_inheritance_spec 2 interfaces maybe> ::=
<public or private> ::= 'public' | 'private'
<init_param_decls maybe> ::= <init_param_decls>
<init_param_decls maybe> ::=
<raises_expr maybe> ::= <raises_expr>
<raises_expr maybe> ::=
<init_param_decl list many> ::= <init_param_decl>+ separator => <comma>
<declarator list many> ::= <declarator>+ separator => <comma>
<case_label any> ::= <case_label>+
<enumerator list many> ::= <enumerator>+ separator => <comma>
<fixed_array_size many> ::= <fixed_array_size>+
<member any> ::= <member>*
<op_attribute maybe> ::= <op_attribute>
<op_attribute maybe> ::=
<context_expr maybe> ::= <context_expr>
<context_expr maybe> ::=
<param_dcl list many> ::= <param_dcl>+ separator => <comma>
<scoped_name list many> ::= <scoped_name>+ separator => <comma>
<string_literal list many> ::= <string_literal>+ separator => <comma>
<simple_declarator list many> ::= <simple_declarator>+ separator => <comma>
<set_excep_expr maybe> ::= <set_excep_expr>
<set_excep_expr maybe> ::=
<component_inheritance_spec maybe> ::= <component_inheritance_spec>
<component_inheritance_spec maybe> ::=
<supported_interface_spec maybe> ::= <supported_interface_spec>
<supported_interface_spec maybe> ::=
<multiple maybe> ::= 'multiple'
<multiple maybe> ::=
<home_inheritance_spec maybe> ::= <home_inheritance_spec>
<home_inheritance_spec maybe> ::=
<primary_key_spec maybe> ::= <primary_key_spec>
<primary_key_spec maybe> ::=
<home_export any> ::= <home_export>*
<comma> ::= ','
<wide_string_literal> ::= 'L' <string_literal>


#
# Copied from C language
#
<string_literal> ::= STRING_LITERAL_UNIT+
:lexeme ~ <STRING_LITERAL_UNIT>
STRING_LITERAL_INSIDE ~ [^"\\\n]
STRING_LITERAL_INSIDE ~ ES
STRING_LITERAL_INSIDE_any ~ STRING_LITERAL_INSIDE*
STRING_LITERAL_UNIT ~ SP_maybe '"' STRING_LITERAL_INSIDE_any '"' WS_any

<integer_literal> ::= I_CONSTANT
:lexeme ~ <I_CONSTANT>
I_CONSTANT ~ HP H_many IS_maybe
           | BP B_many IS_maybe   # Gcc extension: binary constants
           | NZ D_any IS_maybe
           | '0' O_any IS_maybe
I_CONSTANT_INSIDE ~ [^'\\\n]
I_CONSTANT_INSIDE ~ ES
I_CONSTANT_INSIDE_many ~ I_CONSTANT_INSIDE+

<identifier> ::= IDENTIFIER
:lexeme ~ <IDENTIFIER>

IDENTIFIER          ~ L A_any

#
# Original C includes this definition in I_CONSTANT
#
<character_literal> ::= CHARACTER_LITERAL
:lexeme ~ <CHARACTER_LITERAL>
CHARACTER_LITERAL ~ CP_maybe QUOTE I_CONSTANT_INSIDE_many QUOTE

dD ~ [dD]
<fixed_pt_literal> ::= FIXED_PT_LITERAL
:lexeme ~ <FIXED_PT_LITERAL>
FIXED_PT_LITERAL ~ D_many '.' D_many dD
                 |        '.' D_many dD
                 | D_many '.'        dD

<wide_character_literal> ::= 'L' <character_literal>
<floating_pt_literal> ::= F_CONSTANT
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
CP         ~ [uUL]
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
QUOTE     ~ [']
BS        ~ '\'
