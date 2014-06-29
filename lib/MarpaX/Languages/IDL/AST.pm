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
use Clone qw/clone/;
use Hash::Merge;
use Data::Dumper;
use constant {
    SCOPE_TYPE_ANY       => 0,
    SCOPE_TYPE_MODULE    => 1,
    SCOPE_TYPE_INTERFACE => 2,
    SCOPE_TYPE_VALUETYPE => 3,
    SCOPE_TYPE_STRUCT    => 4,
    SCOPE_TYPE_UNION     => 5,
    SCOPE_TYPE_EXCEPTION => 6,
};

our $DATA = do {local $/; <DATA>};
our $G = Marpa::R2::Scanless::G->new({source => \$DATA, bless_package => 'IDL::AST'});
# Marpa follows Unicode recommendation, i.e. perl's \R, that cannot be in a character class
our $NEWLINE_REGEXP = qr/(?>\x0D\x0A|\v)/;

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

    return $self->_ast($datap, $hashOptPtr);
}

sub _ast {
    my ($self, $datap, $hashOptPtr, $recce, $posStart, $posEnd) = @_;

    if (defined($hashOptPtr) && ref($hashOptPtr) ne 'HASH') {
	croak '3rd argument must be a pointer to HASH containing any Marpa::R2::Scanles::R option, except the grammar option';
    }
    $hashOptPtr //= {};
    $posStart //= 0;
    $posEnd //= length(${$datap}) - 1;

    # print STDERR "Doing scanning on [$posStart,$posEnd]\n";

    my $pos = $posStart;
    my $max = $posEnd;
    my $length = $posEnd - $posStart;
    #
    # Recognizer
    #
    my $removeContext = 0;
    if (! defined($recce)) {
	#
	# No recce - per def this is the beginning of the parsing
	#
	$recce = Marpa::R2::Scanless::R->new({grammar => $G,
					      # trace_terminals => 1,
					      %{$hashOptPtr}});
	$pos = $recce->read($datap, $pos, $length);
	#
	## Context variables
	#
	$self->{_context} = {
	    scopeNumber => 0,
	    unnamedScopeNumber => 0,
	    currentRoot => '',
	    currentScope => '',
	    lcIdentifiersPerScope => [ {} ],       # One indice per scope, hash keys inside is lower-cased identifier, values are identifiers
	    modulesSourcePerScope => [ {} ],       # One indice per scope, hash keys inside is lower-cased identifier, value is an array ref of [posStart,posEnd]
	    scopeTypeAndLcIdentifier => [
		{
		    scopeType => SCOPE_TYPE_ANY,
		    scopeLcIdentifier => undef
		}
		],    # One indice per scope, hash keys inside is scope type, value is scope name
	    lastIdentifier => '',
	    lastScopeIdentifier => '',
	    lastIdentifierGlobalName => '',
	    lastStickedIdentifier => ''
	};
	$self->{_context}->{curLcIdentifiersPerScope} = $self->{_context}->{lcIdentifiersPerScope}->[-1];
	$self->{_context}->{curScopeTypeAndLcIdentifier} = $self->{_context}->{scopeTypeAndLcIdentifier}->[-1];
	$removeContext = 1;
    } else {
	# print STDERR "Scanning: <<<CUT HERE>>>" . substr(${$datap}, $pos, $length) . "<<<CUT HERE>>>\n";
	#
	# recce already available, per def this is recursive call from _doEvents()
	#
	$pos = $recce->resume($pos);
    }
    #
    # Parse
    #
    if ($pos <= $max) {
	do {
	    $pos = $self->_doEvents($recce, $pos, $datap);
	    if ($pos <= $max) {
		# print STDERR "resume at $pos\n";
		$pos = $recce->resume($pos);
	    }
	    # print STDERR "pos=$pos max=$max\n";
	} while ($pos <= $max);
    }
    #
    # Remove context variables
    #

    # print STDERR "Done scanning on [$posStart,$posEnd]\n";

    if ($removeContext) {
	delete($self->{_context});
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
	# Top level parsing: we return the value
	#
	return ${$value};
    } else {
	#
	# Recursive parsing: we return the position
	#
	return $pos;
    }
}

sub _doEvents {
    my ($self, $recce, $pos, $datap) = @_;

    my $context = $self->{_context};
    my $eventOrder = 0;
    my %uniqueEvents = map {$_->[0] => $eventOrder++} @{$recce->events()};

    #
    # It is important to remember the order of event type:
    #
    # 1. Completion events
    # 2. Nulled events
    # 3. Prediction events
    #
    # We are not using prediction events
    #
    my @lexemeAlternative = ();
    foreach my $eventName (sort {$uniqueEvents{$a} <=> $uniqueEvents{$b}} keys %uniqueEvents) {
	my %lastLexeme;
	#
	# Completion events
	#
	if ($eventName eq 'IDENTIFIER$') {
	    #
	    # Identifiers that differ only in case collide.
	    # Since identifiers contain only ASCII characters, it is ok to use lc()
	    # to store identifiers
	    #
	    $self->_getLastLexeme(\%lastLexeme, $recce);
	    my $identifier = $lastLexeme{value};
	    my $lcIdentifier = lc($identifier);
	    $context->{lastIdentifier} = $identifier;
	    $context->{lastIdentifierGlobalName} = $context->{currentRoot} . $context->{currentScope} . '::' . $identifier;

	    $self->_checkIdentifierCollision($context, $recce, $datap, $lcIdentifier);
	    #
	    # Deep copy of this new identifier
	    #
	    $context->{curLcIdentifiersPerScope}->{$lcIdentifier} = clone(\%lastLexeme);
	    #
	    # Add scope information
	    #
	    $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{scope} = $context->{scopeNumber};
	    #
	    # Add global name information
	    #
	    $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{globalName} = $context->{lastIdentifierGlobalName};
	    #
	    # Add lcIdentifier itself
	    #
	    $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{lcIdentifier} = $lcIdentifier;
	}
	elsif ($eventName eq 'LCURLY_SCOPE$'           ||
	       $eventName eq 'LCURLY_MODULE_SCOPE$'    ||
	       $eventName eq 'LCURLY_INTERFACE_SCOPE$' ||
	       $eventName eq 'LCURLY_VALUETYPE_SCOPE$' ||
	       $eventName eq 'LCURLY_STRUCT_SCOPE$'    ||
	       $eventName eq 'LCURLY_UNION_SCOPE$'     ||
	       $eventName eq 'LCURLY_EXCEPTION_SCOPE$' ||
	       $eventName eq 'LPAREN_SCOPE$') {
	    my ($scopeType, $scopeLcIdentifier);
	    if ($eventName eq 'LCURLY_MODULE_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_MODULE, $context->{curLcIdentifiersPerScope}->{lc($context->{lastIdentifier})});
	    } elsif ($eventName eq 'LCURLY_INTERFACE_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_INTERFACE, $context->{curLcIdentifiersPerScope}->{lc($context->{lastStickedIdentifier})});
	    } elsif ($eventName eq 'LCURLY_VALUETYPE_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_VALUETYPE, $context->{curLcIdentifiersPerScope}->{lc($context->{lastStickedIdentifier})});
	    } elsif ($eventName eq 'LCURLY_STRUCT_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_STRUCT, $context->{curLcIdentifiersPerScope}->{lc($context->{lastIdentifier})});
	    } elsif ($eventName eq 'LCURLY_UNION_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_UNION, $context->{curLcIdentifiersPerScope}->{lc($context->{lastStickedIdentifier})});
	    } elsif ($eventName eq 'LCURLY_EXCEPTION_SCOPE$') {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_EXCEPTION, $context->{curLcIdentifiersPerScope}->{lc($context->{lastIdentifier})});
	    } else {
		($scopeType, $scopeLcIdentifier) = (SCOPE_TYPE_ANY, undef);
	    }

	    #
	    # Per def, $scopeLcIdentifier must exist in $context->{lcIdentifiersPerScope} if it is defined
	    # and does not particpate in collisions between identifiers
	    #
	    push(@{$context->{scopeTypeAndLcIdentifier}}, { scopeType => $scopeType, scopeLcIdentifier => $scopeLcIdentifier });
	    $context->{curScopeTypeAndLcIdentifier} = $context->{scopeTypeAndLcIdentifier}->[-1];
	    if (defined($scopeLcIdentifier)) {
		my $lcIdentifier = $scopeLcIdentifier->{lcIdentifier};
		delete($context->{curLcIdentifiersPerScope}->{$lcIdentifier});
	    }

	    #
	    # New scopes inherit previous scope identifiers and eventual module definitions within previous scope
	    #
	    push(@{$context->{lcIdentifiersPerScope}}, clone($context->{curLcIdentifiersPerScope}));
	    $context->{curLcIdentifiersPerScope} = $context->{lcIdentifiersPerScope}->[-1];

	    if ($eventName eq 'LCURLY_MODULE_SCOPE$') {
		#
		# module definition. Per def lastIdentifier is the module identifier
		#
		my $lcLastIdentifier = lc($context->{lastIdentifier});
		$context->{modulesSourcePerScope}->[$context->{scopeNumber}] //= {};
		my $modulesSourcePerScope = $context->{modulesSourcePerScope}->[$context->{scopeNumber}];
		if (grep {$_ eq $lcLastIdentifier} keys %{$modulesSourcePerScope}) {
		    #
		    # Reparse old positions to catch eventual identifiers collisions
		    #
		    foreach (@{$modulesSourcePerScope->{$lcLastIdentifier}}) {
			#
			# Injection of previous module content. It is guaranteed to stop
			# when needed before of the 'pause => before' on closing scope lexemes
			#
			$self->_ast($datap, undef, $recce, $_->[0], $_->[1]);
		    }
		    #
		    # Push a new array ref of [position,length]. Length is for the moment unknown.
		    #
		    push(@{$modulesSourcePerScope->{$lcLastIdentifier}}, [$pos, undef]);
		} else {
		    #
		    # push a new hash ref containing current position and unknown length
		    #
		    $modulesSourcePerScope->{$lcLastIdentifier} = [ [$pos, undef] ];
		}
	    }

	    ++$context->{scopeNumber};
	}
	#
	# Nulled events
	#
	elsif ($eventName eq 'appendLastIdentifierToRootName[]') {
	    $context->{currentRoot} .= '::' . $context->{lastIdentifier};
	}
	elsif ($eventName eq 'stickIdentifier[]') {
	    $context->{lastStickedIdentifier} = $context->{lastIdentifier};
	}
	elsif ($eventName eq 'removeIdentifierFromRootName[]') {
	    $context->{currentRoot} =~ s/::[^:]+$//;
	}
	elsif ($eventName eq 'appendLastIdentifierToScopeName[]') {
	    $context->{currentScope} .= '::' . $context->{lastIdentifier};
	}
	elsif ($eventName eq 'appendUnnamedScopeToScopeName[]') {
	    #
	    # We want to make sure unnamed scope contain an invalid character for a normal scope
	    # Arbitrarly, we choose the '(' and ')' characters just to show it correspond to
	    # to an <opDcl>.
	    #
	    $context->{currentScope} .= '::(' . ++$context->{unnamedScopeNumber} . ')';
	}
	elsif ($eventName eq 'appendLastIdentifierToScopeNameWithStickedIdentifier[]') {
	    $context->{currentScope} .= '::' . $context->{lastStickedIdentifier};
	}
	elsif ($eventName eq 'removeIdentifierFromScopeName[]') {
	    $context->{currentScope} =~ s/::[^:]+$//;
	}
	elsif ($eventName eq 'removeUnnamedScopeFromScopeName[]') {
	    --$context->{unnamedScopeNumber};
	    $context->{currentScope} =~ s/::[^:]+$//;
	}
	#
	# Prediction events
	#
	elsif ($eventName eq '^RCURLY_SCOPE' || $eventName eq '^RCURLY_MODULE_SCOPE' || $eventName eq '^RPAREN_SCOPE') {
	    my $c = substr(${$datap}, $pos, 1);
	    # print STDERR "\$c = $c\n";
	    if ($c  eq '}' || $c eq ')') {
		#
		# Spec says that closing of scope happens immediately BEFORE corresponding lexemes.
		# This is not a pb for us to do that just at lexeme pause after though -;
		#
		--$context->{scopeNumber};

		pop(@{$context->{lcIdentifiersPerScope}});
		$context->{curLcIdentifiersPerScope} = $context->{lcIdentifiersPerScope}->[-1];

		pop(@{$context->{scopeTypeAndLcIdentifier}});
		$context->{curScopeTypeAndLcIdentifier} = $context->{scopeTypeAndLcIdentifier}->[-1];

		if ($eventName eq '^RCURLY_MODULE_SCOPE') {
		    #
		    # I could do that in a more efficient way -;
		    #
		    my $modulesSourcePerScope = $context->{modulesSourcePerScope}->[$context->{scopeNumber}];
		    my ($lcIdentifier) = grep {! defined($modulesSourcePerScope->{$_}->[-1]->[1])} keys %{$modulesSourcePerScope};
		    $modulesSourcePerScope->{$lcIdentifier}->[-1]->[1] = $pos - 1;
		}
		if ($c eq '}') {
		    push(@lexemeAlternative, $c, 'RCURLY_SCOPE', 'RCURLY_MODULE_SCOPE');
		} else {
		    push(@lexemeAlternative, $c, 'RPAREN_SCOPE');
		}
	    }
	}
	#print STDERR "After  $eventName " . Data::Dumper->new([$context])->Indent(1)->Maxdepth(1)->Dump;
    }
    if (@lexemeAlternative) {
	my $c = shift(@lexemeAlternative);
	map {$recce->lexeme_alternative($_, $c)} @lexemeAlternative;
	$pos = $recce->lexeme_complete($pos, 1);
	#
	# lexeme_complete() can generate events -;
	#
	$pos = $self->_doEvents($recce, $pos, $datap);
    }
    return $pos;
}

sub _checkIdentifierCollision {
    my ($self, $context, $recce, $datap, $lcIdentifier) = @_;
    #
    # Per def, $lcIdentifier is the found identifier in current scope
    #
    print STDERR Dumper($self->{_context});

    if ($self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeType} != SCOPE_TYPE_ANY &&
	$self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeLcIdentifier}->{lcIdentifier} eq $lcIdentifier) {
	my $prevIdentifier = $self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeLcIdentifier}->{value};
	my $prevIdentifierGlobalName = $self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeLcIdentifier}->{globalName};
	my $prevIdentifierLine = $self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeLcIdentifier}->{line};
	my $prevIdentifierCol = $self->{_context}->{curScopeTypeAndLcIdentifier}->{scopeLcIdentifier}->{col};
	croak "\n" .
	    "*** Scope identifier collision with identifier\n" .
	    "\n" .
	    "$prevIdentifierGlobalName, " .
	    _showLineAndCol($prevIdentifierLine, $prevIdentifierCol, ${$datap}) .
	    "\n\n" .
	    "$context->{lastIdentifierGlobalName}, " .
	    _showLineAndCol(_lineAndCol($recce), ${$datap}) .
	    "\n";
    }

    if (exists($context->{curLcIdentifiersPerScope}->{$lcIdentifier}) &&
	$context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{scope} == $context->{scopeNumber}) {
	#
	# The same lcIdentifier exist and has been defined in the same scope
	#
	my $prevIdentifier = $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{value};
	my $prevIdentifierGlobalName = $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{globalName};
	my $prevIdentifierLine = $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{line};
	my $prevIdentifierCol = $context->{curLcIdentifiersPerScope}->{$lcIdentifier}->{col};
	croak "\n" .
	    "*** Identifier collision\n" .
	    "\n" .
	    "$prevIdentifierGlobalName, " .
	    _showLineAndCol($prevIdentifierLine, $prevIdentifierCol, ${$datap}) .
	    "\n\n" .
	    "$context->{lastIdentifierGlobalName}, " .
	    _showLineAndCol(_lineAndCol($recce), ${$datap}) .
	    "\n";
    }
}

sub _lineAndCol {
    my ($recce, $g1) = @_;

    $g1 //= $recce->current_g1_location();
    my ($start, $length) = $recce->g1_location_to_span($g1);
    my ($line, $column) = $recce->line_column($start);
    return ($line, $column);
}

sub _lastLexemeSpan {
    my ($recce) = @_;
    return $recce->g1_location_to_span($recce->current_g1_location());
}

sub _getLastLexeme {
  my ($self, $lexemeHashp, $recce) = @_;

  my $rc = 0;
  #
  # Get last lexeme span
  #
  my ($start, $length) = _lastLexemeSpan($recce);
  if (defined($start)) {
    ($lexemeHashp->{start}, $lexemeHashp->{length}) = ($start, $length);
    $lexemeHashp->{value} = $recce->literal($lexemeHashp->{start}, $lexemeHashp->{length});
    ($lexemeHashp->{line}, $lexemeHashp->{col}) = _lineAndCol($recce);
    $rc = 1;
  }

  return $rc;
}

sub _showLineAndCol {
    my ($line, $col, $source) = @_;

    my $pointer = ($col > 0 ? '-' x ($col-1) : '') . '^';
    my $content = '';

    my $prevpos = pos($source);
    pos($source) = undef;
    my $thisline = 0;
    my $nbnewlines = 0;
    my $eos = 0;
    while ($source =~ m/\G(.*?)($NEWLINE_REGEXP|\Z)/scmg) {
      if (++$thisline == $line) {
        $content = substr($source, $-[1], $+[1] - $-[1]);
        $eos = (($+[2] - $-[2]) > 0) ? 0 : 1;
        last;
      }
    }
    $content =~ s/\t/ /g;
    if ($content) {
      $nbnewlines = (substr($source, 0, pos($source)) =~ tr/\n//);
      if ($eos) {
        ++$nbnewlines; # End of string instead of $NEWLINE_REGEXP
      }
    }
    pos($source) = $prevpos;

    # return "line:column $line:$col (Unicode newline count) $nbnewlines:$col (\\n count)\n\n$content\n$pointer";
    return "line:column $nbnewlines:$col\n\n$content\n$pointer";
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
<module>                     ::= 'module' <identifier> LCURLY_MODULE_SCOPE (<appendLastIdentifierToRootName>) <definition many> RCURLY_MODULE_SCOPE (<removeIdentifierFromRootName>)
<interface>                  ::= <interfaceDcl>
                             |   <forwardDcl>
<interfaceDcl>              ::= <interfaceHeader> LCURLY_INTERFACE_SCOPE (<appendLastIdentifierToScopeNameWithStickedIdentifier>) <interfaceBody> RCURLY_SCOPE (<removeIdentifierFromScopeName>)
<forwardDcl>                ::= <abstract or local maybe> 'interface' <identifier> (<stickIdentifier>)
<interfaceHeader>           ::= <abstract or local maybe> 'interface' <identifier> (<stickIdentifier>) <interfaceInheritanceSpecMaybe>
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
<valueAbsDcl>              ::= 'abstract' 'valuetype' <identifier> (<stickIdentifier>) <valueInheritanceSpecMaybe> LCURLY_VALUETYPE_SCOPE <export any> RCURLY_SCOPE
<valueDcl>                  ::= <valueHeader> LCURLY_VALUETYPE_SCOPE <valueElementAny> RCURLY_SCOPE
<valueHeader>               ::= <custom maybe> 'valuetype' <identifier> (<stickIdentifier>) <valueInheritanceSpecMaybe>
<valueInheritanceSpec>     ::= <valueInheritanceSpec1ValuesMaybe> <valueInheritanceSpec2InterfacesMaybe>
<valueName>                 ::= <scopedName>
<valueElement>              ::= <export>
                             |   <stateMember>
                             |   <initDcl>
<stateMember>               ::= <public or private> <typeSpec> <declarators> ';'
<initDcl>                   ::= 'factory' <identifier> LPAREN <initParamDeclsMaybe> RPAREN <raisesExprMaybe> ';'
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
                             |   LPAREN_SCOPE <constExp> RPAREN_SCOPE
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
<structType>                ::= 'struct' <identifier> LCURLY_STRUCT_SCOPE (<appendLastIdentifierToScopeName>) <memberList> RCURLY_SCOPE (<removeIdentifierFromScopeName>)
<memberList>                ::= <member>+
<member>                     ::= <typeSpec> <declarators> ';'
<unionType>                 ::= 'union' <identifier> (<stickIdentifier>) 'switch' LPAREN <switchTypeSpec> RPAREN LCURLY_UNION_SCOPE (<appendLastIdentifierToScopeNameWithStickedIdentifier>) <switchBody> RCURLY_SCOPE (<removeIdentifierFromScopeName>)
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
<enumType>                  ::= 'enum' <identifier> LCURLY <enumerator list many> RCURLY
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
<exceptDcl>                 ::= 'exception' <identifier> LCURLY_EXCEPTION_SCOPE (<appendLastIdentifierToScopeName>) <member any> RCURLY_SCOPE (<removeIdentifierFromScopeName>)
<opDcl>                     ::= <opAttributeMaybe> <opTypeSpec> <identifier> <parameterDcls> <raisesExprMaybe> <contextExprMaybe>
<opAttribute>               ::= 'oneway'
<opTypeSpec>               ::= <paramTypeSpec>
                             | 'void'
<parameterDcls>             ::= LPAREN (<appendUnnamedScopeToScopeName>) <paramDclListMany> RPAREN (<removeUnnamedScopeFromScopeName>)
                             |   LPAREN (<appendUnnamedScopeToScopeName>) RPAREN (<removeUnnamedScopeFromScopeName>)
<paramDcl>                  ::= <paramAttribute> <paramTypeSpec> <simpleDeclarator>
<paramAttribute>            ::='in'
                             |   'out'
                             |   'inout'
<raisesExpr>                ::= 'raises' LPAREN <scopedNameListMany> RPAREN
<contextExpr>               ::= 'context' LPAREN <stringLiteralListMany> RPAREN
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
<exceptionList>             ::= LPAREN <scopedNameListMany> RPAREN

# NOTE: Grammar rules 1 through 111 with the exception of the last three lines of rule 2 constitutes the portion of IDL that
# is not related to components.

<component>                  ::= <componentDcl>
                             |   <componentForwardDcl>
<componentForwardDcl>      ::= 'component' <identifier>
<componentDcl>              ::= <componentHeader> LCURLY_SCOPE <componentBody> RCURLY_SCOPE
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
<homeBody>                  ::= LCURLY_SCOPE <homeExportAny> RCURLY_SCOPE
<homeExport>                ::= <export>
                             |   <factoryDcl> ';'
                             |   <finderDcl> ';'
<factoryDcl>                ::= 'factory' <identifier> LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
<finderDcl>                 ::= 'finder' <identifier>  LPAREN [ <initParamDecls> ] RPAREN  <raisesExprMaybe>
<event>                      ::= <eventDcl>
                             |   <eventAbsDcl>
                             |   <eventForwardDcl>
<eventForwardDcl>          ::= <abstract maybe> 'eventtype' <identifier>
<eventAbsDcl>              ::= 'abstract' 'eventtype' <identifier> <valueInheritanceSpecMaybe> LCURLY_SCOPE <export any> RCURLY_SCOPE
<eventDcl>                  ::= <eventHeader> LCURLY_SCOPE <valueElementAny> RCURLY_SCOPE
<eventHeader>               ::= <custom maybe> 'eventtype' <identifier> <valueInheritanceSpecMaybe>

:lexeme ~ <LCURLY_SCOPE> pause => after event => 'LCURLY_SCOPE$'
:lexeme ~ <LCURLY_MODULE_SCOPE> pause => after event => 'LCURLY_MODULE_SCOPE$'
:lexeme ~ <LCURLY_INTERFACE_SCOPE> pause => after event => 'LCURLY_INTERFACE_SCOPE$'
:lexeme ~ <LCURLY_VALUETYPE_SCOPE> pause => after event => 'LCURLY_VALUETYPE_SCOPE$'
:lexeme ~ <LCURLY_STRUCT_SCOPE> pause => after event => 'LCURLY_STRUCT_SCOPE$'
:lexeme ~ <LCURLY_UNION_SCOPE> pause => after event => 'LCURLY_UNION_SCOPE$'
:lexeme ~ <LCURLY_EXCEPTION_SCOPE> pause => after event => 'LCURLY_EXCEPTION_SCOPE$'
_LCURLY ~ '{'
LCURLY ~ _LCURLY
LCURLY_SCOPE ~ _LCURLY
LCURLY_MODULE_SCOPE ~ _LCURLY
LCURLY_INTERFACE_SCOPE ~ _LCURLY
LCURLY_VALUETYPE_SCOPE ~ _LCURLY
LCURLY_STRUCT_SCOPE ~ _LCURLY
LCURLY_UNION_SCOPE ~ _LCURLY
LCURLY_EXCEPTION_SCOPE ~ _LCURLY

:lexeme ~ <RCURLY_SCOPE> pause => before event => '^RCURLY_SCOPE'
:lexeme ~ <RCURLY_MODULE_SCOPE> pause => before event => '^RCURLY_MODULE_SCOPE'
_RCURLY ~ '}'
RCURLY ~ _RCURLY
RCURLY_SCOPE ~ _RCURLY
RCURLY_MODULE_SCOPE ~ _RCURLY

:lexeme ~ <LPAREN_SCOPE> pause => after event => 'LPAREN_SCOPE$'
_LPAREN ~ '('
LPAREN ~ _LPAREN
LPAREN_SCOPE ~ _LPAREN

:lexeme ~ <RPAREN_SCOPE> pause => before event => '^RPAREN_SCOPE'
_RPAREN ~ ')'
RPAREN ~ _RPAREN
RPAREN_SCOPE ~ _RPAREN

#
# Nulled events
#
event 'appendLastIdentifierToRootName[]' = nulled <appendLastIdentifierToRootName>
<appendLastIdentifierToRootName> ::=
event 'removeIdentifierFromRootName[]' = nulled <removeIdentifierFromRootName>
<removeIdentifierFromRootName> ::=
event 'appendLastIdentifierToScopeName[]' = nulled <appendLastIdentifierToScopeName>
<appendLastIdentifierToScopeName> ::=
event 'appendUnnamedScopeToScopeName[]' = nulled <appendUnnamedScopeToScopeName>
<appendUnnamedScopeToScopeName> ::=
event 'appendLastIdentifierToScopeNameWithStickedIdentifier[]' = nulled <appendLastIdentifierToScopeNameWithStickedIdentifier>
<appendLastIdentifierToScopeNameWithStickedIdentifier> ::=
event 'removeIdentifierFromScopeName[]' = nulled <removeIdentifierFromScopeName>
<removeIdentifierFromScopeName> ::=
event 'removeUnnamedScopeFromScopeName[]' = nulled <removeUnnamedScopeFromScopeName>
<removeUnnamedScopeFromScopeName> ::=
event 'stickIdentifier[]' = nulled <stickIdentifier>
<stickIdentifier> ::=

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
:lexeme ~ <IDENTIFIER> priority => -1 pause => after event => 'IDENTIFIER$'

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

