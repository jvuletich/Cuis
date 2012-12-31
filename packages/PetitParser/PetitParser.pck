'From Cuis 4.0 of 21 April 2012 [latest update: #1291] on 3 June 2012 at 11:55:28 am'!
'Description Please enter a description for this package '!
!classDefinition: #PPCharSetPredicate category: #'PetitParser-Tools'!
Object subclass: #PPCharSetPredicate
	instanceVariableNames: 'block classification'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Tools'!
!classDefinition: 'PPCharSetPredicate class' category: #'PetitParser-Tools'!
PPCharSetPredicate class
	instanceVariableNames: ''!

!classDefinition: #PPFailure category: #'PetitParser-Core'!
Object subclass: #PPFailure
	instanceVariableNames: 'message position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Core'!
!classDefinition: 'PPFailure class' category: #'PetitParser-Core'!
PPFailure class
	instanceVariableNames: ''!

!classDefinition: #PPMemento category: #'PetitParser-Core'!
Object subclass: #PPMemento
	instanceVariableNames: 'result count position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Core'!
!classDefinition: 'PPMemento class' category: #'PetitParser-Core'!
PPMemento class
	instanceVariableNames: ''!

!classDefinition: #PPParser category: #'PetitParser-Parsers'!
Object subclass: #PPParser
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPParser class' category: #'PetitParser-Parsers'!
PPParser class
	instanceVariableNames: ''!

!classDefinition: #PPDelegateParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPDelegateParser
	instanceVariableNames: 'parser'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPDelegateParser class' category: #'PetitParser-Parsers'!
PPDelegateParser class
	instanceVariableNames: ''!

!classDefinition: #PPActionParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPActionParser
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPActionParser class' category: #'PetitParser-Parsers'!
PPActionParser class
	instanceVariableNames: ''!

!classDefinition: #PPAndParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPAndParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPAndParser class' category: #'PetitParser-Parsers'!
PPAndParser class
	instanceVariableNames: ''!

!classDefinition: #PPCompositeParser category: #'PetitParser-Tools'!
PPDelegateParser subclass: #PPCompositeParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Tools'!
!classDefinition: 'PPCompositeParser class' category: #'PetitParser-Tools'!
PPCompositeParser class
	instanceVariableNames: ''!

!classDefinition: #PPEndOfInputParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPEndOfInputParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPEndOfInputParser class' category: #'PetitParser-Parsers'!
PPEndOfInputParser class
	instanceVariableNames: ''!

!classDefinition: #PPEpsilonParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPEpsilonParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPEpsilonParser class' category: #'PetitParser-Parsers'!
PPEpsilonParser class
	instanceVariableNames: ''!

!classDefinition: #PPExpressionParser category: #'PetitParser-Tools'!
PPDelegateParser subclass: #PPExpressionParser
	instanceVariableNames: 'operators'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Tools'!
!classDefinition: 'PPExpressionParser class' category: #'PetitParser-Tools'!
PPExpressionParser class
	instanceVariableNames: ''!

!classDefinition: #PPFailingParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPFailingParser
	instanceVariableNames: 'message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPFailingParser class' category: #'PetitParser-Parsers'!
PPFailingParser class
	instanceVariableNames: ''!

!classDefinition: #PPFlattenParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPFlattenParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPFlattenParser class' category: #'PetitParser-Parsers'!
PPFlattenParser class
	instanceVariableNames: ''!

!classDefinition: #PPListParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPListParser
	instanceVariableNames: 'parsers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPListParser class' category: #'PetitParser-Parsers'!
PPListParser class
	instanceVariableNames: ''!

!classDefinition: #PPChoiceParser category: #'PetitParser-Parsers'!
PPListParser subclass: #PPChoiceParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPChoiceParser class' category: #'PetitParser-Parsers'!
PPChoiceParser class
	instanceVariableNames: ''!

!classDefinition: #PPLiteralParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPLiteralParser
	instanceVariableNames: 'literal message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPLiteralParser class' category: #'PetitParser-Parsers'!
PPLiteralParser class
	instanceVariableNames: ''!

!classDefinition: #PPLiteralObjectParser category: #'PetitParser-Parsers'!
PPLiteralParser subclass: #PPLiteralObjectParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPLiteralObjectParser class' category: #'PetitParser-Parsers'!
PPLiteralObjectParser class
	instanceVariableNames: ''!

!classDefinition: #PPLiteralSequenceParser category: #'PetitParser-Parsers'!
PPLiteralParser subclass: #PPLiteralSequenceParser
	instanceVariableNames: 'size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPLiteralSequenceParser class' category: #'PetitParser-Parsers'!
PPLiteralSequenceParser class
	instanceVariableNames: ''!

!classDefinition: #PPMemoizedParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPMemoizedParser
	instanceVariableNames: 'stream buffer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPMemoizedParser class' category: #'PetitParser-Parsers'!
PPMemoizedParser class
	instanceVariableNames: ''!

!classDefinition: #PPNotParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPNotParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPNotParser class' category: #'PetitParser-Parsers'!
PPNotParser class
	instanceVariableNames: ''!

!classDefinition: #PPOptionalParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPOptionalParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPOptionalParser class' category: #'PetitParser-Parsers'!
PPOptionalParser class
	instanceVariableNames: ''!

!classDefinition: #PPPluggableParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPPluggableParser
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPPluggableParser class' category: #'PetitParser-Parsers'!
PPPluggableParser class
	instanceVariableNames: ''!

!classDefinition: #PPPredicateParser category: #'PetitParser-Parsers'!
PPParser subclass: #PPPredicateParser
	instanceVariableNames: 'predicate predicateMessage negated negatedMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPPredicateParser class' category: #'PetitParser-Parsers'!
PPPredicateParser class
	instanceVariableNames: ''!

!classDefinition: #PPPredicateObjectParser category: #'PetitParser-Parsers'!
PPPredicateParser subclass: #PPPredicateObjectParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPPredicateObjectParser class' category: #'PetitParser-Parsers'!
PPPredicateObjectParser class
	instanceVariableNames: ''!

!classDefinition: #PPPredicateSequenceParser category: #'PetitParser-Parsers'!
PPPredicateParser subclass: #PPPredicateSequenceParser
	instanceVariableNames: 'size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPPredicateSequenceParser class' category: #'PetitParser-Parsers'!
PPPredicateSequenceParser class
	instanceVariableNames: ''!

!classDefinition: #PPRepeatingParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPRepeatingParser
	instanceVariableNames: 'min max'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPRepeatingParser class' category: #'PetitParser-Parsers'!
PPRepeatingParser class
	instanceVariableNames: ''!

!classDefinition: #PPLimitedRepeatingParser category: #'PetitParser-Parsers'!
PPRepeatingParser subclass: #PPLimitedRepeatingParser
	instanceVariableNames: 'limit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPLimitedRepeatingParser class' category: #'PetitParser-Parsers'!
PPLimitedRepeatingParser class
	instanceVariableNames: ''!

!classDefinition: #PPGreedyRepeatingParser category: #'PetitParser-Parsers'!
PPLimitedRepeatingParser subclass: #PPGreedyRepeatingParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPGreedyRepeatingParser class' category: #'PetitParser-Parsers'!
PPGreedyRepeatingParser class
	instanceVariableNames: ''!

!classDefinition: #PPLazyRepeatingParser category: #'PetitParser-Parsers'!
PPLimitedRepeatingParser subclass: #PPLazyRepeatingParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPLazyRepeatingParser class' category: #'PetitParser-Parsers'!
PPLazyRepeatingParser class
	instanceVariableNames: ''!

!classDefinition: #PPPossessiveRepeatingParser category: #'PetitParser-Parsers'!
PPRepeatingParser subclass: #PPPossessiveRepeatingParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPPossessiveRepeatingParser class' category: #'PetitParser-Parsers'!
PPPossessiveRepeatingParser class
	instanceVariableNames: ''!

!classDefinition: #PPSequenceParser category: #'PetitParser-Parsers'!
PPListParser subclass: #PPSequenceParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPSequenceParser class' category: #'PetitParser-Parsers'!
PPSequenceParser class
	instanceVariableNames: ''!

!classDefinition: #PPStream category: #'PetitParser-Core'!
ReadStream subclass: #PPStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Core'!
!classDefinition: 'PPStream class' category: #'PetitParser-Core'!
PPStream class
	instanceVariableNames: ''!

!classDefinition: #PPToken category: #'PetitParser-Core'!
Object subclass: #PPToken
	instanceVariableNames: 'collection start stop'
	classVariableNames: 'NewLineParser'
	poolDictionaries: ''
	category: 'PetitParser-Core'!
!classDefinition: 'PPToken class' category: #'PetitParser-Core'!
PPToken class
	instanceVariableNames: ''!

!classDefinition: #PPTokenParser category: #'PetitParser-Parsers'!
PPFlattenParser subclass: #PPTokenParser
	instanceVariableNames: 'tokenClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPTokenParser class' category: #'PetitParser-Parsers'!
PPTokenParser class
	instanceVariableNames: ''!

!classDefinition: #PPTrimmingParser category: #'PetitParser-Parsers'!
PPDelegateParser subclass: #PPTrimmingParser
	instanceVariableNames: 'trimmer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPTrimmingParser class' category: #'PetitParser-Parsers'!
PPTrimmingParser class
	instanceVariableNames: ''!

!classDefinition: #PPUnresolvedParser category: #'PetitParser-Tools'!
PPParser subclass: #PPUnresolvedParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Tools'!
!classDefinition: 'PPUnresolvedParser class' category: #'PetitParser-Tools'!
PPUnresolvedParser class
	instanceVariableNames: ''!

!classDefinition: #PPWrappingParser category: #'PetitParser-Parsers'!
PPActionParser subclass: #PPWrappingParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitParser-Parsers'!
!classDefinition: 'PPWrappingParser class' category: #'PetitParser-Parsers'!
PPWrappingParser class
	instanceVariableNames: ''!


!PPActionParser commentStamp: '<historical>' prior: 0!
A parser that performs an action block with the successful parse result of the delegate.Instance Variables:	block	<BlockClosure>	The action block to be executed.!

!PPAndParser commentStamp: 'TudorGirba 2/27/2011 22:22' prior: 0!
The and-predicate, a parser that succeeds whenever its delegate does, but does not consume the input stream [Parr 1994, 1995].!

!PPChoiceParser commentStamp: 'lr 4/18/2008 15:35' prior: 0!
A parser that uses the first parser that succeeds.!

!PPCompositeParser commentStamp: 'lr 12/4/2009 18:38' prior: 0!
A PPCompositeParser is composed parser built from various primitive parsers. Every production in the receiver is specified as a method that returns its parser. Note that every production requires an instance variable of the same name, otherwise the production is not cached and cannot be used in recursive grammars. Productions should refer to each other by reading the respective inst-var. Note: these inst-vars are typically not written, as the assignment happens in the initialize method using reflection.The start production is defined in the method start. It is aliased to the inst-var parser defined in the superclass of PPCompositeParser.!

!PPDelegateParser commentStamp: '<historical>' prior: 0!
A parser that delegates to another parser.Instance Variables:	parser	<PPParser>	The parser to delegate to.!

!PPEndOfInputParser commentStamp: 'lr 4/18/2008 13:46' prior: 0!
A parser that succeeds only at the end of the input stream.!

!PPEpsilonParser commentStamp: 'lr 5/15/2008 15:09' prior: 0!
A parser that consumes nothing and always succeeds.!

!PPExpressionParser commentStamp: '<historical>' prior: 0!
A PPExpressionParser is a parser to conveniently define an expression grammar with prefix, postfix, and left- and right-associative infix operators.The following code initializes a parser for arithmetic expressions. First we instantiate an expression parser, a simple parser for expressions in parenthesis and a simple parser for integer numbers.	expression := PPExpressionParser new.	parens := $( asParser token trim , expression , $) asParser token trim 		==> [ :nodes | nodes second ].	integer := #digit asParser plus token trim		==> [ :token | token value asInteger ].	Then we define on what term the expression grammar is built on:	expression term: parens / integer.	Finally we define the operator-groups in descending precedence. Note, that the action blocks receive both, the terms and the parsed operator in the order they appear in the parsed input. 		expression		group: [ :g |			g prefix: $- asParser token trim do: [ :op :a | a negated ] ];		group: [ :g |			g postfix: '++' asParser token trim do: [ :a :op | a + 1 ].			g postfix: '--' asParser token trim do: [ :a :op | a - 1 ] ];		group: [ :g |			g right: $^ asParser token trim do: [ :a :op :b | a raisedTo: b ] ];		group: [ :g |			g left: $* asParser token trim do: [ :a :op :b | a * b ].			g left: $/ asParser token trim do: [ :a :op :b | a / b ] ];		group: [ :g |			g left: $+ asParser token trim do: [ :a :op :b | a + b ].			g left: $- asParser token trim do: [ :a :op :b | a - b ] ].		After evaluating the above code the 'expression' is an efficient parser that evaluates examples like:	expression parse: '-8++'.	expression parse: '1+2*3'.	expression parse: '1*2+3'.	expression parse: '(1+2)*3'.	expression parse: '8/4/2'.	expression parse: '8/(4/2)'.	expression parse: '2^2^3'.	expression parse: '(2^2)^3'.	Instance Variables:	operators	<Dictionary>	The operators defined in the current group.!

!PPFailingParser commentStamp: '<historical>' prior: 0!
A parser that consumes nothing and always fails.Instance Variables:	message <String>	The failure message.!

!PPFailure commentStamp: '<historical>' prior: 0!
The failure object in PetitParser. It is the only class that responds to #isPetitFailure with true. It contains an error message and a position of the occurrence of the failure.Instance Variables:	message	<String>	The error message of this failure.	position	<Integer>	The position of this failure in the input stream.!

!PPFlattenParser commentStamp: 'lr 11/22/2009 13:09' prior: 0!
A parser that answers a flat copy of the range my delegate parses.!

!PPGreedyRepeatingParser commentStamp: 'lr 4/3/2011 15:08' prior: 0!
A greedy repeating parser, commonly seen in regular expression implementations. It aggressively consumes as much input as possible and then backtracks to meet the 'limit' condition.This class essentially implements the iterative version of the following recursive parser composition:	| parser |	parser := PPChoiceParser new.	parser setParsers: (Array		with: (self , parser map: [ :each :rest | rest addFirst: each; yourself ])		with: (limit and ==> [ :each | OrderedCollection new ])).	^ parser ==> [ :rest | rest asArray ]!

!PPLazyRepeatingParser commentStamp: 'lr 4/3/2011 15:08' prior: 0!
A lazy repeating parser, commonly seen in regular expression implementations. It limits its consumption to meet the 'limit' condition as early as possible.This class essentially implements the iterative version of the following recursive parser composition:	| parser |	parser := PPChoiceParser new.	parser setParsers: (Array		with: (limit and ==> [ :each | OrderedCollection new ])		with: (self , parser map: [ :each :rest | rest addFirst: each; yourself ])).	^ parser ==> [ :rest | rest asArray ]!

!PPLimitedRepeatingParser commentStamp: 'lr 4/3/2011 14:37' prior: 0!
An abstract parser that repeatedly parses between 'min' and 'max' instances of my delegate and that requires the input to be completed with a specified parser 'limit'. Subclasses provide repeating behavior as typically seen in regular expression implementations (non-blind).Instance Variables:	limit	<PPParser>	The parser to complete the input with.!

!PPListParser commentStamp: '<historical>' prior: 0!
Abstract parser that parses a list of things in some way (to be specified by the subclasses).Instance Variables:	parsers	<SequenceableCollection of: PPParser>	A sequence of other parsers to delegate to.!

!PPLiteralObjectParser commentStamp: '<historical>' prior: 0!
A parser that accepts a single literal object, such as a character. This is the same as the predicate parser 'PPPredicateParser expect: literal' but slightly more efficient.!

!PPLiteralParser commentStamp: '<historical>' prior: 0!
Abstract literal parser that parses some kind of literal type (to be specified by subclasses).Instance Variables:	literal	<Object>	The literal object to be parsed.	message	<String>	The error message to be generated.!

!PPLiteralSequenceParser commentStamp: 'lr 12/4/2009 18:39' prior: 0!
A parser accepts a sequence of literal objects, such as a String. This is an optimization to avoid having to compose longer sequences from PPSequenceParser.!

!PPMemento commentStamp: '<historical>' prior: 0!
PPMemento is an internal class used by PPMemoizedParser to cache results and detect left-recursive calls.Instance Variables:	result	<Object>	The cached result.	count	<Integer>	The number of recursive cycles followed.	position	<Integer>	The position of the cached result in the input stream.!

!PPMemoizedParser commentStamp: '<historical>' prior: 0!
A memoized parser, for refraining redundant computations.Instance Variables:	stream	<PositionableStream>	The stream of the associated memento objects.	buffer	<Array of: PPMemento>	The buffer of memento objects.!

!PPNotParser commentStamp: '<historical>' prior: 0!
The not-predicate, a parser that succeeds whenever its delegate does not, but consumes no input [Parr 1994, 1995].!

!PPOptionalParser commentStamp: 'lr 4/3/2011 14:46' prior: 0!
A parser that optionally parsers its delegate, or answers nil.!

!PPParser commentStamp: '<historical>' prior: 0!
An abstract parser for all parsers in PetitParser. Subclasses implement #parseOn: to perform the actual recursive-descent parsing. All parsers support a variety of methods to perform an actual parse, see the methods in the #parsing protocol. Parsers are combined with a series of operators that can be found in the #operations protocol.Instance Variables:	properties	<Dictionary>	Stores additional state in the parser object.!

!PPPluggableParser commentStamp: '<historical>' prior: 0!
A pluggable parser that passes the parser stream into a block. This enables users to perform manual parsing or to embed other parser frameworks into PetitParser.Instance Variables:	block	<BlockClosure>	The pluggable one-argument block.!

!PPPossessiveRepeatingParser commentStamp: 'lr 4/3/2011 14:35' prior: 0!
The default repeating parser with standard PEG semantics (i.e. possessive, blind, eager).!

!PPPredicateObjectParser commentStamp: '<historical>' prior: 0!
A parser that accepts if a given predicate on one element of the input sequence holds.!

!PPPredicateParser commentStamp: '<historical>' prior: 0!
An abstract parser that accepts if a given predicate holds.Instance Variables:	predicate	<BlockClosure>	The block testing for the predicate.	predicateMessage	<String>	The error message of the predicate.	negated	<BlockClosure>	The block testing for the negation of the predicate.	negatedMessage	<String>	The error message of the negated predicate.!

!PPPredicateSequenceParser commentStamp: '<historical>' prior: 0!
A parser that accepts if a given predicate on an arbitrary number of elements of the input sequence holds.Instance Variables:	size	<Integer>	The number of elements to consume.!

!PPRepeatingParser commentStamp: 'lr 4/3/2011 14:45' prior: 0!
An abstract parser that repeatedly parses between 'min' and 'max' instances of its delegate. The default configuration parses an infinite number of elements, as 'min' is set to 0 and 'max' to infinity (SmallInteger maxVal).Instance Variables:	min	<Integer>	The minimum number of repetitions.	max	<Integer>	The maximum number of repetitions.!

!PPSequenceParser commentStamp: 'lr 4/18/2008 15:34' prior: 0!
A parser that parses a sequence of parsers.!

!PPStream commentStamp: '<historical>' prior: 0!
A positional stream implementation used for parsing. It overrides some methods for optimization reasons.!

!PPToken commentStamp: '<historical>' prior: 0!
PPToken represents a parsed part of the input stream. Contrary to a simple String it remembers where it came from, the original collection and its start and stop position.Instance Variables:	collection	<SequenceableCollection>	The collection this token comes from.	start	<Integer>	The start position in the collection.	stop	<Integer>	The stop position in the collection.!

!PPTokenParser commentStamp: '<historical>' prior: 0!
A parser that answers a token of the range my delegate parses.Instance Variables:	tokenClass	<PPToken class>	The token sub-class to be used.!

!PPTrimmingParser commentStamp: 'lr 4/6/2010 19:27' prior: 0!
A parser that silently consumes spaces before and after the delegate parser.!

!PPUnresolvedParser commentStamp: 'lr 11/28/2009 18:50' prior: 0!
This is a temporary placeholder or forward reference to a parser that has not been defined yet. If everything goes well it will eventually be replaced with the real parser instance.!

!PPWrappingParser commentStamp: '<historical>' prior: 0!
A parser that performs an action block upon activation with the stream and a continuation block.!

!BlockClosure methodsFor: '*petitparser-core-converting' stamp: 'lr 11/29/2011 20:48'!
asParser	"Answer a parser implemented in the receiving one-argument block."	^ PPPluggableParser on: self! !

!BlockContext methodsFor: '*petitparser-core-converting' stamp: 'lr 11/29/2011 20:48'!
asParser	"Answer a parser implemented in the receiving one-argument block."	^ PPPluggableParser on: self! !

!Character methodsFor: '*petitparser-core-operators' stamp: 'lr 6/12/2010 09:04'!
- aCharacter	"Create a range of characters between the receiver and the argument."		^ PPPredicateObjectParser between: self and: aCharacter! !

!Character methodsFor: '*petitparser-core-converting' stamp: 'lr 12/18/2011 15:58'!
asParser	"Answer a parser that accepts the receiving character."		^ PPLiteralObjectParser on: self! !

!Collection methodsFor: '*petitparser-core-converting' stamp: 'lr 11/29/2011 20:38'!
asChoiceParser	^ PPChoiceParser withAll: (self collect: [ :each | each asParser ])! !

!Collection methodsFor: '*petitparser-core-converting' stamp: 'lr 11/29/2011 20:38'!
asSequenceParser	^ PPSequenceParser withAll: (self collect: [ :each | each asParser ])! !

!Object methodsFor: '*petitparser-core-converting' stamp: 'lr 12/18/2011 15:58'!
asParser	"Answer a parser accepting the receiving object."	^ PPPredicateObjectParser expect: self! !

!Object methodsFor: '*petitparser-core-testing' stamp: 'lr 2/7/2010 20:54'!
isPetitFailure	^ false! !

!Object methodsFor: '*petitparser-core-testing' stamp: 'lr 8/6/2010 16:44'!
isPetitParser	^ false! !

!PPActionParser methodsFor: 'accessing' stamp: 'lr 4/30/2010 11:10'!
block	"Answer the action block of the receiver."	^ block! !

!PPActionParser methodsFor: 'parsing' stamp: 'pmon 6/1/2012 00:27'!
parseOn: aStream
	| element |
	^ (element := parser parseOn: aStream) isPetitFailure
		ifFalse: [ block isSymbol
					ifTrue: [ element perform: block]
					ifFalse: [ block value: element ] ]
		ifTrue: [ element ]! !

!PPActionParser methodsFor: 'initialization' stamp: 'lr 5/2/2010 16:58'!
setBlock: aBlock	block := aBlock! !

!PPActionParser class methodsFor: 'instance creation' stamp: 'lr 5/2/2010 16:58'!
on: aParser block: aBlock	^ (self on: aParser) setBlock: aBlock! !

!PPAndParser methodsFor: 'operators' stamp: 'lr 5/1/2010 16:16'!
and	^ self! !

!PPAndParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:10'!
parseOn: aStream	| element position |	position := aStream position.	element := parser parseOn: aStream.	aStream position: position.	^ element! !

!PPCharSetPredicate methodsFor: 'initialization' stamp: 'lr 8/30/2010 12:19'!
initializeOn: aBlock	block := aBlock.	classification := Array new: 255.	1 to: classification size do: [ :index |		classification at: index put: (block			value: (Character value: index)) ]! !

!PPCharSetPredicate methodsFor: 'evaluating' stamp: 'lr 8/30/2010 12:19'!
value: aCharacter	| index |	index := aCharacter asInteger.	index == 0		ifTrue: [ ^ block value: aCharacter ].	index > 255		ifTrue: [ ^ block value: aCharacter ].	^ classification at: index! !

!PPCharSetPredicate class methodsFor: 'instance creation' stamp: 'lr 8/25/2010 11:05'!
on: aBlock	^ self basicNew initializeOn: aBlock! !

!PPChoiceParser methodsFor: 'operators' stamp: 'lr 9/17/2008 00:16'!
/ aRule 	^ self copyWith: aRule! !

!PPChoiceParser methodsFor: 'parsing' stamp: 'lr 5/22/2010 11:48'!
parseOn: aStream	"This is optimized code that avoids unnecessary block activations, do not change. When all choices fail, the last failure is answered."	| element |	1 to: parsers size do: [ :index |		element := (parsers at: index)			parseOn: aStream.		element isPetitFailure			ifFalse: [ ^ element ] ].	^ element! !

!PPCompositeParser methodsFor: 'initialization' stamp: 'lr 5/8/2011 15:27'!
initializeStartingAt: aSymbol	| productionNames |	self initialize.	productionNames := self productionNames.	parser := PPDelegateParser named: aSymbol.	productionNames keysAndValuesDo: [ :key :value |		self instVarAt: key put: (PPDelegateParser named: value) ].	parser setParser: (self perform: aSymbol).	productionNames keysAndValuesDo: [ :key :value |		(self instVarAt: key) setParser: (self perform: value) ]! !

!PPCompositeParser methodsFor: 'querying' stamp: 'lr 12/4/2009 18:39'!
productionAt: aSymbol	"Answer the production named aSymbol."		^ self productionAt: aSymbol ifAbsent: [ nil ]! !

!PPCompositeParser methodsFor: 'querying' stamp: 'lr 6/4/2010 13:37'!
productionAt: aSymbol ifAbsent: aBlock	"Answer the production named aSymbol, if there is no such production answer the result of evaluating aBlock."		(self class ignoredNames includes: aSymbol asString)		ifTrue: [ ^ aBlock value ].	(self class startSymbol = aSymbol)		ifTrue: [ ^ parser ].	^ self instVarAt: (self class allInstVarNames		indexOf: aSymbol asString		ifAbsent: [ ^ aBlock value ])! !

!PPCompositeParser methodsFor: 'querying' stamp: 'lr 5/8/2011 15:45'!
productionNames	"Answer a dictionary of slot indexes and production names."		| productionNames ignoredNames |	productionNames := Dictionary new.	ignoredNames := self class ignoredNames		collect: [ :each | each asSymbol ].	self class allInstVarNames keysAndValuesDo: [ :key :value |		(ignoredNames includes: value asSymbol)			ifFalse: [ productionNames at: key put: value asSymbol ] ].	^ productionNames! !

!PPCompositeParser methodsFor: 'accessing' stamp: 'lr 5/16/2008 17:32'!
start	"Answer the production to start this parser with."		self subclassResponsibility! !

!PPCompositeParser class methodsFor: 'accessing' stamp: 'lr 1/29/2010 11:35'!
ignoredNames	"Answer a collection of instance-variables that should not be automatically initialized with productions, but that are used internal to the composite parser."	^ PPCompositeParser allInstVarNames! !

!PPCompositeParser class methodsFor: 'instance creation' stamp: 'lr 12/7/2009 08:24'!
new	"Answer a new parser starting at the default start symbol."	^ self newStartingAt: self startSymbol! !

!PPCompositeParser class methodsFor: 'instance creation' stamp: 'lr 12/7/2009 08:24'!
newStartingAt: aSymbol	"Answer a new parser starting at aSymbol."	^ self basicNew initializeStartingAt: aSymbol! !

!PPCompositeParser class methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:57'!
parse: anObject	^ self parse: anObject startingAt: self startSymbol! !

!PPCompositeParser class methodsFor: 'parsing' stamp: 'lr 2/7/2010 21:02'!
parse: anObject onError: aBlock	^ self parse: anObject startingAt: self startSymbol onError: aBlock! !

!PPCompositeParser class methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:57'!
parse: anObject startingAt: aSymbol	^ (self newStartingAt: aSymbol) parse: anObject! !

!PPCompositeParser class methodsFor: 'parsing' stamp: 'lr 2/7/2010 21:02'!
parse: anObject startingAt: aSymbol onError: aBlock	^ (self newStartingAt: aSymbol) parse: anObject onError: aBlock! !

!PPCompositeParser class methodsFor: 'accessing' stamp: 'lr 12/7/2009 08:20'!
startSymbol	"Answer the method that represents the default start symbol."	^ #start! !

!PPDelegateParser methodsFor: 'accessing' stamp: 'lr 10/21/2009 16:37'!
children	^ Array with: parser! !

!PPDelegateParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:47'!
parseOn: aStream	^ parser parseOn: aStream! !

!PPDelegateParser methodsFor: 'initialization' stamp: 'lr 4/20/2008 16:23'!
setParser: aParser	parser := aParser! !

!PPDelegateParser class methodsFor: 'instance creation' stamp: 'lr 4/20/2008 16:22'!
on: aParser	^ self new setParser: aParser! !

!PPEndOfInputParser methodsFor: 'operators' stamp: 'lr 12/7/2009 08:53'!
end	^ self! !

!PPEndOfInputParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:10'!
parseOn: aStream	| position result |	position := aStream position.	result := parser parseOn: aStream.	(result isPetitFailure or: [ aStream atEnd ])		ifTrue: [ ^ result ].	result := PPFailure		message: 'end of input expected'		at: aStream position.	aStream position: position.	^ result! !

!PPEpsilonParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:49'!
parseOn: aStream	^ nil! !

!PPExpressionParser methodsFor: 'private' stamp: 'FirstnameLastname 11/26/2009 20:48'!
build: aParser left: aChoiceParser	^ (aParser separatedBy: aChoiceParser) foldLeft: [ :a :op :b | op first value: a value: op second value: b ]! !

!PPExpressionParser methodsFor: 'private' stamp: 'lr 12/4/2009 17:38'!
build: aParser postfix: aChoiceParser	^ aParser , aChoiceParser star map: [ :term :ops | ops inject: term into: [ :result :operator | operator first value: result value: operator second ] ]! !

!PPExpressionParser methodsFor: 'private' stamp: 'lr 12/4/2009 17:39'!
build: aParser prefix: aChoiceParser	^ aChoiceParser star , aParser map: [ :ops :term | ops reversed inject: term into: [ :result :operator | operator first value: operator second value: result ] ]! !

!PPExpressionParser methodsFor: 'private' stamp: 'FirstnameLastname 11/26/2009 20:48'!
build: aParser right: aChoiceParser	^ (aParser separatedBy: aChoiceParser) foldRight: [ :a :op :b | op first value: a value: op second value: b ]! !

!PPExpressionParser methodsFor: 'private' stamp: 'FirstnameLastname 11/26/2009 21:15'!
buildOn: aParser	^ self buildSelectors inject: aParser into: [ :term :selector |		| list |		list := operators at: selector ifAbsent: [ #() ].		list isEmpty			ifTrue: [ term ]			ifFalse: [				self					perform: selector with: term 					with: (list size = 1						ifTrue: [ list first first ==> [ :operator | Array with: list first second with: operator ] ]						ifFalse: [ 							list								inject: PPChoiceParser new								into: [ :choice :each | choice / (each first ==> [ :operator | Array with: each second with: operator ]) ] ]) ] ]! !

!PPExpressionParser methodsFor: 'private' stamp: 'FirstnameLastname 11/26/2009 20:48'!
buildSelectors	^ #(build:prefix: build:postfix: build:right: build:left:)! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'lr 2/7/2010 23:20'!
group: aOneArgumentBlock	"Defines a priority group by evaluating aOneArgumentBlock."		operators := Dictionary new.	parser := [ 		aOneArgumentBlock value: self.	 	self buildOn: parser ]			ensure: [ operators := nil ]! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'FirstnameLastname 11/26/2009 20:49'!
left: aParser do: aThreeArgumentBlock	"Define an operator aParser that is left-associative. Evaluate aThreeArgumentBlock with the first term, the operator, and the second term."		self operator: #build:left: parser: aParser do: aThreeArgumentBlock! !

!PPExpressionParser methodsFor: 'private' stamp: 'lr 2/7/2010 23:23'!
operator: aSymbol parser: aParser do: aBlock	parser isNil		ifTrue: [ ^ self error: 'You did not specify a term when creating the receiver.' ].	operators isNil		ifTrue: [ ^ self error: 'Use #group: to define precedence groups in descending order.' ].	(operators at: aSymbol ifAbsentPut: [ OrderedCollection new ])		addLast: (Array with: aParser asParser with: aBlock)! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'FirstnameLastname 11/26/2009 20:49'!
postfix: aParser do: aTwoArgumentBlock	"Define a postfix operator aParser. Evaluate aTwoArgumentBlock with the term and the operator."	self operator: #build:postfix: parser: aParser do: aTwoArgumentBlock! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'FirstnameLastname 11/26/2009 20:49'!
prefix: aParser do: aTwoArgumentBlock	"Define a prefix operator aParser. Evaluate aTwoArgumentBlock with the operator and the term."	self operator: #build:prefix: parser: aParser do: aTwoArgumentBlock! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'FirstnameLastname 11/26/2009 20:49'!
right: aParser do: aThreeArgumentBlock	"Define an operator aParser that is right-associative. Evaluate aThreeArgumentBlock with the first term, the operator, and the second term."		self operator: #build:right: parser: aParser do: aThreeArgumentBlock! !

!PPExpressionParser methodsFor: 'specifying' stamp: 'FirstnameLastname 11/26/2009 21:26'!
term: aParser	"Defines the initial term aParser of the receiver."		parser isNil		ifTrue: [ parser := aParser ]		ifFalse: [ self error: 'Unable to redefine the term.' ]! !

!PPFailingParser methodsFor: 'accessing' stamp: 'lr 4/30/2010 11:10'!
message	"Answer the error message of the receiving parser."	^ message! !

!PPFailingParser methodsFor: 'parsing' stamp: 'lr 5/5/2010 13:57'!
parseOn: aStream	^ PPFailure message: message at: aStream position! !

!PPFailingParser methodsFor: 'printing' stamp: 'lr 4/16/2010 21:27'!
printNameOn: aStream	super printNameOn: aStream.	aStream nextPutAll: ', '; print: message! !

!PPFailingParser methodsFor: 'initialization' stamp: 'lr 5/2/2010 19:16'!
setMessage: aString	message := aString! !

!PPFailingParser class methodsFor: 'instance creation' stamp: 'lr 5/2/2010 19:16'!
message: aString	^ self new setMessage: aString! !

!PPFailure methodsFor: 'initialization' stamp: 'lr 5/5/2010 13:55'!
initializeMessage: aString at: anInteger	message := aString.	position := anInteger! !

!PPFailure methodsFor: 'testing' stamp: 'lr 2/7/2010 20:54'!
isPetitFailure	"I am the only class that should implement this method to return true."	^ true! !

!PPFailure methodsFor: 'accessing' stamp: 'lr 5/5/2010 13:56'!
message	"Answer a human readable error message of this parse failure."		^ message! !

!PPFailure methodsFor: 'accessing' stamp: 'lr 5/5/2010 13:55'!
position	"Answer the position in the source string that caused this parse failure."	^ position! !

!PPFailure methodsFor: 'printing' stamp: 'lr 5/5/2010 14:01'!
printOn: aStream	aStream nextPutAll: self message; nextPutAll: ' at '; print: position! !

!PPFailure class methodsFor: 'instance creation' stamp: 'lr 5/5/2010 13:56'!
message: aString at: anInteger	^ self basicNew initializeMessage: aString at: anInteger! !

!PPFlattenParser methodsFor: 'private' stamp: 'lr 6/16/2008 10:10'!
create: aCollection start: aStartInteger stop: aStopInteger	^ aCollection copyFrom: aStartInteger to: aStopInteger! !

!PPFlattenParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:11'!
parseOn: aStream	| start element stop |	start := aStream position.	element := parser parseOn: aStream.	element isPetitFailure ifTrue: [		aStream position: start.		^ element ].	stop := aStream position.	^ self create: aStream collection start: start + 1 stop: stop! !

!PPGreedyRepeatingParser methodsFor: 'parsing' stamp: 'lr 4/2/2011 15:54'!
parseOn: aStream	| start element elements positions |	start := aStream position.	elements := OrderedCollection new.	[ elements size < min ] whileTrue: [ 		(element := parser parseOn: aStream) isPetitFailure ifTrue: [ 			aStream position: start.			^ element ].		elements addLast: element ].	positions := OrderedCollection with: aStream position.	[ elements size < max and: [ (element := parser parseOn: aStream) isPetitFailure not ] ] whileTrue: [		elements addLast: element.		positions addLast: aStream position ].	[ positions isEmpty ] whileFalse: [		aStream position: positions last.		element := limit parseOn: aStream.		element isPetitFailure ifFalse: [			aStream position: positions last.			^ elements asArray ].		elements isEmpty ifTrue: [			aStream position: start.			^ element ].		elements removeLast.		positions removeLast ].	aStream position: start.	^ PPFailure message: 'overflow' at: start! !

!PPLazyRepeatingParser methodsFor: 'parsing' stamp: 'lr 4/2/2011 10:14'!
parseOn: aStream	| start element elements |	start := aStream position.	elements := OrderedCollection new.	[ elements size < min ] whileTrue: [		(element := parser parseOn: aStream) isPetitFailure ifTrue: [			aStream position: start.			^ element ].		elements addLast: element ].	[ self matchesLimitOn: aStream ] whileFalse: [		elements size < max ifFalse: [			aStream position: start.			^ PPFailure message: 'overflow' at: start ].		element := parser parseOn: aStream.		element isPetitFailure ifTrue: [			aStream position: start.			^ element ].		elements addLast: element ].	^ elements asArray! !

!PPLimitedRepeatingParser methodsFor: 'accessing' stamp: 'lr 4/4/2011 18:46'!
children	^ Array with: parser with: limit! !

!PPLimitedRepeatingParser methodsFor: 'accessing' stamp: 'lr 4/2/2011 10:00'!
limit	"Answer the parser that limits (or ends) this repetition."		^ limit! !

!PPLimitedRepeatingParser methodsFor: 'private' stamp: 'lr 4/2/2011 10:10'!
matchesLimitOn: aStream	| element position |	position := aStream position.	element := limit parseOn: aStream.	aStream position: position.	^ element isPetitFailure not! !

!PPLimitedRepeatingParser methodsFor: 'initialization' stamp: 'lr 4/2/2011 10:00'!
setLimit: aParser	limit := aParser! !

!PPLimitedRepeatingParser class methodsFor: 'instance creation' stamp: 'lr 4/3/2011 14:58'!
on: aParser limit: aLimitParser	^ (self on: aParser) setLimit: aLimitParser! !

!PPListParser methodsFor: 'accessing' stamp: 'lr 10/21/2009 16:37'!
children	^ parsers! !

!PPListParser methodsFor: 'copying' stamp: 'lr 9/17/2008 22:36'!
copyWith: aParser	^ self species withAll: (parsers copyWith: aParser)! !

!PPListParser methodsFor: 'initialization' stamp: 'lr 4/29/2010 10:12'!
initialize	super initialize.	self setParsers: #()! !

!PPListParser methodsFor: 'copying' stamp: 'lr 5/22/2010 10:26'!
postCopy	super postCopy.	parsers := parsers copy! !

!PPListParser methodsFor: 'initialization' stamp: 'lr 4/29/2010 10:12'!
setParsers: aCollection	parsers := aCollection asArray! !

!PPListParser class methodsFor: 'instance creation' stamp: 'lr 5/3/2010 20:26'!
with: aParser	^ self withAll: (Array with: aParser)! !

!PPListParser class methodsFor: 'instance creation' stamp: 'lr 9/23/2008 18:32'!
with: aFirstParser with: aSecondParser	^ self withAll: (Array with: aFirstParser with: aSecondParser)! !

!PPListParser class methodsFor: 'instance creation' stamp: 'lr 4/29/2010 10:12'!
withAll: aCollection	^ self basicNew setParsers: aCollection! !

!PPLiteralObjectParser methodsFor: 'operators' stamp: 'pmon 5/31/2012 23:48'!
caseInsensitive
	"Answer a parser that can parse the receiver case-insensitive."
	
	literal asUppercase = literal asLowercase ifTrue: [ ^ self ].
	^ PPPredicateObjectParser on: [ :value | literal asLowercase =  value asLowercase ] message: message! !

!PPLiteralObjectParser methodsFor: 'operators' stamp: 'lr 4/28/2011 20:02'!
negate	^ (PPPredicateObjectParser expect: literal message: message) negate! !

!PPLiteralObjectParser methodsFor: 'parsing' stamp: 'lr 10/30/2010 11:48'!
parseOn: aStream	^ (aStream atEnd not and: [ literal = aStream uncheckedPeek ])		ifFalse: [ PPFailure message: message at: aStream position ]		ifTrue: [ aStream next ]! !

!PPLiteralParser methodsFor: 'operators' stamp: 'lr 6/1/2010 22:24'!
caseInsensitive	"Answer a parser that can parse the receiver case-insensitive."		self subclassResponsibility! !

!PPLiteralParser methodsFor: 'initialization' stamp: 'lr 5/2/2010 13:25'!
initializeOn: anObject message: aString	literal := anObject.	message := aString! !

!PPLiteralParser methodsFor: 'accessing' stamp: 'lr 5/2/2010 13:26'!
literal	"Answer the parsed literal."	^ literal! !

!PPLiteralParser methodsFor: 'accessing' stamp: 'lr 5/2/2010 13:26'!
message	"Answer the failure message."		^ message! !

!PPLiteralParser methodsFor: 'printing' stamp: 'lr 4/16/2010 16:38'!
printNameOn: aStream	super printNameOn: aStream.	aStream nextPutAll: ', '; print: literal! !

!PPLiteralParser class methodsFor: 'instance creation' stamp: 'lr 1/7/2010 15:30'!
on: anObject	^ self on: anObject message: anObject printString , ' expected'! !

!PPLiteralParser class methodsFor: 'instance creation' stamp: 'lr 1/7/2010 15:29'!
on: anObject message: aString	^ self new initializeOn: anObject message: aString! !

!PPLiteralSequenceParser methodsFor: 'operators' stamp: 'lr 8/18/2010 20:16'!
caseInsensitive	"Answer a parser that can parse the receiver case-insensitive."		literal asUppercase = literal asLowercase ifTrue: [ ^ self ].	^ PPPredicateSequenceParser on: [ :value | literal sameAs: value ] message: message size: size! !

!PPLiteralSequenceParser methodsFor: 'initialization' stamp: 'lr 6/1/2010 22:21'!
initializeOn: anObject message: aString	super initializeOn: anObject message: aString.	size := literal size! !

!PPLiteralSequenceParser methodsFor: 'parsing' stamp: 'lr 10/30/2010 11:48'!
parseOn: aStream	| position result |	position := aStream position.	result := aStream next: size.	literal = result ifTrue: [ ^ result ].	aStream position: position.	^ PPFailure message: message at: aStream position! !

!PPLiteralSequenceParser methodsFor: 'accessing' stamp: 'lr 9/15/2010 11:16'!
size	"Answer the sequence size of the receiver."	^ size! !

!PPMemento methodsFor: 'accessing-readonly' stamp: 'lr 4/22/2008 18:23'!
count	^ count! !

!PPMemento methodsFor: 'actions' stamp: 'lr 4/22/2008 18:20'!
increment	count := count + 1! !

!PPMemento methodsFor: 'initialization' stamp: 'lr 4/22/2008 18:21'!
initialize	count := 0	! !

!PPMemento methodsFor: 'accessing' stamp: 'lr 4/22/2008 18:23'!
position	^ position! !

!PPMemento methodsFor: 'accessing' stamp: 'lr 4/26/2008 15:48'!
position: anInteger	position := anInteger! !

!PPMemento methodsFor: 'accessing' stamp: 'lr 4/24/2008 10:15'!
result	^ result! !

!PPMemento methodsFor: 'accessing' stamp: 'lr 4/22/2008 18:23'!
result: anObject	result := anObject! !

!PPMemento class methodsFor: 'instance creation' stamp: 'lr 4/22/2008 18:21'!
new	^ self basicNew initialize! !

!PPMemoizedParser methodsFor: 'operators' stamp: 'lr 4/2/2009 19:48'!
memoized	"Ther is no point in memoizing more than once."	^ self! !

!PPMemoizedParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:11'!
parseOn: aStream	| memento |	stream == aStream		ifFalse: [ self reset: aStream ].	memento := (buffer at: stream position + 1)		ifNil: [ buffer at: stream position + 1 put: PPMemento new ].	memento position isNil		ifTrue: [			memento result: (stream size - stream position + 2 < memento count				ifTrue: [ PPFailure message: 'overflow' at: stream position ]				ifFalse: [ memento increment. parser parseOn: stream ]).			memento position: stream position ]		ifFalse: [ stream position: memento position ].	^ memento result! !

!PPMemoizedParser methodsFor: 'private' stamp: 'lr 4/2/2009 19:22'!
reset: aStream	stream := aStream.	buffer := Array new: aStream size + 1! !

!PPNotParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:11'!
parseOn: aStream	| element position |	position := aStream position.	element := parser parseOn: aStream.	aStream position: position.	^ element isPetitFailure		ifFalse: [ PPFailure message: '' at: aStream position ]! !

!PPOptionalParser methodsFor: 'parsing' stamp: 'lr 8/14/2011 11:47'!
parseOn: aStream	| element |	element := parser parseOn: aStream.	^ element isPetitFailure ifFalse: [ element ]! !

!PPParser methodsFor: 'operators' stamp: 'lr 9/23/2008 18:32'!
, aParser 	"Answer a new parser that parses the receiver followed by aParser."	^ PPSequenceParser with: self with: aParser! !

!PPParser methodsFor: 'operators' stamp: 'lr 4/14/2010 11:46'!
/ aParser 	"Answer a new parser that parses the receiver, if the receiver fails try with aParser (ordered-choice)."		^ PPChoiceParser with: self with: aParser! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 5/12/2010 20:32'!
==> aBlock	"Answer a new parser that performs aBlock as action handler on success."	^ PPActionParser on: self block: aBlock! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 6/12/2010 10:20'!
>=> aBlock	"Answer a new parser that wraps the receiving parser with a two argument block. The first argument is the parsed stream, the second argument a continuation block on the delegate parser."	^ PPWrappingParser on: self block: aBlock! !

!PPParser methodsFor: 'operators' stamp: 'lr 5/31/2010 15:12'!
and	"Answer a new parser (logical and-predicate) that succeeds whenever the receiver does, but never consumes input."	^ PPAndParser on: self! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 2/19/2010 07:42'!
answer: anObject	"Answer a new parser that always returns anObject from a successful parse."	^ self ==> [ :nodes | anObject ]! !

!PPParser methodsFor: 'converting' stamp: 'lr 11/29/2011 20:48'!
asParser	"Answer the receiving parser."		^ self! !

!PPParser methodsFor: 'accessing' stamp: 'lr 10/21/2009 16:38'!
children	"Answer a set of child parsers that could follow the receiver."	^ #()! !

!PPParser methodsFor: 'operators' stamp: 'lr 12/3/2010 11:34'!
def: aParser	"Redefine the receiver as the argument aParser. This method is useful when defining recursive parsers: instantiate a PPUnresolvedParser and later redefine it with another one."	^ self becomeForward: (aParser name: self name)! !

!PPParser methodsFor: 'operators-convenience' stamp: 'lr 2/19/2010 07:42'!
delimitedBy: aParser	"Answer a new parser that parses the receiver one or more times, separated and possibly ended by aParser."		^ (self separatedBy: aParser) , (aParser optional) ==> [ :node |		node second isNil			ifTrue: [ node first ]			ifFalse: [ node first copyWith: node second ] ]! !

!PPParser methodsFor: 'operators' stamp: 'lr 4/30/2010 12:13'!
end	"Answer a new parser that succeeds at the end of the input and return the result of the receiver."	^ PPEndOfInputParser on: self! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 5/15/2008 16:08'!
flatten	"Answer a new parser that flattens the underlying collection."		^ PPFlattenParser on: self! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 4/3/2011 15:00'!
foldLeft: aBlock	"Answer a new parser that that folds the result of the receiver from left-to-right into aBlock. The argument aBlock must take two or more arguments."		| size args |	size := aBlock numArgs.	args := Array new: size.	^ self ==> [ :nodes |		args at: 1 put: nodes first.		2 to: nodes size by: size - 1 do: [ :index |			args				replaceFrom: 2 to: size with: nodes startingAt: index;				at: 1 put: (aBlock valueWithArguments: args) ].		args first ]! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 4/3/2011 14:59'!
foldRight: aBlock	"Answer a new parser that that folds the result of the receiver from right-to-left into aBlock. The argument aBlock must take two or more arguments."	| size args |	size := aBlock numArgs.	args := Array new: size.	^ self ==> [ :nodes |		args at: size put: nodes last.		nodes size - size + 1 to: 1 by: 1 - size do: [ :index |			args				replaceFrom: 1 to: size - 1 with: nodes startingAt: index;				at: size put: (aBlock valueWithArguments: args) ].		args at: size ]! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:32'!
hasProperty: aKey	"Test if the property aKey is present."		^ properties notNil and: [ properties includesKey: aKey ]! !

!PPParser methodsFor: 'initialization' stamp: 'lr 4/24/2008 10:33'!
initialize! !

!PPParser methodsFor: 'testing' stamp: 'lr 8/6/2010 16:44'!
isPetitParser	^ true! !

!PPParser methodsFor: 'testing' stamp: 'lr 10/27/2008 11:28'!
isUnresolved	^ false! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 5/6/2011 20:28'!
map: aBlock	"Answer a new parser that works on the receiving sequence an passes in each element as a block argument."		^ aBlock numArgs = 1		ifTrue: [ self ==> aBlock ]		ifFalse: [ self error: aBlock numArgs asString , ' arguments expected.' ]! !

!PPParser methodsFor: 'parsing' stamp: 'lr 2/8/2010 00:30'!
matches: anObject	"Answer if anObject can be parsed by the receiver."		^ (self parse: anObject) isPetitFailure not! !

!PPParser methodsFor: 'parsing' stamp: 'lr 6/4/2011 18:12'!
matchesIn: anObject	"Search anObject repeatedly for the matches of the receiver. Answered an OrderedCollection of the matched parse-trees."	| result |	result := OrderedCollection new.	self 		matchesIn: anObject		do: [ :each | result addLast: each ].	^ result! !

!PPParser methodsFor: 'parsing' stamp: 'lr 3/1/2010 21:51'!
matchesIn: anObject do: aBlock	"Search anObject repeatedly for the matches of the receiver. Evaluate aBlock for each match with the matched parse-tree as the argument. Make sure to always consume exactly one character with each step, to not miss any match."	((self and ==> aBlock , #any asParser) / #any asParser) star parse: anObject! !

!PPParser methodsFor: 'parsing' stamp: 'lr 8/16/2011 07:26'!
matchesSkipIn: anObject	"Search anObject repeatedly for the matches of the receiver. Answer an OrderedCollection of the matched parse-trees. Skip over matches."	| result |	result := OrderedCollection new.	self 		matchesSkipIn: anObject		do: [ :each | result addLast: each ].	^ result! !

!PPParser methodsFor: 'parsing' stamp: 'lr 8/16/2011 07:26'!
matchesSkipIn: anObject do: aBlock	"Search anObject repeatedly for the matches of the receiver. Evaluate aBlock for each match with the matched parse-tree as the argument. Skip over matches."	(self ==> aBlock / #any asParser) star parse: anObject! !

!PPParser methodsFor: 'parsing' stamp: 'lr 6/4/2011 18:12'!
matchingRangesIn: anObject	"Search anObject repeatedly for the matches of the receiver. Answer an OrderedCollection of ranges of each match (index of first character to: index of last character)."		| result |	result := OrderedCollection new.	self		matchingRangesIn: anObject		do: [ :value | result addLast: value ].	^ result! !

!PPParser methodsFor: 'parsing' stamp: 'lr 6/4/2011 18:11'!
matchingRangesIn: anObject do: aBlock	"Search anObject repeatedly for the matches of the receiver. Evaluate aBlock with the range of each match (index of first character to: index of last character)."		[ :stream | stream position + 1 ] asParser , self , [ :stream | stream position ] asParser		matchesIn: anObject		do: [ :value | aBlock value: (value first to: value last) ]! !

!PPParser methodsFor: 'parsing' stamp: 'DamienCassou 10/29/2011 19:18'!
matchingSkipRangesIn: anObject	"Search anObject repeatedly for the matches of the receiver. Skip over matches. Answer an OrderedCollection of ranges of each match (index of first character to: index of last character)."		| result |	result := OrderedCollection new.	self		matchingSkipRangesIn: anObject		do: [ :value | result addLast: value ].	^ result! !

!PPParser methodsFor: 'parsing' stamp: 'DamienCassou 10/29/2011 19:19'!
matchingSkipRangesIn: anObject do: aBlock	"Search anObject repeatedly for the matches of the receiver. Skip over matches. Evaluate aBlock with the range of each match (index of first character to: index of last character)."		[ :stream | stream position + 1 ] asParser , self , [ :stream | stream position ] asParser		matchesSkipIn: anObject		do: [ :value | aBlock value: (value first to: value last) ]! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:03'!
max: anInteger	"Answer a new parser that parses the receiver at most anInteger times."		^ self star setMax: anInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:56'!
max: anInteger greedy: aParser	"Answer a new parser that parses the receiver at most anInteger times until it reaches aParser. This is a greedy non-blind implementation. aParser is not consumed."		^ (self starGreedy: aParser) setMax: anInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:57'!
max: anInteger lazy: aParser	"Answer a new parser that parses the receiver at most anInteger times until it reaches aParser. This is a lazy non-blind implementation. aParser is not consumed."		^ (self starLazy: aParser) setMax: anInteger! !

!PPParser methodsFor: 'operators' stamp: 'lr 5/31/2010 16:34'!
memoized	"Answer a new memoized parser, for refraining redundant computations. This ensures polynomial time O(n^4) for left-recursive grammars and O(n^3) for non left-recursive grammars in the worst case. Not necessary for most grammars that are carefully written and in O(n) anyway."		^ PPMemoizedParser on: self! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:02'!
min: anInteger	"Answer a new parser that parses the receiver at least anInteger times."		^ self star setMin: anInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:56'!
min: anInteger greedy: aParser	"Answer a new parser that parses the receiver at least anInteger times until it reaches aParser. This is a greedy non-blind implementation. aParser is not consumed."		^ (self starGreedy: aParser) setMin: anInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:57'!
min: anInteger lazy: aParser	"Answer a new parser that parses the receiver at least anInteger times until it reaches aParser. This is a lazy non-blind implementation. aParser is not consumed."		^ (self starLazy: aParser) setMin: anInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:03'!
min: aMinInteger max: aMaxInteger	"Answer a new parser that parses the receiver at least aMinInteger and at most aMaxInteger times."		^ self star setMin: aMinInteger; setMax: aMaxInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:56'!
min: aMinInteger max: aMaxInteger greedy: aParser	"Answer a new parser that parses the receiver at least aMinInteger and at most aMaxInteger times until it reaches aParser. This is a greedy non-blind implementation. aParser is not consumed."		^ (self starGreedy: aParser) setMin: aMinInteger; setMax: aMaxInteger! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/3/2011 14:57'!
min: aMinInteger max: aMaxInteger lazy: aParser	"Answer a new parser that parses the receiver at least aMinInteger and at most aMaxInteger times until it reaches aParser. This is a greedy non-blind implementation. aParser is not consumed."		^ (self starLazy: aParser) setMin: aMinInteger; setMax: aMaxInteger! !

!PPParser methodsFor: 'accessing' stamp: 'lr 4/19/2010 10:35'!
name	"Answer the production name of the receiver."		^ self propertyAt: #name ifAbsent: [ nil ]! !

!PPParser methodsFor: 'accessing' stamp: 'lr 4/19/2010 10:38'!
name: aString	self propertyAt: #name put: aString! !

!PPParser methodsFor: 'operators' stamp: 'lr 2/19/2010 07:36'!
negate	"Answer a new parser consumes any input token but the receiver."		^ self not , #any asParser ==> #second! !

!PPParser methodsFor: 'operators' stamp: 'lr 5/31/2010 15:12'!
not	"Answer a new parser (logical not-predicate) that succeeds whenever the receiver fails, but never consumes input."	^ PPNotParser on: self! !

!PPParser methodsFor: 'operators' stamp: 'lr 9/1/2010 22:03'!
optional	"Answer a new parser that parses the receiver, if possible."	^ PPOptionalParser on: self! !

!PPParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:53'!
parse: anObject	"Parse anObject with the receiving parser and answer the parse-result or an instance of PPFailure."		^ self parseOn: anObject asPetitStream! !

!PPParser methodsFor: 'parsing' stamp: 'lr 10/29/2010 17:05'!
parse: anObject onError: aBlock	"Parse anObject with the receiving parser and answer the parse-result or answer the result of evaluating aBlock. Depending on the number of arguments of the block it is simply evaluated, evaluated with the failure object, or evaluated with the error message and position."		| result |	result := self parse: anObject.	result isPetitFailure		ifFalse: [ ^ result ].	aBlock numArgs = 0		ifTrue: [ ^ aBlock value ].	aBlock numArgs = 1		ifTrue: [ ^ aBlock value: result ].	^ aBlock value: result message value: result position! !

!PPParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 22:18'!
parseOn: aStream	"Parse aStream with the receiving parser and answer the parse-result or an instance of PPFailure. Override this method in subclasses to specify custom parse behavior. Do not call this method from outside, instead use #parse:."		self subclassResponsibility! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:03'!
plus	"Answer a new parser that parses the receiver one or more times."	^ self star setMin: 1! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:04'!
plusGreedy: aParser	"Answer a new parser that parses the receiver one or more times until it reaches aParser. This is a greedy non-blind implementation of the star operator. aParser is not consumed."		^ (self starGreedy: aParser) setMin: 1! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/1/2011 21:04'!
plusLazy: aParser	"Answer a new parser that parses the receiver one or more times until it reaches aParser. This is a lazy non-blind implementation of the star operator. aParser is not consumed."		^ (self starLazy: aParser) setMin: 1! !

!PPParser methodsFor: 'copying' stamp: 'lr 4/19/2010 10:33'!
postCopy	super postCopy.	properties := properties copy! !

!PPParser methodsFor: 'printing' stamp: 'lr 4/16/2010 16:36'!
printNameOn: aStream	self name isNil		ifTrue: [ aStream print: self hash ]		ifFalse: [ aStream nextPutAll: self name ]! !

!PPParser methodsFor: 'printing' stamp: 'lr 4/16/2010 16:36'!
printOn: aStream	super printOn: aStream.	aStream nextPut: $(.	self printNameOn: aStream.	aStream nextPut: $)! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:32'!
propertyAt: aKey	"Answer the property value associated with aKey."		^ self propertyAt: aKey ifAbsent: [ self error: 'Property not found' ]! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:32'!
propertyAt: aKey ifAbsent: aBlock	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."		^ properties isNil		ifTrue: [ aBlock value ]		ifFalse: [ properties at: aKey ifAbsent: aBlock ]! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:32'!
propertyAt: aKey ifAbsentPut: aBlock	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."		^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:33'!
propertyAt: aKey put: anObject	"Set the property at aKey to be anObject. If aKey is not found, create a new entry for aKey and set is value to anObject. Answer anObject."	^ (properties ifNil: [ properties := Dictionary new: 1 ])		at: aKey put: anObject! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:33'!
removeProperty: aKey	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."		^ self removeProperty: aKey ifAbsent: [ self error: 'Property not found' ]! !

!PPParser methodsFor: 'accessing-properties' stamp: 'lr 4/19/2010 10:33'!
removeProperty: aKey ifAbsent: aBlock	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."		| answer |	properties isNil ifTrue: [ ^ aBlock value ].	answer := properties removeKey: aKey ifAbsent: aBlock.	properties isEmpty ifTrue: [ properties := nil ].	^ answer! !

!PPParser methodsFor: 'operators-convenience' stamp: 'lr 2/19/2010 07:56'!
separatedBy: aParser	"Answer a new parser that parses the receiver one or more times, separated by aParser."		^ (PPSequenceParser with: self with: (PPSequenceParser with: aParser with: self) star) ==> [ :nodes |		| result |		result := Array new: 2 * nodes second size + 1.		result at: 1 put: nodes first.		nodes second 			keysAndValuesDo: [ :index :pair | result replaceFrom: 2 * index to: 2 * index + 1 with: pair startingAt: 1 ].		result ]! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/2/2011 10:02'!
star	"Answer a new parser that parses the receiver zero or more times. This is a greedy and blind implementation that tries to consume as much input as possible and it does not consider what comes afterwards."	^ PPPossessiveRepeatingParser on: self! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/2/2011 10:01'!
starGreedy: aParser	"Answer a new parser that parses the receiver zero or more times until it reaches aParser. This is a greedy non-blind implementation of the star operator. aParser is not consumed."		^ PPGreedyRepeatingParser on: self limit: aParser! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 4/2/2011 10:01'!
starLazy: aParser	"Answer a new parser that parses the receiver zero or more times until it reaches aParser. This is a lazy non-blind implementation of the star operator. aParser is not consumed."		^ PPLazyRepeatingParser on: self limit: aParser! !

!PPParser methodsFor: 'operators-repeating' stamp: 'lr 9/15/2010 09:34'!
times: anInteger	"Answer a new parser that parses the receiver exactly anInteger times."		^ self min: anInteger max: anInteger! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 6/29/2010 14:25'!
token	"Answer a new parser that transforms the input to a token."		^ PPTokenParser on: self! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 4/6/2010 19:26'!
token: aTokenClass	"Answer a new parser that transforms the input to a token of class aTokenClass."		^ self token tokenClass: aTokenClass! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 7/31/2010 12:06'!
trim	"Answer a new parser that consumes spaces before and after the receiving parser."		^ self trimSpaces! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 7/11/2011 11:03'!
trim: aParser	"Answer a new parser that consumes and ignores aParser repeatedly before and after the receiving parser."		^ PPTrimmingParser on: self trimmer: aParser! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 7/11/2011 11:03'!
trimBlanks	"Answer a new parser that consumes blanks before and after the receiving parser."		^ self trim: #blank asParser! !

!PPParser methodsFor: 'operators-mapping' stamp: 'lr 7/11/2011 11:03'!
trimSpaces	"Answer a new parser that consumes spaces before and after the receiving parser."		^ self trim: #space asParser! !

!PPParser methodsFor: 'operators-convenience' stamp: 'lr 2/25/2012 16:54'!
withoutSeparators	"Filters out the separators from a parse result produced by one of the productions #delimitedBy: or #separatedBy:."		^ self ==> [ :items |		| result |		result := Array new: items size + 1 // 2.		1 to: result size do: [ :index | result at: index put: (items at: 2 * index - 1) ].		result ]! !

!PPParser methodsFor: 'operators' stamp: 'lr 10/23/2008 14:05'!
wrapped	"Answer a new parser that is simply wrapped."		^ PPDelegateParser on: self! !

!PPParser methodsFor: 'operators' stamp: 'lr 4/14/2010 11:53'!
| aParser	"Answer a new parser that either parses the receiver or aParser. Fail if both pass or fail (exclusive choice, unordered choice)."	^ (self not , aParser) / (aParser not , self) ==> #second! !

!PPParser class methodsFor: 'instance creation' stamp: 'lr 10/27/2008 11:17'!
named: aString	^ self new name: aString! !

!PPParser class methodsFor: 'instance creation' stamp: 'lr 4/18/2008 14:00'!
new	^ self basicNew initialize! !

!PPPluggableParser methodsFor: 'accessing' stamp: 'lr 4/30/2010 11:10'!
block	"Answer the pluggable block."	^ block! !

!PPPluggableParser methodsFor: 'initialization' stamp: 'lr 5/2/2010 16:52'!
initializeOn: aBlock	block := aBlock! !

!PPPluggableParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:54'!
parseOn: aStream	| position result |	position := aStream position.	result := block value: aStream.	result isPetitFailure		ifTrue: [ aStream position: position ].	^ result! !

!PPPluggableParser class methodsFor: 'instance creation' stamp: 'lr 5/2/2010 16:52'!
on: aBlock	^ self new initializeOn: aBlock! !

!PPPossessiveRepeatingParser methodsFor: 'parsing' stamp: 'lr 4/2/2011 09:52'!
parseOn: aStream	| start element elements |	start := aStream position.	elements := OrderedCollection new.	[ elements size < min ] whileTrue: [		(element := parser parseOn: aStream) isPetitFailure ifTrue: [			aStream position: start.			^ element ].		elements addLast: element ].	[ elements size < max ] whileTrue: [	 	(element := parser parseOn: aStream) isPetitFailure			ifTrue: [ ^ elements asArray ].		elements addLast: element ].	^ elements asArray! !

!PPPredicateObjectParser methodsFor: 'initialization' stamp: 'lr 6/12/2010 09:12'!
initializeOn: aBlock message: aString negated: aNegatedBlock message: aNegatedString	predicate := aBlock.	predicateMessage := aString.	negated := aNegatedBlock.	negatedMessage := aNegatedString! !

!PPPredicateObjectParser methodsFor: 'operators' stamp: 'lr 6/12/2010 09:12'!
negate	"Answer a parser that is the negation of the receiving predicate parser."		^ self class 		on: negated message: negatedMessage 		negated: predicate message: predicateMessage! !

!PPPredicateObjectParser methodsFor: 'parsing' stamp: 'lr 9/30/2010 11:05'!
parseOn: aStream	^ (aStream atEnd not and: [ predicate value: aStream uncheckedPeek ])		ifFalse: [ PPFailure message: predicateMessage at: aStream position ]		ifTrue: [ aStream next ]! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 6/12/2010 09:10'!
any	^ self		on: [ :each | true ] message: 'input expected'		negated: [ :each | false ] message: 'no input expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 4/1/2011 20:05'!
anyExceptAnyOf: aCollection	^ self		on: [ :each | (aCollection includes: each) not ] message: 'any except ' , aCollection printString , ' expected'		negated: [ :each | aCollection includes: each ] message: aCollection printString ,  ' not expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 4/1/2011 20:05'!
anyOf: aCollection	^ self		on: [ :each | aCollection includes: each ] message: 'any of ' , aCollection printString , ' expected'		negated: [ :each | (aCollection includes: each) not ] message: 'none of ' , aCollection printString ,  'expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 6/12/2010 09:10'!
between: min and: max	^ self		on: [ :each | each >= min and: [ each <= max ] ] message: min printString , '..' , max printString , ' expected'		negated: [ :each | each < min or: [ each > max ] ] message: min printString , '..' , max printString , ' not expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:02'!
blank	^ self chars: (String with: Character space with: Character tab) message: 'blank expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:02'!
char: aCharacter	^ self expect: aCharacter message: (String with: $" with: aCharacter with: $") , ' expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 10:57'!
char: aCharacter message: aString	^ self expect: aCharacter message: aString! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
chars: aCollection message: aString	^ self on: (PPCharSetPredicate on: [ :char | aCollection includes: char ]) message: aString! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'pmon 5/31/2012 23:56'!
cr
	^ self char: Character crCharacter message: 'carriage return expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
digit	^ self on: (PPCharSetPredicate on: [ :char | char isDigit ]) message: 'digit expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 8/25/2010 10:57'!
expect: anObject	^ self expect: anObject message: anObject printString , ' expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-objects' stamp: 'lr 8/25/2010 10:57'!
expect: anObject message: aString	^ self 		on: [ :each | each = anObject ] message: aString		negated: [ :each | each ~= anObject ] message: 'no ' , aString! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
hex	^ self 		on: (PPCharSetPredicate on: [ :char | 			(char between: $0 and: $9) 				or: [ (char between: $a and: $f) 				or: [ (char between: $A and: $F) ] ] ])		message: 'hex digit expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:05'!
letter	^ self on: (PPCharSetPredicate on: [ :char | char isLetter ]) message: 'letter expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'pmon 5/31/2012 23:54'!
lf
	^ self char: Character lfCharacter! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
lowercase	^ self on: (PPCharSetPredicate on: [ :char | char isLowercase ]) message: 'lowercase letter expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'pmon 5/31/2012 23:57'!
newline
	^ self chars: (String with: Character crCharacter with: Character lfCharacter) message: 'newline expected'! !

!PPPredicateObjectParser class methodsFor: 'instance creation' stamp: 'lr 6/12/2010 09:10'!
on: aBlock message: aString	^ self on: aBlock message: aString negated: [ :each | (aBlock value: each) not ] message: 'no ' , aString! !

!PPPredicateObjectParser class methodsFor: 'instance creation' stamp: 'lr 6/12/2010 09:10'!
on: aBlock message: aString negated: aNegatedBlock message: aNegatedString	^ self new initializeOn: aBlock message: aString negated: aNegatedBlock message: aNegatedString! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:04'!
punctuation	^ self chars: '.,"''?!!;:#$%&()*+-/<>=@[]\^_{}|~' message: 'punctuation expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
space	^ self on: (PPCharSetPredicate on: [ :char | char isSeparator ]) message: 'separator expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:04'!
tab	^ self char: Character tab message: 'tab expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
uppercase	^ self on: (PPCharSetPredicate on: [ :char | char isUppercase ]) message: 'uppercase letter expected'! !

!PPPredicateObjectParser class methodsFor: 'factory-chars' stamp: 'lr 8/25/2010 11:06'!
word	^ self on: (PPCharSetPredicate on: [ :char | char isAlphaNumeric ]) message: 'letter or digit expected'! !

!PPPredicateParser methodsFor: 'accessing' stamp: 'lr 5/2/2010 13:36'!
block	"Answer the predicate block of the receiver."		^ predicate! !

!PPPredicateParser methodsFor: 'accessing' stamp: 'lr 5/2/2010 13:36'!
message	"Answer the failure message."		^ predicateMessage! !

!PPPredicateParser methodsFor: 'printing' stamp: 'lr 5/2/2010 13:37'!
printNameOn: aStream	super printNameOn: aStream.	aStream nextPutAll: ', '; print: predicateMessage! !

!PPPredicateSequenceParser methodsFor: 'initialization' stamp: 'lr 6/12/2010 09:13'!
initializeOn: aBlock message: aString negated: aNegatedBlock message: aNegatedString size: anInteger	predicate := aBlock.	predicateMessage := aString.	negated := aNegatedBlock.	negatedMessage := aNegatedString.	size := anInteger ! !

!PPPredicateSequenceParser methodsFor: 'operators' stamp: 'lr 6/12/2010 09:14'!
negate	"Answer a parser that is the negation of the receiving predicate parser."		^ self class 		on: negated message: negatedMessage		negated: predicate message: predicateMessage		size: size! !

!PPPredicateSequenceParser methodsFor: 'parsing' stamp: 'lr 6/12/2010 09:25'!
parseOn: aStream	| position result |	position := aStream position.	result := aStream next: size.	(result size = size and: [ predicate value: result ])		ifTrue: [ ^ result ].	aStream position: position.	^ PPFailure message: predicateMessage at: aStream position! !

!PPPredicateSequenceParser methodsFor: 'accessing' stamp: 'lr 6/12/2010 08:58'!
size	"Answer the sequence size of the receiver."	^ size! !

!PPPredicateSequenceParser class methodsFor: 'instance creation' stamp: 'lr 6/12/2010 09:14'!
on: aBlock message: aString negated: aNegatedBlock message: aNegatedString size: anInteger 	^ self new initializeOn: aBlock message: aString negated: aNegatedBlock message: aNegatedString size: anInteger! !

!PPPredicateSequenceParser class methodsFor: 'instance creation' stamp: 'lr 6/12/2010 09:14'!
on: aBlock message: aString size: anInteger	^ self on: aBlock message: aString negated: [ :each | (aBlock value: each) not ] message: 'no ' , aString size: anInteger ! !

!PPRepeatingParser methodsFor: 'initialization' stamp: 'lr 4/1/2011 21:06'!
initialize	super initialize.	self setMin: 0; setMax: SmallInteger maxVal! !

!PPRepeatingParser methodsFor: 'accessing' stamp: 'lr 4/30/2010 11:08'!
max	"Answer the maximum number of repetitions."	^ max! !

!PPRepeatingParser methodsFor: 'accessing' stamp: 'lr 4/30/2010 11:08'!
min	"Answer the minimum number of repetitions."		^ min! !

!PPRepeatingParser methodsFor: 'printing' stamp: 'lr 6/3/2010 14:00'!
printOn: aStream	super printOn: aStream.	aStream nextPutAll: ' ['; print: min; nextPutAll: ', '; nextPutAll: (max = SmallInteger maxVal		ifTrue: [ '*' ] ifFalse: [ max printString ]); nextPut: $]! !

!PPRepeatingParser methodsFor: 'initialization' stamp: 'lr 4/1/2011 21:00'!
setMax: anInteger	max := anInteger! !

!PPRepeatingParser methodsFor: 'initialization' stamp: 'lr 4/1/2011 21:01'!
setMin: anInteger	min := anInteger! !

!PPSequenceParser methodsFor: 'operators' stamp: 'lr 9/17/2008 00:17'!
, aRule	^ self copyWith: aRule! !

!PPSequenceParser methodsFor: 'operators-mapping' stamp: 'lr 5/6/2011 20:27'!
map: aBlock	^ aBlock numArgs = self children size		ifTrue: [ self ==> [ :nodes | aBlock valueWithArguments: nodes ] ]		ifFalse: [ self error: aBlock numArgs asString , ' arguments expected.' ]! !

!PPSequenceParser methodsFor: 'parsing' stamp: 'lr 5/6/2010 10:47'!
parseOn: aStream	"This is optimized code that avoids unnecessary block activations, do not change."		| start elements element |	start := aStream position.	elements := Array new: parsers size.	1 to: parsers size do: [ :index |		element := (parsers at: index) 			parseOn: aStream.		element isPetitFailure ifTrue: [			aStream position: start.			^ element ].		elements at: index put: element ].	^ elements! !

!PPSequenceParser methodsFor: 'operators-mapping' stamp: 'lr 1/8/2010 12:01'!
permutation: anArrayOfIntegers	"Answer a permutation of the receivers sequence."		anArrayOfIntegers do: [ :index |		(index isInteger and: [ index between: 1 and: parsers size ])			ifFalse: [ self error: 'Invalid permutation index: ' , index printString ] ].	^ self ==> [ :nodes | anArrayOfIntegers collect: [ :index | nodes at: index ] ]! !

!PPStream methodsFor: 'converting' stamp: 'lr 2/7/2010 20:53'!
asPetitStream	^ self! !

!PPStream methodsFor: 'accessing' stamp: 'lr 2/13/2012 20:25'!
collection	"Answer the underlying collection."		^ collection! !

!PPStream methodsFor: 'accessing' stamp: 'lr 4/29/2008 21:48'!
peek	"An improved version of peek, that is slightly faster than the built in version."	^ self atEnd ifFalse: [ collection at: position + 1 ]! !

!PPStream methodsFor: 'accessing' stamp: 'lr 8/25/2010 11:36'!
position: anInteger	"The receiver does not check for invalid arguments passed to this method, as it is solely used with valid indexes for backtracking."	position := anInteger! !

!PPStream methodsFor: 'printing' stamp: 'lr 11/4/2010 19:23'!
printOn: aStream	collection isString		ifFalse: [ ^ super printOn: aStream ].	aStream		nextPutAll: (collection copyFrom: 1 to: position);		nextPutAll: '·';		nextPutAll: (collection copyFrom: position + 1 to: readLimit)! !

!PPStream methodsFor: 'accessing' stamp: 'lr 10/5/2010 16:29'!
uncheckedPeek	"An unchecked version of peek that throws an error if we try to peek over the end of the stream, even faster than #peek."	^ collection at: position + 1! !

!PPToken methodsFor: 'comparing' stamp: 'lr 10/7/2009 09:06'!
= anObject	^ self class = anObject class and: [ self value = anObject value ]! !

!PPToken methodsFor: 'accessing' stamp: 'lr 6/15/2010 23:34'!
collection	"Answer the underlying collection of this token."	^ collection! !

!PPToken methodsFor: 'querying' stamp: 'lr 9/7/2011 20:40'!
column	"Answer the column number of this token in the underlying collection."		| position |	position := 0.	(NewLineParser , [ :stream |		start <= stream position			ifTrue: [ ^ start - position ].		position := stream position ] asParser		/ #any asParser) star			parse: collection.	 ^ start - position! !

!PPToken methodsFor: 'copying' stamp: 'lr 6/16/2008 10:55'!
copyFrom: aStartInteger to: aStopInteger	^ self class on: collection start: start + aStartInteger - 1 stop: stop + aStopInteger - 3! !

!PPToken methodsFor: 'comparing' stamp: 'lr 10/7/2009 09:06'!
hash	^ self value hash! !

!PPToken methodsFor: 'initialization' stamp: 'lr 4/30/2010 12:13'!
initializeOn: aSequenceableCollection start: aStartInteger stop: aStopInteger	collection := aSequenceableCollection.	start := aStartInteger.	stop := aStopInteger! !

!PPToken methodsFor: 'querying' stamp: 'lr 9/7/2011 20:41'!
line	"Answer the line number of this token in the underlying collection."		| line |	line := 1.	(NewLineParser , [ :stream |		start <= stream position			ifTrue: [ ^ line ].		line := line + 1 ] asParser		/ #any asParser) star			parse: collection.	^ line! !

!PPToken methodsFor: 'printing' stamp: 'lr 6/16/2008 10:13'!
printOn: aStream	super printOn: aStream.	aStream nextPut: $(; nextPutAll: self value; nextPut: $)! !

!PPToken methodsFor: 'accessing' stamp: 'lr 6/15/2010 23:39'!
size	"Answer the size of this token."	^ stop - start + 1! !

!PPToken methodsFor: 'accessing' stamp: 'lr 6/15/2010 23:33'!
start	"Answer the start position of this token in the underlying collection."	^ start! !

!PPToken methodsFor: 'accessing' stamp: 'lr 6/15/2010 23:33'!
stop	"Answer the stop position of this token in the underlying collection."		^ stop! !

!PPToken methodsFor: 'accessing' stamp: 'lr 6/15/2010 23:34'!
value	"Answer the contents of this token."	^ collection copyFrom: start to: stop! !

!PPToken class methodsFor: 'initialization' stamp: 'pmon 6/1/2012 22:35'!
initialize
	"Platform independent newline sequence. LF: Unix, CR+LF: Windows, and CR: Apple."

	NewLineParser := (Character lfCharacter asParser) / (Character crCharacter asParser , Character lfCharacter asParser optional)! !

!PPToken class methodsFor: 'instance creation' stamp: 'lr 4/6/2010 20:58'!
new	self error: 'Token can only be created using a dedicated constructor.'! !

!PPToken class methodsFor: 'instance creation' stamp: 'lr 4/30/2010 12:13'!
on: aSequenceableCollection	^ self on: aSequenceableCollection start: 1 stop: aSequenceableCollection size! !

!PPToken class methodsFor: 'instance creation' stamp: 'lr 4/30/2010 12:13'!
on: aSequenceableCollection start: aStartInteger stop: aStopInteger	^ self basicNew 		initializeOn: aSequenceableCollection		start: aStartInteger stop: aStopInteger! !

!PPTokenParser methodsFor: 'private' stamp: 'lr 12/7/2009 09:54'!
create: aCollection start: aStartInteger stop: aStopInteger	^ self tokenClass on: aCollection start: aStartInteger stop: aStopInteger! !

!PPTokenParser methodsFor: 'private' stamp: 'lr 4/6/2010 19:18'!
defaultTokenClass	^ PPToken! !

!PPTokenParser methodsFor: 'initialization' stamp: 'lr 4/6/2010 19:19'!
initialize	tokenClass := self defaultTokenClass	! !

!PPTokenParser methodsFor: 'accessing' stamp: 'lr 4/6/2010 19:23'!
tokenClass	^ tokenClass! !

!PPTokenParser methodsFor: 'accessing' stamp: 'lr 4/6/2010 19:24'!
tokenClass: aTokenClass	tokenClass := aTokenClass! !

!PPTrimmingParser methodsFor: 'parsing' stamp: 'lr 8/1/2010 17:11'!
parseOn: aStream	| position element |	position := aStream position.	[ (trimmer parseOn: aStream) isPetitFailure ]		whileFalse.	element := parser parseOn: aStream.	element isPetitFailure ifTrue: [		aStream position: position.		^ element ].	[ (trimmer parseOn: aStream) isPetitFailure ]		whileFalse.	^ element! !

!PPTrimmingParser methodsFor: 'initialization' stamp: 'lr 7/31/2010 12:00'!
setTrimmer: aParser	trimmer := aParser! !

!PPTrimmingParser class methodsFor: 'instance creation' stamp: 'lr 7/31/2010 12:01'!
on: aParser trimmer: aTrimParser	^ self new		setParser: aParser;		setTrimmer: aTrimParser;		yourself! !

!PPUnresolvedParser methodsFor: 'testing' stamp: 'lr 10/27/2008 11:29'!
isUnresolved	^ true! !

!PPUnresolvedParser methodsFor: 'parsing' stamp: 'lr 2/7/2010 20:51'!
parseOn: aStream	self error: self printString , ' need to be resolved before execution.'! !

!PPWrappingParser methodsFor: 'parsing' stamp: 'lr 5/12/2010 20:19'!
parseOn: aStream	^ block value: aStream value: [ parser parseOn: aStream ]! !

!SequenceableCollection methodsFor: '*petitparser-core-converting' stamp: 'lr 12/18/2011 15:57'!
asParser	self notify: 'SequenceableCollection>>#asParser is no longer supported. If you would like to create a PPSequenceParser from a Collection consider using #asSequenceParser instead.'.	^ self asSequenceParser! !

!SequenceableCollection methodsFor: '*petitparser-core-converting' stamp: 'lr 2/7/2010 20:53'!
asPetitStream	^ PPStream on: self! !

!Set methodsFor: '*petitparser-core-converting' stamp: 'lr 12/18/2011 15:57'!
asParser	self notify: 'Set>>#asParser is no longer supported. If you would like to create a PPChoiceParser from a Collection consider using #asChoiceParser instead. Sets have a unpredictable order and should be avoided'.	^ self asChoiceParser! !

!Stream methodsFor: '*petitparser-core-converting' stamp: 'lr 4/8/2010 14:46'!
asPetitStream	^ self contents asPetitStream! !

!String methodsFor: '*petitparser-core-converting' stamp: 'lr 11/29/2011 20:48'!
asParser	"Answer a parser that accepts the receiving string."	^ PPLiteralSequenceParser on: self! !

!Symbol methodsFor: '*petitparser-core-converting' stamp: 'lr 12/18/2011 15:58'!
asParser	"Answer a predicate parser named after the receiving symbol. Possible symbols are the method selectors on the class-side of PPPredicateObjectParser."	^ PPPredicateObjectParser perform: self! !

!Text methodsFor: '*petitparser-core-converting' stamp: 'lr 2/7/2010 20:53'!
asPetitStream	^ string asPetitStream! !

!UndefinedObject methodsFor: '*petitparser-converting' stamp: 'lr 11/29/2011 20:49'!
asParser	"Answer a parser that succeeds and does not consume anything."		^ PPEpsilonParser new! !
PPToken initialize!
