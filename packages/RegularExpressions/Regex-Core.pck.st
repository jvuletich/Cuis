'From Cuis 4.1 of 12 December 2012 [latest update: #1524] on 30 December 2012 at 6:00:39 pm'!
'Description Please enter a description for this package '!
!classDefinition: #RegexError category: #'Regex-Core-Exceptions'!
Error subclass: #RegexError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core-Exceptions'!
!classDefinition: 'RegexError class' category: #'Regex-Core-Exceptions'!
RegexError class
	instanceVariableNames: ''!

!classDefinition: #RegexCompilationError category: #'Regex-Core-Exceptions'!
RegexError subclass: #RegexCompilationError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core-Exceptions'!
!classDefinition: 'RegexCompilationError class' category: #'Regex-Core-Exceptions'!
RegexCompilationError class
	instanceVariableNames: ''!

!classDefinition: #RegexMatchingError category: #'Regex-Core-Exceptions'!
RegexError subclass: #RegexMatchingError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core-Exceptions'!
!classDefinition: 'RegexMatchingError class' category: #'Regex-Core-Exceptions'!
RegexMatchingError class
	instanceVariableNames: ''!

!classDefinition: #RegexSyntaxError category: #'Regex-Core-Exceptions'!
RegexError subclass: #RegexSyntaxError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core-Exceptions'!
!classDefinition: 'RegexSyntaxError class' category: #'Regex-Core-Exceptions'!
RegexSyntaxError class
	instanceVariableNames: ''!

!classDefinition: #RxCharSetParser category: #'Regex-Core'!
Object subclass: #RxCharSetParser
	instanceVariableNames: 'source lookahead elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxCharSetParser class' category: #'Regex-Core'!
RxCharSetParser class
	instanceVariableNames: ''!

!classDefinition: #RxMatchOptimizer category: #'Regex-Core'!
Object subclass: #RxMatchOptimizer
	instanceVariableNames: 'ignoreCase prefixes nonPrefixes conditions testBlock methodPredicates nonMethodPredicates predicates nonPredicates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxMatchOptimizer class' category: #'Regex-Core'!
RxMatchOptimizer class
	instanceVariableNames: ''!

!classDefinition: #RxMatcher category: #'Regex-Core'!
Object subclass: #RxMatcher
	instanceVariableNames: 'matcher ignoreCase startOptimizer stream markerPositions markerCount lastResult'
	classVariableNames: 'Cr Lf'
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxMatcher class' category: #'Regex-Core'!
RxMatcher class
	instanceVariableNames: ''!

!classDefinition: #RxParser category: #'Regex-Core'!
Object subclass: #RxParser
	instanceVariableNames: 'input lookahead'
	classVariableNames: 'BackslashConstants BackslashSpecials'
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxParser class' category: #'Regex-Core'!
RxParser class
	instanceVariableNames: ''!

!classDefinition: #RxmLink category: #'Regex-Core'!
Object subclass: #RxmLink
	instanceVariableNames: 'next'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmLink class' category: #'Regex-Core'!
RxmLink class
	instanceVariableNames: ''!

!classDefinition: #RxmBranch category: #'Regex-Core'!
RxmLink subclass: #RxmBranch
	instanceVariableNames: 'loopback alternative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmBranch class' category: #'Regex-Core'!
RxmBranch class
	instanceVariableNames: ''!

!classDefinition: #RxmMarker category: #'Regex-Core'!
RxmLink subclass: #RxmMarker
	instanceVariableNames: 'index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmMarker class' category: #'Regex-Core'!
RxmMarker class
	instanceVariableNames: ''!

!classDefinition: #RxmPredicate category: #'Regex-Core'!
RxmLink subclass: #RxmPredicate
	instanceVariableNames: 'predicate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmPredicate class' category: #'Regex-Core'!
RxmPredicate class
	instanceVariableNames: ''!

!classDefinition: #RxmSpecial category: #'Regex-Core'!
RxmLink subclass: #RxmSpecial
	instanceVariableNames: 'matchSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmSpecial class' category: #'Regex-Core'!
RxmSpecial class
	instanceVariableNames: ''!

!classDefinition: #RxmSubstring category: #'Regex-Core'!
RxmLink subclass: #RxmSubstring
	instanceVariableNames: 'sample compare'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmSubstring class' category: #'Regex-Core'!
RxmSubstring class
	instanceVariableNames: ''!

!classDefinition: #RxmTerminator category: #'Regex-Core'!
Object subclass: #RxmTerminator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxmTerminator class' category: #'Regex-Core'!
RxmTerminator class
	instanceVariableNames: ''!

!classDefinition: #RxsNode category: #'Regex-Core'!
Object subclass: #RxsNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsNode class' category: #'Regex-Core'!
RxsNode class
	instanceVariableNames: ''!

!classDefinition: #RxsBranch category: #'Regex-Core'!
RxsNode subclass: #RxsBranch
	instanceVariableNames: 'piece branch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsBranch class' category: #'Regex-Core'!
RxsBranch class
	instanceVariableNames: ''!

!classDefinition: #RxsCharSet category: #'Regex-Core'!
RxsNode subclass: #RxsCharSet
	instanceVariableNames: 'negated elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsCharSet class' category: #'Regex-Core'!
RxsCharSet class
	instanceVariableNames: ''!

!classDefinition: #RxsCharacter category: #'Regex-Core'!
RxsNode subclass: #RxsCharacter
	instanceVariableNames: 'character'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsCharacter class' category: #'Regex-Core'!
RxsCharacter class
	instanceVariableNames: ''!

!classDefinition: #RxsContextCondition category: #'Regex-Core'!
RxsNode subclass: #RxsContextCondition
	instanceVariableNames: 'kind'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsContextCondition class' category: #'Regex-Core'!
RxsContextCondition class
	instanceVariableNames: ''!

!classDefinition: #RxsEpsilon category: #'Regex-Core'!
RxsNode subclass: #RxsEpsilon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsEpsilon class' category: #'Regex-Core'!
RxsEpsilon class
	instanceVariableNames: ''!

!classDefinition: #RxsMessagePredicate category: #'Regex-Core'!
RxsNode subclass: #RxsMessagePredicate
	instanceVariableNames: 'selector negated'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsMessagePredicate class' category: #'Regex-Core'!
RxsMessagePredicate class
	instanceVariableNames: ''!

!classDefinition: #RxsPiece category: #'Regex-Core'!
RxsNode subclass: #RxsPiece
	instanceVariableNames: 'atom min max'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsPiece class' category: #'Regex-Core'!
RxsPiece class
	instanceVariableNames: ''!

!classDefinition: #RxsPredicate category: #'Regex-Core'!
RxsNode subclass: #RxsPredicate
	instanceVariableNames: 'predicate negation'
	classVariableNames: 'EscapedLetterSelectors NamedClassSelectors'
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsPredicate class' category: #'Regex-Core'!
RxsPredicate class
	instanceVariableNames: ''!

!classDefinition: #RxsRange category: #'Regex-Core'!
RxsNode subclass: #RxsRange
	instanceVariableNames: 'first last'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsRange class' category: #'Regex-Core'!
RxsRange class
	instanceVariableNames: ''!

!classDefinition: #RxsRegex category: #'Regex-Core'!
RxsNode subclass: #RxsRegex
	instanceVariableNames: 'branch regex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Core'!
!classDefinition: 'RxsRegex class' category: #'Regex-Core'!
RxsRegex class
	instanceVariableNames: ''!


!RegexCompilationError commentStamp: 'Tbn 11/12/2010 22:38' prior: 0!
This class represents compilation errors in regular expressions.!

!RegexError commentStamp: 'Tbn 11/12/2010 22:37' prior: 0!
This is a common superclass for errors in regular expressions.!

!RegexMatchingError commentStamp: 'Tbn 11/12/2010 22:38' prior: 0!
This class represents matching errors in regular expressions.!

!RegexSyntaxError commentStamp: 'Tbn 11/12/2010 22:38' prior: 0!
This class represents syntax errors in regular expressions.!

!RxCharSetParser commentStamp: 'Tbn 11/12/2010 23:13' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--I am a parser created to parse the insides of a character set ([...]) construct. I create and answer a collection of "elements", each being an instance of one of: RxsCharacter, RxsRange, or RxsPredicate.Instance Variables:	source	<Stream>	open on whatever is inside the square brackets we have to parse.	lookahead	<Character>	The current lookahead character	elements	<Collection of: <RxsCharacter|RxsRange|RxsPredicate>> Parsing result!

!RxMatchOptimizer commentStamp: 'Tbn 11/12/2010 23:13' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A match start optimizer, handy for searching a string. Takes a regex syntax tree and sets itself up so that prefix characters or matcher states that cannot start a match are later recognized with #canStartMatch:in: method.Used by RxMatcher, but can be used by other matchers (if implemented) as well.!

!RxMatcher commentStamp: 'Tbn 11/12/2010 23:13' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--This is a recursive regex matcher. Not strikingly efficient, but simple. Also, keeps track of matched subexpressions.  The life cycle goes as follows:1. Initialization. Accepts a syntax tree (presumably produced by RxParser) and compiles it into a matcher built of other classes in this category.2. Matching. Accepts a stream or a string and returns a boolean indicating whether the whole stream or its prefix -- depending on the message sent -- matches the regex.3. Subexpression query. After a successful match, and before any other match, the matcher may be queried about the range of specific stream (string) positions that matched to certain parenthesized subexpressions of the original expression.Any number of queries may follow a successful match, and any number or matches may follow a successful initialization.Note that `matcher' is actually a sort of a misnomer. The actual matcher is a web of Rxm* instances built by RxMatcher during initialization. RxMatcher is just the interface facade of this network.  It is also a builder of it, and also provides a stream-like protocol to easily access the stream being matched.Instance variables:	matcher				<RxmLink> The entry point into the actual matcher.	stream				<Stream> The stream currently being matched against.	markerPositions		<Array of: Integer> Positions of markers' matches.	markerCount		<Integer> Number of markers.	lastResult 			<Boolean> Whether the latest match attempt succeeded or not.	lastChar			<Character | nil> character last seen in the matcher stream!

!RxParser commentStamp: 'Tbn 11/12/2010 23:13' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--The regular expression parser. Translates a regular expression read from a stream into a parse tree. ('accessing' protocol). The tree can later be passed to a matcher initialization method.  All other classes in this category implement the tree. Refer to their comments for any details.Instance variables:	input		<Stream> A stream with the regular expression being parsed.	lookahead	<Character>!

!RxmBranch commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--This is a branch of a matching process. Either `next' chain should match, or `alternative', if not nil, should match. Since this is also used to build loopbacks to match repetitions, `loopback' variable indicates whether the instance is a loopback: it affects the matcher-building operations (which of the paths through the branch is to consider as the primary when we have to find the "tail" of a matcher construct).Instance variables	alternative		<RxmLink> to match if `next' fails to match.	loopback		<Boolean>!

!RxmLink commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A matcher is built of a number of links interconnected into some intricate structure. Regardless of fancy stuff, any link (except for the terminator) has the next one. Any link can match against a stream of characters, recursively propagating the match to the next link. Any link supports a number of matcher-building messages. This superclass does all of the above. The class is not necessarily abstract. It may double as an empty string matcher: it recursively propagates the match to the next link, thus always matching nothing successfully.Principal method:	matchAgainst: aMatcher		Any subclass will reimplement this to test the state of the matcher, most		probably reading one or more characters from the matcher's stream, and		either decide it has matched and answer true, leaving matcher stream		positioned at the end of match, or answer false and restore the matcher		stream position to whatever it was before the matching attempt.Instance variables:	next		<RxmLink | RxmTerminator> The next link in the structure.!

!RxmMarker commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A marker is used to remember positions of match of certain points of a regular expression. The marker receives an identifying key from the Matcher and uses that key to report positions of successful matches to the Matcher.Instance variables:	index	<Object> Something that makes sense for the Matcher. Received from the latter during initalization and later passed to it to identify the receiver.!

!RxmPredicate commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--Instance holds onto a one-argument block and matches exactly one character if the block evaluates to true when passed the character as the argument.Instance variables:	predicate		<BlockClosure>!

!RxmSpecial commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A special node that matches a specific matcher state rather than any input character.The state is either at-beginning-of-line or at-end-of-line.!

!RxmSubstring commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--Instance holds onto a string and matches exactly this string, and exactly once.Instance variables:	string 	<String>!

!RxmTerminator commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--Instances of this class are used to terminate matcher's chains. When a match reaches this (an instance receives #matchAgainst: message), the match is considered to succeed. Instances also support building protocol of RxmLinks, with some restrictions.!

!RxsBranch commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A Branch is a Piece followed by a Branch or an empty string.Instance variables:	piece		<RxsPiece>	branch		<RxsBranch|RxsEpsilon>!

!RxsCharSet commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A character set corresponds to a [...] construct in the regular expression.Instance variables:	elements	<OrderedCollection> An element can be one of: RxsCharacter, RxsRange, or RxsPredicate.	negated		<Boolean>!

!RxsCharacter commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A character is a literal character that appears either in the expression itself or in a character set within an expression.Instance variables:	character		<Character>!

!RxsContextCondition commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--One of a few special nodes more often representing special state of the match rather than a predicate on a character.  The ugly exception is the #any condition which *is* a predicate on a character.Instance variables:	kind		<Selector>!

!RxsEpsilon commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--This is an empty string.  It terminates some of the recursive constructs.!

!RxsMessagePredicate commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A message predicate represents a condition on a character that is tested (at the match time) by sending a unary message to the character expecting a Boolean answer.Instance variables:	selector		<Symbol>!

!RxsNode commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A generic syntax tree node, provides some common responses to the standard tests, as well as tree structure printing -- handy for debugging.!

!RxsPiece commentStamp: 'Tbn 11/12/2010 23:14' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--A piece is an atom, possibly optional or repeated a number of times.Instance variables:	atom	<RxsCharacter|RxsCharSet|RxsPredicate|RxsRegex|RxsSpecial>	min		<Integer>	max	<Integer|nil> nil means infinity!

!RxsPredicate commentStamp: 'Tbn 11/12/2010 23:15' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--This represents a character that satisfies a certain predicate.Instance Variables:	predicate	<BlockClosure>	A one-argument block. If it evaluates to the value defined by <negated> when it is passed a character, the predicate is considered to match.	negation	<BlockClosure>	A one-argument block that is a negation of <predicate>.!

!RxsRange commentStamp: 'Tbn 11/12/2010 23:15' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--I represent a range of characters as appear in character classes such as	[a-ZA-Z0-9].I appear in a syntax tree only as an element of RxsCharSet.Instance Variables:	first	<Character>	last	<Character>!

!RxsRegex commentStamp: 'Tbn 11/12/2010 23:15' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov--The body of a parenthesized thing, or a top-level expression, also an atom.  Instance variables:	branch		<RxsBranch>	regex		<RxsRegex | RxsEpsilon>!

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
addChar: aChar	elements add: (RxsCharacter with: aChar)! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
addRangeFrom: firstChar to: lastChar	firstChar asInteger > lastChar asInteger ifTrue:		[RxParser signalSyntaxException: ' bad character range'].	elements add: (RxsRange from: firstChar to: lastChar)! !

!RxCharSetParser methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initialize: aStream	source := aStream.	lookahead := aStream next.	elements := OrderedCollection new! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
match: aCharacter	aCharacter = lookahead		ifFalse: [RxParser signalSyntaxException: 'unexpected character: ', (String with: lookahead)].	^source atEnd		ifTrue: [lookahead := nil]		ifFalse: [lookahead := source next]! !

!RxCharSetParser methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
parse	lookahead = $- ifTrue:		[self addChar: $-.		self match: $-].	[lookahead isNil] whileFalse: [self parseStep].	^elements! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
parseCharOrRange	| firstChar |	firstChar := lookahead.	self match: firstChar.	lookahead = $- ifTrue:		[self match: $-.		lookahead isNil			ifTrue: [^self addChar: firstChar; addChar: $-]			ifFalse: 				[self addRangeFrom: firstChar to: lookahead.				^self match: lookahead]].	self addChar: firstChar! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
parseEscapeChar	self match: $\.	$- = lookahead		ifTrue: [elements add: (RxsCharacter with: $-)]		ifFalse: [elements add: (RxsPredicate forEscapedLetter: lookahead)].	self match: lookahead! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
parseNamedSet	| name |	self match: $[; match: $:.	name := (String with: lookahead), (source upTo: $:).	lookahead := source next.	self match: $].	elements add: (RxsPredicate forNamedClass: name)! !

!RxCharSetParser methodsFor: 'parsing' stamp: 'vb 4/11/09 21:56'!
parseStep	lookahead = $[ ifTrue:		[source peek = $:			ifTrue: [^self parseNamedSet]			ifFalse: [^self parseCharOrRange]].	lookahead = $\ ifTrue:		[^self parseEscapeChar].	lookahead = $- ifTrue:		[RxParser signalSyntaxException: 'invalid range'].	self parseCharOrRange! !

!RxCharSetParser class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
on: aStream	^self new initialize: aStream! !

!RxMatchOptimizer methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
canStartMatch: aCharacter in: aMatcher 	"Answer whether a match could commence at the given lookahead	character, or in the current state of <aMatcher>. True answered	by this method does not mean a match will definitly occur, while false	answered by this method *does* guarantee a match will never occur."	aCharacter isNil ifTrue: [^true].	^testBlock == nil or: [testBlock value: aCharacter value: aMatcher]! !

!RxMatchOptimizer methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
conditionTester	"#any condition is filtered at the higher level;	it cannot appear among the conditions here."	| matchCondition |	conditions isEmpty ifTrue: [^nil].	conditions size = 1 ifTrue:		[matchCondition := conditions detect: [:ignored | true].		"Special case all of the possible conditions."		#atBeginningOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfLine]].		#atEndOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfLine]].		#atBeginningOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfWord]].		#atEndOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfWord]].		#atWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher atWordBoundary]].		#notAtWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher notAtWordBoundary]].		RxParser signalCompilationException: 'invalid match condition'].	"More than one condition. Capture them as an array in scope."	matchCondition := conditions asArray.	^[:c :matcher |		matchCondition contains:			[:conditionSelector |			matcher perform: conditionSelector]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
determineTestMethod	"Answer a block closure that will work as a can-match predicate.	Answer nil if no viable optimization is possible (too many chars would	be able to start a match)."	| testers |	(conditions includes: #any) ifTrue: [^nil].	testers := OrderedCollection new: 5.	#(#prefixTester #nonPrefixTester #conditionTester #methodPredicateTester #nonMethodPredicateTester #predicateTester #nonPredicateTester)		do: 			[:selector | 			| tester |			tester := self perform: selector.			tester notNil ifTrue: [testers add: tester]].	testers isEmpty ifTrue: [^nil].	testers size = 1 ifTrue: [^testers first].	testers := testers asArray.	^[:char :matcher | testers contains: [:t | t value: char value: matcher]]! !

!RxMatchOptimizer methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initialize: aRegex ignoreCase: aBoolean 	"Set `testMethod' variable to a can-match predicate block:	two-argument block which accepts a lookahead character	and a matcher (presumably built from aRegex) and answers 	a boolean indicating whether a match could start at the given	lookahead. "	ignoreCase := aBoolean.	prefixes := Set new: 10.	nonPrefixes := Set new: 10.	conditions := Set new: 3.	methodPredicates := Set new: 3.	nonMethodPredicates := Set new: 3.	predicates := Set new: 3.	nonPredicates := Set new: 3.	aRegex dispatchTo: self.	"If the whole expression is nullable, 		end-of-line is an implicit can-match condition!!"	aRegex isNullable ifTrue: [conditions add: #atEndOfLine].	testBlock := self determineTestMethod! !

!RxMatchOptimizer methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
methodPredicateTester	| p selector |	methodPredicates isEmpty ifTrue: [^nil].	p := self optimizeSet: methodPredicates.	"also allows copying closures"	^p size = 1		ifTrue: 			["might be a pretty common case"			selector := p first.			[:char :matcher | 			RxParser doHandlingMessageNotUnderstood:				[char perform: selector]]]		ifFalse: 			[[:char :m | 			RxParser doHandlingMessageNotUnderstood:				[p contains: [:sel | char perform: sel]]]]! !

!RxMatchOptimizer methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
nonMethodPredicateTester	| p selector |	nonMethodPredicates isEmpty ifTrue: [^nil].	p := self optimizeSet: nonMethodPredicates.	"also allows copying closures"	^p size = 1		ifTrue: 			[selector := p first.			[:char :matcher | 			RxParser doHandlingMessageNotUnderstood:				[(char perform: selector) not]]]		ifFalse: 			[[:char :m | 			RxParser doHandlingMessageNotUnderstood:				[p contains: [:sel | (char perform: sel) not]]]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
nonPredicateTester	| p pred |	nonPredicates isEmpty ifTrue: [^nil].	p := self optimizeSet: nonPredicates.	"also allows copying closures"	^p size = 1		ifTrue: 			[pred := p first.			[:char :matcher | (pred value: char) not]]		ifFalse: 			[[:char :m | p contains: [:some | (some value: char) not]]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
nonPrefixTester	| np nonPrefixChar |	nonPrefixes isEmpty ifTrue: [^nil].	np := self optimizeSet: nonPrefixes. "also allows copying closures"	^np size = 1 "might be be pretty common case"		ifTrue: 			[nonPrefixChar := np first.			[:char :matcher | char ~= nonPrefixChar]]		ifFalse: [[:char : matcher | (np includes: char) not]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
optimizeSet: aSet	"If a set is small, convert it to array to speed up lookup	(Array has no hashing overhead, beats Set on small number	of elements)."	^aSet size < 10 ifTrue: [aSet asArray] ifFalse: [aSet]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
predicateTester	| p pred |	predicates isEmpty ifTrue: [^nil].	p := self optimizeSet: predicates.	"also allows copying closures"	^p size = 1		ifTrue: 			[pred := p first.			[:char :matcher | pred value: char]]		ifFalse: 			[[:char :m | p contains: [:some | some value: char]]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
prefixTester	| p prefixChar |	prefixes isEmpty ifTrue: [^nil].	p := self optimizeSet: prefixes. "also allows copying closures"	ignoreCase ifTrue: [p := p collect: [:each | each asUppercase]].	^p size = 1 "might be a pretty common case"		ifTrue: 			[prefixChar := p first.			ignoreCase				ifTrue: [[:char :matcher | char sameAs: prefixChar]]				ifFalse: [[:char :matcher | char = prefixChar]]]		ifFalse:			[ignoreCase				ifTrue: [[:char :matcher | p includes: char asUppercase]]				ifFalse: [[:char :matcher | p includes: char]]]! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxAny	"Any special char is among the prefixes."	conditions add: #any! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxBeginningOfLine	"Beginning of line is among the prefixes."	conditions add: #atBeginningOfLine! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxBeginningOfWord	"Beginning of line is among the prefixes."	conditions add: #atBeginningOfWord! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxBranch: branchNode	"If the head piece of the branch is transparent (allows 0 matches),	we must recurse down the branch. Otherwise, just the head atom	is important."	(branchNode piece isNullable and: [branchNode branch notNil])		ifTrue: [branchNode branch dispatchTo: self].	branchNode piece dispatchTo: self! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxCharSet: charSetNode 	"All these (or none of these) characters is the prefix."	charSetNode isNegated		ifTrue: [nonPrefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)]		ifFalse: [prefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)].	charSetNode hasPredicates ifTrue: 			[charSetNode isNegated				ifTrue: [nonPredicates addAll: charSetNode predicates]				ifFalse: [predicates addAll: charSetNode predicates]]! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxCharacter: charNode	"This character is the prefix, of one of them."	prefixes add: charNode character! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEndOfLine	"Beginning of line is among the prefixes."	conditions add: #atEndOfLine! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEndOfWord	conditions add: #atEndOfWord! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEpsilon	"Empty string, terminate the recursion (do nothing)."! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxMessagePredicate: messagePredicateNode 	messagePredicateNode negated		ifTrue: [nonMethodPredicates add: messagePredicateNode selector]		ifFalse: [methodPredicates add: messagePredicateNode selector]! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxNonWordBoundary	conditions add: #notAtWordBoundary! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxPiece: pieceNode	"Pass on to the atom."	pieceNode atom dispatchTo: self! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxPredicate: predicateNode 	predicates add: predicateNode predicate! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxRegex: regexNode	"All prefixes of the regex's branches should be combined.	Therefore, just recurse."	regexNode branch dispatchTo: self.	regexNode regex notNil		ifTrue: [regexNode regex dispatchTo: self]! !

!RxMatchOptimizer methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxWordBoundary	conditions add: #atWordBoundary! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
allocateMarker	"Answer an integer to use as an index of the next marker."	markerCount := markerCount + 1.	^markerCount! !

!RxMatcher methodsFor: 'testing' stamp: 'lr 1/15/2010 21:12'!
atBeginningOfLine	^self position = 0 or: [self lastChar = Cr]! !

!RxMatcher methodsFor: 'testing' stamp: 'lr 1/15/2010 21:12'!
atBeginningOfWord	^(self isWordChar: self lastChar) not		and: [self isWordChar: stream peek]! !

!RxMatcher methodsFor: 'streaming' stamp: 'vb 4/11/09 21:56'!
atEnd	^stream atEnd! !

!RxMatcher methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
atEndOfLine	^self atEnd or: [stream peek = Cr]! !

!RxMatcher methodsFor: 'testing' stamp: 'lr 1/15/2010 21:12'!
atEndOfWord	^(self isWordChar: self lastChar)		and: [(self isWordChar: stream peek) not]! !

!RxMatcher methodsFor: 'testing' stamp: 'lr 1/15/2010 21:12'!
atWordBoundary	^(self isWordChar: self lastChar)		xor: (self isWordChar: stream peek)! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
buildFrom: aSyntaxTreeRoot	"Private - Entry point of matcher build process."	markerCount := 0.  "must go before #dispatchTo: !!"	matcher := aSyntaxTreeRoot dispatchTo: self.	matcher terminateWith: RxmTerminator new! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
copy: aString replacingMatchesWith: replacementString	"Copy <aString>, except for the matches. Replace each match with <aString>."	| answer |	answer := (String new: 40) writeStream.	self		copyStream: aString readStream		to: answer		replacingMatchesWith: replacementString.	^answer contents! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
copy: aString translatingMatchesUsing: aBlock	"Copy <aString>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and replace the match with the answer."	| answer |	answer := (String new: 40) writeStream.	self copyStream: aString readStream to: answer translatingMatchesUsing: aBlock.	^answer contents! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'lr 1/15/2010 21:45'!
copyStream: aStream to: writeStream replacingMatchesWith: aString	"Copy the contents of <aStream> on the <writeStream>, except for the matches. Replace each match with <aString>."	| searchStart matchStart matchEnd |	stream := aStream.	markerPositions := nil.	[searchStart := aStream position.	self proceedSearchingStream: aStream] whileTrue:		[matchStart := (self subBeginning: 1) first.		matchEnd := (self subEnd: 1) first.		aStream position: searchStart.		searchStart to: matchStart - 1 do:			[:ignoredPos | writeStream nextPut: aStream next].		writeStream nextPutAll: aString.		aStream position: matchEnd.		"Be extra careful about successful matches which consume no input.		After those, make sure to advance or finish if already at end."		matchEnd = searchStart ifTrue: 			[aStream atEnd				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]				ifFalse:	[writeStream nextPut: aStream next]]].	aStream position: searchStart.	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'lr 1/15/2010 21:45'!
copyStream: aStream to: writeStream translatingMatchesUsing: aBlock	"Copy the contents of <aStream> on the <writeStream>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and write the answer to <writeStream> in place of the match."	| searchStart matchStart matchEnd match |	stream := aStream.		markerPositions := nil.	[searchStart := aStream position.	self proceedSearchingStream: aStream] whileTrue:		[matchStart := (self subBeginning: 1) first.		matchEnd := (self subEnd: 1) first.		aStream position: searchStart.		searchStart to: matchStart - 1 do:			[:ignoredPos | writeStream nextPut: aStream next].		match := (String new: matchEnd - matchStart + 1) writeStream.		matchStart to: matchEnd - 1 do:			[:ignoredPos | match nextPut: aStream next].		writeStream nextPutAll: (aBlock value: match contents).		"Be extra careful about successful matches which consume no input.		After those, make sure to advance or finish if already at end."		matchEnd = searchStart ifTrue: 			[aStream atEnd				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]				ifFalse:	[writeStream nextPut: aStream next]]].	aStream position: searchStart.	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]! !

!RxMatcher methodsFor: 'privileged' stamp: 'lr 1/15/2010 21:13'!
currentState	"Answer an opaque object that can later be used to restore the	matcher's state (for backtracking)."	| origPosition origLastChar |	origPosition := stream position.	^	[stream position: origPosition]! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
hookBranchOf: regexNode onto: endMarker	"Private - Recurse down the chain of regexes starting at	regexNode, compiling their branches and hooking their tails 	to the endMarker node."	| rest |	rest := regexNode regex isNil		ifTrue: [nil]		ifFalse: [self hookBranchOf: regexNode regex onto: endMarker].	^RxmBranch new		next: ((regexNode branch dispatchTo: self)					pointTailTo: endMarker; 					yourself);		alternative: rest;		yourself! !

!RxMatcher methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initialize: syntaxTreeRoot ignoreCase: aBoolean	"Compile thyself for the regex with the specified syntax tree.	See comment and `building' protocol in this class and 	#dispatchTo: methods in syntax tree components for details 	on double-dispatch building. 	The argument is supposedly a RxsRegex."	ignoreCase := aBoolean.	self buildFrom: syntaxTreeRoot.	startOptimizer := RxMatchOptimizer new initialize: syntaxTreeRoot ignoreCase: aBoolean! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
isWordChar: aCharacterOrNil	"Answer whether the argument is a word constituent character:	alphanumeric or _."	^aCharacterOrNil ~~ nil		and: [aCharacterOrNil isAlphaNumeric]! !

!RxMatcher methodsFor: 'accessing' stamp: 'lr 1/15/2010 21:14'!
lastChar	^ stream position = 0		ifFalse: [ stream skip: -1; next ]! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
lastResult	^lastResult! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
makeOptional: aMatcher	"Private - Wrap this matcher so that the result would match 0 or 1	occurrences of the matcher."	| dummy branch |	dummy := RxmLink new.	branch := (RxmBranch new beLoopback)		next: aMatcher;		alternative: dummy.	aMatcher pointTailTo: dummy.	^branch! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
makePlus: aMatcher	"Private - Wrap this matcher so that the result would match 1 and more	occurrences of the matcher."	| loopback |	loopback := (RxmBranch new beLoopback)		next: aMatcher.	aMatcher pointTailTo: loopback.	^aMatcher! !

!RxMatcher methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
makeStar: aMatcher	"Private - Wrap this matcher so that the result would match 0 and more	occurrences of the matcher."	| dummy detour loopback |	dummy := RxmLink new.	detour := RxmBranch new		next: aMatcher;		alternative: dummy.	loopback := (RxmBranch new beLoopback)		next: aMatcher;		alternative: dummy.	aMatcher pointTailTo: loopback.	^detour! !

!RxMatcher methodsFor: 'privileged' stamp: 'vb 4/11/09 21:56'!
markerPositionAt: anIndex add: position	"Remember position of another instance of the given marker."	(markerPositions at: anIndex) addFirst: position! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
matches: aString	"Match against a string."	^self matchesStream: aString readStream! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesIn: aString	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of all matches (substrings)."	| result |	result := OrderedCollection new.	self		matchesOnStream: aString readStream		do: [:match | result add: match].	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesIn: aString collect: aBlock	"Search aString repeatedly for the matches of the receiver.  Evaluate aBlock for each match passing the matched substring as the argument, collect evaluation results in an OrderedCollection, and return in. The following example shows how to use this message to split a string into words."	"'\w+' asRegex matchesIn: 'Now is the Time' collect: [:each | each asLowercase]"	| result |	result := OrderedCollection new.	self		matchesOnStream: aString readStream		do: [:match | result add: (aBlock value: match)].	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesIn: aString do: aBlock	"Search aString repeatedly for the matches of the receiver.	Evaluate aBlock for each match passing the matched substring	as the argument."	self		matchesOnStream: aString readStream		do: aBlock! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesOnStream: aStream	| result |	result := OrderedCollection new.	self		matchesOnStream: aStream		do: [:match | result add: match].	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesOnStream: aStream collect: aBlock	| result |	result := OrderedCollection new.	self		matchesOnStream: aStream		do: [:match | result add: (aBlock value: match)].	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchesOnStream: aStream do: aBlock	"Be extra careful about successful matches which consume no input.	After those, make sure to advance or finish if already at end."	| position |	[position := aStream position.	self searchStream: aStream] whileTrue:		[aBlock value: (self subexpression: 1).		position = aStream position ifTrue: 			[aStream atEnd				ifTrue: [^self]				ifFalse: [aStream next]]]! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
matchesPrefix: aString	"Match against a string."	^self matchesStreamPrefix: aString readStream! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
matchesStream: theStream	"Match thyself against a positionable stream."	^(self matchesStreamPrefix: theStream)		and: [stream atEnd]! !

!RxMatcher methodsFor: 'accessing' stamp: 'lr 1/15/2010 21:45'!
matchesStreamPrefix: theStream	"Match thyself against a positionable stream."	stream := theStream.	markerPositions := nil.	^self tryMatch! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'vb 4/11/09 21:56'!
matchingRangesIn: aString	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of ranges of each match (index of first character to: index of last character)."	| result |	result := OrderedCollection new.	self		matchesIn: aString 		do: [:match | result add: (self position - match size + 1 to: self position)].	^result! !

!RxMatcher methodsFor: 'streaming' stamp: 'lr 1/15/2010 21:13'!
next	^ stream next! !

!RxMatcher methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
notAtWordBoundary	^self atWordBoundary not! !

!RxMatcher methodsFor: 'streaming' stamp: 'vb 4/11/09 21:56'!
position	^stream position! !

!RxMatcher methodsFor: 'private' stamp: 'lr 1/15/2010 21:17'!
proceedSearchingStream: aStream	| position |	position := aStream position.	[aStream atEnd] whileFalse:		[self tryMatch ifTrue: [^true].		aStream position: position; next.		position := aStream position].	"Try match at the very stream end too!!"	self tryMatch ifTrue: [^true]. 	^false! !

!RxMatcher methodsFor: 'privileged' stamp: 'vb 4/11/09 21:56'!
restoreState: aBlock	aBlock value! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
search: aString	"Search the string for occurrence of something matching myself.	Answer a Boolean indicating success."	^self searchStream: aString readStream! !

!RxMatcher methodsFor: 'accessing' stamp: 'lr 1/15/2010 21:45'!
searchStream: aStream	"Search the stream for occurrence of something matching myself.	After the search has occurred, stop positioned after the end of the	matched substring. Answer a Boolean indicating success."	| position |	stream := aStream.	position := aStream position.	markerPositions := nil.	[aStream atEnd] whileFalse:		[self tryMatch ifTrue: [^true].		aStream position: position; next.		position := aStream position].	"Try match at the very stream end too!!"	self tryMatch ifTrue: [^true]. 	^false! !

!RxMatcher methodsFor: 'accessing' stamp: 'jannik.laval 5/2/2010 06:53'!
split: aString	| result lastPosition |	result := OrderedCollection new.	stream := aString readStream.	lastPosition := stream position.	[ self searchStream: stream ] whileTrue:		[ result add: (aString copyFrom: lastPosition+1 to: (self subBeginning: 1) first).		[lastPosition < stream position] assertWithDescription: 'Regex cannot match null string'.		lastPosition := stream position ].	result add: (aString copyFrom: lastPosition+1 to: aString size).	^ result! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
subBeginning: subIndex	^markerPositions at: subIndex * 2 - 1! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
subEnd: subIndex	^markerPositions at: subIndex * 2! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
subexpression: subIndex	"Answer a string that matched the subexpression at the given index.	If there are multiple matches, answer the last one.	If there are no matches, answer nil. 	(NB: it used to answer an empty string but I think nil makes more sense)."	| matches |	matches := self subexpressions: subIndex.	^matches isEmpty ifTrue: [nil] ifFalse: [matches last]! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
subexpressionCount	^markerCount // 2! !

!RxMatcher methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
subexpressions: subIndex	"Answer an array of all matches of the subexpression at the given index.	The answer is always an array; it is empty if there are no matches."	| originalPosition startPositions stopPositions reply |	originalPosition := stream position.	startPositions := self subBeginning: subIndex.	stopPositions := self subEnd: subIndex.	(startPositions isEmpty or: [stopPositions isEmpty]) ifTrue: [^Array new].	reply := OrderedCollection new.	startPositions with: stopPositions do:		[:start :stop |		stream position: start.		reply add: (stream next: stop - start)].	stream position: originalPosition.	^reply asArray! !

!RxMatcher methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
supportsSubexpressions	^true! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxAny	"Double dispatch from the syntax tree. 	Create a matcher for any non-null character."	^RxmPredicate new		predicate: [:char | char asInteger ~= 0]! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxBeginningOfLine	"Double dispatch from the syntax tree. 	Create a matcher for beginning-of-line condition."	^RxmSpecial new beBeginningOfLine! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxBeginningOfWord	"Double dispatch from the syntax tree. 	Create a matcher for beginning-of-word condition."	^RxmSpecial new beBeginningOfWord! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'PeterHugossonMiller 9/3/2009 11:08'!
syntaxBranch: branchNode	"Double dispatch from the syntax tree. 	Branch node is a link in a chain of concatenated pieces.	First build the matcher for the rest of the chain, then make 	it for the current piece and hook the rest to it."	| result next rest |	branchNode branch isNil		ifTrue: [^branchNode piece dispatchTo: self].	"Optimization: glue a sequence of individual characters into a single string to match."	branchNode piece isAtomic ifTrue:		[result := (String new: 40) writeStream.		next := branchNode tryMergingInto: result.		result := result contents.		result size > 1 ifTrue: "worth merging"			[rest := next notNil 				ifTrue: [next dispatchTo: self]				ifFalse: [nil].			^(RxmSubstring new substring: result ignoreCase: ignoreCase)				pointTailTo: rest;				yourself]].	"No optimization possible or worth it, just concatenate all. "	^(branchNode piece dispatchTo: self)		pointTailTo: (branchNode branch dispatchTo: self);		yourself! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxCharSet: charSetNode	"Double dispatch from the syntax tree. 	A character set is a few characters, and we either match any of them,	or match any that is not one of them."	^RxmPredicate with: (charSetNode predicateIgnoringCase: ignoreCase)! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxCharacter: charNode	"Double dispatch from the syntax tree. 	We get here when no merging characters into strings was possible."	| wanted |	wanted := charNode character.	^RxmPredicate new predicate: 		(ignoreCase			ifTrue: [[:char | char sameAs: wanted]]			ifFalse: [[:char | char = wanted]])! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEndOfLine	"Double dispatch from the syntax tree. 	Create a matcher for end-of-line condition."	^RxmSpecial new beEndOfLine! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEndOfWord	"Double dispatch from the syntax tree. 	Create a matcher for end-of-word condition."	^RxmSpecial new beEndOfWord! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxEpsilon	"Double dispatch from the syntax tree. Match empty string. This is unlikely	to happen in sane expressions, so we'll live without special epsilon-nodes."	^RxmSubstring new		substring: String new		ignoreCase: ignoreCase! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxMessagePredicate: messagePredicateNode	"Double dispatch from the syntax tree. 	Special link can handle predicates."	^messagePredicateNode negated		ifTrue: [RxmPredicate new bePerformNot: messagePredicateNode selector]		ifFalse: [RxmPredicate new bePerform: messagePredicateNode selector]! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxNonWordBoundary	"Double dispatch from the syntax tree. 	Create a matcher for the word boundary condition."	^RxmSpecial new beNotWordBoundary! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxPiece: pieceNode	"Double dispatch from the syntax tree. 	Piece is an atom repeated a few times. Take care of a special	case when the atom is repeated just once."	| atom |	atom := pieceNode atom dispatchTo: self.	^pieceNode isSingular		ifTrue: [atom]		ifFalse: [pieceNode isStar			ifTrue: [self makeStar: atom]			ifFalse: [pieceNode isPlus				ifTrue: [self makePlus: atom]				ifFalse: [pieceNode isOptional					ifTrue: [self makeOptional: atom]					ifFalse: [RxParser signalCompilationException: 						'repetitions are not supported by RxMatcher']]]]! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxPredicate: predicateNode	"Double dispatch from the syntax tree. 	A character set is a few characters, and we either match any of them,	or match any that is not one of them."	^RxmPredicate with: predicateNode predicate! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxRegex: regexNode	"Double dispatch from the syntax tree. 	Regex node is a chain of branches to be tried. Should compile this 	into a bundle of parallel branches, between two marker nodes." 		| startIndex endIndex endNode alternatives |	startIndex := self allocateMarker.	endIndex := self allocateMarker.	endNode := RxmMarker new index: endIndex.	alternatives := self hookBranchOf: regexNode onto: endNode.	^(RxmMarker new index: startIndex)		pointTailTo: alternatives;		yourself! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'vb 4/11/09 21:56'!
syntaxWordBoundary	"Double dispatch from the syntax tree. 	Create a matcher for the word boundary condition."	^RxmSpecial new beWordBoundary! !

!RxMatcher methodsFor: 'private' stamp: 'lr 1/15/2010 21:51'!
tryMatch	"Match thyself against the current stream."	| oldMarkerPositions |	oldMarkerPositions := markerPositions.	markerPositions := Array new: markerCount.	1 to: markerCount do: [ :i | markerPositions at: i put: OrderedCollection new ].	lastResult := startOptimizer isNil		ifTrue: [ matcher matchAgainst: self]		ifFalse: [ (startOptimizer canStartMatch: stream peek in: self) and: [ matcher matchAgainst: self ] ].	"check for duplicates"	(lastResult not or: [ oldMarkerPositions isNil or: [ oldMarkerPositions size ~= markerPositions size ] ])		ifTrue: [ ^ lastResult ].	oldMarkerPositions with: markerPositions do: [ :oldPos :newPos |		oldPos size = newPos size 			ifFalse: [ ^ lastResult ].		oldPos with: newPos do: [ :old :new |			old = new				ifFalse: [ ^ lastResult ] ] ].	"this is a duplicate"	^ lastResult := false! !

!RxMatcher class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
for: aRegex	"Create and answer a matcher that will match a regular expression	specified by the syntax tree of which `aRegex' is a root."	^self for: aRegex ignoreCase: false! !

!RxMatcher class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
for: aRegex ignoreCase: aBoolean	"Create and answer a matcher that will match a regular expression	specified by the syntax tree of which `aRegex' is a root."	^self new		initialize: aRegex		ignoreCase: aBoolean! !

!RxMatcher class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
forString: aString	"Create and answer a matcher that will match the regular expression	`aString'."	^self for: (RxParser new parse: aString)! !

!RxMatcher class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
forString: aString ignoreCase: aBoolean	"Create and answer a matcher that will match the regular expression	`aString'."	^self for: (RxParser new parse: aString) ignoreCase: aBoolean! !

!RxMatcher class methodsFor: 'class initialization' stamp: 'avi 11/30/2003 13:30'!
initialize	"RxMatcher initialize"	Cr := Character cr.	Lf := Character lf.! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
atom	"An atom is one of a lot of possibilities, see below."	| atom |	(lookahead = #epsilon or: 			[lookahead = $| or: 					[lookahead = $)						or: [lookahead = $* or: [lookahead = $+ or: [lookahead = $?]]]]])		ifTrue: [^RxsEpsilon new].	lookahead = $( ifTrue: 			["<atom> ::= '(' <regex> ')' "			self match: $(.			atom := self regex.			self match: $).			^atom].	lookahead = $[ ifTrue: 			["<atom> ::= '[' <characterSet> ']' "			self match: $[.			atom := self characterSet.			self match: $].			^atom].	lookahead = $: ifTrue: 			["<atom> ::= ':' <messagePredicate> ':' "			self match: $:.			atom := self messagePredicate.			self match: $:.			^atom].	lookahead = $. ifTrue: 			["any non-whitespace character"			self next.			^RxsContextCondition new beAny].	lookahead = $^ ifTrue: 			["beginning of line condition"			self next.			^RxsContextCondition new beBeginningOfLine].	lookahead = $$ ifTrue: 			["end of line condition"			self next.			^RxsContextCondition new beEndOfLine].	lookahead = $\ ifTrue: 			["<atom> ::= '\' <character>"			self next.			lookahead = #epsilon ifTrue: 				[self signalParseError: 'bad quotation'].			(BackslashConstants includesKey: lookahead) ifTrue:				[atom := RxsCharacter with: (BackslashConstants at: lookahead).				self next.				^atom].			self ifSpecial: lookahead				then: [:node | self next. ^node]].	"If passed through the above, the following is a regular character."	atom := RxsCharacter with: lookahead.	self next.	^atom! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
branch	"<branch> ::= e | <piece> <branch>"	| piece branch |	piece := self piece.	(lookahead = #epsilon or: [lookahead = $| or: [lookahead = $) ]])		ifTrue: [branch := nil]		ifFalse: [branch := self branch].	^RxsBranch new 		initializePiece: piece 		branch: branch! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
characterSet	"Match a range of characters: something between `[' and `]'.	Opening bracked has already been seen, and closing should	not be consumed as well. Set spec is as usual for	sets in regexes."	| spec errorMessage |	errorMessage := ' no terminating "]"'.	spec := self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage.	(spec isEmpty or: [spec = '^']) ifTrue: "This ']' was literal."		[self next.		spec := spec, ']', (self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage)].	^self characterSetFrom: spec! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
characterSetFrom: setSpec	"<setSpec> is what goes between the brackets in a charset regex	(a String). Make a string containing all characters the spec specifies.	Spec is never empty."	| negated spec |	spec := ReadStream on: setSpec.	spec peek = $^		ifTrue: 	[negated := true.				spec next]		ifFalse:	[negated := false].	^RxsCharSet new		initializeElements: (RxCharSetParser on: spec) parse		negated: negated! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
ifSpecial: aCharacter then: aBlock	"If the character is such that it defines a special node when follows a $\,	then create that node and evaluate aBlock with the node as the parameter.	Otherwise just return."	| classAndSelector |	classAndSelector := BackslashSpecials at: aCharacter ifAbsent: [^self].	^aBlock value: (classAndSelector key new perform: classAndSelector value)! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
inputUpTo: aCharacter errorMessage: aString	"Accumulate input stream until <aCharacter> is encountered	and answer the accumulated chars as String, not including	<aCharacter>. Signal error if end of stream is encountered,	passing <aString> as the error description."	| accumulator |	accumulator := WriteStream on: (String new: 20).	[lookahead ~= aCharacter and: [lookahead ~= #epsilon]]		whileTrue:			[accumulator nextPut: lookahead.			self next].	lookahead = #epsilon ifTrue: [self signalParseError: aString].	^accumulator contents! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
inputUpTo: aCharacter nestedOn: anotherCharacter errorMessage: aString 	"Accumulate input stream until <aCharacter> is encountered	and answer the accumulated chars as String, not including	<aCharacter>. Signal error if end of stream is encountered,	passing <aString> as the error description."	| accumulator nestLevel |	accumulator := WriteStream on: (String new: 20).	nestLevel := 0.	[lookahead ~= aCharacter or: [nestLevel > 0]] whileTrue: 			[#epsilon = lookahead ifTrue: [self signalParseError: aString].			accumulator nextPut: lookahead.			lookahead = anotherCharacter ifTrue: [nestLevel := nestLevel + 1].			lookahead = aCharacter ifTrue: [nestLevel := nestLevel - 1].			self next].	^accumulator contents! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
match: aCharacter	"<aCharacter> MUST match the current lookeahead.	If this is the case, advance the input. Otherwise, blow up."	aCharacter ~= lookahead 		ifTrue: [^self signalParseError].	"does not return"	self next! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
messagePredicate	"Match a message predicate specification: a selector (presumably	understood by a Character) enclosed in :'s ."	| spec negated |	spec := (self inputUpTo: $: errorMessage: ' no terminating ":"').	negated := false.	spec first = $^ ifTrue:		[negated := true.		spec := spec copyFrom: 2 to: spec size].	^RxsMessagePredicate new 		initializeSelector: spec asSymbol		negated: negated! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
next	"Advance the input storing the just read character	as the lookahead."	input atEnd		ifTrue: [lookahead := #epsilon]		ifFalse: [lookahead := input next]! !

!RxParser methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
parse: aString	"Parse input from a string <aString>.	On success, answers an RxsRegex -- parse tree root.	On error, raises `RxParser syntaxErrorSignal' with the current	input stream position as the parameter."	^self parseStream: (ReadStream on: aString)! !

!RxParser methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
parseStream: aStream	"Parse an input from a character stream <aStream>.	On success, answers an RxsRegex -- parse tree root.	On error, raises `RxParser syntaxErrorSignal' with the current	input stream position as the parameter."	| tree |	input := aStream.	lookahead := nil.	self match: nil.	tree := self regex.	self match: #epsilon.	^tree! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
piece	"<piece> ::= <atom> | <atom>* | <atom>+ | <atom>?"	| atom errorMessage |	errorMessage := ' nullable closure'.	atom := self atom.	lookahead = $* ifTrue: 		[self next.		atom isNullable ifTrue: [self signalParseError: errorMessage].		^RxsPiece new initializeStarAtom: atom].	lookahead = $+ ifTrue: 		[self next.		atom isNullable ifTrue: [self signalParseError: errorMessage].		^RxsPiece new initializePlusAtom: atom].	lookahead = $? ifTrue: 		[self next.		atom isNullable ifTrue: [self signalParseError: errorMessage].		^RxsPiece new initializeOptionalAtom: atom].	^RxsPiece new initializeAtom: atom! !

!RxParser methodsFor: 'recursive descent' stamp: 'vb 4/11/09 21:56'!
regex	"<regex> ::= e | <branch> `|' <regex>"	| branch regex |	branch := self branch.	(lookahead = #epsilon or: [lookahead = $)])		ifTrue: [regex := nil]		ifFalse: 			[self match: $|.			regex := self regex].	^RxsRegex new initializeBranch: branch regex: regex! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
signalParseError	self class signalSyntaxException: 'Regex syntax error'! !

!RxParser methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
signalParseError: aString	self class signalSyntaxException: aString! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'vb 4/11/09 21:56'!
a: x introduction: xx " A regular expression is a template specifying a class of strings. Aregular expression matcher is an tool that determines whether a stringbelongs to a class specified by a regular expression.  This is acommon task of a user input validation code, and the use of regularexpressions can GREATLY simplify and speed up development of suchcode.  As an example, here is how to verify that a string is a validhexadecimal number in Smalltalk notation, using this matcher package:	aString matchesRegex: '16r[[:xdigit:]]+'(Coding the same ``the hard way'' is an exercise to a curious reader).This matcher is offered to the Smalltalk community in hope it will beuseful. It is free in terms of money, and to a large extent--in termsof rights of use. Refer to `Boring Stuff' section for legalese.The 'What's new in this release' section describes the functionalityintroduced in 1.1 release.The `Syntax' section explains the recognized syntax of regularexpressions.The `Usage' section explains matcher capabilities that go beyond whatString>>matchesRegex: method offers.The `Implementation notes' sections says a few words about what isunder the hood.Happy hacking,--Vassili Bykov <vassili@objectpeople.com> <vassili@magma.ca>August 6, 1996April 4, 1999"	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'lr 1/7/2010 20:10'!
b: x whatsNewInThisRelease: xx"VERSION 1.3.1 (September 2008)1. Updated documentation of character classes, making clear the problems of locale - an area for future improvementVERSION 1.3 (September 2008)1. \w now matches underscore as well as alphanumerics, in line with most other regex libraries (and our documentation!!).  2. \W rejects underscore as well as alphanumerics3. added tests for this at end of testSuite4. updated documentation and added note to old incorrect comments in version 1.1 belowVERSION 1.2.3 (November 2007)1. Regexs with ^ or $ applied to copy empty strings caused infinite loops, e.g. ('' copyWithRegex: '^.*$' matchesReplacedWith: 'foo'). Applied a similar correction to that from version 1.1c, to #copyStream:to:(replacingMatchesWith:|translatingMatchesUsing:).2. Extended RxParser testing to run each test for #copy:translatingMatchesUsing: as well as #search:.3. Corrected #testSuite test that a dot does not match a null, which was passing by luck with Smalltalk code in a literal array.4. Added test to end of test suite for fix 1 above.VERSION 1.2.2 (November 2006)There was no way to specify a backslash in a character set. Now [\\] is accepted.VERSION 1.2.1	(August 2006)1. Support for returning all ranges (startIndex to: stopIndex) matching a regex - #allRangesOfRegexMatches:, #matchingRangesIn:2. Added hint to usage documentation on how to get more information about matches when enumerating3. Syntax description of dot corrected: matches anything but NUL since 1.1aVERSION 1.2	(May 2006)Fixed case-insensitive search for character sets.VERSION 1.1c	(December 2004)Fixed the issue with #matchesOnStream:do: which caused infinite loops for matches that matched empty strings.VERSION 1.1b	(November 2001)Changes valueNowOrOnUnwindDo: to ensure:, plus incorporates some earlier fixes.VERSION 1.1a	(May 2001)1. Support for keeping track of multiple subexpressions.2. Dot (.) matches anything but NUL character, as it should per POSIX spec.3. Some bug fixes.VERSION 1.1	(October 1999)Regular expression syntax corrections and enhancements:1. Backslash escapes similar to those in Perl are allowed in patterns:	\w	any word constituent character (equivalent to [a-zA-Z0-9_]) *** underscore only since 1.3 ***	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9_] *** underscore only since 1.3 ***	\d	a digit (same as [0-9])	\D	anything but a digit	\s 	a whitespace character	\S	anything but a whitespace character	\b	an empty string at a word boundary	\B	an empty string not at a word boundary	\<	an empty string at the beginning of a word	\>	an empty string at the end of a wordFor example, '\w+' is now a valid expression matching any word.2. The following backslash escapes are also allowed in character sets(between square brackets):	\w, \W, \d, \D, \s, and \S.3. The following grep(1)-compatible named character classes arerecognized in character sets as well:	[:alnum:]	[:alpha:]	[:cntrl:]	[:digit:]	[:graph:]	[:lower:]	[:print:]	[:punct:]	[:space:]	[:upper:]	[:xdigit:]For example, the following patterns are equivalent:	'[[:alnum:]_]+' '\w+'  '[\w]+' '[a-zA-Z0-9_]+' *** underscore only since 1.3 ***4. Some non-printable characters can be represented in regularexpressions using a common backslash notation:	\t	tab (Character tab)	\n	newline (Character lf)	\r	carriage return (Character cr)	\f	form feed (Character newPage)	\e	escape (Character esc)5. A dot is corectly interpreted as 'any character but a newline'instead of 'anything but whitespace'.6. Case-insensitive matching.  The easiest access to it are newmessages CharacterArray understands: #asRegexIgnoringCase,#matchesRegexIgnoringCase:, #prefixMatchesRegexIgnoringCase:.7. The matcher (an instance of RxMatcher, the result ofString>>asRegex) now provides a collection-like interface to matchesin a particular string or on a particular stream, as well assubstitution protocol. The interface includes the following messages:	matchesIn: aString	matchesIn: aString collect: aBlock	matchesIn: aString do: aBlock	matchesOnStream: aStream	matchesOnStream: aStream collect: aBlock	matchesOnStream: aStream do: aBlock	copy: aString translatingMatchesUsing: aBlock	copy: aString replacingMatchesWith: replacementString	copyStream: aStream to: writeStream translatingMatchesUsing: aBlock	copyStream: aStream to: writeStream replacingMatchesWith: aStringExamples:	'\w+' asRegex matchesIn: 'now is the time'returns an OrderedCollection containing four strings: 'now', 'is','the', and 'time'.	'\<t\w+' asRegexIgnoringCase		copy: 'now is the Time'		translatingMatchesUsing: [:match | match asUppercase]returns 'now is THE TIME' (the regular expression matches wordsbeginning with either an uppercase or a lowercase T).ACKNOWLEDGEMENTSSince the first release of the matcher, thanks to the input fromseveral fellow Smalltalkers, I became convinced a native Smalltalkregular expression matcher was worth the effort to keep it alive. Forthe contributions, suggestions, and bug reports that made this release possible, I want to thank:	Felix Hack	Peter Hatch	Alan Knight	Eliot Miranda	Thomas Muhr	Robb Shecter	David N. Smith	Francis Wolinskiand anyone whom I haven't yet met or heard from, but who agrees thishas not been a complete waste of time.--Vassili BykovOctober 3, 1999"	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'lr 1/7/2010 20:10'!
c: x syntax: xx" [You can select and `print it' examples in this method. Just don'tforget to cancel the changes.]The simplest regular expression is a single character.  It matchesexactly that character. A sequence of characters matches a string withexactly the same sequence of characters:	'a' matchesRegex: 'a'				-- true	'foobar' matchesRegex: 'foobar'		-- true	'blorple' matchesRegex: 'foobar'		-- falseThe above paragraph introduced a primitive regular expression (acharacter), and an operator (sequencing). Operators are applied toregular expressions to produce more complex regular expressions.Sequencing (placing expressions one after another) as an operator is,in a certain sense, `invisible'--yet it is arguably the most common.A more `visible' operator is Kleene closure, more often simplyreferred to as `a star'.  A regular expression followed by an asteriskmatches any number (including 0) of matches of the originalexpression. For example:	'ab' matchesRegex: 'a*b'		 		-- true	'aaaaab' matchesRegex: 'a*b'	 	-- true	'b' matchesRegex: 'a*b'		 		-- true	'aac' matchesRegex: 'a*b'	 		-- false: b does not matchA star's precedence is higher than that of sequencing. A star appliesto the shortest possible subexpression that precedes it. For example,'ab*' means `a followed by zero or more occurrences of b', not `zeroor more occurrences of ab':	'abbb' matchesRegex: 'ab*'	 		-- true	'abab' matchesRegex: 'ab*'		 	-- falseTo actually make a regex matching `zero or more occurrences of ab',`ab' is enclosed in parentheses:	'abab' matchesRegex: '(ab)*'		 	-- true	'abcab' matchesRegex: '(ab)*'	 	-- false: c spoils the funTwo other operators similar to `*' are `+' and `?'. `+' (positiveclosure, or simply `plus') matches one or more occurrences of theoriginal expression. `?' (`optional') matches zero or one, but nevermore, occurrences.	'ac' matchesRegex: 'ab*c'	 		-- true	'ac' matchesRegex: 'ab+c'	 		-- false: need at least one b	'abbc' matchesRegex: 'ab+c'		 	-- true	'abbc' matchesRegex: 'ab?c'		 	-- false: too many b'sAs we have seen, characters `*', `+', `?', `(', and `)' have specialmeaning in regular expressions. If one of them is to be usedliterally, it should be quoted: preceded with a backslash. (Thus,backslash is also special character, and needs to be quoted for aliteral match--as well as any other special character describedfurther).	'ab*' matchesRegex: 'ab*'		 	-- false: star in the right string is special	'ab*' matchesRegex: 'ab\*'	 		-- true	'a\c' matchesRegex: 'a\\c'		 	-- trueThe last operator is `|' meaning `or'. It is placed between tworegular expressions, and the resulting expression matches if one ofthe expressions matches. It has the lowest possible precedence (lowerthan sequencing). For example, `ab*|ba*' means `a followed by anynumber of b's, or b followed by any number of a's':	'abb' matchesRegex: 'ab*|ba*'	 	-- true	'baa' matchesRegex: 'ab*|ba*'	 	-- true	'baab' matchesRegex: 'ab*|ba*'	 	-- falseA bit more complex example is the following expression, matching thename of any of the Lisp-style `car', `cdr', `caar', `cadr',... functions:	c(a|d)+rIt is possible to write an expression matching an empty string, forexample: `a|'.  However, it is an error to apply `*', `+', or `?' tosuch expression: `(a|)*' is an invalid expression.So far, we have used only characters as the 'smallest' components ofregular expressions. There are other, more `interesting', components.A character set is a string of characters enclosed in squarebrackets. It matches any single character if it appears between thebrackets. For example, `[01]' matches either `0' or `1':	'0' matchesRegex: '[01]'		 		-- true	'3' matchesRegex: '[01]'		 		-- false	'11' matchesRegex: '[01]'		 		-- false: a set matches only one characterUsing plus operator, we can build the following binary numberrecognizer:	'10010100' matchesRegex: '[01]+'	 	-- true	'10001210' matchesRegex: '[01]+'	 	-- falseIf the first character after the opening bracket is `^', the set isinverted: it matches any single character *not* appearing between thebrackets:	'0' matchesRegex: '[^01]'		  		-- false	'3' matchesRegex: '[^01]'		 		-- trueFor convenience, a set may include ranges: pairs of charactersseparated with `-'. This is equivalent to listing all charactersbetween them: `[0-9]' is the same as `[0123456789]'.Special characters within a set are `^', `-', and `]' that closes theset. Below are the examples of how to literally use them in a set:	[01^]		-- put the caret anywhere except the beginning	[01-]		-- put the dash as the last character	[]01]		-- put the closing bracket as the first character 	[^]01]			(thus, empty and universal sets cannot be specified)Regular expressions can also include the following backquote escapesto refer to popular classes of characters:	\w	any word constituent character (same as [a-zA-Z0-9_])	\W	any character but a word constituent	\d	a digit (same as [0-9])	\D	anything but a digit	\s 	a whitespace character (same as [:space:] below)	\S	anything but a whitespace characterThese escapes are also allowed in character classes: '[\w+-]' means'any character that is either a word constituent, or a plus, or aminus'.Character classes can also include the following grep(1)-compatibleelements to refer to:	[:alnum:]		any alphanumeric character (same as [a-zA-Z0-9])	[:alpha:]		any alphabetic character (same as [a-zA-Z])	[:cntrl:]		any control character. (any character with code < 32)	[:digit:]		any decimal digit (same as [0-9])	[:graph:]		any graphical character. (any character with code >= 32).	[:lower:]		any lowercase character (including non-ASCII lowercase characters)	[:print:]		any printable character. In this version, this is the same as [:graph:]	[:punct:]		any punctuation character:  . , !! ? ; : ' - ( ) ` and double quotes	[:space:]		any whitespace character (space, tab, CR, LF, null, form feed, Ctrl-Z, 16r2000-16r200B, 16r3000)	[:upper:]		any uppercase character (including non-ASCII uppercase characters)	[:xdigit:]		any hexadecimal character (same as [a-fA-F0-9]).Note that many of these are only as consistent or inconsistent on issuesof locale as the underlying Smalltalk implementation. Values shown hereare for VisualWorks 7.6.Note that these elements are components of the character classes,i.e. they have to be enclosed in an extra set of square brackets toform a valid regular expression.  For example, a non-empty string ofdigits would be represented as '[[:digit:]]+'.The above primitive expressions and operators are common to manyimplementations of regular expressions. The next primitive expressionis unique to this Smalltalk implementation.A sequence of characters between colons is treated as a unary selectorwhich is supposed to be understood by Characters. A character matchessuch an expression if it answers true to a message with thatselector. This allows a more readable and efficient way of specifyingcharacter classes. For example, `[0-9]' is equivalent to `:isDigit:',but the latter is more efficient. Analogously to character sets,character classes can be negated: `:^isDigit:' matches a Characterthat answers false to #isDigit, and is therefore equivalent to`[^0-9]'.As an example, so far we have seen the following equivalent ways towrite a regular expression that matches a non-empty string of digits:	'[0-9]+'	'\d+'	'[\d]+'	'[[:digit:]]+'	:isDigit:+'The last group of special primitive expressions includes: 	.	matching any character except a NULL; 	^	matching an empty string at the beginning of a line; 	$	matching an empty string at the end of a line.	\b	an empty string at a word boundary	\B	an empty string not at a word boundary	\<	an empty string at the beginning of a word	\>	an empty string at the end of a word	'axyzb' matchesRegex: 'a.+b'		-- true	'ax zb' matchesRegex: 'a.+b'			-- true (space is matched by `.')	'axzb' matchesRegex: 'a.+b'				-- true (carriage return is matched by `.')Again, the dot ., caret ^ and dollar $ characters are special and should be quotedto be matched literally.	EXAMPLESAs the introductions said, a great use for regular expressions is userinput validation. Following are a few examples of regular expressionsthat might be handy in checking input entered by the user in an inputfield. Try them out by entering something between the quotes andprint-iting. (Also, try to imagine Smalltalk code that each validationwould require if coded by hand).  Most example expressions could havebeen written in alternative ways.Checking if aString may represent a nonnegative integer number:	'' matchesRegex: ':isDigit:+'or	'' matchesRegex: '[0-9]+'or	'' matchesRegex: '\d+'Checking if aString may represent an integer number with an optionalsign in front:	'' matchesRegex: '(\+|-)?\d+'Checking if aString is a fixed-point number, with at least one digitis required after a dot:	'' matchesRegex: '(\+|-)?\d+(\.\d+)?'The same, but allow notation like `123.':	'' matchesRegex: '(\+|-)?\d+(\.\d*)?'Recognizer for a string that might be a name: one word with firstcapital letter, no blanks, no digits.  More traditional:	'' matchesRegex: '[A-Z][A-Za-z]*'more Smalltalkish:	'' matchesRegex: ':isUppercase::isAlphabetic:*'A date in format MMM DD, YYYY with any number of spaces in between, inXX century:	'' matchesRegex: '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(\d\d?)[ ]*,[ ]*19(\d\d)'Note parentheses around some components of the expression above. As`Usage' section shows, they will allow us to obtain the actual stringsthat have matched them (i.e. month name, day number, and year number).For dessert, coming back to numbers: here is a recognizer for ageneral number format: anything like 999, or 999.999, or -999.999e+21.	'' matchesRegex: '(\+|-)?\d+(\.\d*)?((e|E)(\+|-)?\d+)?'"	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'vb 4/11/09 21:56'!
d: x usage: xx" The preceding section covered the syntax of regular expressions. Itused the simplest possible interface to the matcher: sending#matchesRegex: message to the sample string, with regular expressionstring as the argument.  This section explains hairier ways of usingthe matcher.	PREFIX MATCHING AND CASE-INSENSITIVE MATCHINGA CharacterArray (an EsString in VA) also understands these messages:	#prefixMatchesRegex: regexString	#matchesRegexIgnoringCase: regexString	#prefixMatchesRegexIgnoringCase: regexString#prefixMatchesRegex: is just like #matchesRegex, except that the wholereceiver is not expected to match the regular expression passed as theargument; matching just a prefix of it is enough.  For example:	'abcde' matchesRegex: '(a|b)+'		-- false	'abcde' prefixMatchesRegex: '(a|b)+'	-- trueThe last two messages are case-insensitive versions of matching.	ENUMERATION INTERFACEAn application can be interested in all matches of a certain regularexpression within a String.  The matches are accessible using aprotocol modelled after the familiar Collection-like enumerationprotocol:	#regex: regexString matchesDo: aBlockEvaluates a one-argument <aBlock> for every match of the regularexpression within the receiver string.	#regex: regexString matchesCollect: aBlockEvaluates a one-argument <aBlock> for every match of the regularexpression within the receiver string. Collects results of evaluationsand anwers them as a SequenceableCollection.	#allRegexMatches: regexStringReturns a collection of all matches (substrings of the receiverstring) of the regular expression.  It is an equivalent of <aStringregex: regexString matchesCollect: [:each | each]>.	#allRangesOfRegexMatches: regexStringReturns a collection of all character ranges (startIndex to: stopIndex)that match the regular expression.	REPLACEMENT AND TRANSLATIONIt is possible to replace all matches of a regular expression with acertain string using the message:	#copyWithRegex: regexString matchesReplacedWith: aStringFor example:	'ab cd ab' copyWithRegex: '(a|b)+' matchesReplacedWith: 'foo'A more general substitution is match translation:	#copyWithRegex: regexString matchesTranslatedUsing: aBlockThis message evaluates a block passing it each match of the regularexpression in the receiver string and answers a copy of the receiverwith the block results spliced into it in place of the respectivematches.  For example:	'ab cd ab' copyWithRegex: '(a|b)+' matchesTranslatedUsing: [:each | each asUppercase]All messages of enumeration and replacement protocols perform acase-sensitive match.  Case-insensitive versions are not provided aspart of a CharacterArray protocol.  Instead, they are accessible usingthe lower-level matching interface.	LOWER-LEVEL INTERFACEInternally, #matchesRegex: works as follows:1. A fresh instance of RxParser is created, and the regular expressionstring is passed to it, yielding the expression's syntax tree.2. The syntax tree is passed as an initialization parameter to aninstance of RxMatcher. The instance sets up some data structure thatwill work as a recognizer for the regular expression described by thetree.3. The original string is passed to the matcher, and the matcherchecks for a match.	THE MATCHERIf you repeatedly match a number of strings against the same regularexpression using one of the messages defined in CharacterArray, theregular expression string is parsed and a matcher is created anew forevery match.  You can avoid this overhead by building a matcher forthe regular expression, and then reusing the matcher over and overagain. You can, for example, create a matcher at a class or instanceinitialization stage, and store it in a variable for future use.You can create a matcher using one of the following methods:	- Sending #forString:ignoreCase: message to RxMatcher class, withthe regular expression string and a Boolean indicating whether case isignored as arguments.	- Sending #forString: message.  It is equivalent to <... forString:regexString ignoreCase: false>.A more convenient way is using one of the two matcher-created messagesunderstood by CharacterArray.	- <regexString asRegex> is equivalent to <RxMatcher forString:regexString>.	- <regexString asRegexIgnoringCase> is equivalent to <RxMatcherforString: regexString ignoreCase: true>.Here are four examples of creating a matcher:	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+'	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+' ignoreCase: false	hexRecognizer := '16r[0-9A-Fa-f]+' asRegex	hexRecognizer := '16r[0-9A-F]+' asRegexIgnoringCase	MATCHINGThe matcher understands these messages (all of them return true toindicate successful match or search, and false otherwise):matches: aString	True if the whole target string (aString) matches.matchesPrefix: aString	True if some prefix of the string (not necessarily the whole	string) matches.search: aString	Search the string for the first occurrence of a matching	substring. (Note that the first two methods only try matching from	the very beginning of the string). Using the above example with a	matcher for `a+', this method would answer success given a string	`baaa', while the previous two would fail.matchesStream: aStreammatchesStreamPrefix: aStreamsearchStream: aStream	Respective analogs of the first three methods, taking input from a	stream instead of a string. The stream must be positionable and	peekable.All these methods answer a boolean indicating success. The matcheralso stores the outcome of the last match attempt and can report it:lastResult	Answers a Boolean -- the outcome of the most recent match	attempt. If no matches were attempted, the answer is unspecified.	SUBEXPRESSION MATCHESAfter a successful match attempt, you can query the specifics of whichpart of the original string has matched which part of the wholeexpression.A subexpression is a parenthesized part of a regular expression, orthe whole expression. When a regular expression is compiled, itssubexpressions are assigned indices starting from 1, depth-first,left-to-right. For example, `((ab)+(c|d))?ef' includes the followingsubexpressions with these indices:	1:	((ab)+(c|d))?ef	2:	(ab)+(c|d)	3:	ab	4:	c|dAfter a successful match, the matcher can report what part of theoriginal string matched what subexpression. It understandards thesemessages:subexpressionCount	Answers the total number of subexpressions: the highest value that	can be used as a subexpression index with this matcher. This value	is available immediately after initialization and never changes.subexpression: anIndex	An index must be a valid subexpression index, and this message	must be sent only after a successful match attempt. The method	answers a substring of the original string the corresponding	subexpression has matched to.subBeginning: anIndexsubEnd: anIndex	Answer positions within the original string or stream where the	match of a subexpression with the given index has started and	ended, respectively.This facility provides a convenient way of extracting parts of inputstrings of complex format. For example, the following piece of codeuses the 'MMM DD, YYYY' date format recognizer example from the`Syntax' section to convert a date to a three-element array with year,month, and day strings (you can select and evaluate it right here):	| matcher |	matcher := RxMatcher forString: '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(:isDigit::isDigit:?)[ ]*,[ ]*(19|20)(:isDigit::isDigit:)'.	(matcher matches: 'Aug 6, 1996')		ifTrue: 			[Array 				with: (matcher subexpression: 5)				with: (matcher subexpression: 2)				with: (matcher subexpression: 3)]		ifFalse: ['no match'](should answer ` #('96' 'Aug' '6')').	ENUMERATION AND REPLACEMENTThe enumeration and replacement protocols exposed in CharacterArrayare actually implemented by the matcher.  The following messages areunderstood:	#matchesIn: aString	#matchesIn: aString do: aBlock	#matchesIn: aString collect: aBlock	#copy: aString replacingMatchesWith: replacementString	#copy: aString translatingMatchesUsing: aBlock	#matchingRangesIn: aString	#matchesOnStream: aStream	#matchesOnStream: aStream do: aBlock	#matchesOnStream: aStream collect: aBlock	#copy: sourceStream to: targetStream replacingMatchesWith: replacementString	#copy: sourceStream to: targetStream translatingMatchesWith: aBlockNote that in those methods that take a block, the block may refer to the rxMatcher itself, e.g. to collect information about the position the match occurred at, or thesubexpressions of the match. An example can be seen in #matchingRangesIn:	ERROR HANDLINGException signaling objects (Signals in VisualWorks, Exceptions in VisualAge) areaccessible through RxParser class protocol. To handle possible errors, usethe protocol described below to obtain the exception objects and use theprotocol of the native Smalltalk implementation to handle them.If a syntax error is detected while parsing expression,RxParser>>syntaxErrorSignal is raised/signaled.If an error is detected while building a matcher,RxParser>>compilationErrorSignal is raised/signaled.If an error is detected while matching (for example, if a bad selectorwas specified using `:<selector>:' syntax, or because of the matcher'sinternal error), RxParser>>matchErrorSignal is raisedRxParser>>regexErrorSignal is the parent of all three.  Since any ofthe three signals can be raised within a call to #matchesRegex:, it ishandy if you want to catch them all.  For example:VisualWorks:	RxParser regexErrorSignal		handle: [:ex | ex returnWith: nil]		do: ['abc' matchesRegex: '))garbage[']VisualAge:	['abc' matchesRegex: '))garbage[']		when: RxParser regexErrorSignal		do: [:signal | signal exitWith: nil]"	self error: 'comment only'! !

!RxParser class methodsFor: 'exception signaling' stamp: 'lr 11/4/2009 22:37'!
doHandlingMessageNotUnderstood: aBlock	"MNU should be trapped and resignaled as a match error in a few places in the matcher.	This method factors out this dialect-dependent code to make porting easier."	^ aBlock		on: MessageNotUnderstood		do: [:ex | RxParser signalMatchException: 'invalid predicate selector']! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'vb 4/11/09 21:56'!
e: x implementationNotes: xx"		Version:		1.1	Released:		October 1999	Mail to:		Vassili Bykov <vassili@parcplace.com>, <v_bykov@yahoo.com>	Flames to:		/dev/null	WHAT IS ADDEDThe matcher includes classes in two categories:	VB-Regex-Syntax	VB-Regex-Matcherand a few CharacterArray methods in `VB-regex' protocol.  No systemclasses or methods are modified.	WHAT TO LOOK AT FIRSTString>>matchesRegex: -- in 90% cases this method is all you need toaccess the package.RxParser -- accepts a string or a stream of characters with a regularexpression, and produces a syntax tree corresponding to theexpression. The tree is made of instances of Rxs<whatever> classes.RxMatcher -- accepts a syntax tree of a regular expression built bythe parser and compiles it into a matcher: a structure made ofinstances of Rxm<whatever> classes. The RxMatcher instance can testwhether a string or a positionable stream of characters matches theoriginal regular expression, or search a string or a stream forsubstrings matching the expression. After a match is found, thematcher can report a specific string that matched the wholeexpression, or any parenthesized subexpression of it.All other classes support the above functionality and are used byRxParser, RxMatcher, or both.	CAVEATSThe matcher is similar in spirit, but NOT in the design--let alone thecode--to the original Henry Spencer's regular expressionimplementation in C.  The focus is on simplicity, not on efficiency.I didn't optimize or profile anything.  I may in future--or I may not:I do this in my spare time and I don't promise anything.The matcher passes H. Spencer's test suite (see 'test suite'protocol), with quite a few extra tests added, so chances are goodthere are not too many bugs.  But watch out anyway.	EXTENSIONS, FUTURE, ETC.With the existing separation between the parser, the syntax tree, andthe matcher, it is easy to extend the system with other matchers basedon other algorithms. In fact, I have a DFA-based matcher right now,but I don't feel it is good enough to include it here.  I might addautomata-based matchers later, but again I don't promise anything.	HOW TO REACH MEAs of today (December 20, 2000), you can contact me at<vassili@parcplace.com>. If this doesn't work, look aroundcomp.lang.smalltalk or comp.lang.lisp.  "	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'vb 4/11/09 21:56'!
f: x boringStuff: xx"The Regular Expression Matcher (``The Software'') is Copyright (C) 1996, 1999 Vassili Bykov.  It is provided to the Smalltalk community in hope it will be useful.1. This license applies to the package as a whole, as well as to any   component of it. By performing any of the activities described   below, you accept the terms of this agreement.2. The software is provided free of charge, and ``as is'', in hope   that it will be useful, with ABSOLUTELY NO WARRANTY. The entire   risk and all responsibility for the use of the software is with   you.  Under no circumstances the author may be held responsible for   loss of data, loss of profit, or any other damage resulting   directly or indirectly from the use of the software, even if the   damage is caused by defects in the software.3. You may use this software in any applications you build.4. You may distribute this software provided that the software   documentation and copyright notices are included and intact.5. You may create and distribute modified versions of the software,   such as ports to other Smalltalk dialects or derived work, provided   that:    a. any modified version is expressly marked as such and is not   misrepresented as the original software;    b. credit is given to the original software in the source code and   documentation of the derived work;    c. the copyright notice at the top of this document accompanies   copyright notices of any modified version.  "	self error: 'comment only'! !

!RxParser class methodsFor: 'class initialization' stamp: 'avi 11/30/2003 13:26'!
initialize	"self initialize"	self		initializeBackslashConstants;		initializeBackslashSpecials! !

!RxParser class methodsFor: 'class initialization' stamp: 'lr 11/4/2009 22:14'!
initializeBackslashConstants	"self initializeBackslashConstants"	(BackslashConstants := Dictionary new)		at: $e put: Character escape;		at: $n put: Character lf;		at: $r put: Character cr;		at: $f put: Character newPage;		at: $t put: Character tab! !

!RxParser class methodsFor: 'class initialization' stamp: 'vb 4/11/09 21:56'!
initializeBackslashSpecials	"Keys are characters that normally follow a \, the values are	associations of classes and initialization selectors on the instance side	of the classes."	"self initializeBackslashSpecials"	(BackslashSpecials := Dictionary new)		at: $w put: (Association key: RxsPredicate value: #beWordConstituent);		at: $W put: (Association key: RxsPredicate value: #beNotWordConstituent);		at: $s put: (Association key: RxsPredicate value: #beSpace);		at: $S put: (Association key: RxsPredicate value: #beNotSpace);		at: $d put: (Association key: RxsPredicate value: #beDigit);		at: $D put: (Association key: RxsPredicate value: #beNotDigit);		at: $b put: (Association key: RxsContextCondition value: #beWordBoundary);		at: $B put: (Association key: RxsContextCondition value: #beNonWordBoundary);		at: $< put: (Association key: RxsContextCondition value: #beBeginningOfWord);		at: $> put: (Association key: RxsContextCondition value: #beEndOfWord)! !

!RxParser class methodsFor: 'utilities' stamp: 'vb 4/11/09 21:56'!
parse: aString	"Parse the argument and return the result (the parse tree).	In case of a syntax error, the corresponding exception is signaled."	^self new parse: aString! !

!RxParser class methodsFor: 'preferences' stamp: 'vb 4/11/09 21:56'!
preferredMatcherClass	"The matcher to use. For now just one is available, but in	principle this determines the matchers built implicitly,	such as by String>>asRegex, or String>>matchesRegex:.	This might seem a bit strange place for this preference, but	Parser is still more or less `central' thing in the whole package."	^RxMatcher! !

!RxParser class methodsFor: 'utilities' stamp: 'avi 11/30/2003 13:23'!
safelyParse: aString	"Parse the argument and return the result (the parse tree).	In case of a syntax error, return nil.	Exception handling here is dialect-dependent."	^ [self new parse: aString] on: RegexSyntaxError do: [:ex | nil]! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalCompilationException: errorString	RegexCompilationError new signal: errorString! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalMatchException: errorString	RegexMatchingError new signal: errorString! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalSyntaxException: errorString	RegexSyntaxError new signal: errorString! !

!RxmBranch methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
alternative: aBranch	"See class comment for instance variable description."	alternative := aBranch! !

!RxmBranch methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beLoopback	"See class comment for instance variable description."	loopback := true! !

!RxmBranch methodsFor: 'initialize-release' stamp: 'lr 11/4/2009 22:38'!
initialize	"See class comment for instance variable description."	super initialize.	loopback := false! !

!RxmBranch methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"Match either `next' or `alternative'. Fail if the alternative is nil."	^(next matchAgainst: aMatcher)		or: [alternative notNil			and: [alternative matchAgainst: aMatcher]]! !

!RxmBranch methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
pointTailTo: aNode	"See superclass for explanations."	loopback		ifTrue: [alternative == nil			ifTrue: [alternative := aNode]			ifFalse: [alternative pointTailTo: aNode]]		ifFalse: [super pointTailTo: aNode]! !

!RxmBranch methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
terminateWith: aNode	"See superclass for explanations."	loopback		ifTrue: [alternative == nil			ifTrue: [alternative := aNode]			ifFalse: [alternative terminateWith: aNode]]		ifFalse: [super terminateWith: aNode]! !

!RxmLink methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"If a link does not match the contents of the matcher's stream,	answer false. Otherwise, let the next matcher in the chain match."	^next matchAgainst: aMatcher! !

!RxmLink methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
next		^next! !

!RxmLink methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
next: aLink	"Set the next link, either an RxmLink or an RxmTerminator."	next := aLink! !

!RxmLink methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
pointTailTo: anRxmLink	"Propagate this message along the chain of links.	Point `next' reference of the last link to <anRxmLink>.	If the chain is already terminated, blow up."	next == nil		ifTrue: [next := anRxmLink]		ifFalse: [next pointTailTo: anRxmLink]! !

!RxmLink methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
terminateWith: aTerminator	"Propagate this message along the chain of links, and	make aTerminator the `next' link of the last link in the chain.	If the chain is already reminated with the same terminator, 	do not blow up."	next == nil		ifTrue: [next := aTerminator]		ifFalse: [next terminateWith: aTerminator]! !

!RxmMarker methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
index: anIndex	"An index is a key that makes sense for the matcher.	This key can be passed to marker position getters and	setters to access position for this marker in the current	matching session."	index := anIndex! !

!RxmMarker methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"If the rest of the link chain matches successfully, report the	position of the stream *before* the match started to the matcher."	| startPosition |	startPosition := aMatcher position.	(next matchAgainst: aMatcher)		ifTrue:			[aMatcher markerPositionAt: index add: startPosition.			^true].	^false! !

!RxmPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
bePerform: aSelector	"Match any single character that answers true  to this message."	self predicate: 		[:char | 		RxParser doHandlingMessageNotUnderstood: [char perform: aSelector]]! !

!RxmPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
bePerformNot: aSelector	"Match any single character that answers false to this message."	self predicate: 		[:char | 		RxParser doHandlingMessageNotUnderstood: [(char perform: aSelector) not]]! !

!RxmPredicate methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"Match if the predicate block evaluates to true when given the	current stream character as the argument."	| original |	original := aMatcher currentState.	(aMatcher atEnd not 		and: [(predicate value: aMatcher next)			and: [next matchAgainst: aMatcher]])		ifTrue: [^true]		ifFalse:			[aMatcher restoreState: original.			^false]! !

!RxmPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
predicate: aBlock	"This link will match any single character for which <aBlock>	evaluates to true."	aBlock numArgs ~= 1 ifTrue: [self error: 'bad predicate block'].	predicate := aBlock.	^self! !

!RxmPredicate class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
with: unaryBlock	^self new predicate: unaryBlock! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beBeginningOfLine	matchSelector := #atBeginningOfLine! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beBeginningOfWord	matchSelector := #atBeginningOfWord! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beEndOfLine	matchSelector := #atEndOfLine! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beEndOfWord	matchSelector := #atEndOfWord! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beNotWordBoundary	matchSelector := #notAtWordBoundary! !

!RxmSpecial methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beWordBoundary	matchSelector := #atWordBoundary! !

!RxmSpecial methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"Match without consuming any input, if the matcher is	in appropriate state."	^(aMatcher perform: matchSelector)		and: [next matchAgainst: aMatcher]! !

!RxmSubstring methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beCaseInsensitive	compare := [:char1 :char2 | char1 sameAs: char2]! !

!RxmSubstring methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beCaseSensitive	compare := [:char1 :char2 | char1 = char2]! !

!RxmSubstring methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
character: aCharacter ignoreCase: aBoolean	"Match exactly this character."	sample := String with: aCharacter.	aBoolean ifTrue: [self beCaseInsensitive]! !

!RxmSubstring methodsFor: 'initialize-release' stamp: 'lr 11/4/2009 22:38'!
initialize	super initialize.	self beCaseSensitive! !

!RxmSubstring methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aMatcher	"Match if my sample stream is exactly the current prefix	of the matcher stream's contents."	| originalState sampleStream mismatch |	originalState := aMatcher currentState.	sampleStream := self sampleStream.	mismatch := false.	[sampleStream atEnd		or: [aMatcher atEnd		or: [mismatch := (compare value: sampleStream next value: aMatcher next) not]]] whileFalse.	(mismatch not and: [sampleStream atEnd and: [next matchAgainst: aMatcher]])		ifTrue: [^true]		ifFalse: 			[aMatcher restoreState: originalState.			^false]! !

!RxmSubstring methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
sampleStream	^sample readStream! !

!RxmSubstring methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
substring: aString ignoreCase: aBoolean	"Match exactly this string."	sample := aString.	aBoolean ifTrue: [self beCaseInsensitive]! !

!RxmTerminator methodsFor: 'matching' stamp: 'vb 4/11/09 21:56'!
matchAgainst: aStream	"If got here, the match is successful."	^true! !

!RxmTerminator methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
pointTailTo: anRxmLink	"Branch tails are never redirected by the build algorithm.	Healthy terminators should never receive this."	RxParser signalCompilationException:		'internal matcher build error - redirecting terminator tail'! !

!RxmTerminator methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
terminateWith: aTerminator	"Branch terminators are never supposed to change.	Make sure this is the case."	aTerminator ~~ self		ifTrue: [RxParser signalCompilationException:				'internal matcher build error - wrong terminator']! !

!RxsBranch methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
branch	^branch! !

!RxsBranch methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aMatcher	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aMatcher syntaxBranch: self! !

!RxsBranch methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializePiece: aPiece branch: aBranch	"See class comment for instance variables description."	piece := aPiece.	branch := aBranch! !

!RxsBranch methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	^piece isNullable and: [branch isNil or: [branch isNullable]]! !

!RxsBranch methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
piece	^piece! !

!RxsBranch methodsFor: 'optimization' stamp: 'vb 4/11/09 21:56'!
tryMergingInto: aStream	"Concatenation of a few simple characters can be optimized	to be a plain substring match. Answer the node to resume	syntax tree traversal at. Epsilon node used to terminate the branch	will implement this to answer nil, thus indicating that the branch	has ended."	piece isAtomic ifFalse: [^self].	aStream nextPut: piece character.	^branch isNil		ifTrue: [branch]		ifFalse: [branch tryMergingInto: aStream]! !

!RxsCharSet methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aMatcher	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aMatcher syntaxCharSet: self! !

!RxsCharSet methodsFor: 'privileged' stamp: 'vb 4/11/09 21:56'!
enumerablePartPredicateIgnoringCase: aBoolean	| enumeration |	enumeration := self optimalSetIgnoringCase: aBoolean.	^negated		ifTrue: [[:char | (enumeration includes: char) not]]		ifFalse: [[:char | enumeration includes: char]]! !

!RxsCharSet methodsFor: 'privileged' stamp: 'vb 4/11/09 21:56'!
enumerableSetIgnoringCase: aBoolean	"Answer a collection of characters that make up the portion of me	that can be enumerated."	| set |	set := Set new.	elements do:		[:each |		each isEnumerable ifTrue:			[each enumerateTo: set ignoringCase: aBoolean]].	^set! !

!RxsCharSet methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
hasPredicates	^elements contains: [:some | some isEnumerable not]! !

!RxsCharSet methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeElements: aCollection negated: aBoolean	"See class comment for instance variables description."	elements := aCollection.	negated := aBoolean! !

!RxsCharSet methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isEnumerable	elements detect: [:some | some isEnumerable not] ifNone: [^true].	^false! !

!RxsCharSet methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNegated	^negated! !

!RxsCharSet methodsFor: 'privileged' stamp: 'lr 11/4/2009 22:27'!
optimalSetIgnoringCase: aBoolean	"Assuming the client with search the `set' using #includes:,	answer a collection with the contents of `set', of the class	that will provide the fastest lookup. Strings are faster than	Sets for short strings."	| set |	set := self enumerableSetIgnoringCase: aBoolean.	^set size < 10		ifTrue: [set asArray]		ifFalse: [set]! !

!RxsCharSet methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
predicateIgnoringCase: aBoolean	| predicate enumerable |	enumerable := self enumerablePartPredicateIgnoringCase: aBoolean.	^self hasPredicates		ifFalse: [enumerable]		ifTrue:			[predicate := self predicatePartPredicate.			negated				ifTrue: [[:char | (enumerable value: char) and: [predicate value: char]]]				ifFalse: [[:char | (enumerable value: char) or: [predicate value: char]]]]! !

!RxsCharSet methodsFor: 'privileged' stamp: 'vb 4/11/09 21:56'!
predicatePartPredicate	"Answer a predicate that tests all of my elements that cannot be	enumerated."	| predicates |	predicates := elements reject: [:some | some isEnumerable].	predicates isEmpty		ifTrue: [^[:char | negated]].	predicates size = 1		ifTrue: [^negated			ifTrue: [predicates first predicateNegation]			ifFalse: [predicates first predicate]].	predicates := predicates collect: [:each | each predicate].	^negated		ifFalse:			[[:char | predicates contains: [:some | some value: char]]]		ifTrue:			[[:char | (predicates contains: [:some | some value: char]) not]]! !

!RxsCharSet methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
predicates	^(elements reject: [:some | some isEnumerable])		collect: [:each | each predicate]! !

!RxsCharacter methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
character	^character! !

!RxsCharacter methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aMatcher	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aMatcher syntaxCharacter: self! !

!RxsCharacter methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
enumerateTo: aSet ignoringCase: aBoolean	aBoolean		ifTrue: 			[aSet 				add: character asUppercase;				add: character asLowercase]		ifFalse: [aSet add: character]! !

!RxsCharacter methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeCharacter: aCharacter	"See class comment for instance variable description."	character := aCharacter! !

!RxsCharacter methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isAtomic	"A character is always atomic."	^true! !

!RxsCharacter methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isEnumerable	^true! !

!RxsCharacter methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	^false! !

!RxsCharacter class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
with: aCharacter	^self new initializeCharacter: aCharacter! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beAny	"Matches anything but a newline."	kind := #syntaxAny! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beBeginningOfLine	"Matches empty string at the beginning of a line."	kind := #syntaxBeginningOfLine! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beBeginningOfWord	"Matches empty string at the beginning of a word."	kind := #syntaxBeginningOfWord! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beEndOfLine	"Matches empty string at the end of a line."	kind := #syntaxEndOfLine! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beEndOfWord	"Matches empty string at the end of a word."	kind := #syntaxEndOfWord! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beNonWordBoundary	"Analog of \B."	kind := #syntaxNonWordBoundary! !

!RxsContextCondition methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beWordBoundary	"Analog of \w (alphanumeric plus _)."	kind := #syntaxWordBoundary! !

!RxsContextCondition methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aBuilder	^aBuilder perform: kind! !

!RxsContextCondition methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	^#syntaxAny ~~ kind! !

!RxsEpsilon methodsFor: 'building' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aBuilder	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aBuilder syntaxEpsilon! !

!RxsEpsilon methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	"See comment in the superclass."	^true! !

!RxsMessagePredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aBuilder	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aBuilder syntaxMessagePredicate: self! !

!RxsMessagePredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeSelector: aSelector	"The selector must be a one-argument message understood by Character."	selector := aSelector! !

!RxsMessagePredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeSelector: aSelector negated: aBoolean	"The selector must be a one-argument message understood by Character."	selector := aSelector.	negated := aBoolean! !

!RxsMessagePredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
negated	^negated! !

!RxsMessagePredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
selector	^selector! !

!RxsNode methodsFor: 'constants' stamp: 'vb 4/11/09 21:56'!
indentCharacter	"Normally, #printOn:withIndent: method in subclasses	print several characters returned by this method to indicate	the tree structure."	^$+! !

!RxsNode methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isAtomic	"Answer whether the node is atomic, i.e. matches exactly one 	constant predefined normal character.  A matcher may decide to 	optimize matching of a sequence of atomic nodes by glueing them 	together in a string."	^false "tentatively"! !

!RxsNode methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	"True if the node can match an empty sequence of characters."	^false "for most nodes"! !

!RxsPiece methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
atom	^atom! !

!RxsPiece methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
character	"If this node is atomic, answer the character it	represents. It is the caller's responsibility to make sure this	node is indeed atomic before using this."	^atom character! !

!RxsPiece methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aMatcher	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aMatcher syntaxPiece: self! !

!RxsPiece methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeAtom: anAtom	"This piece is exactly one occurrence of the specified RxsAtom."	self initializeAtom: anAtom min: 1 max: 1! !

!RxsPiece methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeAtom: anAtom min: minOccurrences max: maxOccurrences	"This piece is from <minOccurrences> to <maxOccurrences> 	occurrences of the specified RxsAtom."	atom := anAtom.	min := minOccurrences.	max := maxOccurrences! !

!RxsPiece methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeOptionalAtom: anAtom	"This piece is 0 or 1 occurrences of the specified RxsAtom."	self initializeAtom: anAtom min: 0 max: 1! !

!RxsPiece methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializePlusAtom: anAtom	"This piece is one or more occurrences of the specified RxsAtom."	self initializeAtom: anAtom min: 1 max: nil! !

!RxsPiece methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeStarAtom: anAtom	"This piece is any number of occurrences of the atom."	self initializeAtom: anAtom min: 0 max: nil! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isAtomic	"A piece is atomic if only it contains exactly one atom	which is atomic (sic)."	^self isSingular and: [atom isAtomic]! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	"A piece is nullable if it allows 0 matches. 	This is often handy to know for optimization."	^min = 0 or: [atom isNullable]! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isOptional	^min = 0 and: [max = 1]! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isPlus	^min = 1 and: [max == nil]! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isSingular	"A piece with a range is 1 to 1 needs can be compiled	as a simple match."	^min = 1 and: [max = 1]! !

!RxsPiece methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isStar	^min = 0 and: [max == nil]! !

!RxsPiece methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
max	"The value answered may be nil, indicating infinity."	^max! !

!RxsPiece methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
min	^min! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beAlphaNumeric	predicate := [:char | char isAlphaNumeric].	negation := [:char | char isAlphaNumeric not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'lr 11/4/2009 22:29'!
beAlphabetic	predicate := [:char | char isLetter].	negation := [:char | char isLetter not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beBackslash	predicate := [:char | char == $\].	negation := [:char | char ~~ $\]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beControl	predicate := [:char | char asInteger < 32].	negation := [:char | char asInteger >= 32]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beDigit	predicate := [:char | char isDigit].	negation := [:char | char isDigit not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beGraphics	self		beControl;		negate! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beHexDigit	| hexLetters |	hexLetters := 'abcdefABCDEF'.	predicate := [:char | char isDigit or: [hexLetters includes: char]].	negation := [:char | char isDigit not and: [(hexLetters includes: char) not]]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beLowercase	predicate := [:char | char isLowercase].	negation := [:char | char isLowercase not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beNotDigit	self		beDigit;		negate! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beNotSpace	self		beSpace;		negate! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beNotWordConstituent	self		beWordConstituent;		negate! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
bePrintable	self		beControl;		negate! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
bePunctuation	| punctuationChars |	punctuationChars := #($. $, $!! $? $; $: $" $' $- $( $) $`).	predicate := [:char | punctuationChars includes: char].	negation := [:char | (punctuationChars includes: char) not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beSpace	predicate := [:char | char isSeparator].	negation := [:char | char isSeparator not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
beUppercase	predicate := [:char | char isUppercase].	negation := [:char | char isUppercase not]! !

!RxsPredicate methodsFor: 'initialize-release' stamp: 'lr 1/7/2010 20:06'!
beWordConstituent	predicate := [:char | char isAlphaNumeric or: [char == $_]].	negation := [:char | char isAlphaNumeric not and: [char ~~ $_]]! !

!RxsPredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: anObject	^anObject syntaxPredicate: self! !

!RxsPredicate methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isAtomic	"A predicate is a single character but the character is not known in advance."	^false! !

!RxsPredicate methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isEnumerable	^false! !

!RxsPredicate methodsFor: 'private' stamp: 'vb 4/11/09 21:56'!
negate	| tmp |	tmp := predicate.	predicate := negation.	negation := tmp! !

!RxsPredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
negated	^self copy negate! !

!RxsPredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
predicate	^predicate! !

!RxsPredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
predicateNegation	^negation! !

!RxsPredicate methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
value: aCharacter	^predicate value: aCharacter! !

!RxsPredicate class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
forEscapedLetter: aCharacter	^self new perform:		(EscapedLetterSelectors			at: aCharacter			ifAbsent: [RxParser signalSyntaxException: 'bad backslash escape'])! !

!RxsPredicate class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
forNamedClass: aString	^self new perform:		(NamedClassSelectors			at: aString			ifAbsent: [RxParser signalSyntaxException: 'bad character class name'])! !

!RxsPredicate class methodsFor: 'class initialization' stamp: 'vb 4/11/09 21:56'!
initialize	"self initialize"	self		initializeNamedClassSelectors;		initializeEscapedLetterSelectors! !

!RxsPredicate class methodsFor: 'class initialization' stamp: 'vb 4/11/09 21:56'!
initializeEscapedLetterSelectors	"self initializeEscapedLetterSelectors"	(EscapedLetterSelectors := Dictionary new)		at: $w put: #beWordConstituent;		at: $W put: #beNotWordConstituent;		at: $d put: #beDigit;		at: $D put: #beNotDigit;		at: $s put: #beSpace;		at: $S put: #beNotSpace;		at: $\ put: #beBackslash! !

!RxsPredicate class methodsFor: 'class initialization' stamp: 'vb 4/11/09 21:56'!
initializeNamedClassSelectors	"self initializeNamedClassSelectors"	(NamedClassSelectors := Dictionary new)		at: 'alnum' put: #beAlphaNumeric;		at: 'alpha' put: #beAlphabetic;		at: 'cntrl' put: #beControl;		at: 'digit' put: #beDigit;		at: 'graph' put: #beGraphics;		at: 'lower' put: #beLowercase;		at: 'print' put: #bePrintable;		at: 'punct' put: #bePunctuation;		at: 'space' put: #beSpace;		at: 'upper' put: #beUppercase;		at: 'xdigit' put: #beHexDigit! !

!RxsRange methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
enumerateTo: aSet ignoringCase: aBoolean	"Add all of the elements I represent to the collection."	first asInteger to: last asInteger do:		[:charCode | | character |		character := charCode asCharacter.		aBoolean		ifTrue: 			[aSet 				add: character asUppercase;				add: character asLowercase]		ifFalse: [aSet add: character]]! !

!RxsRange methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeFirst: aCharacter last: anotherCharacter	first := aCharacter.	last := anotherCharacter! !

!RxsRange methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isEnumerable	^true! !

!RxsRange class methodsFor: 'instance creation' stamp: 'vb 4/11/09 21:56'!
from: aCharacter to: anotherCharacter	^self new initializeFirst: aCharacter last: anotherCharacter! !

!RxsRegex methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
branch	^branch! !

!RxsRegex methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
dispatchTo: aMatcher	"Inform the matcher of the kind of the node, and it	will do whatever it has to."	^aMatcher syntaxRegex: self! !

!RxsRegex methodsFor: 'initialize-release' stamp: 'vb 4/11/09 21:56'!
initializeBranch: aBranch regex: aRegex	"See class comment for instance variable description."	branch := aBranch.	regex := aRegex! !

!RxsRegex methodsFor: 'testing' stamp: 'vb 4/11/09 21:56'!
isNullable	^branch isNullable or: [regex notNil and: [regex isNullable]]! !

!RxsRegex methodsFor: 'accessing' stamp: 'vb 4/11/09 21:56'!
regex	^regex! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:31'!
allRangesOfRegexMatches: rxString

	^rxString asRegex matchingRangesIn: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:32'!
allRegexMatches: rxString

	^rxString asRegex matchesIn: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:32'!
asRegex
	"Compile the receiver as a regex matcher. May raise RxParser>>syntaxErrorSignal
	or RxParser>>compilationErrorSignal.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^RxParser preferredMatcherClass for: (RxParser new parse: self)! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:33'!
asRegexIgnoringCase
	"Compile the receiver as a regex matcher. May raise RxParser>>syntaxErrorSignal
	or RxParser>>compilationErrorSignal.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^RxParser preferredMatcherClass
		for: (RxParser new parse: self)
		ignoreCase: true! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:33'!
copyWithRegex: rxString matchesReplacedWith: aString

	^rxString asRegex
		copy: self replacingMatchesWith: aString! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:33'!
copyWithRegex: rxString matchesTranslatedUsing: aBlock

	^rxString asRegex
		copy: self translatingMatchesUsing: aBlock! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:34'!
matchesRegex: regexString
	"Test if the receiver matches a regex.  May raise RxParser>>regexErrorSignal or
	child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegex matches: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:34'!
matchesRegexIgnoringCase: regexString
	"Test if the receiver matches a regex.  May raise RxParser>>regexErrorSignal or
	child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegexIgnoringCase matches: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:34'!
prefixMatchesRegex: regexString
	"Test if the receiver's prefix matches a regex.	
	May raise RxParser class>>regexErrorSignal or child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegex matchesPrefix: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:35'!
prefixMatchesRegexIgnoringCase: regexString
	"Test if the receiver's prefix matches a regex.	
	May raise RxParser class>>regexErrorSignal or child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegexIgnoringCase matchesPrefix: self! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:35'!
regex: rxString matchesCollect: aBlock

	^rxString asRegex matchesIn: self collect: aBlock! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:35'!
regex: rxString matchesDo: aBlock

	^rxString asRegex matchesIn: self do: aBlock! !

!String methodsFor: '*Regex-Core' stamp: 'gsa 12/20/2012 10:36'!
search: aString
	"compatibility method to make regexp and strings work polymorphicly"
	^ aString includesSubString: self! !
RxMatcher initialize!
RxParser initialize!
RxsPredicate initialize!
