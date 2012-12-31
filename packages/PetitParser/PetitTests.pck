'From Cuis 4.0 of 21 April 2012 [latest update: #1291] on 3 June 2012 at 12:01:25 pm'!
'Description Please enter a description for this package '!
!classDefinition: #PPAbstractParserTest category: #'PetitTests-Core'!
TestCase subclass: #PPAbstractParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Core'!
!classDefinition: 'PPAbstractParserTest class' category: #'PetitTests-Core'!
PPAbstractParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPArithmeticParser category: #'PetitTests-Examples'!
PPCompositeParser subclass: #PPArithmeticParser
	instanceVariableNames: 'terms addition factors multiplication power primary parentheses number'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Examples'!
!classDefinition: 'PPArithmeticParser class' category: #'PetitTests-Examples'!
PPArithmeticParser class
	instanceVariableNames: ''!

!classDefinition: #PPComposedTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPComposedTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPComposedTest class' category: #'PetitTests-Tests'!
PPComposedTest class
	instanceVariableNames: ''!

!classDefinition: #PPCompositeParserTest category: #'PetitTests-Core'!
PPAbstractParserTest subclass: #PPCompositeParserTest
	instanceVariableNames: 'parser result'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Core'!
!classDefinition: 'PPCompositeParserTest class' category: #'PetitTests-Core'!
PPCompositeParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPArithmeticParserTest category: #'PetitTests-Tests'!
PPCompositeParserTest subclass: #PPArithmeticParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPArithmeticParserTest class' category: #'PetitTests-Tests'!
PPArithmeticParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPExpressionParserTest category: #'PetitTests-Tests'!
PPArithmeticParserTest subclass: #PPExpressionParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPExpressionParserTest class' category: #'PetitTests-Tests'!
PPExpressionParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPExtensionTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPExtensionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPExtensionTest class' category: #'PetitTests-Tests'!
PPExtensionTest class
	instanceVariableNames: ''!

!classDefinition: #PPLambdaParser category: #'PetitTests-Examples'!
PPCompositeParser subclass: #PPLambdaParser
	instanceVariableNames: 'expression abstraction application variable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Examples'!
!classDefinition: 'PPLambdaParser class' category: #'PetitTests-Examples'!
PPLambdaParser class
	instanceVariableNames: ''!

!classDefinition: #PPLambdaParserTest category: #'PetitTests-Tests'!
PPCompositeParserTest subclass: #PPLambdaParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPLambdaParserTest class' category: #'PetitTests-Tests'!
PPLambdaParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPObjectTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPObjectTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPObjectTest class' category: #'PetitTests-Tests'!
PPObjectTest class
	instanceVariableNames: ''!

!classDefinition: #PPParserResource category: #'PetitTests-Core'!
TestResource subclass: #PPParserResource
	instanceVariableNames: 'parsers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Core'!
!classDefinition: 'PPParserResource class' category: #'PetitTests-Core'!
PPParserResource class
	instanceVariableNames: ''!

!classDefinition: #PPParserTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPParserTest class' category: #'PetitTests-Tests'!
PPParserTest class
	instanceVariableNames: ''!

!classDefinition: #PPPredicateTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPPredicateTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPPredicateTest class' category: #'PetitTests-Tests'!
PPPredicateTest class
	instanceVariableNames: ''!

!classDefinition: #PPScriptingTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPScriptingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPScriptingTest class' category: #'PetitTests-Tests'!
PPScriptingTest class
	instanceVariableNames: ''!

!classDefinition: #PPTokenTest category: #'PetitTests-Tests'!
PPAbstractParserTest subclass: #PPTokenTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTests-Tests'!
!classDefinition: 'PPTokenTest class' category: #'PetitTests-Tests'!
PPTokenTest class
	instanceVariableNames: ''!


!PPScriptingTest commentStamp: '<historical>' prior: 0!
These are some simple demo-scripts of parser combinators for the compiler construction course.http://www.iam.unibe.ch/~scg/Teaching/CC/index.html!

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:50'!
assert: aParser fail: aCollection	^ self assert: aParser fail: aCollection end: 0! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:49'!
assert: aParser fail: aCollection end: anInteger	| stream result |	self 		assert: aParser isPetitParser		description: 'Parser invalid'.	stream := aCollection asPetitStream.	result := aParser parse: stream.	self 		assert: result isPetitFailure		description: 'Parser did not fail'.	self		assert: stream position = anInteger		description: 'Parser failed at wrong position'.	^ result! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:49'!
assert: aParser parse: aCollection	^ self assert: aParser parse: aCollection to: nil end: aCollection size ! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:49'!
assert: aParser parse: aCollection end: anInteger	^ self assert: aParser parse: aCollection to: nil end: anInteger! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:49'!
assert: aParser parse: aCollection to: anObject	^ self assert: aParser parse: aCollection to: anObject end: aCollection size ! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'DamienPollet 8/11/2011 01:49'!
assert: aParser parse: aParseObject to: aTargetObject end: anInteger	| stream result |	self 		assert: aParser isPetitParser		description: 'Parser invalid'.	stream := aParseObject asPetitStream.	result := aParser parse: stream.	aTargetObject isNil		ifTrue: [ self deny: result isPetitFailure ]		ifFalse: [ self assert: result = aTargetObject ].	self 		assert: stream position = anInteger		description: 'Parser accepted at wrong position'.	^ result! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'lr 10/6/2009 08:21'!
assert: aParser parse: aParserObject toToken: from stop: to	| token |	token := PPToken on: aParserObject start: from stop: to.	^ self assert: aParser parse: aParserObject to: token! !

!PPAbstractParserTest methodsFor: 'utilities' stamp: 'lr 10/6/2009 08:22'!
assert: aParser parse: aParserObject toToken: from stop: to end: end	| token |	token := PPToken on: aParserObject start: from stop: to.	^ self assert: aParser parse: aParserObject to: token end: end! !

!PPAbstractParserTest class methodsFor: 'testing' stamp: 'lr 1/12/2011 21:23'!
isAbstract	^ self name = #PPAbstractParserTest! !

!PPAbstractParserTest class methodsFor: 'accessing' stamp: 'lr 6/12/2010 08:22'!
packageNamesUnderTest	^ #('PetitParser' 'PetitTests')! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 4/6/2010 19:38'!
addition	^ (factors separatedBy: ($+ asParser / $- asParser) token trim) 		foldLeft: [ :a :op :b | a perform: op value asSymbol with: b ]! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 9/15/2008 09:28'!
factors	^ multiplication / power! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 4/6/2010 19:38'!
multiplication	^ (power separatedBy: ($* asParser / $/ asParser) token trim)		foldLeft: [ :a :op :b | a perform: op value asSymbol with: b ]! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 4/6/2010 19:38'!
number	^ ($- asParser optional , #digit asParser plus , ($. asParser , #digit asParser plus) optional) token trim ==> [ :token | token value asNumber ]! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 4/6/2010 19:38'!
parentheses	^ $( asParser flatten trim , terms , $) asParser flatten trim ==> #second! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 4/6/2010 19:38'!
power	^ (primary separatedBy: $^ asParser token trim) foldRight: [ :a :op :b | a raisedTo: b ]! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 9/15/2008 09:28'!
primary	^ number / parentheses! !

!PPArithmeticParser methodsFor: 'accessing' stamp: 'lr 7/3/2008 17:06'!
start	^ terms end! !

!PPArithmeticParser methodsFor: 'grammar' stamp: 'lr 9/15/2008 09:29'!
terms	^ addition / factors! !

!PPArithmeticParserTest methodsFor: 'accessing' stamp: 'FirstnameLastname 11/26/2009 21:53'!
parserClass	^ PPArithmeticParser! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/30/2008 17:21'!
testAdd	self assert: '1 + 2' is: 3.	self assert: '2 + 1' is: 3.	self assert: '1 + 2.3' is: 3.3.	self assert: '2.3 + 1' is: 3.3.	self assert: '1 + -2' is: -1.	self assert: '-2 + 1' is: -1! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/21/2008 10:23'!
testAddMany	self assert: '1' is: 1.	self assert: '1 + 2' is: 3.	self assert: '1 + 2 + 3' is: 6.	self assert: '1 + 2 + 3 + 4' is: 10.	self assert: '1 + 2 + 3 + 4 + 5' is: 15! !

!PPArithmeticParserTest methodsFor: 'testing-expression' stamp: 'lr 4/21/2008 10:03'!
testBrackets	self assert: '(1)' is: 1.	self assert: '(1 + 2)' is: 3.		self assert: '((1))' is: 1.	self assert: '((1 + 2))' is: 3.	self assert: '2 * (3 + 4)' is: 14.	self assert: '(2 + 3) * 4' is: 20.	self assert: '6 / (2 + 4)' is: 1.	self assert: '(2 + 6) / 2' is: 4! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/21/2008 09:32'!
testDiv	self assert: '12 / 3' is: 4.	self assert: '-16 / -4' is: 4! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 7/3/2008 15:46'!
testDivMany	self assert: '100 / 2' is: 50.	self assert: '100 / 2 / 2' is: 25.	self assert: '100 / 2 / 2 / 5' is: 5.	self assert: '100 / 2 / 2 / 5 / 5' is: 1	! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/21/2008 09:31'!
testMul	self assert: '2 * 3' is: 6.	self assert: '2 * -4' is: -8! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/21/2008 10:16'!
testMulMany	self assert: '1 * 2' is: 2.	self assert: '1 * 2 * 3' is: 6.	self assert: '1 * 2 * 3 * 4' is: 24.	self assert: '1 * 2 * 3 * 4 * 5' is: 120! !

!PPArithmeticParserTest methodsFor: 'testing' stamp: 'lr 4/21/2008 09:32'!
testNum	self assert: '0' is: 0.	self assert: '0.0' is: 0.0.	self assert: '1' is: 1.	self assert: '1.2' is: 1.2.	self assert: '34' is: 34.	self assert: '56.78' is: 56.78.	self assert: '-9' is: -9.	self assert: '-9.9' is: -9.9! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 7/3/2008 15:28'!
testPow	self assert: '2 ^ 3' is: 8.	self assert: '-2 ^ 3' is: -8.	self assert: '-2 ^ -3' is: -0.125! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 7/3/2008 15:45'!
testPowMany	self assert: '4 ^ 3' is: 64.	self assert: '4 ^ 3 ^ 2' is: 262144.	self assert: '4 ^ 3 ^ 2 ^ 1' is: 262144.	self assert: '4 ^ 3 ^ 2 ^ 1 ^ 0' is: 262144! !

!PPArithmeticParserTest methodsFor: 'testing-expression' stamp: 'lr 4/21/2008 10:00'!
testPriority	self assert: '2 * 3 + 4' is: 10.	self assert: '2 + 3 * 4' is: 14.	self assert: '6 / 3 + 4' is: 6.	self assert: '2 + 6 / 2' is: 5! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 8/14/2010 13:38'!
testSub	self assert: '1 - 2' is: -1.	self assert: '1.2 - 1.2' is: 0.	self assert: '1 - -2' is: 3.	self assert: '-1 - -2' is: 1! !

!PPArithmeticParserTest methodsFor: 'testing-operations' stamp: 'lr 4/28/2008 11:56'!
testSubMany	self assert: '1' is: 1.	self assert: '1 - 2' is: -1.	self assert: '1 - 2 - 3' is: -4.	self assert: '1 - 2 - 3 - 4' is: -8.	self assert: '1 - 2 - 3 - 4 - 5' is: -13! !

!PPComposedTest methodsFor: 'accessing' stamp: 'lr 2/8/2010 16:44'!
comment	^ ($" asParser , $" asParser negate star , $" asParser) flatten! !

!PPComposedTest methodsFor: 'accessing' stamp: 'lr 2/8/2010 16:44'!
identifier	^ (#letter asParser , #word asParser star) flatten! !

!PPComposedTest methodsFor: 'accessing' stamp: 'lr 2/8/2010 16:44'!
number	^ ($- asParser optional , #digit asParser plus , ($. asParser , #digit asParser plus) optional) flatten! !

!PPComposedTest methodsFor: 'testing-examples' stamp: 'lr 2/8/2010 16:44'!
testComment	self assert: self comment parse: '""' to: '""'.	self assert: self comment parse: '"a"' to: '"a"'.	self assert: self comment parse: '"ab"' to: '"ab"'.	self assert: self comment parse: '"abc"' to: '"abc"'.	self assert: self comment parse: '""a' to: '""' end: 2.	self assert: self comment parse: '"a"a' to: '"a"' end: 3.	self assert: self comment parse: '"ab"a' to: '"ab"' end: 4.	self assert: self comment parse: '"abc"a' to: '"abc"' end: 5.	self assert: self comment fail: '"'.	self assert: self comment fail: '"a'.	self assert: self comment fail: '"aa'.	self assert: self comment fail: 'a"'.	self assert: self comment fail: 'aa"'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 7/6/2009 08:34'!
testDoubledString	| parser |	parser := ($' asParser , (($' asParser , $' asParser) / $' asParser negate) star flatten , $' asParser) 		==> [ :nodes | nodes second copyReplaceAll: '''''' with: '''' ].	self assert: parser parse: '''''' to: ''.	self assert: parser parse: '''a''' to: 'a'.	self assert: parser parse: '''ab''' to: 'ab'.	self assert: parser parse: '''a''''b''' to: 'a''b'.	self assert: parser parse: '''a''''''''b''' to: 'a''''b'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 12/5/2010 14:25'!
testEvenNumber	"Create a grammar that parses an even number of a's and b's."		| a as b bs s |	a := $a asParser ==> [ :char | as := as + 1 ].	b := $b asParser ==> [ :char | bs := bs + 1 ].	s := (a / b) star >=> [ :stream :cc |		as := bs := 0.		cc value.		(as even and: [ bs even ])			ifFalse: [ PPFailure message: 'Even number of a and b expected' at: 0 ] ].	self assert: s fail: 'a' end: 1.	self assert: s fail: 'b' end: 1.	self assert: s fail: 'ab' end: 2.	self assert: s fail: 'ba' end: 2.	self assert: s fail: 'aaa' end: 3.	self assert: s fail: 'bbb' end: 3.	self assert: s fail: 'aab' end: 3.	self assert: s fail: 'abb' end: 3.		self assert: s parse: ''.	self assert: s parse: 'aa'.	self assert: s parse: 'bb'.	self assert: s parse: 'aaaa'.	self assert: s parse: 'aabb'.	self assert: s parse: 'abab'.	self assert: s parse: 'baba'.	self assert: s parse: 'bbaa'.	self assert: s parse: 'bbbb'! !

!PPComposedTest methodsFor: 'testing-examples' stamp: 'lr 2/8/2010 16:44'!
testIdentifier	self assert: self identifier parse: 'a' to: 'a'.	self assert: self identifier parse: 'a1' to: 'a1'.	self assert: self identifier parse: 'a12' to: 'a12'.	self assert: self identifier parse: 'ab' to: 'ab'.	self assert: self identifier parse: 'a1b' to: 'a1b'.	self assert: self identifier parse: 'a_' to: 'a' end: 1.	self assert: self identifier parse: 'a1-' to: 'a1' end: 2.	self assert: self identifier parse: 'a12+' to: 'a12' end: 3.	self assert: self identifier parse: 'ab^' to: 'ab' end: 2.	self assert: self identifier parse: 'a1b*' to: 'a1b' end: 3.	self assert: self identifier fail: ''.	self assert: self identifier fail: '1'.	self assert: self identifier fail: '1a'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:11'!
testIfThenElse	"S ::= if C then S else S | if C then S | X"	| start if then else cond expr parser |	start := PPDelegateParser new.	if := 'if' asParser token trim.	then := 'then' asParser token trim.	else := 'else' asParser token trim.	cond := 'C' asParser token trim.	expr := 'X' asParser token trim.	start setParser: (if , cond , then , start , else , start) / (if , cond , then , start) / expr.	parser := start end.	self assert: parser parse: 'X'.	self assert: parser parse: 'if C then X'.	self assert: parser parse: 'if C then X else X'.	self assert: parser parse: 'if C then if C then X'.	self assert: parser parse: 'if C then if C then X else if C then X'.	self assert: parser parse: 'if C then if C then X else X else if C then X'.	self assert: parser parse: 'if C then if C then X else X else if C then X else X'.	self assert: parser fail: 'if C'.	self assert: parser fail: 'if C else X'.	self assert: parser fail: 'if C then if C'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:09'!
testLeftRecursion	"S ::= S 'x' S / '1'"		| parser |	parser := PPDelegateParser new.	parser setParser: ((parser , $x asParser , parser) / $1 asParser) memoized flatten.	self assert: parser parse: '1' to: '1'.	self assert: parser parse: '1x1' to: '1x1'.	self assert: parser parse: '1x1x1' to: '1x1x1'.	self assert: parser parse: '1x1x1x1' to: '1x1x1x1'.	self assert: parser parse: '1x1x1x1x1' to: '1x1x1x1x1'.	self assert: parser parse: '1x1x1x1x1x1' to: '1x1x1x1x1x1'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 4/6/2010 19:34'!
testListOfIntegers	"S ::= S , number | number"		| number list parser |	number := #digit asParser plus token trim		==> [ :node | node value asInteger ].	list := (number separatedBy: $, asParser token trim)		==> [ :node | node select: [ :each | each isInteger ] ].	parser := list end.	self assert: parser parse: '1' to: (1 to: 1) asArray.	self assert: parser parse: '1,2' to: (1 to: 2) asArray.	self assert: parser parse: '1,2,3' to: (1 to: 3) asArray.	self assert: parser parse: '1,2,3,4' to: (1 to: 4) asArray.	self assert: parser parse: '1,2,3,4,5' to: (1 to: 5) asArray.	self assert: parser parse: '1' to: (1 to: 1) asArray.	self assert: parser parse: '1, 2' to: (1 to: 2) asArray.	self assert: parser parse: '1, 2, 3' to: (1 to: 3) asArray.	self assert: parser parse: '1, 2, 3, 4' to: (1 to: 4) asArray.	self assert: parser parse: '1, 2, 3, 4, 5' to: (1 to: 5) asArray.	self assert: parser parse: '1' to: (1 to: 1) asArray.	self assert: parser parse: '1 ,2' to: (1 to: 2) asArray.	self assert: parser parse: '1 ,2 ,3' to: (1 to: 3) asArray.	self assert: parser parse: '1 ,2 ,3 ,4' to: (1 to: 4) asArray.	self assert: parser parse: '1 ,2 ,3 ,4 ,5' to: (1 to: 5) asArray.		self assert: parser fail: ''.	self assert: parser fail: ','.	self assert: parser fail: '1,'.	self assert: parser fail: '1,,2'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:09'!
testNestedComments	"C ::= B I* E"	"I ::= !!E (C | T)"	"B ::= /*"	"E ::= */"	"T ::= ."		| begin end any inside parser |	begin := '/*' asParser.	end := '*/' asParser.	any := #any asParser.		parser := PPDelegateParser new.	inside := end not , (parser / any).	parser setParser: begin , inside star , end.		self assert: parser parse: '/*ab*/cd' end: 6.	self assert: parser parse: '/*a/*b*/c*/'.	self assert: parser fail: '/*a/*b*/c'! !

!PPComposedTest methodsFor: 'testing-examples' stamp: 'lr 2/8/2010 16:44'!
testNumber	self assert: self number parse: '1' to: '1'.	self assert: self number parse: '12' to: '12'.	self assert: self number parse: '12.3' to: '12.3'.	self assert: self number parse: '12.34' to: '12.34'.	self assert: self number parse: '1..' to: '1' end: 1.	self assert: self number parse: '12-' to: '12' end: 2.	self assert: self number parse: '12.3.' to: '12.3' end: 4.	self assert: self number parse: '12.34.' to: '12.34' end: 5.		self assert: self number parse: '-1' to: '-1'.	self assert: self number parse: '-12' to: '-12'.	self assert: self number parse: '-12.3' to: '-12.3'.	self assert: self number parse: '-12.34' to: '-12.34'.		self assert: self number fail: ''.	self assert: self number fail: '-'.	self assert: self number fail: '.'.	self assert: self number fail: '.1'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:10'!
testPalindrome	"S0 ::= a S1 a | b S1 b | ...	 S1 ::= S0 | epsilon"		| s0 s1 parser |	s0 := PPDelegateParser new.	s1 := PPDelegateParser new.	s0 setParser: ($a asParser , s1 , $a asParser)		/ ($b asParser , s1 , $b asParser)		/ ($c asParser , s1 , $c asParser).		s1 setParser: s0 / nil asParser.	parser := s0 flatten end.	self assert: parser parse: 'aa' to: 'aa'.	self assert: parser parse: 'bb' to: 'bb'.	self assert: parser parse: 'cc' to: 'cc'.		self assert: parser parse: 'abba' to: 'abba'.	self assert: parser parse: 'baab' to: 'baab'.	self assert: parser parse: 'abccba' to: 'abccba'.	self assert: parser parse: 'abaaba' to: 'abaaba'.	self assert: parser parse: 'cbaabc' to: 'cbaabc'.	self assert: parser fail: 'a'.	self assert: parser fail: 'ab'.	self assert: parser fail: 'aab'.	self assert: parser fail: 'abccbb'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:10'!
testParseAaaBbb	"S0 ::= a S1 b	 S1 ::= S0 | epsilon"		| s0 s1 parser |	s0 := PPDelegateParser new.	s1 := PPDelegateParser new.	s0 setParser: $a asParser , s1 , $b asParser.	s1 setParser: s0 / nil asParser.	parser := s0 flatten.	self assert: parser parse: 'ab' to: 'ab'.	self assert: parser parse: 'aabb' to: 'aabb'.	self assert: parser parse: 'aaabbb' to: 'aaabbb'.	self assert: parser parse: 'aaaabbbb' to: 'aaaabbbb'.	self assert: parser parse: 'abb' to: 'ab' end: 2.	self assert: parser parse: 'aabbb' to: 'aabb' end: 4.	self assert: parser parse: 'aaabbbb' to: 'aaabbb' end: 6.	self assert: parser parse: 'aaaabbbbb' to: 'aaaabbbb' end: 8.	self assert: parser fail: 'a'.	self assert: parser fail: 'b'.	self assert: parser fail: 'aab'.	self assert: parser fail: 'aaabb'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:10'!
testParseAaaaaa	"S ::= a a S | epsilon"		| s0 s1 parser |	s0 := PPDelegateParser new.	s1 := $a asParser , $a asParser , s0.	s0 setParser: s1 / nil asParser.	parser := s0 flatten.	self assert: parser parse: '' to: ''.	self assert: parser parse: 'aa' to: 'aa'.	self assert: parser parse: 'aaaa' to: 'aaaa'.	self assert: parser parse: 'aaaaaa' to: 'aaaaaa'.	self assert: parser parse: 'a' to: '' end: 0.	self assert: parser parse: 'aaa' to: 'aa' end: 2.	self assert: parser parse: 'aaaaa' to: 'aaaa' end: 4.	self assert: parser parse: 'aaaaaaa' to: 'aaaaaa' end: 6! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 9/18/2008 09:26'!
testParseAbAbAb	"S ::= (A B)+"		| parser |	parser := ($a asParser , $b asParser) plus flatten.	self assert: parser parse: 'ab' to: 'ab'.	self assert: parser parse: 'abab' to: 'abab'.	self assert: parser parse: 'ababab' to: 'ababab'.	self assert: parser parse: 'abababab' to: 'abababab'.	self assert: parser parse: 'abb' to: 'ab' end: 2.	self assert: parser parse: 'ababa' to: 'abab' end: 4.	self assert: parser parse: 'abababb' to: 'ababab' end: 6.	self assert: parser parse: 'ababababa' to: 'abababab' end: 8.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'bab'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 9/18/2008 09:26'!
testParseAbabbb	"S ::= (A | B)+"	| parser |	parser := ($a asParser / $b asParser) plus flatten.	self assert: parser parse: 'a' to: 'a'.	self assert: parser parse: 'b' to: 'b'.	self assert: parser parse: 'ab' to: 'ab'.	self assert: parser parse: 'ba' to: 'ba'.	self assert: parser parse: 'aaa' to: 'aaa'.	self assert: parser parse: 'aab' to: 'aab'.	self assert: parser parse: 'aba' to: 'aba'.	self assert: parser parse: 'baa' to: 'baa'.	self assert: parser parse: 'abb' to: 'abb'.	self assert: parser parse: 'bab' to: 'bab'.	self assert: parser parse: 'bba' to: 'bba'.	self assert: parser parse: 'bbb' to: 'bbb'.	self assert: parser parse: 'ac' to: 'a' end: 1.	self assert: parser parse: 'bc' to: 'b' end: 1.	self assert: parser parse: 'abc' to: 'ab' end: 2.	self assert: parser parse: 'bac' to: 'ba' end: 2.		self assert: parser fail: ''.	self assert: parser fail: 'c'! !

!PPComposedTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:11'!
testParseAnBnCn	"PEGs for a non context- free language: 				a^n , b^n , c^n			S <- &P1 P2 	P1 <- AB 'c' 	AB <- 'a' AB 'b' / epsilon	P2 <- 'a'* BC end	BC <- 'b' BC 'c' / epsilon"		| s p1 ab p2 bc |	s := PPDelegateParser new.	p1 := PPDelegateParser new.	ab := PPDelegateParser new.	p2 := PPDelegateParser new.	bc := PPDelegateParser new.		s setParser: (p1 and , p2 end) flatten.	p1 setParser: ab , $c asParser.	ab setParser: ($a asParser , ab , $b asParser) optional.	p2 setParser: $a asParser star , bc.	bc setParser: ($b asParser , bc , $c asParser) optional.		self assert: s parse: 'abc' to: 'abc'.	self assert: s parse: 'aabbcc' to: 'aabbcc'.	self assert: s parse: 'aaabbbccc' to: 'aaabbbccc'.	self assert: s fail: 'bc'.	self assert: s fail: 'ac'.	self assert: s fail: 'ab'.	self assert: s fail: 'abbcc'.	self assert: s fail: 'aabcc'.	self assert: s fail: 'aabbc'! !

!PPComposedTest methodsFor: 'testing-examples' stamp: 'lr 6/3/2010 13:51'!
testReturn	| number spaces return |	number := #digit asParser plus token.	spaces := #space asParser star.	return := (spaces , $^ asParser token , spaces , number)		==> [ :nodes | Array with: #return with: (nodes at: 4) value ].	self assert: return parse: '^1' to: #(return '1').	self assert: return parse: '^12' to: #(return '12').	self assert: return parse: '^ 123' to: #(return '123').	self assert: return parse: '^  1234' to: #(return '1234').		self assert: return fail: '1'.	self assert: return fail: '^'! !

!PPCompositeParserTest methodsFor: 'utilities' stamp: 'lr 11/29/2010 11:20'!
assert: aCollection is: anObject	self parse: aCollection.	self		assert: result = anObject		description: 'Got: ' , result printString , '; Expected: ' , anObject printString		resumable: true! !

!PPCompositeParserTest methodsFor: 'parsing' stamp: 'lr 11/18/2011 19:45'!
fail: aString rule: aSymbol 	| production |	production := self parserInstanceFor: aSymbol.	result := production end parse: aString.	self		assert: result isPetitFailure		description: 'Able to parse ' , aString printString.	^ result! !

!PPCompositeParserTest methodsFor: 'parsing' stamp: 'lr 11/29/2010 11:26'!
parse: aString 	^ self parse: aString rule: #start! !

!PPCompositeParserTest methodsFor: 'parsing' stamp: 'lr 11/18/2011 19:45'!
parse: aString rule: aSymbol	| production |	production := self parserInstanceFor: aSymbol.	result := production end parse: aString.	self		deny: result isPetitFailure		description: 'Unable to parse ' , aString printString.	^ result! !

!PPCompositeParserTest methodsFor: 'accessing' stamp: 'FirstnameLastname 11/26/2009 21:52'!
parserClass	self subclassResponsibility! !

!PPCompositeParserTest methodsFor: 'accessing' stamp: 'lr 3/29/2010 15:21'!
parserInstance	^ PPParserResource current parserAt: self parserClass! !

!PPCompositeParserTest methodsFor: 'accessing' stamp: 'lr 11/18/2011 19:44'!
parserInstanceFor: aSymbol	^ aSymbol = #start		ifTrue: [ self parserInstance ]		ifFalse: [			self parserInstance				productionAt: aSymbol 				ifAbsent: [ self error: 'Production ' , self parserClass name , '>>' , aSymbol printString , ' not found.' ] ]! !

!PPCompositeParserTest methodsFor: 'running' stamp: 'FirstnameLastname 11/26/2009 21:48'!
setUp	super setUp.	parser := self parserInstance! !

!PPCompositeParserTest methodsFor: 'running' stamp: 'lr 11/29/2010 11:19'!
tearDown	super tearDown.	parser := result := nil! !

!PPCompositeParserTest class methodsFor: 'testing' stamp: 'lr 10/4/2009 17:09'!
isAbstract	^ self name = #PPCompositeParserTest! !

!PPCompositeParserTest class methodsFor: 'accessing' stamp: 'lr 3/29/2010 15:21'!
resources	^ Array with: PPParserResource! !

!PPExpressionParserTest methodsFor: 'accessing' stamp: 'lr 4/6/2010 19:39'!
parserInstance	| expression parens number |	expression := PPExpressionParser new.	parens := $( asParser token trim , expression , $) asParser token trim		==> [ :nodes | nodes second ].	number := (#digit asParser plus , ($. asParser , #digit asParser plus) optional) token trim		==> [ :token | token value asNumber ].	expression term: parens / number.	expression		group: [ :g |			g prefix: $- asParser token trim do: [ :op :a | a negated ] ];		group: [ :g |			g postfix: '++' asParser token trim do: [ :a :op | a + 1 ].			g postfix: '--' asParser token trim do: [ :a :op | a - 1 ] ];		group: [ :g |			g right: $^ asParser token trim do: [ :a :op :b | a raisedTo: b ] ];		group: [ :g |			g left: $* asParser token trim do: [ :a :op :b | a * b ].			g left: $/ asParser token trim do: [ :a :op :b | a / b ] ];		group: [ :g |			g left: $+ asParser token trim do: [ :a :op :b | a + b ].			g left: $- asParser token trim do: [ :a :op :b | a - b ] ].	^ expression end! !

!PPExpressionParserTest methodsFor: 'testing' stamp: 'FirstnameLastname 11/26/2009 22:13'!
testPostfixAdd	self assert: '0++' is: 1.	self assert: '0++++' is: 2.	self assert: '0++++++' is: 3.	self assert: '0+++1' is: 2.	self assert: '0+++++1' is: 3.	self assert: '0+++++++1' is: 4! !

!PPExpressionParserTest methodsFor: 'testing' stamp: 'FirstnameLastname 11/26/2009 22:11'!
testPostfixSub	self assert: '1--' is: 0.	self assert: '2----' is: 0.	self assert: '3------' is: 0.	self assert: '2---1' is: 0.	self assert: '3-----1' is: 0.	self assert: '4-------1' is: 0.! !

!PPExpressionParserTest methodsFor: 'testing' stamp: 'FirstnameLastname 11/26/2009 22:13'!
testPrefixNegate	self assert: '1' is: 1.	self assert: '-1' is: -1.	self assert: '--1' is: 1.	self assert: '---1' is: -1! !

!PPExpressionParserTest class methodsFor: 'testing' stamp: 'lr 4/6/2010 19:40'!
shouldInheritSelectors	^ true! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 5/2/2010 18:18'!
testCharacter	| parser |	parser := $a asParser.	self assert: parser parse: 'a' to: $a.	self assert: parser fail: 'b'! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 11/29/2011 20:38'!
testChoice	| parser |	parser := #(1 2) asChoiceParser.	self assert: parser parse: #(1) to: 1.	self assert: parser parse: #(2) to: 2.	self assert: parser parse: #(1 2) to: 1 end: 1.	self assert: parser parse: #(2 1) to: 2 end: 1.	self assert: parser fail: #().	self assert: parser fail: #(3)! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 5/5/2010 14:03'!
testClosure	| parser |	parser := [ :stream | stream upTo: $s ] asParser.	self assert: parser parse: '' to: ''.	self assert: parser parse: 'a' to: 'a'.	self assert: parser parse: 'aa' to: 'aa'.	self assert: parser parse: 's' to: ''.	self assert: parser parse: 'as' to: 'a'.	self assert: parser parse: 'aas' to: 'aa'.	self assert: parser parse: 'sa' to: '' end: 1.	self assert: parser parse: 'saa' to: '' end: 1.		parser := [ :stream | stream upTo: $s. PPFailure message: 'stream' at: stream position ] asParser.	self assert: parser fail: ''.	self assert: parser fail: 's'.	self assert: parser fail: 'as'	! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 11/20/2009 15:29'!
testEpsilon	| parser |	parser := nil asParser.	self assert: parser asParser = parser! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 11/20/2009 15:30'!
testParser	| parser |	parser := $a asParser.	self assert: parser asParser = parser! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 9/17/2008 22:48'!
testRange	| parser |	parser := $a - $c.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'b' to: $b.	self assert: parser parse: 'c' to: $c.	self assert: parser fail: 'd'! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 11/29/2011 20:40'!
testSequence	| parser |	parser := #(1 2) asSequenceParser.	self assert: parser parse: #(1 2) to: #(1 2).	self assert: parser parse: #(1 2 3) to: #(1 2) end: 2.	self assert: parser fail: #().	self assert: parser fail: #(1).	self assert: parser fail: #(1 1).	self assert: parser fail: #(1 1 2)! !

!PPExtensionTest methodsFor: 'testing-stream' stamp: 'lr 9/30/2010 11:12'!
testStream	| stream |	stream := 'abc' readStream asPetitStream.	self assert: (stream class = PPStream).	self assert: (stream printString = '·abc').	self assert: (stream peek) = $a.	self assert: (stream uncheckedPeek = $a).	self assert: (stream next) = $a.	self assert: (stream printString = 'a·bc').	self assert: (stream asPetitStream = stream)! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 5/2/2010 18:18'!
testString	| parser |	parser := 'ab' asParser.	self assert: parser parse: 'ab' to: 'ab'.	self assert: parser parse: 'aba' to: 'ab' end: 2.	self assert: parser parse: 'abb' to: 'ab' end: 2.	self assert: parser fail: 'a'.	self assert: parser fail: 'ac'! !

!PPExtensionTest methodsFor: 'testing-parser' stamp: 'lr 9/17/2008 22:03'!
testSymbol	| parser |	parser := #any asParser.	self assert: parser parse: 'a'.	self assert: parser fail: ''! !

!PPExtensionTest methodsFor: 'testing-stream' stamp: 'lr 2/7/2010 20:53'!
testText	| stream |	stream := 'abc' asText asPetitStream.	self assert: stream class = PPStream! !

!PPLambdaParser methodsFor: 'productions' stamp: 'lr 4/6/2010 19:38'!
abstraction	^ $\ asParser token trim , variable , $. asParser token trim , expression ==> [ :node | Array with: node second with: node fourth ]! !

!PPLambdaParser methodsFor: 'productions' stamp: 'lr 4/6/2010 19:38'!
application	^ $( asParser token trim , expression , expression , $) asParser token trim ==> [ :node | Array with: node second with: node third ]! !

!PPLambdaParser methodsFor: 'productions' stamp: 'lr 9/15/2008 09:29'!
expression	^ variable / abstraction / application! !

!PPLambdaParser methodsFor: 'accessing' stamp: 'lr 5/19/2008 11:35'!
start	^ expression end! !

!PPLambdaParser methodsFor: 'productions' stamp: 'lr 4/6/2010 19:37'!
variable	^ (#letter asParser , #word asParser star) token trim ==> [ :token | token value ]! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
and	^ self parse: '\p.\q.((p q) p)'! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
false	^ self parse: '\x.\y.y'! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
ifthenelse	^ self parse: '\p.p'! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
not	^ self parse: '\p.\a.\b.((p b) a)'! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
or	^ self parse: '\p.\q.((p p) q)'! !

!PPLambdaParser class methodsFor: 'curch-booleans' stamp: 'lr 4/3/2009 08:28'!
true	^ self parse: '\x.\y.x'! !

!PPLambdaParserTest methodsFor: 'accessing' stamp: 'FirstnameLastname 11/26/2009 21:53'!
parserClass	^ PPLambdaParser! !

!PPLambdaParserTest methodsFor: 'testing' stamp: 'lr 4/30/2008 09:38'!
testAbstraction	self assert: '\x.y' is: #('x' 'y').	self assert: '\x.\y.z' is: #('x' ('y' 'z'))! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testAnd	self assert: self parserClass and = #('p' ('q' (('p' 'q') 'p')))! !

!PPLambdaParserTest methodsFor: 'testing' stamp: 'lr 4/30/2008 09:38'!
testApplication	self assert: '(x x)' is: #('x' 'x').	self assert: '(x y)' is: #('x' 'y').	self assert: '((x y) z)' is: #(('x' 'y') 'z').	self assert: '(x (y z))' is: #('x' ('y' 'z'))! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testFalse	self assert: self parserClass false = #('x' ('y' 'y'))! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testIfThenElse	self assert: self parserClass ifthenelse = #('p' 'p')! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testNot	self assert: self parserClass not = #('p' ('a' ('b' (('p' 'b') 'a'))))! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testOr	self assert: self parserClass or = #('p' ('q' (('p' 'p') 'q')))! !

!PPLambdaParserTest methodsFor: 'testing-utilities' stamp: 'lr 11/29/2010 11:29'!
testParseOnError	| beenHere |	result := self parserClass		parse: '\x.y'		onError: [ self fail ].	self assert: result = #('x' 'y').		beenHere := false.	result := self parserClass		parse: '\x.'		onError: [ beenHere := true ].	self assert: beenHere.		beenHere := false.	result := self parserClass		parse: '\x.'		onError: [ :fail | beenHere := true. fail ].	self assert: beenHere.	self assert: (result message includesSubString: '$(').	self assert: (result message includesSubString: 'expected').	self assert: (result position = 0).	beenHere := false.	result := self parserClass		parse: '\x.'		onError: [ :msg :pos | 			self assert: (msg includesSubString: '$(').			self assert: (msg includesSubString: 'expected').			self assert: (pos = 0).			beenHere := true ].	self assert: result.	self assert: beenHere! !

!PPLambdaParserTest methodsFor: 'testing-utilities' stamp: 'lr 11/29/2010 11:29'!
testParseStartingAtOnError	| beenHere |	result := self parserClass		parse: 'x'		startingAt: #variable		onError: [ self fail ].	self assert: result = 'x'.		beenHere := false.	result := self parserClass		parse: '\'		startingAt: #variable		onError: [ beenHere := true ].	self assert: beenHere.		beenHere := false.	result := self parserClass		parse: '\'		startingAt: #variable		onError: [ :fail | beenHere := true. fail ].	self assert: beenHere.	self assert: result message = 'letter expected'.	self assert: result position = 0.	beenHere := false.	result := self parserClass		parse: '\'		startingAt: #variable		onError: [ :msg :pos | 			self assert: msg = 'letter expected'.			self assert: pos = 0.			beenHere := true ].	self assert: beenHere! !

!PPLambdaParserTest methodsFor: 'testing-utilities' stamp: 'FirstnameLastname 11/26/2009 21:56'!
testProductionAt	self assert: (parser productionAt: #foo) isNil.	self assert: (parser productionAt: #foo ifAbsent: [ true ]).		self assert: (parser productionAt: #start) notNil.	self assert: (parser productionAt: #start ifAbsent: [ true ]) notNil.		self assert: (parser productionAt: #variable) notNil.	self assert: (parser productionAt: #variable ifAbsent: [ true ]) notNil! !

!PPLambdaParserTest methodsFor: 'testing-curch' stamp: 'FirstnameLastname 11/26/2009 21:53'!
testTrue	self assert: self parserClass true = #('x' ('y' 'x'))! !

!PPLambdaParserTest methodsFor: 'testing' stamp: 'lr 4/30/2008 09:33'!
testVariable	self assert: 'x' is: 'x'.	self assert: 'xy' is: 'xy'.	self assert: 'x12' is: 'x12'! !

!PPObjectTest methodsFor: 'parsers' stamp: 'lr 12/9/2010 10:25'!
integer	^ PPPredicateObjectParser		on: [ :each | each isInteger ]		message: 'integer expected'! !

!PPObjectTest methodsFor: 'parsers' stamp: 'lr 10/30/2010 12:45'!
string	^ PPPredicateObjectParser		on: [ :each | each isString ]		message: 'string expected'! !

!PPObjectTest methodsFor: 'testing-operators' stamp: 'lr 12/9/2010 10:25'!
testChoice	| parser |	parser := self integer / self string.	self assert: parser parse: #(123) to: 123.	self assert: parser parse: #('abc') to: 'abc'! !

!PPObjectTest methodsFor: 'testing-fancy' stamp: 'lr 12/9/2010 10:25'!
testFibonacci	"This parser accepts fibonacci sequences with arbitrary start pairs."		| parser |	parser := ((self integer , self integer) end ==> [ :pair | pair first + pair last ])		/ (self integer , (self integer , self integer) and >=> [ :stream :continuation |			| result |			result := continuation value.			(result isPetitFailure or: [ result first + result last first ~= result last last ])				ifFalse: [ parser parseOn: stream ]				ifTrue: [ PPFailure message: 'invalid fibonacci sequence' at: stream position ] ]).	self assert: parser parse: #(1 1) to: 2.	self assert: parser parse: #(1 1 2) to: 3.	self assert: parser parse: #(1 1 2 3) to: 5.	self assert: parser parse: #(1 1 2 3 5) to: 8.	self assert: parser parse: #(1 1 2 3 5 8) to: 13.	self assert: parser parse: #(1 1 2 3 5 8 13) to: 21.	self assert: parser fail: #().	self assert: parser fail: #(1).	self assert: parser fail: #(1 2 3 4) end: 2	! !

!PPObjectTest methodsFor: 'testing' stamp: 'lr 12/9/2010 10:25'!
testInteger	self assert: self integer parse: #(123) to: 123.	self assert: self integer fail: #('abc')! !

!PPObjectTest methodsFor: 'testing-operators' stamp: 'lr 12/9/2010 10:25'!
testSequence	| parser |	parser := self integer , self string.	self assert: parser parse: #(123 'abc') to: #(123 'abc').	self assert: parser fail: #(123 456).	self assert: parser fail: #('abc' 'def').	self assert: parser fail: #('abc' 123)	! !

!PPObjectTest methodsFor: 'testing' stamp: 'lr 10/30/2010 12:47'!
testString	self assert: self string parse: #('abc') to: 'abc'.	self assert: self string fail: #(123)! !

!PPParserResource methodsFor: 'accessing' stamp: 'lr 9/15/2010 12:12'!
parserAt: aParserClass	"Answer a cached instance of aParserClass."		^ parsers at: aParserClass name ifAbsentPut: [ aParserClass new ]! !

!PPParserResource methodsFor: 'running' stamp: 'lr 3/29/2010 15:20'!
setUp	super setUp.	parsers := Dictionary new! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 5/2/2010 12:22'!
testAction	| block parser |	block := [ :char | char asUppercase ].	parser := #any asParser ==> block.	self assert: parser block = block.	self assert: parser parse: 'a' to: $A.	self assert: parser parse: 'b' to: $B! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 6/24/2011 06:15'!
testAnd	| parser |	parser := 'foo' asParser flatten , 'bar' asParser flatten and.		self assert: parser parse: 'foobar' to: #('foo' 'bar') end: 3.	self assert: parser fail: 'foobaz'.		parser := 'foo' asParser and.	self assert: parser and = parser! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testAnswer	| parser |	parser := $a asParser answer: $b.		self assert: parser parse: 'a' to: $b.		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/2/2009 19:56'!
testBlock	| parser |	parser := [ :s | s next ] asParser.		self assert: parser parse: 'ab' to: $a end: 1.	self assert: parser parse: 'b' to: $b.	self assert: parser parse: '' to: nil! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 2/7/2010 22:15'!
testChildren	| p1 p2 p3 |	p1 := #lowercase asParser.	p2 := p1 ==> #asUppercase.	p3 := PPUnresolvedParser new.	p3 def: p2 / p3.	self assert: p1 children isEmpty.	self assert: p2 children size = 1.	self assert: p3 children size = 2! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 9/15/2008 09:24'!
testChoice	| parser |	parser := $a asParser / $b asParser.		self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'b' to: $b.	self assert: parser parse: 'ab' to: $a end: 1.	self assert: parser parse: 'ba' to: $b end: 1.	self assert: parser fail: ''.	self assert: parser fail: 'c'.	self assert: parser fail: 'ca'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/7/2008 08:58'!
testDelimitedBy	| parser |	parser := $a asParser delimitedBy: $b asParser.		self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aba' to: #($a $b $a).	self assert: parser parse: 'ababa' to: #($a $b $a $b $a).	self assert: parser parse: 'ab' to: #($a $b).	self assert: parser parse: 'abab' to: #($a $b $a $b).	self assert: parser parse: 'ababab' to: #($a $b $a $b $a $b).		self assert: parser parse: 'ac' to: #($a) end: 1.	self assert: parser parse: 'abc' to: #($a $b) end: 2.	self assert: parser parse: 'abac' to: #($a $b $a) end: 3.	self assert: parser parse: 'ababc' to: #($a $b $a $b) end: 4.		self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'c'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 2/25/2012 16:56'!
testDelimitedByWithoutSeparators	| parser |	parser := ($a asParser delimitedBy: $b asParser)		withoutSeparators.		self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aba' to: #($a $a).	self assert: parser parse: 'ababa' to: #($a $a $a).	self assert: parser parse: 'ab' to: #($a).	self assert: parser parse: 'abab' to: #($a $a).	self assert: parser parse: 'ababab' to: #($a $a $a).		self assert: parser parse: 'ac' to: #($a) end: 1.	self assert: parser parse: 'abc' to: #($a) end: 2.	self assert: parser parse: 'abac' to: #($a $a) end: 3.	self assert: parser parse: 'ababc' to: #($a $a) end: 4.		self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'c'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 1/29/2010 11:39'!
testEndOfInput	| parser |	parser := PPEndOfInputParser on: $a asParser.	self assert: parser end = parser.		self assert: parser parse: 'a' to: $a.	self assert: parser fail: ''.	self assert: parser fail: 'aa'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 9/17/2008 22:47'!
testEndOfInputAfterMatch	| parser |	parser := 'stuff' asParser end.	self assert: parser parse: 'stuff' to: 'stuff'.	self assert: parser fail: 'stufff'.	self assert: parser fail: 'fluff'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 11/20/2009 15:31'!
testEpsilon	| parser |	parser := nil asParser.		self assert: parser parse: '' to: nil.		self assert: parser parse: 'a' to: nil end: 0.	self assert: parser parse: 'ab' to: nil end: 0! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/5/2010 14:10'!
testFailing	| parser result |	parser := PPFailingParser message: 'Plonk'.	self assert: parser message = 'Plonk'.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'aa'.		result := parser parse: 'a'.	self assert: result message = 'Plonk'.	self assert: result printString = 'Plonk at 0'! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 5/5/2010 13:58'!
testFailure	| failure |	failure := PPFailure message: 'Error' at: 3.		self assert: failure message = 'Error'.	self assert: failure position = 3.	self assert: failure isPetitFailure.	self deny: 4 isPetitFailure.	self deny: 'foo' isPetitFailure! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 5/2/2010 12:18'!
testFlatten	| parser |	parser := $a asParser flatten.		self assert: parser parse: 'a' to: 'a'.	self assert: parser parse: #($a) to: #($a).		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testFoldLeft2	| parser |	parser := #any asParser star 		foldLeft: [ :a :b | Array with: a with: b ].	self assert: parser parse: #(a) to: #a.	self assert: parser parse: #(a b) to: #(a b).	self assert: parser parse: #(a b c) to: #((a b) c).	self assert: parser parse: #(a b c d) to: #(((a b) c) d).	self assert: parser parse: #(a b c d e) to: #((((a b) c) d) e)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testFoldLeft3	| parser |	parser := #any asParser star 		foldLeft: [ :a :b :c | Array with: a with: b with: c ].	self assert: parser parse: #(a) to: #a.	self assert: parser parse: #(a b c) to: #(a b c).	self assert: parser parse: #(a b c d e) to: #((a b c) d e)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testFoldRight2	| parser |	parser := #any asParser star 		foldRight: [ :a :b | Array with: a with: b ].	self assert: parser parse: #(a) to: #a.	self assert: parser parse: #(a b) to: #(a b).	self assert: parser parse: #(a b c) to: #(a (b c)).	self assert: parser parse: #(a b c d) to: #(a (b (c d))).	self assert: parser parse: #(a b c d e) to: #(a (b (c (d e))))! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testFoldRight3	| parser |	parser := #any asParser star 		foldRight: [ :a :b :c | Array with: a with: b with: c ].	self assert: parser parse: #(a) to: #a.	self assert: parser parse: #(a b c) to: #(a b c).	self assert: parser parse: #(a b c d e) to: #(a b (c d e))! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:38'!
testHasProperty	| parser |	parser := PPParser new.	self deny: (parser hasProperty: #foo).	parser propertyAt: #foo put: 123.	self assert: (parser hasProperty: #foo)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 5/31/2010 19:27'!
testListConstructor	| p1 p2 p3 |	p1 := PPChoiceParser with: $a asParser.	p2 := PPChoiceParser with: $a asParser with: $b asParser.	p3 := PPChoiceParser withAll: (Array with: $a asParser with: $b asParser with: $c asParser).		self assert: p1 children size = 1.	self assert: p2 children size = 2.	self assert: p3 children size = 3! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/2/2010 18:20'!
testLiteralObject	| parser |	parser := PPLiteralObjectParser 		on: $a		message: 'letter "a" expected'.	self assert: parser literal = $a.	self assert: parser message = 'letter "a" expected'.		self assert: parser parse: 'a' to: $a.	self assert: parser fail: 'b'	! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 6/1/2010 22:30'!
testLiteralObjectCaseInsensitive	| parser |	parser := $a asParser caseInsensitive.		self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'A' to: $A.	self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'B'	! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 9/15/2010 12:00'!
testLiteralSequence	| parser |	parser := PPLiteralSequenceParser 		on: 'abc'		message: 'sequence "abc" expected'.	self assert: parser size = 3.	self assert: parser literal = 'abc'.	self assert: parser message = 'sequence "abc" expected'.		self assert: parser parse: 'abc' to: 'abc'.	self assert: parser fail: 'ab'.	self assert: parser fail: 'abd'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 6/1/2010 22:31'!
testLiteralSequenceCaseInsensitive	| parser |	parser := 'abc' asParser caseInsensitive.		self assert: parser parse: 'abc' to: 'abc'.	self assert: parser parse: 'ABC' to: 'ABC'.	self assert: parser parse: 'abC' to: 'abC'.	self assert: parser parse: 'AbC' to: 'AbC'.		self assert: parser fail: 'ab'.	self assert: parser fail: 'abd'! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testMap1	| parser |	parser := #any asParser 		map: [ :a | Array with: a ].	self assert: parser parse: #(a) to: #(a)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testMap2	| parser |	parser := (#any asParser , #any asParser) 		map: [ :a :b | Array with: b with: a ].	self assert: parser parse: #(a b) to: #(b a)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testMap3	| parser |	parser := (#any asParser , #any asParser , #any asParser)		map: [ :a :b :c | Array with: c with: b with: a ].	self assert: parser parse: #(a b c) to: #(c b a)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testMapFail1	self		should: [ #any asParser map: [  ] ]		raise: Error.	self		should: [ #any asParser map: [ :a :b | ] ]		raise: Error! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 6/24/2011 06:16'!
testMapFail2	self		should: [ (#any asParser , #any asParser) map: [ :a | ] ]		raise: Error.	self		should: [ (#any asParser , #any asParser) map: [ :a :b :c | ] ]		raise: Error! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 2/8/2010 00:32'!
testMatches	| parser |	parser := $a asParser.		self assert: (parser matches: 'a').	self deny: (parser matches: 'b').		self assert: (parser matches: 'a' readStream).	self deny: (parser matches: 'b' readStream)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 2/8/2010 00:32'!
testMatchesIn	| parser result |	parser := $a asParser.		result := parser matchesIn: 'abba'.	self assert: result size = 2.	self assert: result first = $a.	self assert: result last = $a.		result := parser matchesIn: 'baaah'.	self assert: result size = 3.	self assert: result first = $a.	self assert: result last = $a! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 3/3/2010 15:33'!
testMatchesInEmpty	"Empty matches should properly advance and match at each position and at the end."	| parser result |	parser := [ :stream | stream position ] asParser.		result := parser matchesIn: '123'.	self assert: result asArray = #(0 1 2 3)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 3/3/2010 15:31'!
testMatchesInOverlapping	"Matches that overlap should be properly reported."	| parser result |	parser := #digit asParser , #digit asParser.		result := parser matchesIn: 'a123b'.	self assert: result size = 2.	self assert: result first = #($1 $2).	self assert: result last = #($2 $3)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/16/2011 07:27'!
testMatchesSkipIn	| parser result |	parser := $a asParser.		result := parser matchesSkipIn: 'abba'.	self assert: result size = 2.	self assert: result first = $a.	self assert: result last = $a.		result := parser matchesSkipIn: 'baaah'.	self assert: result size = 3.	self assert: result first = $a.	self assert: result last = $a! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/16/2011 07:28'!
testMatchesSkipInOverlapping	"Matches that overlap should be properly reported."	| parser result |	parser := #digit asParser , #digit asParser.		result := parser matchesSkipIn: 'a123b'.	self assert: result size = 1.	self assert: result first = #($1 $2)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 10/30/2011 12:13'!
testMatchingRangesIn	| input parser result |	input := 'a12b3'.	parser := #digit asParser plus.	result := parser matchingRangesIn: input.	result := result collect: [ :each | input copyFrom: each first to: each last ].	self assert: result size = 3.	self assert: result first = '12'.	self assert: result second = '2'.	self assert: result last = '3'! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 10/30/2011 12:12'!
testMatchingSkipRangesIn	| input parser result |	input := 'a12b3'.	parser := #digit asParser plus.	result := parser matchingSkipRangesIn: input.	result := result collect: [ :each | input copyFrom: each first to: each last ].	self assert: result size = 2.	self assert: result first = '12'.	self assert: result last = '3'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/2/2010 12:18'!
testMax	| parser |	parser := $a asParser max: 2.	self assert: parser min = 0.	self assert: parser max = 2.	self assert: parser parse: '' to: #().	self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a) end: 2.	self assert: parser parse: 'aaaa' to: #($a $a) end: 2.		self assert: (parser printString endsWith: '[0, 2]')! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 21:02'!
testMaxGreedy	| parser |	parser := #word asParser max: 2 greedy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abc'.		self assert: parser parse: '1' to: #() end: 0.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser fail: 'abc1'.		self assert: parser parse: '12' to: #($1) end: 1.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser fail: 'abc12'.		self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1) end: 2.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser fail: 'abc123'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 21:03'!
testMaxLazy	| parser |	parser := #word asParser max: 2 lazy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abc'.		self assert: parser parse: '1' to: #() end: 0.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser fail: 'abc1'.		self assert: parser parse: '12' to: #() end: 0.	self assert: parser parse: 'a12' to: #($a) end: 1.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser fail: 'abc12'.		self assert: parser parse: '123' to: #() end: 0.	self assert: parser parse: 'a123' to: #($a) end: 1.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser fail: 'abc123'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/2/2009 20:35'!
testMemoized	| count parser twice |	count := 0.	parser := [ :s | count := count + 1. s next ] asParser memoized.	twice := parser and , parser.		count := 0.	self assert: parser parse: 'a' to: $a.	self assert: count = 1.	count := 0.	self assert: twice parse: 'a' to: #($a $a).	self assert: count = 1.		self assert: parser memoized = parser! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/2/2010 12:18'!
testMin	| parser |	parser := $a asParser min: 2.	self assert: parser min = 2.	self assert: parser max > parser min.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a $a).	self assert: parser parse: 'aaaa' to: #($a $a $a $a).		self assert: (parser printString endsWith: '[2, *]')! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 21:12'!
testMinGreedy	| parser |	parser := #word asParser min: 2 greedy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abcde'.		self assert: parser fail: '1'.	self assert: parser fail: 'a1'.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1' to: #($a $b $c $d) end: 4.	self assert: parser parse: 'abcde1' to: #($a $b $c $d $e) end: 5.		self assert: parser fail: '12'.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b $1) end: 3.	self assert: parser parse: 'abc12' to: #($a $b $c $1) end: 4.	self assert: parser parse: 'abcd12' to: #($a $b $c $d $1) end: 5.	self assert: parser parse: 'abcde12' to: #($a $b $c $d $e $1) end: 6.		self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1 $2) end: 3.	self assert: parser parse: 'ab123' to: #($a $b $1 $2) end: 4.	self assert: parser parse: 'abc123' to: #($a $b $c $1 $2) end: 5.	self assert: parser parse: 'abcd123' to: #($a $b $c $d $1 $2) end: 6.	self assert: parser parse: 'abcde123' to: #($a $b $c $d $e $1 $2) end: 7.		self assert: parser parse: '1234' to: #($1 $2 $3) end: 3.	self assert: parser parse: 'a1234' to: #($a $1 $2 $3) end: 4.	self assert: parser parse: 'ab1234' to: #($a $b $1 $2 $3) end: 5.	self assert: parser parse: 'abc1234' to: #($a $b $c $1 $2 $3) end: 6.	self assert: parser parse: 'abcd1234' to: #($a $b $c $d $1 $2 $3) end: 7.	self assert: parser parse: 'abcde1234' to: #($a $b $c $d $e $1 $2 $3) end: 8! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 21:15'!
testMinLazy	| parser |	parser := #word asParser min: 2 lazy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abcde'.		self assert: parser fail: '1'.	self assert: parser fail: 'a1'.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1' to: #($a $b $c $d) end: 4.	self assert: parser parse: 'abcde1' to: #($a $b $c $d $e) end: 5.		self assert: parser fail: '12'.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser parse: 'abc12' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd12' to: #($a $b $c $d) end: 4.	self assert: parser parse: 'abcde12' to: #($a $b $c $d $e) end: 5.		self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1) end: 2.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser parse: 'abc123' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd123' to: #($a $b $c $d) end: 4.	self assert: parser parse: 'abcde123' to: #($a $b $c $d $e) end: 5.		self assert: parser parse: '1234' to: #($1 $2) end: 2.	self assert: parser parse: 'a1234' to: #($a $1) end: 2.	self assert: parser parse: 'ab1234' to: #($a $b) end: 2.	self assert: parser parse: 'abc1234' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1234' to: #($a $b $c $d) end: 4.	self assert: parser parse: 'abcde1234' to: #($a $b $c $d $e) end: 5! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/2/2010 12:19'!
testMinMax	| parser |	parser := $a asParser min: 2 max: 4.	self assert: parser min = 2.	self assert: parser max = 4.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a $a).	self assert: parser parse: 'aaaa' to: #($a $a $a $a).	self assert: parser parse: 'aaaaa' to: #($a $a $a $a) end: 4.	self assert: parser parse: 'aaaaaa' to: #($a $a $a $a) end: 4.		self assert: (parser printString endsWith: '[2, 4]')! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 20:54'!
testMinMaxGreedy	| parser |	parser := #word asParser min: 2 max: 4 greedy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abcde'.		self assert: parser fail: '1'.	self assert: parser fail: 'a1'.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde1'.		self assert: parser fail: '12'.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b $1) end: 3.	self assert: parser parse: 'abc12' to: #($a $b $c $1) end: 4.	self assert: parser parse: 'abcd12' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde12'.		self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1 $2) end: 3.	self assert: parser parse: 'ab123' to: #($a $b $1 $2) end: 4.	self assert: parser parse: 'abc123' to: #($a $b $c $1) end: 4.	self assert: parser parse: 'abcd123' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde123'.		self assert: parser parse: '1234' to: #($1 $2 $3) end: 3.	self assert: parser parse: 'a1234' to: #($a $1 $2 $3) end: 4.	self assert: parser parse: 'ab1234' to: #($a $b $1 $2) end: 4.	self assert: parser parse: 'abc1234' to: #($a $b $c $1) end: 4.	self assert: parser parse: 'abcd1234' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde1234'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/3/2011 20:57'!
testMinMaxLazy	| parser |	parser := #word asParser min: 2 max: 4 lazy: #digit asParser.		self assert: parser fail: ''.	self assert: parser fail: 'abcde'.		self assert: parser fail: '1'.	self assert: parser fail: 'a1'.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde1'.		self assert: parser fail: '12'.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser parse: 'abc12' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd12' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde12'.		self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1) end: 2.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser parse: 'abc123' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd123' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde123'.		self assert: parser parse: '1234' to: #($1 $2) end: 2.	self assert: parser parse: 'a1234' to: #($a $1) end: 2.	self assert: parser parse: 'ab1234' to: #($a $b) end: 2.	self assert: parser parse: 'abc1234' to: #($a $b $c) end: 3.	self assert: parser parse: 'abcd1234' to: #($a $b $c $d) end: 4.	self assert: parser fail: 'abcde1234'! !

!PPParserTest methodsFor: 'testing-accessing' stamp: 'lr 3/30/2009 16:36'!
testNamed	| parser |	parser := PPSequenceParser new.	self assert: parser name isNil.		parser := PPChoiceParser named: 'choice'.	self assert: parser name = 'choice'.		parser := $* asParser name: 'star'.	self assert: parser name = 'star'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 2/7/2010 20:10'!
testNegate	| parser |	parser := 'foo' asParser negate.		self assert: parser parse: 'f' to: $f end: 1.	self assert: parser parse: 'fo' to: $f end: 1.	self assert: parser parse: 'fob' to: $f end: 1.	self assert: parser parse: 'ffoo' to: $f end: 1.		self assert: parser fail: ''.	self assert: parser fail: 'foo'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/26/2010 09:54'!
testNot	| parser |	parser := 'foo' asParser flatten , 'bar' asParser flatten not.		self assert: parser parse: 'foobaz' to: #('foo' nil) end: 3.	self assert: parser fail: 'foobar'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/29/2008 11:32'!
testOptional	| parser |	parser := $a asParser optional.		self assert: parser parse: '' to: nil.	self assert: parser parse: 'a' to: $a.		self assert: parser parse: 'aa' to: $a end: 1.	self assert: parser parse: 'ab' to: $a end: 1.	self assert: parser parse: 'b' to: nil end: 0.	self assert: parser parse: 'bb' to: nil end: 0.	self assert: parser parse: 'ba' to: nil end: 0! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/14/2010 13:28'!
testParse	| parser result |	parser := $a asParser.		self assert: (parser parse: 'a') = $a.	self assert: (result := parser parse: 'b') isPetitFailure.	self assert: (result message includesSubString: '$a').	self assert: (result message includesSubString: 'expected').	self assert: (result position = 0).		self assert: (parser parse: 'a' readStream) = $a.	self assert: (result := parser parse: 'b' readStream) isPetitFailure.	self assert: (result message includesSubString: '$a').	self assert: (result message includesSubString: 'expected').	self assert: (result position = 0)! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 2/7/2010 23:00'!
testParseOnError0	| parser result seen |	parser := $a asParser.	result := parser parse: 'a' onError: [ self signalFailure: 'Not supposed to report an error' ].	self assert: result = $a.	result := parser parse: 'b' onError: [ seen := true ].	self assert: result.	self assert: seen! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/14/2010 13:30'!
testParseOnError1	| parser result seen |	parser := $a asParser.		result := parser parse: 'a' onError: [ self signalFailure: 'Not supposed to report an error' ].	self assert: result = $a.		result := parser parse: 'b' onError: [ :failure | 		self assert: (failure position = 0).		self assert: (failure message includesSubString: '$a').		self assert: (failure message includesSubString: 'expected').		seen := true ].	self assert: result.	self assert: seen! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/14/2010 13:28'!
testParseOnError2	| parser result seen |	parser := $a asParser.		result := parser parse: 'a' onError: [ self signalFailure: 'Not supposed to report an error' ].	self assert: result = $a.	result := parser parse: 'b' onError: [ :msg :pos | 		self assert: (msg includesSubString: '$a').		self assert: (msg includesSubString: 'expected').		self assert: pos = 0.		seen := true ].	self assert: result.	self assert: seen! !

!PPParserTest methodsFor: 'testing-utilities' stamp: 'lr 8/6/2010 19:06'!
testParser	| parser |	parser := PPParser new.		self assert: parser isPetitParser.	self deny: 4 isPetitParser.	self deny: 'foo' isPetitParser! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 1/8/2010 12:09'!
testPermutation	| parser |	parser := #any asParser , #any asParser , #any asParser.	self assert: (parser permutation: #()) parse: '123' to: #().	self assert: (parser permutation: #(1)) parse: '123' to: #($1).	self assert: (parser permutation: #(1 3)) parse: '123' to: #($1 $3).	self assert: (parser permutation: #(3 1)) parse: '123' to: #($3 $1).	self assert: (parser permutation: #(2 2)) parse: '123' to: #($2 $2).	self assert: (parser permutation: #(3 2 1)) parse: '123' to: #($3 $2 $1).		self should: [ parser permutation: #(0) ] raise: Error.	self should: [ parser permutation: #(4) ] raise: Error.	self should: [ parser permutation: #($2) ] raise: Error! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 5/2/2010 12:26'!
testPluggable	| block parser |	block := [ :stream | stream position ].	parser := block asParser.	self assert: parser block = block! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:01'!
testPlus	| parser |	parser := $a asParser plus.		self assert: parser min = 1.	self assert: parser max > parser min.		self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a $a).		self assert: parser parse: 'ab' to: #($a) end: 1.	self assert: parser parse: 'aab' to: #($a $a) end: 2.	self assert: parser parse: 'aaab' to: #($a $a $a) end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'ba'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:01'!
testPlusGreedy	| limit parser |	limit := #digit asParser.	parser := #word asParser plusGreedy: limit.		self assert: parser min = 1.	self assert: parser max > parser min.		self assert: parser limit = limit.	self assert: parser children size = 2.	self assert: parser children last = limit.	self assert: parser fail: ''.	self assert: parser fail: '1'.	self assert: parser fail: 'a'.	self assert: parser fail: 'ab'.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b $1) end: 3.	self assert: parser parse: 'abc12' to: #($a $b $c $1) end: 4.	self assert: parser parse: 'a123' to: #($a $1 $2) end: 3.	self assert: parser parse: 'ab123' to: #($a $b $1 $2) end: 4.	self assert: parser parse: 'abc123' to: #($a $b $c $1 $2) end: 5.! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:01'!
testPlusLazy	| limit parser |	limit := #digit asParser.	parser := #word asParser plusLazy: limit.		self assert: parser min = 1.	self assert: parser max > parser min.		self assert: parser limit = limit.	self assert: parser children size = 2.	self assert: parser children last = limit.	self assert: parser fail: ''.	self assert: parser fail: '1'.	self assert: parser fail: 'a'.	self assert: parser fail: 'ab'.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: 'a12' to: #($a) end: 1.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser parse: 'abc12' to: #($a $b $c) end: 3.	self assert: parser parse: 'a123' to: #($a) end: 1.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser parse: 'abc123' to: #($a $b $c) end: 3! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:41'!
testPostCopy	| parser copy |	parser := PPParser new.	parser propertyAt: #foo put: true.	copy := parser copy.	copy propertyAt: #foo put: false.	self assert: (parser propertyAt: #foo).	self deny: (copy propertyAt: #foo)! !

!PPParserTest methodsFor: 'testing-accessing' stamp: 'lr 8/14/2010 13:16'!
testPrint	| parser |	parser := PPParser new.	self assert: (parser printString includesSubString: 'PPParser').		parser := PPParser named: 'choice'.	self assert: (parser printString includesSubString: 'PPParser(choice').		parser := PPLiteralObjectParser on: $a.	self assert: (parser printString includesSubString: '$a').		parser := PPFailingParser message: 'error'.	self assert: (parser printString includesSubString: 'error').		parser := PPPredicateObjectParser on: [ :c | true ] message: 'error'.	self assert: (parser printString includesSubString: 'error')! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:36'!
testPropertyAt	| parser |	parser := PPParser new.	self should: [ parser propertyAt: #foo ] raise: Error.	parser propertyAt: #foo put: true.	self assert: (parser propertyAt: #foo)! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:37'!
testPropertyAtIfAbsent	| parser |	parser := PPParser new.	self assert: (parser propertyAt: #foo ifAbsent: [ true ]).	parser propertyAt: #foo put: true.	self assert: (parser propertyAt: #foo ifAbsent: [ false ])! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:37'!
testPropertyAtIfAbsentPut	| parser |	parser := PPParser new.	self assert: (parser propertyAt: #foo ifAbsentPut: [ true ]).	self assert: (parser propertyAt: #foo ifAbsentPut: [ false ])! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:37'!
testRemoveProperty	| parser |	parser := PPParser new.	self should: [ parser removeProperty: #foo ] raise: Error.	parser propertyAt: #foo put: true.	self assert: (parser removeProperty: #foo)! !

!PPParserTest methodsFor: 'testing-properties' stamp: 'lr 4/19/2010 10:37'!
testRemovePropertyIfAbsent	| parser |	parser := PPParser new.	self assert: (parser removeProperty: #foo ifAbsent: [ true ]).	parser propertyAt: #foo put: true.	self assert: (parser removeProperty: #foo ifAbsent: [ false ])! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/29/2008 11:42'!
testSeparatedBy	| parser |	parser := $a asParser separatedBy: $b asParser.		self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aba' to: #($a $b $a).	self assert: parser parse: 'ababa' to: #($a $b $a $b $a).		self assert: parser parse: 'ab' to: #($a) end: 1.	self assert: parser parse: 'abab' to: #($a $b $a) end: 3.	self assert: parser parse: 'ac' to: #($a) end: 1.	self assert: parser parse: 'abac' to: #($a $b $a) end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'c'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 2/25/2012 16:55'!
testSeparatedByWithoutSeparators	| parser |	parser := ($a asParser separatedBy: $b asParser)		withoutSeparators.		self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aba' to: #($a $a).	self assert: parser parse: 'ababa' to: #($a $a $a).		self assert: parser parse: 'ab' to: #($a) end: 1.	self assert: parser parse: 'abab' to: #($a $a) end: 3.	self assert: parser parse: 'ac' to: #($a) end: 1.	self assert: parser parse: 'abac' to: #($a $a) end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'c'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/29/2008 11:33'!
testSequence	| parser |	parser := $a asParser , $b asParser.		self assert: parser parse: 'ab' to: #($a $b).		self assert: parser parse: 'aba' to: #($a $b) end: 2.	self assert: parser parse: 'abb' to: #($a $b) end: 2.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'aa'.	self assert: parser fail: 'ba'.	self assert: parser fail: 'bab'! !

!PPParserTest methodsFor: 'testing-fixtures' stamp: 'lr 2/7/2010 22:00'!
testSideEffectChoice	"Adding another element to a choice should create a copy, otherwise we get unwanted side-effects."	| p1 p2 p3 |	p1 := $a asParser.	p2 := p1 / $b asParser.	p3 := p1 / $c asParser.		self assert: p1 parse: 'a'.	self assert: p1 fail: 'b'.	self assert: p1 fail: 'c'.		self assert: p2 parse: 'a'.	self assert: p2 parse: 'b'.	self assert: p2 fail: 'c'.		self assert: p3 parse: 'a'.	self assert: p3 fail: 'b'.	self assert: p3 parse: 'c'! !

!PPParserTest methodsFor: 'testing-fixtures' stamp: 'lr 5/31/2010 19:25'!
testSideEffectListCopy	| old new |	old := $a asParser , $b asParser.	new := old copy.		self deny: old == new.	self deny: old children == new children.	self assert: old children first == new children first.	self assert: old children last == new children last! !

!PPParserTest methodsFor: 'testing-fixtures' stamp: 'lr 4/14/2010 11:38'!
testSideEffectSequence	"Adding another element to a sequence should create a copy, otherwise we get unwanted side-effects."	| p1 p2 p3 |	p1 := $a asParser.	p2 := p1 , $b asParser.	p3 := p1 , $c asParser.		self assert: p1 parse: 'a'.		self assert: p1 parse: 'ab' end: 1.	self assert: p1 parse: 'ac' end: 1.		self assert: p2 fail: 'a'.		self assert: p2 parse: 'ab'.	self assert: p2 fail: 'ac'.	self assert: p3 fail: 'a'.		self assert: p3 fail: 'ab'.	self assert: p3 parse: 'ac'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:02'!
testStar	| parser |	parser := $a asParser star.		self assert: parser min = 0.	self assert: parser max > parser min.		self assert: parser parse: '' to: #().	self assert: parser parse: 'a' to: #($a).	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a $a).		self assert: parser parse: 'b' to: #() end: 0.	self assert: parser parse: 'ab' to: #($a) end: 1.	self assert: parser parse: 'aab' to: #($a $a) end: 2.	self assert: parser parse: 'aaab' to: #($a $a $a) end: 3! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:02'!
testStarGreedy	| limit parser |	limit := #digit asParser.	parser := #word asParser starGreedy: limit.		self assert: parser min = 0.	self assert: parser max > parser min.	self assert: parser limit = limit.	self assert: parser children size = 2.	self assert: parser children last = limit.	self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'ab'.	self assert: parser parse: '1' to: #() end: 0.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: '12' to: #($1) end: 1.	self assert: parser parse: 'a12' to: #($a $1) end: 2.	self assert: parser parse: 'ab12' to: #($a $b $1) end: 3.	self assert: parser parse: 'abc12' to: #($a $b $c $1) end: 4.	self assert: parser parse: '123' to: #($1 $2) end: 2.	self assert: parser parse: 'a123' to: #($a $1 $2) end: 3.	self assert: parser parse: 'ab123' to: #($a $b $1 $2) end: 4.	self assert: parser parse: 'abc123' to: #($a $b $c $1 $2) end: 5! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 8/17/2011 10:02'!
testStarLazy	| limit parser |	limit := #digit asParser.	parser := #word asParser starLazy: limit.		self assert: parser min = 0.	self assert: parser max > parser min.	self assert: parser limit = limit.	self assert: parser children size = 2.	self assert: parser children last = limit.	self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'ab'.	self assert: parser parse: '1' to: #() end: 0.	self assert: parser parse: 'a1' to: #($a) end: 1.	self assert: parser parse: 'ab1' to: #($a $b) end: 2.	self assert: parser parse: 'abc1' to: #($a $b $c) end: 3.	self assert: parser parse: '12' to: #() end: 0.	self assert: parser parse: 'a12' to: #($a) end: 1.	self assert: parser parse: 'ab12' to: #($a $b) end: 2.	self assert: parser parse: 'abc12' to: #($a $b $c) end: 3.	self assert: parser parse: '123' to: #() end: 0.	self assert: parser parse: 'a123' to: #($a) end: 1.	self assert: parser parse: 'ab123' to: #($a $b) end: 2.	self assert: parser parse: 'abc123' to: #($a $b $c) end: 3! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 9/15/2010 09:53'!
testTimes	| parser |	parser := $a asParser times: 2.		self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser parse: 'aa' to: #($a $a).	self assert: parser parse: 'aaa' to: #($a $a) end: 2! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 4/6/2010 19:47'!
testToken	| parser |	parser := $a asParser token.	self assert: parser tokenClass = PPToken.	self assert: parser parse: 'a' toToken: 1 stop: 1.		self assert: parser fail: 'b'.	self assert: parser fail: ''.		parser := $a asParser token: PPToken.	self assert: parser tokenClass = PPToken.	self assert: parser parse: 'a' toToken: 1 stop: 1.		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 7/11/2011 11:05'!
testTrim	| parser |	parser := $a asParser token trim.		self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: 'a ' toToken: 1 stop: 1.	self assert: parser parse: 'a	' toToken: 1 stop: 1.	self assert: parser parse: 'a  ' toToken: 1 stop: 1.	self assert: parser parse: 'a 	 ' toToken: 1 stop: 1.			self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: ' a' toToken: 2 stop: 2.	self assert: parser parse: '	a' toToken: 2 stop: 2.	self assert: parser parse: '    a' toToken: 5 stop: 5.	self assert: parser parse: '   a' toToken: 5 stop: 5.		self assert: parser parse: 'aa' toToken: 1 stop: 1 end: 1.	self assert: parser parse: 'a	a' toToken: 1 stop: 1 end: 2.	self assert: parser parse: 'a  a' toToken: 1 stop: 1 end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 7/31/2010 12:07'!
testTrimBlanks	| parser |	parser := $a asParser token trimBlanks.		self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: 'a ' toToken: 1 stop: 1.	self assert: parser parse: 'a	' toToken: 1 stop: 1.	self assert: parser parse: 'a  ' toToken: 1 stop: 1.		self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: ' a' toToken: 2 stop: 2.	self assert: parser parse: '	a' toToken: 2 stop: 2.	self assert: parser parse: '    a' toToken: 5 stop: 5.		self assert: parser parse: 'aa' toToken: 1 stop: 1 end: 1.	self assert: parser parse: 'a	a' toToken: 1 stop: 1 end: 2.	self assert: parser parse: 'a  a' toToken: 1 stop: 1 end: 3.		self assert: parser fail: ''.	self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: 'b'.! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 7/11/2011 11:05'!
testTrimCustom	| parser |	parser := $a asParser token trim: $b asParser.		self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: 'ab' toToken: 1 stop: 1.	self assert: parser parse: 'abb' toToken: 1 stop: 1.			self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: 'ba' toToken: 2 stop: 2.	self assert: parser parse: 'bba' toToken: 3 stop: 3.		self assert: parser parse: 'aa' toToken: 1 stop: 1 end: 1.	self assert: parser parse: 'ab' toToken: 1 stop: 1 end: 2.	self assert: parser parse: 'abba' toToken: 1 stop: 1 end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 7/31/2010 12:07'!
testTrimSpaces	| parser |	parser := $a asParser token trimSpaces.		self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: 'a ' toToken: 1 stop: 1.	self assert: parser parse: 'a	' toToken: 1 stop: 1.	self assert: parser parse: 'a  ' toToken: 1 stop: 1.	self assert: parser parse: 'a 	 ' toToken: 1 stop: 1.			self assert: parser parse: 'a' toToken: 1 stop: 1.	self assert: parser parse: ' a' toToken: 2 stop: 2.	self assert: parser parse: '	a' toToken: 2 stop: 2.	self assert: parser parse: '    a' toToken: 5 stop: 5.	self assert: parser parse: '   a' toToken: 5 stop: 5.		self assert: parser parse: 'aa' toToken: 1 stop: 1 end: 1.	self assert: parser parse: 'a	a' toToken: 1 stop: 1 end: 2.	self assert: parser parse: 'a  a' toToken: 1 stop: 1 end: 3.		self assert: parser fail: ''.	self assert: parser fail: 'b'! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 11/20/2009 15:31'!
testUnresolved	| parser |	parser := PPUnresolvedParser new.		self assert: parser isUnresolved.	self should: [ parser parse: '' ] raise: Error.	self should: [ parser parse: 'a' ] raise: Error.	self should: [ parser parse: 'ab' ] raise: Error.		parser := nil asParser.	self deny: parser isUnresolved! !

!PPParserTest methodsFor: 'testing' stamp: 'tg 7/29/2010 22:39'!
testWrapped	| parser |	parser := $a asParser wrapped.		self assert: parser parse: 'a' to: $a.	self assert: parser fail: 'b'.		parser := (($a asParser , $b asParser ) wrapped , $c asParser).	self assert: parser parse: 'abc' to: #(#($a $b) $c)! !

!PPParserTest methodsFor: 'testing-mapping' stamp: 'lr 5/12/2010 20:40'!
testWrapping	| parser result |	parser := #digit asParser plus >=> [ :stream :cc | 		Array 			with: stream position 			with: cc value 			with: stream position ].	self assert: parser parse: '1' to: #(0 ($1) 1).	self assert: parser parse: '12' to: #(0 ($1 $2) 2).	self assert: parser parse: '123' to: #(0 ($1 $2 $3) 3).		result := parser parse: 'a'.	self assert: result first = 0.	self assert: result second isPetitFailure.	self assert: result last = 0! !

!PPParserTest methodsFor: 'testing' stamp: 'lr 4/14/2010 16:30'!
testXor	| parser |	parser := ($a asParser / $b asParser)			|  ($b asParser / $c asParser).		self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'c' to: $c.		self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'd'.		" truly symmetric "	parser := ($b asParser / $c asParser)			|  ($a asParser / $b asParser).		self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'c' to: $c.		self assert: parser fail: ''.	self assert: parser fail: 'b'.	self assert: parser fail: 'd'! !

!PPPredicateTest methodsFor: 'utilities' stamp: 'lr 6/12/2010 08:37'!
assertCharacterSets: aParser	"Assert the character set of aParser does not overlap with the character set with the negated parser, and that they both cover the complete character space."	| positives negatives |	positives := self parsedCharacterSet: aParser.	negatives := self parsedCharacterSet: aParser negate.	self charactersDo: [ :char | 		| positive negative |		positive := positives includes: char.		negative := negatives includes: char.		self 			assert: ((positive and: [ negative not ])				or: [ positive not and: [ negative ] ])			description: char printString , ' should be in exactly one set' ]! !

!PPPredicateTest methodsFor: 'private' stamp: 'pmon 5/31/2012 23:53'!
charactersDo: aBlock
	0 to: 255 do: [ :index | aBlock value: (Character value: index) ]! !

!PPPredicateTest methodsFor: 'utilities' stamp: 'lr 6/12/2010 08:37'!
parsedCharacterSet: aParser	| result |	result := WriteStream on: String new.	self charactersDo: [ :char |		(aParser matches: (String with: char))			ifTrue: [ result nextPut: char ] ].	^ result contents! !

!PPPredicateTest methodsFor: 'testing-objects' stamp: 'lr 11/29/2009 09:32'!
testAny	| parser |	parser := #any asParser.	self assertCharacterSets: parser.	self assert: parser parse: ' ' to: $ .	self assert: parser parse: '1' to: $1.	self assert: parser parse: 'a' to: $a.	self assert: parser fail: ''! !

!PPPredicateTest methodsFor: 'testing-objects' stamp: 'tg 7/12/2010 11:26'!
testAnyExceptAnyOf	| parser |	parser := PPPredicateObjectParser anyExceptAnyOf: #($: $,).	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'z' to: $z.	self assert: parser fail: ':'.	self assert: parser fail: ','! !

!PPPredicateTest methodsFor: 'testing-objects' stamp: 'lr 6/12/2010 09:16'!
testAnyOf	| parser |	parser := PPPredicateObjectParser anyOf: #($a $z).	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'z' to: $z.	self assert: parser fail: 'x'! !

!PPPredicateTest methodsFor: 'testing-objects' stamp: 'lr 6/12/2010 09:16'!
testBetweenAnd	| parser |	parser := PPPredicateObjectParser between: $b and: $d.	self assertCharacterSets: parser.	self assert: parser fail: 'a'.	self assert: parser parse: 'b' to: $b.	self assert: parser parse: 'c' to: $c.	self assert: parser parse: 'd' to: $d.	self assert: parser fail: 'e'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'pmon 5/31/2012 23:57'!
testBlank
	| parser |
	parser := #blank asParser.
	self assertCharacterSets: parser.
	self assert: parser parse: (String with: Character space) to: Character space.
	self assert: parser parse: (String with: Character tab) to: Character tab.
	self assert: parser fail: ''.
	self assert: parser fail: '1'.
	self assert: parser fail: (String with: Character crCharacter)! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 5/2/2010 12:51'!
testChar	| parser |	parser := $* asParser.	self assertCharacterSets: parser.	self assert: parser parse: '*' to: $*.	self assert: parser parse: '**' to: $* end: 1.	self assert: parser fail: ''.	self assert: parser fail: '1'.	self assert: parser fail: 'a'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'pmon 5/31/2012 23:56'!
testCr
	| parser |
	parser := #cr asParser.
	self assertCharacterSets: parser.
	self assert: parser parse: (String with: Character crCharacter) to: Character crCharacter! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testDigit	| parser |	parser := #digit asParser.	self assertCharacterSets: parser.	self assert: parser parse: '0' to: $0.	self assert: parser parse: '9' to: $9.	self assert: parser fail: ''.	self assert: parser fail: 'a'! !

!PPPredicateTest methodsFor: 'testing-objects' stamp: 'lr 6/12/2010 09:16'!
testExpect	| parser |	parser := PPPredicateObjectParser expect: $a.	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser fail: 'b'.	self assert: parser fail: ''! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testHex	| parser |	parser := #hex asParser.	self assertCharacterSets: parser.	self assert: parser parse: '0' to: $0.	self assert: parser parse: '5' to: $5.	self assert: parser parse: '9' to: $9.	self assert: parser parse: 'A' to: $A.	self assert: parser parse: 'D' to: $D.	self assert: parser parse: 'F' to: $F.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'e' to: $e.	self assert: parser parse: 'f' to: $f.	self assert: parser fail: ''.	self assert: parser fail: 'g'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testLetter	| parser |	parser := #letter asParser.	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'Z' to: $Z.	self assert: parser fail: ''.	self assert: parser fail: '0'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'pmon 5/31/2012 23:55'!
testLf
	| parser |
	parser := #lf asParser.
	self assertCharacterSets: parser.
	self assert: parser parse: (String with: Character lfCharacter) to: Character lfCharacter! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testLowercase	| parser |	parser := #lowercase asParser.	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'z' to: $z.	self assert: parser fail: ''.	self assert: parser fail: 'A'.	self assert: parser fail: '0'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'pmon 5/31/2012 23:57'!
testNewline
	| parser |
	parser := #newline asParser.
	self assertCharacterSets: parser.
	self assert: parser parse: (String with: Character crCharacter) to: Character crCharacter.
	self assert: parser parse: (String with: Character lfCharacter) to: Character lfCharacter.
	self assert: parser fail: ' '! !

!PPPredicateTest methodsFor: 'testing' stamp: 'lr 6/12/2010 09:17'!
testOnMessage	| block parser |	block := [ :char | char = $* ].	parser := PPPredicateObjectParser on: block message: 'starlet'.	self assert: parser block = block.	self assert: parser message = 'starlet'.		self assertCharacterSets: parser.	self assert: parser parse: '*' to: $*.	self assert: parser parse: '**' to: $* end: 1.	self assert: parser fail: ''.	self assert: parser fail: '1'.	self assert: parser fail: 'a'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 5/5/2010 14:14'!
testPunctuation	| parser |	parser := #punctuation asParser.	self assertCharacterSets: parser.	self assert: parser parse: '.' to: $..	self assert: parser parse: ',' to: $,.	self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: '1'! !

!PPPredicateTest methodsFor: 'testing-sequence' stamp: 'lr 6/12/2010 09:27'!
testSequenceParser	| parser |	parser := PPPredicateSequenceParser 		on: [ :value | value first isUppercase ] 		message: 'uppercase 3 letter words'		size: 3.	self assert: parser size = 3.	self assert: parser parse: 'Abc'.	self assert: parser parse: 'ABc'.	self assert: parser parse: 'ABC'.	self assert: parser fail: 'abc'.	self assert: parser fail: 'aBC'.	self assert: parser fail: 'Ab'.		parser := parser negate.	self assert: parser size = 3.	self assert: parser fail: 'Abc'.	self assert: parser fail: 'ABc'.	self assert: parser fail: 'ABC'.	self assert: parser parse: 'abc'.	self assert: parser parse: 'aBC'.	self assert: parser fail: 'Ab'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testSpace	| parser |	parser := #space asParser.	self assertCharacterSets: parser.	self assert: parser parse: (String with: Character tab) to: Character tab.	self assert: parser parse: ' ' to: Character space.	self assert: parser fail: ''.	self assert: parser fail: 'a'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testTab	| parser |	parser := #tab asParser.	self assertCharacterSets: parser.	self assert: parser parse: (String with: Character tab) to: Character tab! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testUppercase	| parser |	parser := #uppercase asParser.	self assertCharacterSets: parser.	self assert: parser parse: 'A' to: $A.	self assert: parser parse: 'Z' to: $Z.	self assert: parser fail: ''.	self assert: parser fail: 'a'.	self assert: parser fail: '0'! !

!PPPredicateTest methodsFor: 'testing-chars' stamp: 'lr 11/29/2009 09:32'!
testWord	| parser |	parser := #word asParser.	self assertCharacterSets: parser.	self assert: parser parse: 'a' to: $a.	self assert: parser parse: 'A' to: $A.	self assert: parser parse: '0' to: $0.	self assert: parser fail: ''.	self assert: parser fail: '-'! !

!PPScriptingTest methodsFor: 'examples' stamp: 'pmon 5/31/2012 23:59'!
expressionInterpreter
	"Same as #expressionInterpreter but with semantic actions."
	
	| mul prim add dec |
	add := PPUnresolvedParser new.
	mul := PPUnresolvedParser new.
	prim := PPUnresolvedParser new.
	dec := ($0 - $9) ==> [ :token | token asciiValue - $0 asciiValue ].
	add def: ((mul , $+ asParser , add) ==> [ :nodes | (nodes at: 1) + (nodes at: 3) ])
		/ mul.
	mul def: ((prim , $* asParser , mul) ==> [ :nodes | (nodes at: 1) * (nodes at: 3) ])
		/ prim.
	prim def: (($( asParser , add , $) asParser) ==> [ :nodes | nodes at: 2 ])
		/ dec.
	^ add end! !

!PPScriptingTest methodsFor: 'examples' stamp: 'lr 6/12/2010 08:30'!
expressionParser	"Simple demo of scripting an expression parser."		| mul prim add dec |	add := PPUnresolvedParser new.	mul := PPUnresolvedParser new.	prim := PPUnresolvedParser new.	dec := ($0 - $9).	add def: (mul , $+ asParser , add)		/ mul.	mul def: (prim , $* asParser , mul)		/ prim.	prim def: ($( asParser , add , $) asParser)		/ dec.	^ add end! !

!PPScriptingTest methodsFor: 'examples' stamp: 'lr 10/20/2008 13:27'!
straightLineParser	| goal stm stmList id char dec exp expList mulExp primExp nonzero num lower upper |	goal := PPUnresolvedParser new.	stmList := PPUnresolvedParser new.	stm := PPUnresolvedParser new.	exp := PPUnresolvedParser new.	expList := PPUnresolvedParser new.	mulExp := PPUnresolvedParser new.	primExp := PPUnresolvedParser new.		lower := $a - $z.	upper := $A - $Z.	char := lower / upper.	nonzero := $1 - $9.	dec := $0 - $9.	id := char, ( char / dec ) star.	num := $0 asParser / ( nonzero, dec star).	goal def: stmList end.	stmList def: stm , ( $; asParser, stm ) star.	stm def: ( id, ':=' asParser, exp )		/ ( 'print' asParser, $( asParser, expList, $) asParser ). 	exp def: mulExp, ( ( $+ asParser / $- asParser ), mulExp ) star.	expList def: exp, ( $, asParser, exp ) star.	mulExp def: primExp, ( ( $* asParser / $/ asParser ), primExp ) star.	primExp def: id		/ num		/ ( $( asParser, stmList, $, asParser, exp, $) asParser ).	^ goal! !

!PPScriptingTest methodsFor: 'tests' stamp: 'lr 6/12/2010 08:31'!
testExpressionInterpreter	self 		assert: self expressionInterpreter		parse: '2*(3+4)'		to: 14! !

!PPScriptingTest methodsFor: 'tests' stamp: 'lr 6/12/2010 08:31'!
testExpressionParser	self		assert: self expressionParser		parse: '2*(3+4)'		to: #($2 $* ($( ($3 $+ $4) $)))! !

!PPScriptingTest methodsFor: 'tests' stamp: 'lr 9/17/2008 22:44'!
testSLassign		self assert: self straightLineParser		parse: 'abc:=1'		to: #(#($a #($b $c) ':=' #(#(#($1 #()) #()) #())) #())! !

!PPScriptingTest methodsFor: 'tests' stamp: 'lr 6/12/2010 08:27'!
testSLprint	self 		assert: self straightLineParser		parse: 'print(3,4)'		to: #(('print' $( ((($3 ()) ()) () (($, ((($4 ()) ()) ())))) $)) ())! !

!PPTokenTest methodsFor: 'accessing' stamp: 'lr 4/3/2009 08:51'!
identifier	^ #word asParser plus token! !

!PPTokenTest methodsFor: 'utilities' stamp: 'lr 3/29/2010 15:34'!
parse: aString using: aParser	^ aParser parse: aString! !

!PPTokenTest methodsFor: 'testing' stamp: 'lr 4/3/2009 08:49'!
testCollection	| input result |	input := 'foo    '.	result := self 		parse: input		using: self identifier.	self assert: (result collection = input).	self assert: (result collection == input)! !

!PPTokenTest methodsFor: 'testing-querying' stamp: 'pmon 6/1/2012 00:00'!
testColumn
	| input parser result |
	input := '1' , (String with: Character crCharacter) , '12' , (String with: Character crCharacter with: Character lfCharacter) , '123' , (String with: Character lfCharacter) , '1234'.
	parser := #any asParser token star.
	result := parser parse: input.
	result 
		with:  #(1 2 1 2 3 4 1 2 3 4 1 2 3 4)
		do: [ :token :line | self assert: token column = line ]! !

!PPTokenTest methodsFor: 'testing-copying' stamp: 'lr 4/21/2009 08:50'!
testCopyFromTo	| result other |	result := PPToken on: 'abc'.	other := result copyFrom: 2 to: 2.		self assert: other size = 1.	self assert: other start = 2.	self assert: other stop = 2.	self assert: other collection = result collection! !

!PPTokenTest methodsFor: 'testing-comparing' stamp: 'lr 10/23/2009 11:37'!
testEquality	| token1 token2 |	token1 := self  parse: 'foo' using: self identifier.	token2 := self  parse: 'foo' using: self identifier.	self deny: token1 == token2.	self assert: token1 = token2.	self assert: token1 hash = token2 hash.! !

!PPTokenTest methodsFor: 'testing-querying' stamp: 'pmon 6/1/2012 00:01'!
testLine
	| input parser result |
	input := '1' , (String with: Character crCharacter) , '12' , (String with: Character crCharacter with: Character lfCharacter) , '123' , (String with: Character lfCharacter) , '1234'.
	parser := #any asParser token star.
	result := parser parse: input.
	result 
		with: #(1 1 2 2 2 2 3 3 3 3 4 4 4 4) 
		do: [ :token :line | self assert: token line = line ]! !

!PPTokenTest methodsFor: 'testing' stamp: 'lr 4/14/2010 11:44'!
testNew	self should: [ PPToken new ] raise: Error.	! !

!PPTokenTest methodsFor: 'testing' stamp: 'lr 8/14/2010 13:16'!
testPrinting	| result |	result := PPToken on: 'var'.	self assert: (result printString includesSubString: 'PPToken(var)')! !

!PPTokenTest methodsFor: 'testing' stamp: 'TestRunner 12/4/2009 19:16'!
testSize	| result |	result := self 		parse: 'foo'		using: self identifier.	self assert: result size = 3! !

!PPTokenTest methodsFor: 'testing' stamp: 'TestRunner 12/4/2009 19:16'!
testStart	| result |	result := self 		parse: 'foo'		using: self identifier.	self assert: result start = 1! !

!PPTokenTest methodsFor: 'testing' stamp: 'TestRunner 12/4/2009 19:16'!
testStop	| result |	result := self 		parse: 'foo'		using: self identifier.	self assert: result stop = 3! !

!PPTokenTest methodsFor: 'testing' stamp: 'lr 4/3/2009 08:51'!
testValue	| input result |	input := 'foo'.	result := self 		parse: input		using: self identifier.	self assert: result value = input.	self deny: result value == input! !
