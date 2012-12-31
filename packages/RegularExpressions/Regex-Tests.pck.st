'From Cuis 4.1 of 12 December 2012 [latest update: #1524] on 30 December 2012 at 6:01:06 pm'!
'Description Please enter a description for this package '!
!classDefinition: #RxMatcherTest category: #'Regex-Tests-Core'!
TestCase subclass: #RxMatcherTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Tests-Core'!
!classDefinition: 'RxMatcherTest class' category: #'Regex-Tests-Core'!
RxMatcherTest class
	instanceVariableNames: ''!

!classDefinition: #RxParserTest category: #'Regex-Tests-Core'!
TestCase subclass: #RxParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Regex-Tests-Core'!
!classDefinition: 'RxParserTest class' category: #'Regex-Tests-Core'!
RxParserTest class
	instanceVariableNames: ''!


!RxMatcherTest commentStamp: 'Tbn 11/12/2010 22:31' prior: 0!
This class provides tests for the regular expression matcher.!

!RxParserTest commentStamp: 'Tbn 11/12/2010 22:31' prior: 0!
This class provides tests for the regular expression parser.!

!RxMatcherTest methodsFor: 'utilties' stamp: 'TestRunner 1/15/2010 19:38'!
compileRegex: aString	"Compile the regex and answer the matcher, or answer nil if compilation fails."	| syntaxTree |	syntaxTree := self parserClass safelyParse: aString.	^ syntaxTree isNil ifFalse: [ self matcherClass for: syntaxTree ]! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
henryReadme	self error: 'The tests in this category are based on the ones in Henry Spencer''s regexp.c package.'! !

!RxMatcherTest methodsFor: 'accessing' stamp: 'gsa 12/22/2012 12:25'!
matcherClass
	^ RxMatcher! !

!RxMatcherTest methodsFor: 'accessing' stamp: 'lr 1/15/2010 19:39'!
parserClass	^ RxParser! !

!RxMatcherTest methodsFor: 'utilties' stamp: 'gsa 12/22/2012 12:26'!
runMatcher: aMatcher with: aString expect: aBoolean withSubexpressions: anArray
	| copy got |
	copy := aMatcher
		copy: aString
		translatingMatchesUsing: [ :each | each ].
	self 
		assert: copy = aString
		description: 'Copying: expected ' , aString printString , ', but got ' , copy printString.
	got := aMatcher search: aString.
	self
		assert: got = aBoolean 
		description: 'Searching: expected ' , aBoolean printString , ', but got ' , got printString.
	(anArray isNil or: [ aMatcher supportsSubexpressions not ])
		ifTrue: [ ^ self ].
	1 to: anArray size by: 2 do: [ :index |
		| sub subExpect subGot |
		sub := anArray at: index.
		subExpect := anArray at: index + 1.
		subGot := aMatcher subexpression: sub.
		self
			assert: subExpect = subGot
			description: 'Subexpression ' , sub printString , ': expected ' , subExpect printString , ', but got ' , subGot printString ]! !

!RxMatcherTest methodsFor: 'utilties' stamp: 'lr 1/15/2010 19:31'!
runRegex: anArray	"Run a clause anArray against a set of tests. Each clause is an array with a regex source string followed by sequence of 3-tuples. Each three-element group is one test to try against the regex, and includes: 1) test string; 2) expected result; 3) expected subexpression as an array of (index, substring), or nil."	| source matcher |	source := anArray first.	matcher := self compileRegex: source.	matcher isNil		ifTrue: [			(anArray at: 2) isNil				ifFalse: [ self signalFailure: 'Compilation failed, should have succeeded: ' , source printString ] ]		ifFalse: [			(anArray at: 2) isNil				ifTrue: [ self signalFailure: 'Compilation succeeded, should have failed: ' , source printString ]				ifFalse: [					2 to: anArray size by: 3 do: [ :index | 						self 							runMatcher: matcher							with: (anArray at: index)							expect: (anArray at: index + 1)							withSubexpressions: (anArray at: index + 2) ] ] ]! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:28'!
testCaseInsensitive	| matcher |	matcher := self matcherClass forString: 'the quick brown fox' ignoreCase: true.	self assert: (matcher search: 'the quick brown fox').	self assert: (matcher search: 'The quick brown FOX').	self assert: (matcher search: 'What do you know about the quick brown fox?').	self assert: (matcher search: 'What do you know about THE QUICK BROWN FOX?')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:28'!
testCaseSensitive	| matcher |	matcher := self matcherClass forString: 'the quick brown fox' ignoreCase: false.	self assert: (matcher search: 'the quick brown fox').	self deny: (matcher search: 'The quick brown FOX').	self assert: (matcher search: 'What do you know about the quick brown fox?').	self deny: (matcher search: 'What do you know about THE QUICK BROWN FOX?')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:38'!
testCopyReplacingMatches	"See that the match context is preserved while copying stuff between matches:"		| matcher |	matcher := self matcherClass forString: '\<\d\D+'.	self assert: (matcher copy: '9aaa1bbb 8ccc' replacingMatchesWith: 'foo')		= 'foo1bbb foo'! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'hfm 4/2/2010 13:52'!
testCopyTranslatingMatches	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher copy: 'now is  the   time    ' translatingMatchesUsing: [ :each | each reversed ])		= 'won si  eht   emit    '! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/20/2012 20:02'!
testEmptyStringAtBeginningOfLine
	| matcher |
	matcher := self matcherClass forString: '^'.
	self
		assert: (matcher copy: 'foo1 bar1' , String crString , 'foo2 bar2' replacingMatchesWith: '*')
			= ('*foo1 bar1' , String crString , '*foo2 bar2')
		description: 'An empty string at the beginning of a line'! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/22/2012 12:34'!
testEmptyStringAtBeginningOfWord
	| matcher |
	matcher := self matcherClass forString: '\<'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= '*foo *bar'
		description: 'An empty string at the beginning of a word'! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/20/2012 20:02'!
testEmptyStringAtEndOfLine
	| matcher |
	matcher := self matcherClass forString: '$'.
	self
		assert: (matcher copy: 'foo1 bar1' , String crString , 'foo2 bar2' replacingMatchesWith: '*')
			= ('foo1 bar1*', String crString , 'foo2 bar2*')
		description: 'An empty string at the end of a line'! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/22/2012 12:34'!
testEmptyStringAtEndOfWord
	| matcher |
	matcher := self matcherClass forString: '\>'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= 'foo* bar*'
		description: 'An empty string at the end of a word'! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/22/2012 12:34'!
testEmptyStringAtWordBoundary
	| matcher |
	matcher := self matcherClass forString: '\b'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= '*foo* *bar*'
		description: 'An empty string at a word boundary'! !

!RxMatcherTest methodsFor: 'testing-empty' stamp: 'gsa 12/22/2012 12:35'!
testEmptyStringNotAtWordBoundary
	| matcher |
	matcher := self matcherClass forString: '\B'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= 'f*o*o b*a*r'
		description: 'An empty string not at a word boundary'! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry001	self runRegex: #('abc'		'abc' true (1 'abc')		'xbc' false nil		'axc' false nil		'abx' false nil		'xabcy' true (1 'abc')		'ababc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry002	self runRegex: #('ab*c'		'abc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry003	self runRegex: #('ab*bc'		'abc' true (1 'abc')		'abbc' true (1 'abbc')		'abbbbc' true (1 'abbbbc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry004	self runRegex: #('ab+bc'			'abbc' true (1 'abbc')		'abc' false nil		'abq' false nil		'abbbbc' true (1 'abbbbc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry005	self runRegex: #('ab?bc'		'abbc' true (1 'abbc')		'abc' true (1 'abc')		'abbbbc' false nil		'abc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry006	self runRegex: #('^abc$'		'abc' true (1 'abc')		'abcc' false nil		'aabc' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry007	self runRegex: #('^abc'		'abcc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry008	self runRegex: #('abc$'		'aabc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry009	self runRegex: #('^'		'abc' true nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry010	self runRegex: #('$'		'abc' true nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry011	self runRegex: #('a.c'		'abc' true (1 'abc')		'axc' true (1 'axc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry012	"Need to get creative to include the null character..."	self runRegex: #('a.*c'			'axyzc' true (1 'axyzc')		'axy zc' true (1 'axy zc') "testing that a dot matches a space"		), (Array with: 'axy', (String with: 0 asCharacter), 'zc'), #(false nil "testing that a dot does not match a null"		'axyzd' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry013	self runRegex: #('.a.*'		'1234abc' true (1 '4abc')		'abcd' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry014	self runRegex: #('a\w+c'		' abbbbc ' true (1 'abbbbc')		'abb bc' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry015	self runRegex: #('\w+'		'  	foobar	quux' true (1 'foobar')		' 	~!!@#$%^&*()-+=\|/?.>,<' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry016	self runRegex: #('a\W+c'		'a   c' true (1 'a   c')		'a bc' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry017	self runRegex: #('\W+'		'foo!!@#$bar' true (1 '!!@#$')		'foobar' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry018	self runRegex: #('a\s*c'		'a   c' true (1 'a   c')		'a bc' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry019	self runRegex: #('\s+'		'abc3457 sd' true (1 ' ')		'1234$^*^&asdfb' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry020	self runRegex: #('a\S*c'		'aqwertyc' true (1 'aqwertyc')		'ab c' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry021	self runRegex: #('\S+'		'     	asdf		' true (1 'asdf')		' 				' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry022	self runRegex: #('a\d+c'		'a0123456789c' true (1 'a0123456789c')		'a12b34c' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry023	self runRegex: #('\d+'		'foo@#$%123ASD #$$%^&' true (1 '123')		'foo!!@#$asdfl;' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry024	self runRegex: #('a\D+c'		'aqwertyc' true (1 'aqwertyc')		'aqw6ertc' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry025	self runRegex: #('\D+'		'1234 abc 456' true (1 ' abc ')		'1234567890' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry026	self runRegex: #('(f|o)+\b'		'foo' true (1 'foo')		' foo ' true (1 'foo'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry027	self runRegex: #('\ba\w+' "a word beginning with an A"		'land ancient' true (1 'ancient')		'antique vase' true (1 'antique')		'goofy foobar' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry028	self runRegex: #('(f|o)+\B'		'quuxfoobar' true (1 'foo')		'quuxfoo ' true (1 'fo'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry029	self runRegex: #('\Ba\w+' "a word with an A in the middle, match at A and further"		'land ancient' true (1 'and')		'antique vase' true (1 'ase')		'smalltalk shall overcome' true (1 'alltalk')		'foonix is better' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry030	self runRegex: #('fooa\>.*'		'fooa ' true nil		'fooa123' false nil		'fooa bar' true nil		'fooa' true nil		'fooargh' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry031	self runRegex: #('\>.+abc'		' abcde fg' false nil		'foo abcde' true (1 ' abc')		'abcde' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry032	self runRegex: #('\<foo.*'		'foo' true nil		'foobar' true nil		'qfoobarq foonix' true (1 'foonix')		' foo' true nil		' 12foo' false nil		'barfoo' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry033	self runRegex: #('.+\<foo'		'foo' false nil		'ab foo' true (1 'ab foo')		'abfoo' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry034	self runRegex: #('a[bc]d'		'abc' false nil		'abd' true (1 'abd'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry035	self runRegex: #('a[b-d]e'		'abd' false nil		'ace' true (1 'ace'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry036	self runRegex: #('a[b-d]'		'aac' true (1 'ac'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry037	self runRegex: #('a[-b]'		'a-' true (1 'a-'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry038	self runRegex: #('a[b-]'		'a-' true (1 'a-'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry039	self runRegex: #('a[a-b-c]' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry040	self runRegex: #('[k]'		'ab' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry041	self runRegex: #('a[b-a]' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry042	self runRegex: #('a[]b' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry043	self runRegex: #('a[' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry044	self runRegex: #('a]' 		'a]' true (1 'a]'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry045	self runRegex: #('a[]]b'		'a]b' true (1 'a]b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry046	self runRegex: #('a[^bc]d'		'aed' true (1 'aed')		'abd' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry047	self runRegex: #('a[^-b]c'		'adc' true (1 'adc')		'a-c' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry048	self runRegex: #('a[^]b]c'		'a]c' false nil		'adc' true (1 'adc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry049	self runRegex: #('[\de]+'		'01234' true (1 '01234')		'0123e456' true (1 '0123e456')		'0123e45g78' true (1 '0123e45'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry050	self runRegex: #('[e\d]+' "reversal of the above, should be the same"		'01234' true (1 '01234')		'0123e456' true (1 '0123e456')		'0123e45g78' true (1 '0123e45'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry051	self runRegex: #('[\D]+'		'123abc45def78' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry052	self runRegex: #('[[:digit:]e]+'		'01234' true (1 '01234')		'0123e456' true (1 '0123e456')		'0123e45g78' true (1 '0123e45'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry053	self runRegex: #('[\s]+'		'2  spaces' true (1 '  '))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry054	self runRegex: #('[\S]+'		'  word12!!@#$  ' true (1 'word12!!@#$'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry055	self runRegex: #('[\w]+'		' 	foo123bar	45' true (1 'foo123bar'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry056	self runRegex: #('[\W]+'		'fii234!!@#$34f' true (1 '!!@#$'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry057	self runRegex: #('[^[:alnum:]]+'		'fii234!!@#$34f' true (1 '!!@#$'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry058	self runRegex: #('[%&[:alnum:]]+'		'foo%3' true (1 'foo%3')		'foo34&rt4$57a' true (1 'foo34&rt4')		'!!@#$' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry059	self runRegex: #('[[:alpha:]]+'		' 123foo3 ' true (1 'foo')		'123foo' true (1 'foo')		'foo1b' true (1 'foo'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry060	self runRegex: #('[[:cntrl:]]+'		' a 1234asdf' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry061	self runRegex: #('[[:lower:]]+'		'UPPERlower1234' true (1 'lower')		'lowerUPPER' true (1 'lower'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry062	self runRegex: #('[[:upper:]]+'		'UPPERlower1234' true (1 'UPPER')		'lowerUPPER ' true (1 'UPPER'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry063	self runRegex: #('[[:space:]]+'		'2  spaces' true (1 '  '))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry064	self runRegex: #('[^[:space:]]+'		'  word12!!@#$  ' true (1 'word12!!@#$'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry065	self runRegex: #('[[:graph:]]+'		'abc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry066	self runRegex: #('[[:print:]]+'		'abc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry067	self runRegex: #('[^[:punct:]]+'		'!!hello,world!!' true (1 'hello'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry068	self runRegex: #('[[:xdigit:]]+'		'  x10FCD  ' true (1 '10FCD')		' hgfedcba0123456789ABCDEFGH '			true (1 'fedcba0123456789ABCDEF'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry069	self runRegex: #('ab|cd'		'abc' true (1 'ab')		'abcd' true (1 'ab'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry070	self runRegex: #('()ef'		'def' true (1 'ef' 2 ''))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry071	self runRegex: #('()*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry072	self runRegex: #('*a' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry073	self runRegex: #('^*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry074	self runRegex: #('$*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry075	self runRegex: #('(*)b' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry076	self runRegex: #('$b'	'b' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry077	self runRegex: #('a\' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry078	self runRegex: #('a\(b'		'a(b' true (1 'a(b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry079	self runRegex: #('a\(*b'		'ab' true (1 'ab')		'a((b' true (1 'a((b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry080	self runRegex: #('a\\b'		'a\b' true (1 'a\b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry081	self runRegex: #('abc)' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry082	self runRegex: #('(abc' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry083	self runRegex: #('((a))'		'abc' true (1 'a' 2 'a' 3 'a'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry084	self runRegex: #('(a)b(c)'		'abc' true (1 'abc' 2 'a' 3 'c'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry085	self runRegex: #('a+b+c'		'aabbabc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry086	self runRegex: #('a**' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry087	self runRegex: #('a*?' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry088	self runRegex: #('(a*)*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry089	self runRegex: #('(a*)+' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry090	self runRegex: #('(a|)*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry091	self runRegex: #('(a*|b)*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry092	self runRegex: #('(a+|b)*'		'ab' true (1 'ab' 2 'b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry093	self runRegex: #('(a+|b)+'		'ab' true (1 'ab' 2 'b'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry094	self runRegex: #('(a+|b)?'		'ab' true (1 'a' 2 'a'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry095	self runRegex: #('[^ab]*'		'cde' true (1 'cde'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry096	self runRegex: #('(^)*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry097	self runRegex: #('(ab|)*' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry098	self runRegex: #(')(' nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry099	self runRegex: #('' 'abc' true (1 ''))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry100	self runRegex: #('abc' '' false nil)! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry101	self runRegex: #('a*'		'' true '')! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry102	self runRegex: #('abcd'		'abcd' true (1 'abcd'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry103	self runRegex: #('a(bc)d'		'abcd' true (1 'abcd' 2 'bc'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry104	self runRegex: #('([abc])*d'		'abbbcd' true (1 'abbbcd' 2 'c'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry105	self runRegex: #('([abc])*bcd'		'abcd' true (1 'abcd' 2 'a'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry106	self runRegex: #('a|b|c|d|e' 'e' true (1 'e'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry107	self runRegex: #('(a|b|c|d|e)f'		'ef' true (1 'ef' 2 'e'))	"	((a*|b))*	-	c	-	-"! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry108	self runRegex: #('abcd*efg' 		'abcdefg' true (1 'abcdefg'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry109	self runRegex: #('ab*' 		'xabyabbbz' true (1 'ab')		'xayabbbz' true (1 'a'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry110	self runRegex: #('(ab|cd)e' 'abcde' true (1 'cde' 2 'cd'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry111	self runRegex: #('[abhgefdc]ij' 'hij' true (1 'hij'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry112	self runRegex: #('^(ab|cd)e' 'abcde' false nil)	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry113	self runRegex: #('(abc|)def' 'abcdef' true nil)	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry114	self runRegex: #('(a|b)c*d' 'abcd' true (1 'bcd' 2 'b'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry115	self runRegex: #('(ab|ab*)bc' 'abc' true (1 'abc' 2 'a'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry116	self runRegex: #('a([bc]*)c*' 'abc' true (1 'abc' 2 'bc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry117	self runRegex: #('a([bc]*)(c*d)' 'abcd' true (1 'abcd' 2 'bc' 3 'd'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry118	self runRegex: #('a([bc]+)(c*d)' 'abcd' true (1 'abcd' 2 'bc' 3 'd'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry119	self runRegex: #('a([bc]*)(c+d)' 'abcd' true (1 'abcd' 2 'b' 3 'cd'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry120	self runRegex: #('a[bcd]*dcdcde' 'adcdcde' true (1 'adcdcde'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry121	self runRegex: #('a[bcd]+dcdcde' 'adcdcde' false nil)	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry122	self runRegex: #('(ab|a)b*c' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry123	self runRegex: #('((a)(b)c)(d)' 'abcd' true (1 'abcd' 3 'a' 4 'b' 5 'd'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry124	self runRegex: #('[ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry125	self runRegex: #('[ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry126	self runRegex: #('[ -~ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry127	self runRegex: #('[ -~ -~ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry128	self runRegex: #('[ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry129	self runRegex: #('[ -~ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry130	self runRegex: #('[ -~ -~ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry131	self runRegex: #('[a-zA-Z_][a-zA-Z0-9_]*' 'alpha' true (1 'alpha'))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry132	self runRegex: #('^a(bc+|b[eh])g|.h$' 'abh' true (1 'bh' 2 nil))	! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry133	self runRegex: #('(bc+d$|ef*g.|h?i(j|k))' 		'effgz' true (1 'effgz' 2 'effgz' 3 nil)		'ij' true (1 'ij' 2 'ij' 3 'j')		'effg' false nil		'bcdd' false nil		'reffgz' true (1 'effgz' 2 'effgz' 3 nil))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry134	self runRegex: #('(((((((((a)))))))))' 'a' true (1 'a'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry135	self runRegex: #('multiple words of text' 		'uh-uh' false nil		'multiple words of text, yeah' true (1 'multiple words of text'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry136	self runRegex: #('(.*)c(.*)' 'abcde' true (1 'abcde' 2 'ab' 3 'de'))! !

!RxMatcherTest methodsFor: 'testing-henry' stamp: 'lr 1/15/2010 19:46'!
testHenry137	self runRegex: #('\((.*), (.*)\)' '(a, b)' true (2 'a' 3 'b'))! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:45'!
testMatches	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher matches: 'now').	self deny: (matcher matches: 'now is')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:38'!
testMatchesIn	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher matchesIn: 'now is the time') asArray 		= #('now' 'is' 'the' 'time')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'hfm 4/2/2010 13:52'!
testMatchesInCollect	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher		matchesIn: 'now is the time'		collect: [ :each | each reversed ]) asArray			= #('won' 'si' 'eht' 'emit')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:44'!
testMatchesInDo	| matcher expected |	matcher := self matcherClass forString: '\w+'.	expected := #('now' 'is' 'the' 'time') asOrderedCollection.	matcher 		matchesIn: 'now is the time'		do: [ :each | self assert: each = expected removeFirst ].	self assert: expected isEmpty! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:39'!
testMatchesOnStream	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher matchesOnStream: 'now is the time' readStream) asArray 		= #('now' 'is' 'the' 'time')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'hfm 4/2/2010 13:52'!
testMatchesOnStreamCollect	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher 		matchesOnStream: 'now is the time' readStream 		collect: [ :each | each reversed ]) asArray			= #('won' 'si' 'eht' 'emit')! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:44'!
testMatchesOnStreamDo	| matcher expected |	matcher := self matcherClass forString: '\w+'.	expected := #('now' 'is' 'the' 'time') asOrderedCollection.	matcher 		matchesOnStream: 'now is the time' readStream		do: [ :each | self assert: each = expected removeFirst ].	self assert: expected isEmpty! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:45'!
testMatchesStream	| matcher |	matcher := self matcherClass forString: '\w+'.	self assert: (matcher matchesStream: 'now' readStream).	self deny: (matcher matchesStream: 'now is' readStream)! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'TestRunner 1/15/2010 20:51'!
testMatchingRangesIn	| matcher expected |	matcher := self matcherClass forString: '\w+'.	expected := #(1 3 5 6 8 10 12 15) asOrderedCollection.	(matcher matchingRangesIn: 'now is the time') do: [ :range |		self assert: range first = expected removeFirst.		self assert: range last = expected removeFirst ].	self assert: expected isEmpty! !

!RxMatcherTest methodsFor: 'testing' stamp: 'lr 1/15/2010 19:47'!
testRegex001	self runRegex: #('^.*$' 		'' true (1 '')		'a' true (1 'a')		'abc' true (1 'abc'))! !

!RxMatcherTest methodsFor: 'testing' stamp: 'lr 1/15/2010 19:47'!
testRegex002	self runRegex: #('a\w+c'		' abb_bbc ' true (1 'abb_bbc')		'abb-bc' false nil)! !

!RxMatcherTest methodsFor: 'testing' stamp: 'lr 1/15/2010 19:47'!
testRegex003	self runRegex: #('a\W+c'		' abb_bbc ' false nil		'abb-bc' false nil		'a.,:;-&!!"#%/()={[]}+?\~*''c' true (1 'a.,:;-&!!"#%/()={[]}+?\~*''c'))! !

!RxMatcherTest methodsFor: 'testing' stamp: 'lr 4/23/2010 09:01'!
testRegex004	self runRegex: #(':isVowel:'		'aei' true nil		'xyz' false nil)! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'gsa 12/22/2012 12:26'!
testStringAllRangesOfRegexMatches
	| result |
	result := 'aabbcc' allRangesOfRegexMatches: 'b+'.
	self assert: result size = 1.
	self assert: result first first = 3.
	self assert: result first last = 4
	! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:50'!
testStringAllRegexMatches	| result |	result := 'aabbcc' allRegexMatches: 'b+'.	self assert: result size = 1.	self assert: result first = 'bb'! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:51'!
testStringAsRegex	self assert: 'b+' asRegex class = RxParser preferredMatcherClass! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:52'!
testStringAsRegexIgnoringCase	self assert: 'b+' asRegexIgnoringCase class = RxParser preferredMatcherClass! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:53'!
testStringCopyWithRegexMatchesReplacedWith	self assert: ('aabbcc' copyWithRegex: 'b+' matchesReplacedWith: 'X') = 'aaXcc'! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:53'!
testStringCopyWithRegexMatchesTranslatedUsing	self assert: ('aabbcc' 		copyWithRegex: 'b+' 		matchesTranslatedUsing: [ :each | 			self assert: each = 'bb'.			'X' ]) = 'aaXcc'! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:55'!
testStringMatchesRegex	self deny: ('aabbcc' matchesRegex: 'a+').	self deny: ('aabbcc' matchesRegex: 'b+c+').	self assert: ('aabbcc' matchesRegex: 'a+b+c+')! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:55'!
testStringMatchesRegexIgnoringCase	self deny: ('AABBCC' matchesRegexIgnoringCase: 'a+').	self deny: ('AABBCC' matchesRegexIgnoringCase: 'b+c+').	self assert: ('AABBCC' matchesRegexIgnoringCase: 'a+b+c+')! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:56'!
testStringPrefixMatchesRegex	self assert: ('aabbcc' prefixMatchesRegex: 'a+').	self deny: ('aabbcc' prefixMatchesRegex: 'b+')! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:56'!
testStringPrefixMatchesRegexIgnoringCase	self assert: ('AABBCC' prefixMatchesRegexIgnoringCase: 'a+').	self deny: ('AABBCC' prefixMatchesRegexIgnoringCase: 'b+')! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:57'!
testStringRegexMatchesCollect	| result |	result := 'aabbcc' regex: 'b+' matchesCollect: [ :each | each asUppercase ].	self assert: result size = 1.	self assert: result first = 'BB'! !

!RxMatcherTest methodsFor: 'testing-extensions' stamp: 'lr 4/23/2010 08:58'!
testStringRegexMatchesDo	| result |	result := OrderedCollection new.	'aabbcc' regex: 'b+' matchesDo: [ :each | result add: each ].	self assert: result size = 1.	self assert: result first = 'bb'! !

!RxMatcherTest methodsFor: 'testing-protocol' stamp: 'lr 1/15/2010 20:55'!
testSubexpressionCount	| matcher |	#(('a' 1) ('a(b)' 2) ('a(b(c))' 3) ('(a)(b)' 3) ('(a(b))*' 3)) do: [ :pair |		matcher := self matcherClass forString: pair first.		matcher supportsSubexpressions 			ifTrue: [ self assert: matcher subexpressionCount = pair last ] ]! !

!RxMatcherTest class methodsFor: 'accessing' stamp: 'lr 1/15/2010 19:48'!
packageNamesUnderTest	^ #('VB-Regex')! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/5/2006 00:11'!
DoesNotWorktestBackQuotesEscape	"self debug: #testBackQuotesEscape"		"Regular expressions can also include the following backquote escapesto refer to popular classes of characters:	\w	any word constituent character (same as [a-zA-Z0-9:=])	\W	any character but a word constituent	\d	a digit (same as [0-9])	\D	anything but a digit	\s 	a whitespace character	\S	anything but a whitespace characterThese escapes are also allowed in character classes: '[\w+-]' means'any character that is either a word constituent, or a plus, or aminus'."		self assert: ('one word' matchesRegex: '\w').				self assert: ('one' matchesRegex: '\w').		  		! !

!RxParserTest methodsFor: 'tests' stamp: 'StephaneDucasse 12/11/2010 15:00'!
test	"self debug: #test"		self assert: ('\<t\w+' asRegexIgnoringCase		copy: 'now is the Time'		translatingMatchesUsing: [:match | match asUppercase]) = 'now is THE TIME'.	"the regular expression matches words beginning with either an uppercase or a lowercase T"! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 16:24'!
testCadrMatching	"self debug: #testCadrMatching"	"A bit more complex example is the following expression, matching thename of any of the Lisp-style `car', `cdr', `caar', `cadr',... functions:"	self assert: ( 'car' matchesRegex: 'c(a|d)+r').	self assert: ( 'cdr' matchesRegex: 'c(a|d)+r').	self assert: ( 'caar' matchesRegex: 'c(a|d)+r').	self assert: ( 'cadr' matchesRegex: 'c(a|d)+r').	self assert: ( 'caddar' matchesRegex: 'c(a|d)+r').! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 16:49'!
testCharacterSet	"self debug: #testCharacterSet"		"So far, we have used only characters as the 'smallest' components ofregular expressions. There are other, more `interesting', components.A character set is a string of characters enclosed in squarebrackets. It matches any single character if it appears between thebrackets. For example, `[01]' matches either `0' or `1':"	self assert: ('0' matchesRegex: '[01]').	 		self deny: ('3' matchesRegex: '[01]'). 	 	self deny: ('11' matchesRegex: '[01]').	"-- false: a set matches only one character"	self deny: ('01' matchesRegex: '[01]').! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 17:39'!
testCharacterSetBinaryNumber	"self debug: #testCharacterSetBinaryNumber"		"Using plus operator, we can build the following binary numberrecognizer:"	self assert: ('10010100' matchesRegex: '[01]+').	 		self deny: ('10001210' matchesRegex: '[01]+')	 ! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/5/2006 00:01'!
testCharacterSetInversion	"self debug: #testCharacterSetInversion"		"If the first character after the opening bracket is `^', the set isinverted: it matches any single character *not* appearing between thebrackets:"		self deny: ('0' matchesRegex: '[^01]').		  		"0 appears in 01 so there is no match"		self assert: ('3' matchesRegex: '[^01]').	"3 is not in 01 so it matches"			self deny: ('30' matchesRegex: '[^01]').			self deny: ('33333333333333333333333330' matchesRegex: '[^01]').		"there is one zero so it does not match"! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 23:20'!
testCharacterSetRange	"self debug: #testCharacterSetRange"		"For convenience, a set may include ranges: pairs of charactersseparated with `-'. This is equivalent to listing all charactersbetween them: `[0-9]' is the same as `[0123456789]'. "		self assert: ('0' matchesRegex: '[0-9]').		self assert: ('9' matchesRegex: '[0-9]').		self deny: ('a' matchesRegex: '[0-9]').	self deny: ('01' matchesRegex: '[0-9]').		self assert: ('01442128629839374565' matchesRegex: '[0-9]+').		! !

!RxParserTest methodsFor: 'tests' stamp: 'marcus.denker 10/22/2008 10:47'!
testMatchesInwW	"self debug: #testMatchesInwW"		"1. Backslash escapes similar to those in Perl are allowed in patterns:	\w	any word constituent character (equivalent to [a-zA-Z0-9:=])	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9:=]"	self assert: ('\w+' asRegex matchesIn: 'now is the time') asArray = #('now' 'is' 'the' 'time').	self assert: ('\W+' asRegex matchesIn: 'now is the time') asArray = #(' ' ' ' ' ').		"why do we get that"	self assert: ('\w' asRegex matchesIn: 'now') asArray = #('n' 'o' 'w').! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 16:38'!
testOrOperator	"self debug: #testOrOperator"		"The last operator is `|' meaning `or'. It is placed between tworegular expressions, and the resulting expression matches if one ofthe expressions matches. It has the lowest possible precedence (lowerthan sequencing). For example, `ab*|ba*' means `a followed by anynumber of b's, or b followed by any number of a's':"	self assert: ('abb' matchesRegex: 'ab*|ba*').  		self assert: ('baa' matchesRegex: 'ab*|ba*').	 		self deny: ('baab' matchesRegex: 'ab*|ba*').		"It is possible to write an expression matching an empty string, forexample: `a|'.  However, it is an error to apply `*', `+', or `?' tosuch expression: `(a|)*' is an invalid expression."	self should: ['(a|)*' asRegex] raise: Error.! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 16:17'!
testQuotingOperators	"self debug: #testQuotingOperators"		"As we have seen, characters `*', `+', `?', `(', and `)' have specialmeaning in regular expressions. If one of them is to be usedliterally, it should be quoted: preceded with a backslash. (Thus,backslash is also special character, and needs to be quoted for aliteral match--as well as any other special character describedfurther)."	self deny: ('ab*' matchesRegex: 'ab*'). "	-- false: star in the right string is special"	self assert: ('ab*' matchesRegex: 'ab\*').	 			self assert: ('a\c' matchesRegex: 'a\\c').		 	! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/3/2006 19:50'!
testSimpleMatchesRegex	"self debug: #testSimpleMatchesRegex"		"The simplest regular expression is a single character.  It matchesexactly that character. A sequence of characters matches a string withexactly the same sequence of characters:"	self assert: ('a' matchesRegex: 'a').	self assert: ('foobar' matchesRegex: 'foobar')	.	self deny: ('blorple' matchesRegex: 'foobar')! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/3/2006 21:41'!
testSimpleMatchesRegexWithStar	"self debug: #testSimpleMatchesRegexWithStar"		"The above paragraph in testSimpleMatchesRegex introduced a primitive regular expression (acharacter), and an operator (sequencing). Operators are applied toregular expressions to produce more complex regular expressions.Sequencing (placing expressions one after another) as an operator is,in a certain sense, `invisible'--yet it is arguably the most common.A more `visible' operator is Kleene closure, more often simplyreferred to as `a star'.  A regular expression followed by an asteriskmatches any number (including 0) of matches of the originalexpression. For example:"	self assert: ('ab' matchesRegex: 'a*b').		 			self assert: ('aaaaab' matchesRegex: 'a*b').		self assert: ('b' matchesRegex: 'a*b').	 		self deny: ('aac' matchesRegex: 'a*b').	 		! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 23:38'!
testSpecialCharacterInSetRange	"self debug: #testSpecialCharacterInSetRange"		"Special characters within a set are `^', `-', and `]' that closes theset. Below are the examples of how to literally use them in a set:	[01^]		-- put the caret anywhere except the beginning	[01-]		-- put the dash as the last character	[]01]		-- put the closing bracket as the first character 	[^]01]			(thus, empty and universal sets cannot be specified)"	self assert: ('0' matchesRegex: '[01^]').	self assert: ('1' matchesRegex: '[01^]').	self assert: ('^' matchesRegex: '[01^]').		self deny: ('0' matchesRegex: '[^01]').	self deny: ('1' matchesRegex: '[^01]').		"[^abc] means that everything except abc is matche"	self assert: ('^' matchesRegex: '[^01]').	! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 16:26'!
testStarPlusQuestionMark	"self debug: #testStarPlusQuestionMark"		"Two other operators similar to `*' are `+' and `?'. `+' (positiveclosure, or simply `plus') matches one or more occurrences of theoriginal expression. `?' (`optional') matches zero or one, but nevermore, occurrences."	self assert: ('ac' matchesRegex: 'ab*c').  			self deny: ('ac' matchesRegex: 'ab+c'). 		"-- false: need at least one b"	self assert: ('abbc' matchesRegex: 'ab+c').		self assert: ('abbbbbbc' matchesRegex: 'ab+c').		self deny: ('abbc' matchesRegex: 'ab?c')	 	"-- false: too many b's"! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/3/2006 22:57'!
testStarPrecedence	"self debug: #testStarPrecedence"		"A star's precedence is higher than that of sequencing. A star appliesto the shortest possible subexpression that precedes it. For example,'ab*' means `a followed by zero or more occurrences of b', not `zeroor more occurrences of ab':"	self assert: ('abbb' matchesRegex: 'ab*'). 	self deny: ('abab' matchesRegex: 'ab*').	 				"To actually make a regex matching `zero or more occurrences of ab',`ab' is enclosed in parentheses:"	self assert: ('abab' matchesRegex: '(ab)*'). 	self deny: ('abcab' matchesRegex: '(ab)*')! !

!RxParserTest methodsFor: 'tests' stamp: 'StephaneDucasse 12/11/2010 15:00'!
testTranslatingMatchesUsing	"self debug: #testTranslatingMatchesUsing"		self assert: ('\<t\w+' asRegexIgnoringCase		copy: 'now is the Time'		translatingMatchesUsing: [:match | match asUppercase]) = 'now is THE TIME'.	"the regular expression matches words beginning with either an uppercase or a lowercase T"! !

!RxParserTest methodsFor: 'tests' stamp: 'sd 9/4/2006 23:29'!
toDotestSpecialCharacterInSetRange	"self debug: #testSpecialCharacterInSetRange"		"Special characters within a set are `^', `-', and `]' that closes theset. Below are the examples of how to literally use them in a set:	[01^]		-- put the caret anywhere except the beginning	[01-]		-- put the dash as the last character	[]01]		-- put the closing bracket as the first character 	[^]01]			(thus, empty and universal sets cannot be specified)"	self assert: ('0' matchesRegex: '[01^]').		self assert: ('0' matchesRegex: '[0-9]').		! !
