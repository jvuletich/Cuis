'From Cuis 4.0 of 21 April 2012 [latest update: #1292] on 1 June 2012 at 11:41:51 pm'!
'Description Please enter a description for this package '!
!classDefinition: #ExpressionGrammar category: #PetitTutorial!
PPCompositeParser subclass: #ExpressionGrammar
	instanceVariableNames: 'add prod term mul prim parens number'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTutorial'!
!classDefinition: 'ExpressionGrammar class' category: #PetitTutorial!
ExpressionGrammar class
	instanceVariableNames: ''!

!classDefinition: #ExpressionEvaluator category: #PetitTutorial!
ExpressionGrammar subclass: #ExpressionEvaluator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTutorial'!
!classDefinition: 'ExpressionEvaluator class' category: #PetitTutorial!
ExpressionEvaluator class
	instanceVariableNames: ''!

!classDefinition: #ExpressionPrinter category: #PetitTutorial!
ExpressionGrammar subclass: #ExpressionPrinter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PetitTutorial'!
!classDefinition: 'ExpressionPrinter class' category: #PetitTutorial!
ExpressionPrinter class
	instanceVariableNames: ''!


!ExpressionEvaluator methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:33'!
add

	^ super add ==> [ :nodes | nodes first + nodes last ]! !

!ExpressionEvaluator methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:33'!
mul

	^ super mul ==> [ :nodes | nodes first * nodes last ]! !

!ExpressionEvaluator methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:34'!
parens

	^ super parens ==> [ :nodes | nodes second ]! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:27'!
add

	^ prod , $+ asParser trim , term! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:28'!
mul

	^ prim , $* asParser trim , prod! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:22'!
number
	^ #digit asParser plus token trim ==> [ :token | token value asNumber ] ! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:29'!
parens

	^ $( asParser trim , term , $) asParser trim! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:28'!
prim

	^ parens / number! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:27'!
prod

	^ mul / prim! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:29'!
start

	^ term end! !

!ExpressionGrammar methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:26'!
term

	^ add / prod! !

!ExpressionPrinter methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:38'!
add

	^ super add ==> [ :nodes | nodes first asString , ' + ' , nodes last asString ]! !

!ExpressionPrinter methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:38'!
mul

	^ super mul ==> [ :nodes | nodes first asString , ' * ' , nodes last asString ]! !

!ExpressionPrinter methodsFor: 'as yet unclassified' stamp: 'pmon 6/1/2012 23:39'!
parens

	^ super parens ==> [ :nodes | '(' , nodes second asString , ')' ]! !
