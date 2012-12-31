| identifier number term prod prim start parser |

identifier := #letter asParser , #word asParser star.

identifier parse: 'yeah' .  "#($y #($e $a $h))"
identifier parse: 'f12'.  "#($f #($1 $2))"
identifier parse: '123' .  "letter expected at 0"

identifier
	parse: '123'
	onError: [ :msg :pos | self error: msg ].

identifier matches: 'foo'.  "true"
identifier matches: '123'.  "false"

identifier matchesIn: 'foo 123 bar12' .

identifier matchingRangesIn: 'foo 123 bar12' .

$a asParser .
'abc' asParser .
#any asParser .
#digit asParser .
#letter asParser .

identifier := #letter asParser , ( #letter asParser / #digit asParser ) star .

identifier := ( #letter asParser , ( #letter asParser / #digit asParser ) star ) flatten .

number := #digit asParser plus token trim ==> [ :token | token value asNumber ] .
term := PPUnresolvedParser new.
prod := PPUnresolvedParser new.
prim := PPUnresolvedParser new.

term def: (prod , $+ asParser trim , term ==> [ :nodes | nodes first + nodes last ]) / prod .
prod def: (prim , $* asParser trim , prod ==> [ :nodes | nodes first * nodes last ]) / prim .
prim def: ($( asParser trim , term , $) asParser trim ==> [ :nodes | nodes second ]) / number .

start := term end.

start parse: '1 + 2 * 3' .  "7"
start parse: '(1 + 2) * 3' .  "9"


parser := ExpressionGrammar new.
parser parse: '1 + 2 * 3' . 
parser parse: '(1 + 2) * 3' .

parser := ExpressionEvaluator new.
parser parse: '1 + 2 * 3' . 
parser parse: '(1 + 2) * 3' . 

parser := ExpressionPrinter new.
parser parse: '1+2 * 3' . 
parser parse: '(1+ 2 )* 3' . 
