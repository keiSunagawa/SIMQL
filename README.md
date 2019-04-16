[wip]

## SIMQL
simql language can transpile for SQL.

[playground](https://keisunagawa.github.io/simql/)

### Features
- apply query snippet by function call.
- can quickly writed query on short and simple syntax keyword.
- can pre define functions, from local file.

### MVP
```
// you will be able to use "define block" predefined on local file.
define {
  defun c(col: Expr) => Raw {
    q{ $`COUNT(?)`($col) }
  }
  defun like(col: Symbol, keyword: String) => Expr {
    let lk = $cst($keyword, "%")
    let lk = $cst("%", $lk)
    q{ $`? LIKE(?)`($col, $lk) }
  }
  defun isActive(table_index: Number) => Expr {
    q{ status == "enable" && ($table_access(title, $table_index) != null || $table_access(content, $table_index) != null) }
  }
}

users << tasks ?> id == $1.assign_user_id :> $c(id) ?> $isActive(1) && $like(name, "K")

↓↓↓↓ transepile to ↓↓↓↓

SELECT (COUNT(id))
FROM users LEFT JOIN tasks ON (id) = (tasks.assign_user_id)
WHERE (((status) = ('enable')) AND ((((tasks.title IS NOT NULL)) OR ((tasks.content IS NOT NULL))))) AND ((name LIKE('%K%')))
```

### syntax
```
string ::= // any character sequence in between double quote.
number ::= // decimal number.
boolean ::= "true" | "false"
symbol ::= /[a-zA-Z][a-zA-Z0-9_]*/
null ::= "null"
raw = "$`" /.*/ "`" [ "(" term { "," term } ")" ]
column ::= [ "$" /[0-9]/ "." ] symbol

rbracket ::= "(" expr ")"
call ::= /\$[a-zA-Z][a-zA-Z0-9_]*/ [ "(" expr { "," expr } ")" ]

term ::= null | boolean | string | number | column | rbracket | raw | call

op0 ::= ">" | "<" | ">=" | "<=" | "==" | "!="
op1 ::= "&&" | "||"

expr0 ::= term {op0 term}
expr ::= expr0 {op1 expr0}


symbolWithAccessor ::= [accessor"."]symbol
tableSymbol ::= macroApply | raw | symbol
highSymbol ::= macroApply | raw | symbolWithAccessor

joinType ::= "<<" | "><"
join ::= joinType term "?>" expr

from ::= term { join }
select ::= ":>" term { term }
where ::= "?>" expr
limitOffset ::= "@" number [- number] // TODO ignore float number

orderType ::= "/>" | "\>"
order ::= orderType term { term }

simql ::= from [ select ] [ where ] [ limitOffset ] [ order ]

groupBy ::= // TODO maybe omit

// fimql syntax
atomicType ::= "String" | "Number" | "Boolean" | "Symbol" | "Raw" | "Expr"
generics ::= \[A-Z]+\
listType ::= "List" "<" simqlType ">"
singleType ::= atomicType | listType | "(" functionType ")"
functionType ::= singleType "=>" singleType { "=>" singleType }
simqlType ::= functionType | singleType | generics

functionParam ::= symbol ":" simqlType

nil ::= "nil" "<" simqLType ">"
dterm ::= string | nil | number | boolean | symbol | call
dexpr ::= queryBlock | dterm | function

queryBlock ::= "q{" expr "}"
function ::= [ "<" generics { "," generics } ">" ] "(" functionParam { "," functionParam } ")" "=>" simQLType "{" { bind } dexpr "}"

bind ::= "let" symbol "=" dexpr


defun = "defun" symbol function

definition ::= defun
definitionBlock ::= "define" "{" { definition } "}"
```
