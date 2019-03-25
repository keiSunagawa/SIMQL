## RUN Application
```
# compile scala code
$ cd siphonophorae/scripts
$ ./scalajs-compile.sh

# build electron view
$ cd siphonophorae/electron-view
$ yarn
$ yarn webpack
$ yarn start
```

## SIMQL syntax
```
string ::= // string
number ::= // decimal number
null ::= "null"
symbol ::= [a-zA-Z][a-zA-Z0-9_]*
accessor ::= "$"[0-9]
raw = "\$`.*`" [ "(" { term } ")" ]
macroArg ::= expr | symbolWithAccessor | string | number
macroApply ::= "\$[a-zA-Z][a-zA-Z0-9_]*"(" [macroArg] {"," macroArg} ")"

symbolWithAccessor ::= [accessor"."]symbol
tableSymbol ::= macroApply | raw | symbol
highSymbol ::= macroApply | raw | symbolWithAccessor
term ::= (null | highSymbol | string | number)

binaryOp ::= (">" | "<" | ">=" | "<=" | "==" | "!=")
binaryCond ::= highSymbol binaryOp term
cond ::= binaryCond | macroApply | cond

logicalOp ::= "&&" | "||"

expr ::= cond {logicalOp cond}

joinType ::= "<<" | "><"
join ::= joinType TableSymbokl "?" expr

orderType ::= "/>" | "\>"

from ::= TableSymbol {join}
select ::= ":" highSymbol {highSymbol}
where ::= "?" expr
limitOffset ::= "@" number [- number] // TODO ignore float number
order ::= orderType highSymbol {highSymbol}

simql ::= from [select] [where] [limitOffset] [order]

order ::= // TODO
groupBy ::= // TODO maybe omit

// define
macroParamType ::= "String" | "Number" | "Symbol" | "Expr"
macroParam ::= symbol ":" macroParamType
macroReturnType ::= "Cond" | "Symbol"
quasiquote ::= "q{"*"}"
macroStatement ::= quasiquote // or TODO
macroFuncBody ::= {macroStatement}
macroFunc = "defun" symbol (" { macroParam } ")" "=>" macroReturnType "=" "{" macroFuncBody "}"

definition ::= macroFunc // ひとまずmacroFuncのみ
definitionBlock ::= define "{" {definition} "}"
```

## SIMQL macro function example
```
defun safeLowerLimit(col: Symbol, min: Number) => Cond = {
  // TODO can use expressions
  q{ $col >= min && $col != null }
}
```
