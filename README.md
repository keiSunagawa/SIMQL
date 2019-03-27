[wip]

## SIMQL
simql language can transpile for SQL.

### Features
- apply query snippet by function call.
- can quickly writed query on short and simple syntax keyword.
- can pre define functions, by local file.

### MVP
```
// you will be able to use "define block" predefined on local file.
define {
  defun like(col: Symbol keyword: String) => Cond {
    let kl = "%" + keyword + "%"
    q{ $`? LIKE(?)`($col, $kl) }
  }

  defun c(col: Symbol) => Symbol {
    q{ $`COUNT(?)`($col) }
  }

  defun isActive() => Cond {
    q{ status == "enable" && ($1.title != null || $1.content != null) }
  }
}

users << tasks ?> id == $1.assign_user_id :> $c(id) ?> $isActive() && $like(name, "K")

↓↓↓↓ transepile to ↓↓↓↓

SELECT COUNT(id)
FROM users
LET JOIN tasks ON users.id = tasks.assign_user_id
WHERE (
  users.status = "enable"
    AND (tasks.title IS NOT NULL OR tasks.content IS NOT NULL)
  ) AND name LIKE("%K%")
```

### syntax
```
string ::= // string
number ::= // decimal number
null ::= "null"
symbol ::= [a-zA-Z][a-zA-Z0-9_]*
accessor ::= "$"[0-9]
raw = "\$`.*`" [ "(" { term } ")" ]
macroArg ::= cond | symbolWithAccessor | string | number
macroApply ::= "\$[a-zA-Z][a-zA-Z0-9_]*"(" [macroArg] {"," macroArg} ")"

symbolWithAccessor ::= [accessor"."]symbol
tableSymbol ::= macroApply | raw | symbol
highSymbol ::= macroApply | raw | symbolWithAccessor
term ::= null | highSymbol | string | number | rbracket
rbracket ::= "(" cond ")"

op0 ::= ">" | "<" | ">=" | "<=" | "==" | "!="
op1 ::= "&&" | "||"

cond0 ::= term {op0 term}
cond ::= cond0 {op1 cond0}

joinType ::= "<<" | "><"
join ::= joinType TableSymbokl "?>" expr

orderType ::= "/>" | "\>"

from ::= TableSymbol {join}
select ::= ":>" highSymbol {highSymbol}
where ::= "?>" expr
limitOffset ::= "@" number [- number] // TODO ignore float number
order ::= orderType highSymbol {highSymbol}

simql ::= from [select] [where] [limitOffset] [order]

groupBy ::= // TODO maybe omit

// define
macroParamType ::= "String" | "Number" | "Symbol" | "Cond"
macroParam ::= symbol ":" macroParamType
macroReturnType ::= "Cond" | "Symbol" | "Term"
quasiquote ::= "q{"*"}"
macroStatement ::= quasiquote // or TODO
macroFuncBody ::= {macroStatement}
macroFunc = "defun" symbol (" { macroParam } ")" "=>" macroReturnType "=" "{" macroFuncBody "}"

definition ::= macroFunc // ひとまずmacroFuncのみ
definitionBlock ::= "define" "{" {definition} "}"
```
