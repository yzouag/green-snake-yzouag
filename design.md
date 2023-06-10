## concrete syntax

new features beyond Diamondback is marked as (!new)
```
<prog> := <defn>* <expr>               
<defn> := (fun (<name> <name>*) <expr>)
<expr> :=
  | <number>
  | true
  | false
  | input
  | nil                               (!new)
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (loop <expr>)
  | (break <expr>)
  | (<name> <expr>*)
  | (vec <expr>*)                     (!new)
  | (make-vec <expr> <expr>)          (!new)
  | (vec-get <expr> <expr>)           (!new)
  | (vec-set! <expr> <expr> <expr>)   (!new)
  | (vec-len <expr>)                  (!new)
  | (equals! <expr> <expr>)           (!new)
  | (gc)                              (!new)
<op1> := add1 | sub1 | isnum | isbool | isvec (!new) | print
<op2> := + | - | * | / (!new) | < | > | >= | <= | =

<binding> := (<identifier> <expr>)
```
