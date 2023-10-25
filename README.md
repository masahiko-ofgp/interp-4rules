# interp-4rules

## My Environment

- Manjaro Linux i3
- SBCL
- Quicklisp


## Usage

example-prog is:
```
x = 2 + 2 * 3
y = x / 4
print y
```

```
* (ql:quickload :interp-4rules)
* (in-package :interp-4rules)
* (interp example-prog)
2
NIL
* (interp (stmts (assign "x" (num 1)) (print- (id "x"))))
1
NIL
```
