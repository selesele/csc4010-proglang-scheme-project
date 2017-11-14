#lang racket
(require racket/include)
(include "ll1-parser.scm")

"Testing for EPS"
(newline)
(eps calc-gram)
"should be"
'("FT" "SL" "TT")
(newline)
"Testing for FIRST"
(newline)
(gen-first-set "T" calc-gram (eps calc-gram))
"should be"
'("id" "num" "(")
(newline)
(gen-first-set "FT" calc-gram (eps calc-gram))
"should be"
'("*" "/")
(newline)
"Testing for FOLLOW"
(newline)
(gen-follow-set "T" calc-gram)
"should be"
'("+" "-" ")" "id" "read" "write" "$$")
(newline)
(gen-follow-set "FT" calc-gram)
"should be"
'("+" "-" ")" "id" "read" "write" "$$")