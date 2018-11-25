#lang racket

(define keywords
  '("$"
    "as"
    "at"
    "behind"
    "call"
    "expression"
    "hide"
    "if"
    "in"
    "image"
    "init"
    "jump"
    "menu"
    "onlayer"
    "python"
    "return"
    "scene"
    "set"
    "show"
    "with"
    "while"
    "zorder"
    "transform"))

(define operators
  '("<"
    "<="
    ">"
    ">="
    "<>"
    "!="
    "=="
    "|"
    "^"
    "&"
    "<<"
    ">>"
    "+"
    "-"
    "*"
    "/"
    "//"
    "%"
    "~"
    "**"))

(define escaped-operators
  '("or" "and" "not" "is" "in"))