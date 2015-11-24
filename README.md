# fJS 
A static type programming language implemented in Haskell.  

## Installation  
> ghc Main.hs  
> ./Main   

These commands compile the interpreter and execute the program in "test/lambda.js"  

## Features  
* JavaScript-like sytax (without "return");  
* Automatic currying;  
* Furely functional;  
* Statically typed;
* Hindley–Milner type system  

## Syntax
This language adpots a JavaScript-like syntax that looks like:  

    function fix(f) {
        var x = f(x);
        x
    }
    function main(argument) {
        fix
    }
This code segment (at toplevel) will be compiled to the intermediate representation:    

    let rec fix = function (f) 
                    let rec x = f(x)
                    in x
    in fix

Every file has to contain a "main" function that is same with the C language.  

This language supports several types (function, list, Number, Bool, String) and the operations one those types (head, tail, cons on lists; +, -, *, \, ==, !=, <, >, <=, >= ... on Number, if else on Bool).

## Author  
* Fei Peng

## Copyright

Copyright (c) 2015 Fei Peng