# Compiling
Either `ghc Mlang.hs -o mlang` or `ghc Mlang.hs -o mlang -dynamic` should work.

# Mlang syntax

I highly encourage every reader to see the examples provided in `examples` directory of this repo. Everything written below is subject to change in future versions (if there be any).

## Types

There are 4 implemented types:
- `void`
- `int`
- `string`
- `bool`

## Declaring functions

To declare a function use this syntax:
```
fn (_type1_ _name1_, _type2_ _name2_, ..., _typen_ _namen_) _fun_name_:
  _body_
end
```
for example:
```
fn () void main:
  //body here
end
```
declares a function with no arguments, return type of `void` and name `main`,

and:

```
fn (int a, int b) int add:
  return a+b
end
```
declares a function with two arguments of type `int`, named `a` and `b`, return type `int` and name `add`.

All functions besides of those returning void should have and explicit `return` statement.

For now there is no syntax for optional arguments and variable length arguments.

## Function body – statements

Every statement is either:
- an expression – see expression
- a declaration of variable: `_type_ _var_name_`,or combined with assignment: `_type_ _var_name_ = _expression of the same type_`
- an assignment: `variable_name = expression` (note that the variable needs to be already declared)
- a while loop: 
```
while _bool_expression_ do
  //body
end
```
- an if statement:
```
if _bool_expression_ then
  //body
end
```
or
```
if _bool_expression_ then
  //body
else
  //else body
end
```
- an input statement: `input _var_name_`
- a print statement: `print _expression_`
- or a return statement: `return _expression_`.

## Expressions

Expression is either:
- a variable: `_var_name_`
- a constant value:
  - of integer type (example `1` or `-42`)
  - of string type (example `abc`)
  - of bool type (`true` or `false`)
  - void – `void`
- an aplication of function: `_fun_name_(_exp1_,_exp2_)`, `add(1,x)`
- or a primitive operation:
  - `e1 == e2` - returns `true` when `e1` is equal to `e2` or `false` otherwise
  - `- _integer_expression_` - integer negation
  - `_int_exp_ + _int_exp_` - sum of two integer expressions, there also is `-`,`*`,`/` – self explenatory and `^` – exponentiation
  - `! _bool_expression_` – boolean negation
  - `_bool_exp_ and _bool_exp_` – boolean conjunction
  - `_bool_exp_ or _bool_exp_` – boolean disjunction
  - `_string_exp_ @ _string_exp_` – string concatenation

## IO operations

It's worth noting that both `input` and `print` are not functions (at least not ATM). The `input` statement works as follows:
1. Read **line** from standard input
2. Parse it so it matches variable type
3. Assign parsed value to variable

The `print` statement simply prints **a line** consisting of parsed value. There is no way to change this behaviour (once again at least not ATM).