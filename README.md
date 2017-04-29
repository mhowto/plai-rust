Solutions for Programming Languages: Application and Interpretation

# Ch8. Mutation: Structures and Variables

# Ch9. Recursion and Cycles: Procedures and Data

## 9.1 Recursive and Cyclic Data
Exercise
```
Add lists and binary trees as built-in datatypes to the programming language.
```
Solution: 

Add such sub-types into ExprC:
```
[nilC]
[consC (head : ExprC) (tail : ExprC)]
[nodeC (value : ExprC) (left : ExprC) (right : ExprC)]
```

Exercise
```
Run the equivalent program through your interpreter for boxes and make sure it produces a cyclic value. How do you check this?
```
Solution:
Like the test in test_cyclic_data:
```
   (let b
         (box 3)
         (seq (setbox b b)
              (unbox (unbox b))))";
```
(unbox (unbox (unbox .. (unbox b)))) will return the result BoxV(1).


## 9.2 Recursive Functions
See the test in test_recursive_function_fact.

