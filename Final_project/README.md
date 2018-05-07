# SymbolicAlgebra
Scheme project
***Running the code***
The testing file should run with the code no problem, If the name of the main code is changed Just adjust the top of the testing code.
All components work if tested correctly.
Input should be in the format (variable ((coeff order)(coeff order)...))

To make a poly use (make-poly 'variable (make-term coeff order) (make-term coeff order) (make-term coeff order)))
Call all terms before this list. See test file and bottom of main code for examples of how to run specific functions.
There is a pre-made poly called poly1 it can be called if you need a polynomial made correctly

An outside resource was used for part of Mul-terms (recursion aspect) and Jeremy Wood helped set up all Unit testing.

PROMPT
1. Design a representation for polynomials. (a) make-poly
(b) variable (c) term-list
(d) zero-poly?
(e) equal-poly? (f) same-variable?
2. Design a representation for term lists. (a) the-empty-termlist
(b) empty-termlist?
(c) first-term
(d) rest-terms (e) adjoin-term
3. Design a representation for terms. (a) make-term
(b) order (c) coeff
4. implement 5. Implement 6. Implement 7. Implement 8. Implement 9. Implement
value: value(3x4 + x2 + 8, 1) = 12
negation: negate(3x4 + x2 + 8) = −3x4 − x2 − 8 subtraction.
multiplication.
differentiation.
integration.
10. (Extra Credit) Implement division. 11. (Extra Credit) Implement remainder.
2
CSCI202 Spring 2018
 12. (Extra Credit) Provide a user interface that allows the user to input a string looking more or less like a usual form of a polynomial. Something like “3xˆ4 + xˆ2 + 8”, for example. The interface should also print the result as the same sort of string.
13. (Extra Credit) Any other cool thing you can think of.
14. (Super Extra Credit) Implement multivariable polynomial operations. Note: Some of your representations may have to change to accomplish this.
Submission
You may work in teams of up to two on this assignment. Note that all members of a team will receive the same grade on the assignment.
This assignment is due by 11:55 PM on Monday, May 7. A single version of the assignment is due from each team. Submit your source files in a zipped folder to Moodle in the usual fashion.
