Description:

This is a modularity project for CS 589. It involves two main parts serving different clients: the first is a shape module and the other is a robot module. We imagine that each module is extended with a new data variant and operation making the modules Rect and Step respectively by different clients too. Then a new client builds a robotic interaction module with uses both parts. 

To compile:

basically use GHCI which is a Haskell compiler. 

These libraries are assumed to be available. 
AlaCarte.hs
Prim.hs
Cond.hs 

For the shape module:
$ ghci Shape.hs

For the shape ext module:
$ ghci Rect.hs

For the robotic move module:
$ ghci Move.hs 

For the robotic move ext module:
$ ghci Step.hs


To test:

Each of the modules have its examples. 
For example, to test the shape module pretty print of example 4 in the file, you run the following. 
$ testEx4'
The result is: line: from (5.0, 3.0) to (((2.0 * 3.0) + -4.0), 3.0)

to rest the evaluation of the same example you run the following.
$ testEx4
The result is: center: (3.5, 3.0) height: 0.0 width: 3.0

Multiple example of different numbers exist in all files. 







