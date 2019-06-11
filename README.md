# Description:

The system consists of three main modules: primitive terms, shapes and robotic moves. The primitiveterms module is provided by the professor and I am are directly using it. The system is assumed tobe used by multiple clients. The shape library is used in its own by people in geometry. The roboticmoves library is used by people in robotics. The assumption is that different clients extended the systemmultiple times in all dimensions. Primitives are used in both shapes and robotic moves and this requiresextending evaluation and pretty printing operations. The shape module is extended with an additionalshape and a function that works with shapes. The robotic moves module is extended with an additionalrobot move. Robotic moves also use the shape module which requires extending some operations. Lastly,a new operation is made to map shapes into robotic moves.

# To compile:

Basically use GHCI which is a Haskell compiler. 

**These libraries are assumed to be available.** 

```
AlaCarte.hs

Prim.hs

Cond.hs 
```

**For shapes** 

For the shape module:

```
$ ghci Shape.hs
```


For the ShapeArea module (ext operation):
```
$ ghci ShapeArea.hs
```


For the Rect module (ext data variant):
```
$ ghci Rect.hs
```

For the ShapeCircumference module (ext operation):
```
$ ghci ShapeCircumference.hs
```


**For Robots** 

For the robotic move module:

```
$ ghci Move.hs 
```

For the ShapeToMoves module (ext operation):

```
$ ghci ShapeToMoves.hs 
```

For the Step module (ext data variant):
```
$ ghci Step.hs
```

# To test:

First compile the testing module. 

```
$ ghci Testing.hs
```

This files contanis examples for testing every module. It shows how the examples grow as the extensions happen. 

Use the comments to guide how the examples relate to modules. 


For example,

To test the shape module evaluation function of example 4, you run the following.

```
$ testEx4
```

The result is: 
```
center: (3.5, 3.0) height: 0.0 width: 3.0
```

To test the shape module pretty print of the same example, you run the following. 
```
$ testEx4'
```

The result is: 
```
line: from (5.0, 3.0) to (((2.0 * 3.0) + -4.0), 3.0)
```


