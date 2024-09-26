# Haskell Connect4

This project is a Haskell implementation of Connect 4 game. It was developed with a focus on using functional programming paradigm.

## Table of Contents
- [Features](#features)
- [Structure](#structure)
  - [Rose Tree Data Structure](#rose-tree-data-structure)
  - [Game State Representation](#game-state-representation)
  - [Game State Manipulation Using Monads](#game-state-manipulation-using-monads)
  - [Parsing Game State and Moves](#parsing-game-state-and-moves)
- [Requirements](#requirements)

## Features
- Game state management using functional data structures.
- Simulation of valid and invalid moves with error handling.
- Victory detection and game termination logic.
- Parsing of game state and moves from a file.

## Structure
### 1. Rose Tree Data Structure
The Rose Tree is a tree where each node has a value and a variable number of child nodes. We use a Rose Tree data structure to analyze potential game moves and their outcomes.

Data Definition:
```haskell
data Rose a = Node a [Rose a]
```
Functions Implemented: <br>
```size```: returns the number of nodes in the tree.<br>
```height```: calculates the longest path from the root to any leaf.<br>
```leavesCount```: counts the number of leaves in the tree.<br>
```leaves```: returns a list of the values in the leaf nodes.<br>
```elemsOnDepth```: returns values at a specified depth.<br>
```foldRose```: performs a fold operation on tree.<br>
```generateRose```: generates a rose tree based on a root element, a function and specified depth.<br>
Functor instance:
```haskell
instance Functor Rose where
    fmap f (Node root children) = Node (f root) (map (fmap f) children)
```

### 2. Game State Representation
Game state is represented with a grid that is not fixed in size (dynamic number of rows and columns) and functions to simulate gameplay.

Some Functions:<br>
```validMoves```: returns all valid moves for a given game state.<br>
```applyValidMoves```: simulates applying a move and returns the updated game state, switching the player's turn.<br>
```endGame```: checks if the game is in a winning state or if the grid is full (draw).<br>
```generateGameTree```: using the rose tree structure, we create a game tree that represents all possible moves from the current game state up to a specified number of moves. Leaf nodes represent terminal game states.<br>

### 3. Game State Manipulation Using Monads
Game state is created using a custom monadic type GameStateOp to manage both valid and invalid states:<br>
```haskell
instance Monad (GameStateOp currState) where
    return = pure

    (GameStateOp ra) >>= f = GameStateOp $ \s ->
        let (resultA, newState) = ra s
        in case resultA of
            Valid a -> let (GameStateOp rb) = f a in rb newState
            Invalid msg -> (Invalid msg, newState)
```

Some Functions:<br>
```applyMove```: simulates a single move, returning either a valid new state or an error message if the move is invalid.<br>
```applyMovesFromList```: takes a list of moves and applies them to the current game state, returning either the new state or an error message.<br>

```haskell
applyMoves = do
    applyMove (1, 1)
    applyMove (1, 2)
    applyMove (1, 3)
    applyMove (0, 0)
    applyMove (0, 10)
```
### 4. Parsing Game State and Moves
It is possible to read the game state and moves from a file.

Input Format:
```
| | | | | | 
| |Z| | | | 
| |C|Z| | | 
| |C|Z| | | 
1,1
1,4
1,5
3,5
```
## Requirements
- stack 2.15.5
- ghci 9.4.8
- cabal 3.10.3
