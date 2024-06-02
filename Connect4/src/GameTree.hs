module GameTree (
    generateGameTree
) where

import RoseTree
import GameState

type GameState = (Board Field, Player)


isEndState :: GameState -> Bool
isEndState (board, player) = endGame board player || null (emptyFields board)


nextGameStates :: GameState -> [GameState]
nextGameStates (board, player) = [(applyValidMoves board player coord, nextPlayer player) | coord <- emptyFields board]
  where
    nextPlayer P1 = P2
    nextPlayer P2 = P1


generateGameTree :: Int -> GameState -> Rose GameState
generateGameTree depth initialState = genereteRose nextStates depth initialState
  where
    nextStates state = if isEndState state then [] else nextGameStates state


testGameTree :: Rose GameState
testGameTree = generateGameTree 3 (initialBoard, P1)
  where
    initialBoard = listToBoard [[P, P, P, P, P],
                                [P, P, P, P, P],
                                [P, P, P, P, P],
                                [P, P, P, P, P],
                                [P, P, P, P, P]]
