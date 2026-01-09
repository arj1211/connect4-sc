package connect4.logic

import connect4.domain._
import scala.collection.immutable.VectorImpl
import connect4.domain.{
  Board,
  Draw,
  GameState,
  GameStatus,
  Ongoing,
  Player,
  Red,
  Win,
  Yellow
}

object GameLogic {
  def createGame(
      rows: Int = 6,
      cols: Int = 7,
      initBoard: Option[Board] = None,
      initPlayer: Player = Red
  ): GameState = {
    val emptyGrid = Vector.fill(rows, cols)(None: Board.Cell)
    val initialBoard = initBoard.getOrElse(Board(emptyGrid, rows, cols))
    GameState(
      initialBoard,
      initPlayer,
      Ongoing
    )
  }

  def placeDisc(
      gameState: GameState,
      column: Int
  ): Either[GameError, GameState] = {
    if (column < 0 || column >= gameState.board.cols)
      Left(ColumnOutOfBounds)
    else
      gameState.status match {
        case Ongoing =>
          val targetRow = (gameState.board.rows - 1 to 0 by -1).find { row =>
            gameState.board.grid(row)(column).isEmpty
          }
          targetRow match {
            case None      => Left(ColumnFull)
            case Some(row) =>
              val newRow = gameState.board
                .grid(row)
                .updated(column, Some(gameState.currentPlayer))
              val newGrid = gameState.board.grid.updated(row, newRow)
              val newBoard = gameState.board.copy(newGrid)
              val newStatus =
                checkWinner(newBoard, gameState.currentPlayer)
              val nextPlayer = gameState.currentPlayer match {
                case Red    => Yellow
                case Yellow => Red
              }
              Right(GameState(newBoard, nextPlayer, newStatus))
          }
        case Win(_) | Draw => Left(GameAlreadyOver)
      }
  }

  def isBoardFull(board: Board): Boolean = !board.grid.flatten.contains(None)

  def getAllLines(board: Board): Vector[Vector[Board.Cell]] = {
    // we're considering horizontal lines of length 4
    val horizontals = for {
      row <- 0 until board.rows
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row)(col + d))
    // we're considering vertical lines of length 4
    val verticals = for {
      row <- 0 to (board.rows - 4)
      col <- 0 until board.cols
    } yield Vector.tabulate(4)(d => board.grid(row + d)(col))
    // starting at top left corner and going top-left-down-right for length 4 at a time
    val diagonalsNeg = for {
      row <- 0 to (board.rows - 4)
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row + d)(col + d))
    // starting at row < 3 doesn't yield an down-left-up-right diagonal of length 4
    val diagonalsPos = for {
      row <- 3 until board.rows
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row - d)(col + d))
    (horizontals ++ verticals ++ diagonalsNeg ++ diagonalsPos).toVector
  }

  private def checkWinner(
      board: Board,
      player: Player
  ): GameStatus = {
    def isWinningLine(line: Vector[Board.Cell]): Boolean =
      line.forall(_ == Some(player))
    if (getAllLines(board).exists(isWinningLine))
      Win(player)
    else if (isBoardFull(board)) Draw
    else Ongoing
  }
}
