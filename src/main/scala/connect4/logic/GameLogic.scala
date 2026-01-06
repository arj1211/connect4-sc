package connect4.logic

import connect4.domain._
import scala.collection.immutable.VectorImpl

object GameLogic {
  def createGame(
      rows: Int = 6,
      cols: Int = 7,
      initBoard: Option[Board] = None
  ): GameState = {
    val emptyGrid = Vector.fill(rows, cols)(None: Board.Cell)
    val initialBoard = initBoard.getOrElse(Board(emptyGrid, rows, cols))
    GameState(
      initialBoard,
      Red,
      Ongoing
    )
  }

  def placeDisc(gameState: GameState, column: Int): Option[GameState] = {
    gameState.status match {
      case Ongoing =>
        val targetRow = (gameState.board.rows - 1 to 0 by -1).find { row =>
          gameState.board.grid(row)(column).isEmpty
        }
        targetRow match {
          case None      => None
          case Some(row) =>
            val newRow = gameState.board
              .grid(row)
              .updated(column, Some(gameState.currentPlayer))
            val newGrid = gameState.board.grid.updated(row, newRow)
            val newBoard = gameState.board.copy(newGrid)
            val newStatus =
              checkWinner(newBoard, gameState.currentPlayer, row, column)
            val nextPlayer = gameState.currentPlayer match {
              case Red    => Yellow
              case Yellow => Red
            }

            Some(GameState(newBoard, nextPlayer, newStatus))
        }
      case Win(_) | Draw => None
    }
  }

  def isBoardFull(board: Board): Boolean = !board.grid.flatten.contains(None)

  private def checkWinner(
      board: Board,
      player: Player,
      lastRow: Int,
      lastCol: Int
  ): GameStatus = {

    def checkLine(line: Vector[Board.Cell]): Boolean =
      line.forall(_ == Some(player))

    def inBounds(r: Int, c: Int) =
      r >= 0 && r < board.rows && c >= 0 && c < board.cols

    def traceDiag(
        r: Int,
        c: Int,
        dr: Int,
        dc: Int,
        accumulation: Vector[Board.Cell]
    ): Vector[Board.Cell] =
      if (!inBounds(r, c)) accumulation
      else traceDiag(r + dr, c + dc, dr, dc, board.grid(r)(c) +: accumulation)

    def getFullDiag(r: Int, c: Int, dr: Int, dc: Int): Vector[Board.Cell] =
      traceDiag(
        r,
        c,
        dr,
        dc,
        Vector()
      ) ++ traceDiag(
        r,
        c,
        -dr,
        -dc,
        Vector()
      ).reverse.tail

    def containsWin(g: Vector[Vector[Board.Cell]]) =
      g.map(checkLine).contains(true)

    // 1. Horizontals
    val potentialRows: Vector[Vector[Board.Cell]] =
      board.grid.zipWithIndex
        .filter(z => Math.abs(lastRow - z._2) <= 3)
        .filter(z => z._2 == lastRow)
        .map { x => x._1 }
        .head
        .zipWithIndex
        .filter(z => Math.abs(lastCol - z._2) <= 3)
        .map { x => x._1 }
        .sliding(4)
        .toVector

    // 2. Verticals
    val potentialCols: Vector[Vector[Board.Cell]] =
      board.grid.transpose.zipWithIndex
        .filter(z => Math.abs(lastCol - z._2) <= 3)
        .filter(z => z._2 == lastCol)
        .map { x => x._1 }
        .head
        .zipWithIndex
        .filter(z => Math.abs(lastRow - z._2) <= 3)
        .map { x => x._1 }
        .sliding(4)
        .toVector

    // 3. Diagonals
    val negDiag: Vector[Vector[Board.Cell]] =
      getFullDiag(lastRow, lastCol, -1, -1).zipWithIndex
        .filter(z =>
          (Math.abs(lastCol - z._2) <= 3) || (Math.abs(
            lastRow - z._2
          ) <= 3)
        )
        .map { x => x._1 }
        .sliding(4)
        .toVector

    val posDiag: Vector[Vector[Board.Cell]] =
      getFullDiag(lastRow, lastCol, 1, -1).zipWithIndex
        .filter(z =>
          (Math.abs(lastCol - z._2) <= 3) || (Math.abs(
            lastRow - z._2
          ) <= 3)
        )
        .map { x => x._1 }
        .sliding(4)
        .toVector

    if (
      List(potentialRows, potentialCols, negDiag, posDiag)
        .map(containsWin)
        .contains(true)
    )
      Win(player)
    else if (isBoardFull(board)) Draw
    else Ongoing
  }
}
