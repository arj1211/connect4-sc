package connect4.domain

// Sum Type for Player
sealed trait Player
case object Red extends Player
case object Yellow extends Player

// Sum Type for Game Status
sealed trait GameStatus
case object Ongoing extends GameStatus
case class Win(winner: Player) extends GameStatus
case object Draw extends GameStatus

// Product Type for the Board
case class Board(grid: Vector[Vector[Board.Cell]], rows: Int, cols: Int) {
  override def toString(): String =
    grid
      .map[String](x =>
        x.map(y =>
          y match {
            case Some(y) => y.toString.head
            case None    => "."
          }
        ).mkString
      )
      .mkString("\n")
}

// Define a companion object for Board to hold related types/helpers
object Board {
  // Type alias is now correctly scoped inside an object
  type Cell = Option[Player]
}

// Final Game State
case class GameState(board: Board, currentPlayer: Player, status: GameStatus)

sealed trait GameError
case object ColumnFull extends GameError
case object ColumnOutOfBounds extends GameError
case object GameAlreadyOver extends GameError
