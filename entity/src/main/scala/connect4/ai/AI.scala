package connect4.ai

import connect4.domain._

import scala.util.Random

case class RandomAI() extends PlayerEntity {
  override val name = "Random AI"
  override def chooseMove(gameState: GameState): Either[GameError, Int] = {
    val availableColumns = (0 until gameState.board.cols).filter { col =>
      (0 until gameState.board.rows).exists { row =>
        gameState.board.grid(row)(col).isEmpty
      }
    }
    if (availableColumns.isEmpty) Left(ColumnFull)
    else Right(availableColumns(Random.nextInt(availableColumns.length)))
  }
}
