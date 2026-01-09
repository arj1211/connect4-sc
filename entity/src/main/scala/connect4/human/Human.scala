package connect4.human

import connect4.domain._

import scala.io.StdIn

case class Human() extends PlayerEntity {
  override val name = "Human"
  override def chooseMove(gameState: GameState): Either[GameError, Int] = {
    scala.util.Try(StdIn.readInt()).toOption match {
      case None      => Left(ColumnOutOfBounds)
      case Some(col) =>
        if (col < 0 || col > 6) Left(ColumnOutOfBounds)
        else Right(col)
    }
  }
}
