package connect4
import connect4.logic.GameLogic
import connect4.domain._
import scala.io.StdIn
import scala.annotation.tailrec

object CliApp {
  @tailrec
  def gameLoop(
      currentGameState: GameState
  ): Unit = {

    println(s"\n${currentGameState.board}\n")

    currentGameState.status match {
      case Ongoing =>
        print(
          s"${currentGameState.currentPlayer}'s turn (0-${currentGameState.board.cols - 1}): "
        )
        scala.util.Try(StdIn.readInt()).toOption match {
          case Some(col) =>
            GameLogic.placeDisc(currentGameState, col) match {
              case Left(err) =>
                println(s"Error: $err\nTry again.")
                gameLoop(currentGameState)
              case Right(nextState) => gameLoop(nextState)
            }
          case None =>
            println("Invalid move, enter a number 0-6")
            gameLoop(currentGameState)
        }
      case Win(winner) =>
        println("\n" + currentGameState.board + "\n" + s"$winner wins!")
      case Draw =>
        println("\n" + currentGameState.board + "\n" + "Game is a draw!")
    }

  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Connect 4!")
    val newGame = GameLogic.createGame()
    gameLoop(newGame)
  }
}
