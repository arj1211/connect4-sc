package connect4
import connect4.logic.GameLogic
import connect4.domain._
import connect4.ai._
import connect4.human.Human
import scala.io.StdIn
import scala.annotation.tailrec

object CliApp {
  @tailrec
  def gameLoop(
      currentGameState: GameState,
      redPlayer: PlayerEntity,
      yellowPlayer: PlayerEntity
  ): Unit = {
    println(s"\n${currentGameState.board}\n")
    currentGameState.status match {
      case Ongoing =>
        val currentEntity = currentGameState.currentPlayer match {
          case Red    => redPlayer
          case Yellow => yellowPlayer
        }
        print(
          s"${currentGameState.currentPlayer}'s turn (0-${currentGameState.board.cols - 1}): "
        )
        currentEntity.chooseMove(currentGameState) match {
          case Left(err) =>
            println("Invalid move, enter a number 0-6")
            gameLoop(currentGameState, redPlayer, yellowPlayer)
          case Right(col) =>
            GameLogic.placeDisc(currentGameState, col) match {
              case Left(err) =>
                println(s"Error: $err\nTry again.")
                gameLoop(currentGameState, redPlayer, yellowPlayer)
              case Right(nextState) =>
                gameLoop(nextState, redPlayer, yellowPlayer)
            }
        }
      case Win(winner) =>
        println("\n" + currentGameState.board + "\n" + s"$winner wins!")
      case Draw =>
        println("\n" + currentGameState.board + "\n" + "Game is a draw!")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Connect 4!")
    println("Choose game mode:")
    println("1. Human vs Human")
    println("2. Human vs AI (Random)")
    println("3. Human vs AI (Minimax)")
    println("4. AI (Random) vs AI (Minimax)")
    println("5. AI (Minimax) vs AI (Minimax)")

    val (red, yellow) = StdIn.readInt() match {
      case 1 => (Human(), Human())
      case 2 => (Human(), RandomAI())
      case 3 => (Human(), MinimaxAI(maxDepth = 4))
      case 4 => (RandomAI(), MinimaxAI(maxDepth = 4))
      case 5 => (MinimaxAI(maxDepth = 4), MinimaxAI(maxDepth = 4))
      case _ =>
        println("Invalid choice, defaulting to Human vs Human")
        (Human(), Human())
    }

    val newGame = GameLogic.createGame()
    gameLoop(newGame, red, yellow)
  }
}
