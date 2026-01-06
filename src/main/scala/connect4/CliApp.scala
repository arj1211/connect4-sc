package connect4
import connect4.logic.GameLogic
import connect4.domain._
import scala.io.StdIn

object CliApp {

  def gameLoop(
      currentGameState: GameState,
      updateFunction: (GameState, Int) => Option[GameState]
  ): Unit = {

    println()
    println(currentGameState.board)
    println()
    print(s"${currentGameState.currentPlayer}: ")

    val input = StdIn.readInt()
    val nextState =
      updateFunction(currentGameState, input)
        .getOrElse(currentGameState)
    if (nextState.equals(currentGameState)) println("Invalid move")
    nextState.status match {
      case Win(winner) =>
        println("\n" + nextState.board + "\n" + s"$winner wins!")
      case Draw    => println("\n" + nextState.board + "\n" + "Game is a draw!")
      case Ongoing => gameLoop(nextState, updateFunction)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Connect 4!")
    val newGame = GameLogic.createGame()
    gameLoop(newGame, GameLogic.placeDisc)
  }
}
