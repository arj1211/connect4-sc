package connect4

import connect4.domain._
import connect4.logic.GameLogic
import connect4.ai._
import connect4.human._

import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import scala.annotation.tailrec
import scala.util.Random

class AISpec extends munit.ScalaCheckSuite {
  test("Test: RandomAI chooses valid move on empty board") {
    val ai = RandomAI()
    val game = GameLogic.createGame()
    val move = ai.chooseMove(game)
    assert(move.isRight)
    move.foreach { column =>
      assert(column >= 0 && column < 7)
    }
  }
  test("Test: MinimaxAI chooses valid move on empty board") {
    val ai = MinimaxAI(maxDepth = 3)
    val game = GameLogic.createGame()
    val move = ai.chooseMove(game)
    assert(move.isRight)
    move.foreach { column =>
      assert(column >= 0 && column < 7)
    }
  }
  test("Test: Minimax has winning advantage over Random") {
    val randomAI = RandomAI()
    val minimaxAI = MinimaxAI(maxDepth = 4)

    val numGames = 100

    @tailrec
    def simGame(
        currentGameState: GameState,
        redPlayer: PlayerEntity,
        yellowPlayer: PlayerEntity
    ): Either[GameError, GameState] = {
      currentGameState.status match {
        case Ongoing =>
          val currentPlayer = currentGameState.currentPlayer match {
            case Red    => redPlayer
            case Yellow => yellowPlayer
          }
          currentPlayer.chooseMove(currentGameState) match {
            case Left(err)  => Left(err)
            case Right(col) =>
              GameLogic.placeDisc(currentGameState, col) match {
                case Left(err)        => Left(err)
                case Right(nextState) =>
                  simGame(nextState, redPlayer, yellowPlayer)
              }
          }
        case Win(_) | Draw => Right(currentGameState)
      }
    }

    val minimaxYellow: Vector[Option[Int]] =
      (1 to numGames / 2)
        .map(_ =>
          simGame(
            GameLogic.createGame(),
            redPlayer = randomAI,
            yellowPlayer = minimaxAI
          ) match {
            case Left(err)         => None
            case Right(finalState) =>
              finalState.status match {
                case Win(Yellow) => Some(1)
                case Win(Red)    => Some(-1)
                case Draw        => Some(0)
                case _           => None
              }

          }
        )
        .toVector

    val minimaxRed: Vector[Option[Int]] = (1 to numGames / 2)
      .map(_ =>
        simGame(
          GameLogic.createGame(),
          redPlayer = minimaxAI,
          yellowPlayer = randomAI
        ) match {
          case Left(err)         => None
          case Right(finalState) =>
            finalState.status match {
              case Win(Red)    => Some(1)
              case Win(Yellow) => Some(-1)
              case Draw        => Some(0)
              case _           => None
            }
        }
      )
      .toVector

    assertEquals(minimaxYellow.length + minimaxRed.length, numGames)

    val minimaxWins =
      (minimaxYellow ++ minimaxRed).count(v => v.getOrElse(0) == 1)

    assert(minimaxWins > (numGames - minimaxWins))
    assert(minimaxWins.toDouble / numGames > 0.7)

  }
  test("Test: RandomAI should return error when board is full") {
    val ai = RandomAI()
    val fullGrid = Vector.fill(6, 7)(Some(Red): Board.Cell)
    val fullBoard = Board(fullGrid, 6, 7)
    val game = GameState(fullBoard, Red, Ongoing)
    val move = ai.chooseMove(game)
    assert(move.isLeft)
    assertEquals(move.left.get, ColumnFull)
  }
  test("Test: RandomAI should only choose from available columns") {
    val grid = Vector.tabulate(6, 7) { (row, col) =>
      if (col >= 2 || row >= 3) Some(Red) else None
    }
    val board = Board(grid.toVector.map(_.toVector), 6, 7)
    val game = GameState(board, Red, Ongoing)
    val ai = RandomAI()
    (1 to 100).foreach { _ =>
      val move = ai.chooseMove(game)
      assert(move.isRight)
      move.foreach { col =>
        assert(col == 0 || col == 1)
      }
    }
  }
  test("Test: MinimaxAI should return error when game is over") {
    val ai = MinimaxAI(maxDepth = 3)
    val winningGrid = Vector.tabulate(6, 7) { (row, col) =>
      if (row == 5 && col < 4) Some(Red) else None
    }
    val winningBoard = Board(winningGrid.toVector.map(_.toVector), 6, 7)
    val winningGame = GameState(winningBoard, Red, Win(Red))
    val move1 = ai.chooseMove(winningGame)
    assert(move1.isLeft)
    assertEquals(move1.left.get, GameAlreadyOver)
    val drawGame = GameState(winningBoard, Red, Draw)
    val move2 = ai.chooseMove(drawGame)
    assert(move2.isLeft)
    assertEquals(move2.left.get, GameAlreadyOver)
  }
  test("Test: MinimaxAI should block opponent's winning move") {
    val ai = MinimaxAI(maxDepth = 3)
    val board = Board(
      Vector(
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector(
          Some(Yellow),
          Some(Yellow),
          Some(Yellow),
          None,
          None,
          None,
          None
        )
      ),
      6,
      7
    )
    val game = GameState(board, Red, Ongoing)
    val move = ai.chooseMove(game)
    assert(move.isRight)
    assertEquals(move.right.get, 3)
  }
  test("Test: MinimaxAI should take winning move when available") {
    val ai = MinimaxAI(maxDepth = 3)
    val board = Board(
      Vector(
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector(
          Some(Red),
          Some(Red),
          Some(Red),
          None,
          None,
          None,
          None
        )
      ),
      6,
      7
    )
    val game = GameState(board, Red, Ongoing)
    val move = ai.chooseMove(game)
    assert(move.isRight)
    assertEquals(move.right.get, 3)
  }
  property("Property: RandomAI always returns valid move or error") {
    forAll { (seed: Int) =>
      val random = new scala.util.Random(seed)
      val grid = Vector.tabulate(6, 7) { (row, col) =>
        if (random.nextDouble() > 0.5) Some(Red) else None
      }
      val board = Board(grid.toVector.map(_.toVector), 6, 7)
      val game = GameState(board, Red, Ongoing)
      val ai = RandomAI()
      val move = ai.chooseMove(game)
      move match {
        case Left(error) =>
          error == ColumnFull || error == GameAlreadyOver
        case Right(column) =>
          column >= 0 && column < 7 &&
          (0 until 6).exists(row => board.grid(row)(column).isEmpty)
      }
    }
  }
  test("Test: pattern heuristic should recognize immediate threats") {
    val winningBoard = Board(
      Vector(
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector(
          Some(Red),
          Some(Red),
          Some(Red),
          None,
          None,
          None,
          None
        )
      ),
      6,
      7
    )
    val heuristicValue = MinimaxAI(4).evaluateBoardHeuristic(winningBoard, Red)
    assert(heuristicValue > 50)
  }
  test("Test: pattern heuristic should recognize opponent threats") {
    val threatenedBoard = Board(
      Vector(
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector.fill(7)(None),
        Vector(
          Some(Yellow),
          Some(Yellow),
          Some(Yellow),
          None,
          None,
          None,
          None
        )
      ),
      6,
      7
    )
    val heuristicValue =
      MinimaxAI(4).evaluateBoardHeuristic(threatenedBoard, Red)
    assert(heuristicValue < -50)
  }
  test("Test: center control should be valued") {
    val emptyBoard = GameLogic.createGame().board
    val centerHeuristic = MinimaxAI(4).evaluateBoardHeuristic(emptyBoard, Red)
    val centerBoard = Board(
      Vector
        .tabulate(6, 7) { (row, col) =>
          if (row == 5 && col == 3) Some(Red) else None
        }
        .toVector
        .map(_.toVector),
      6,
      7
    )
    val withCenterHeuristic =
      MinimaxAI(4).evaluateBoardHeuristic(centerBoard, Red)
    assert(withCenterHeuristic > centerHeuristic)
  }
  property("Property: Minimax never chooses invalid move") {
    forAll(Gen.choose(1, 6), Gen.oneOf(Red, Yellow)) { (depth, player) =>
      val ai = MinimaxAI(maxDepth = depth)
      val board = randomBoardSimple()
      val game = GameState(board, player, Ongoing)
      val move = ai.chooseMove(game)
      move.forall { col =>
        col >= 0 && col < 7 &&
        board.grid.exists(row => row(col).isEmpty)
      }
    }
  }
  property("Property: Minimax returns error for terminal states") {
    forAll(Gen.choose(1, 6)) { depth =>
      val ai = MinimaxAI(maxDepth = depth)
      val winningBoard = randomBoardSimple()
      val winningGame = GameState(winningBoard, Red, Win(Red))
      val move1 = ai.chooseMove(winningGame)
      assert(move1.isLeft)
      assertEquals(move1.left.get, GameAlreadyOver)
    }
  }
  def randomBoardSimple(emptyProbability: Double = 0.7): Board = {
    val rand = new Random()
    val rows = 6
    val cols = 7
    val grid = Vector
      .tabulate(rows, cols) { (_, _) =>
        if (rand.nextDouble() > emptyProbability) {
          Some(if (rand.nextBoolean()) Red else Yellow)
        } else {
          None
        }
      }
      .toVector
      .map(_.toVector)
    Board(grid, rows, cols)
  }
}
