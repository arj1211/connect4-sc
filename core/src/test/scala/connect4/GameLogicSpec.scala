package connect4

import connect4.logic.GameLogic
import connect4.domain._
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

class GameLogicSpec extends munit.ScalaCheckSuite {
  val exampleBoard = Board(
    Vector(
      Vector(None, None, None, None, None, None, None),
      Vector(None, None, None, None, None, None, None),
      Vector(None, None, None, None, None, None, None),
      Vector(None, None, None, None, None, None, None),
      Vector(
        Some(Red),
        Some(Red),
        Some(Yellow),
        None,
        None,
        None,
        None
      ),
      Vector(
        Some(Yellow),
        Some(Yellow),
        Some(Yellow),
        None,
        Some(Red),
        Some(Red),
        Some(Red)
      )
    ),
    6,
    7
  )
  test("Test: GameLogic.createGame") {
    val nRows = 6
    val nCols = 7
    val testGameState = GameLogic.createGame()
    assertEquals(testGameState.status, Ongoing)
    assertEquals(testGameState.board.rows, nRows)
    assertEquals(testGameState.board.cols, nCols)
    assertEquals(testGameState.board.grid.length, nRows)
    assertEquals(testGameState.board.grid(0).length, nCols)
    assertEquals(
      testGameState.board.grid.map(row => row.length).distinct.length,
      1
    )
    assertEquals(testGameState.currentPlayer, Red)
  }
  test("Test: GameLogic.placeDisc") {
    val initState = GameLogic.createGame(initBoard = Some(exampleBoard))
    var nextState = GameLogic.placeDisc(initState, 1).getOrElse(initState)
    assertNotEquals(initState, nextState)
    assertEquals(nextState.board.grid(0), Vector.fill(7)(None))
    assertEquals(nextState.board.grid(1), Vector.fill(7)(None))
    assertEquals(nextState.board.grid(2), Vector.fill(7)(None))
    assertEquals(
      nextState.board.grid(3),
      Vector(None, Some(Red), None, None, None, None, None)
    )
    assertEquals(
      nextState.board.grid(4),
      Vector(Some(Red), Some(Red), Some(Yellow), None, None, None, None)
    )
    assertEquals(
      nextState.board.grid(5),
      Vector(
        Some(Yellow),
        Some(Yellow),
        Some(Yellow),
        None,
        Some(Red),
        Some(Red),
        Some(Red)
      )
    )
    nextState = GameLogic.placeDisc(nextState, 3).getOrElse(nextState)
    assertEquals(
      nextState.board.grid(5),
      Vector(
        Some(Yellow),
        Some(Yellow),
        Some(Yellow),
        Some(Yellow),
        Some(Red),
        Some(Red),
        Some(Red)
      )
    )
    assertEquals(nextState.status, Win(Yellow))
  }
  test("Test: Generate Horizontal Lines") {
    val initState = GameLogic.createGame(initBoard = Some(exampleBoard))
    val board = initState.board
    val horizontals = for {
      row <- 0 until board.rows
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row)(col + d))
    val testElements = Vector(
      Vector(Some(Red), Some(Red), Some(Yellow), None),
      Vector(Some(Yellow), None, None, None),
      Vector(Some(Yellow), Some(Yellow), None, Some(Red))
    )
    assert(
      testElements
        .map(elem => horizontals.contains(elem))
        .forall(_ == true)
    )
  }
  test("Test: Generate Vertical Lines") {
    val initState = GameLogic.createGame(initBoard = Some(exampleBoard))
    val board = initState.board
    val verticals = for {
      row <- 0 to (board.rows - 4)
      col <- 0 until board.cols
    } yield Vector.tabulate(4)(d => board.grid(row + d)(col))
    val testElements = Vector(
      Vector(None, None, Some(Red), Some(Yellow)),
      Vector(None, None, Some(Yellow), Some(Yellow)),
      Vector(None, None, None, Some(Red)),
      Vector(None, None, None, None)
    )
    assert(
      testElements
        .map(elem => verticals.contains(elem))
        .forall(_ == true)
    )
  }
  test("Test: Generate Diagonal Lines") {
    val initState = GameLogic.createGame(initBoard = Some(exampleBoard))
    val board = initState.board
    val diagonalsNeg = for {
      row <- 0 to (board.rows - 4)
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row + d)(col + d))
    val diagonalsPos = for {
      row <- 3 until board.rows
      col <- 0 to (board.cols - 4)
    } yield Vector.tabulate(4)(d => board.grid(row - d)(col + d))
    val testElements = Vector(
      Vector(Some(Yellow), Some(Red), None, None),
      Vector(Some(Yellow), Some(Yellow), None, None),
      Vector(Some(Yellow), None, None, None),
      Vector(None, Some(Yellow), None, None),
      Vector(Some(Red), None, None, None)
    )
    val allDiagonals =
      (diagonalsNeg ++ diagonalsNeg.map(v =>
        v.reverse
      ) ++ diagonalsPos ++ diagonalsPos.map(v => v.reverse)).distinct
    assert(
      testElements
        .map(elem => allDiagonals.contains(elem))
        .forall(_ == true)
    )
  }
  test("Test: Check Winner") {
    var initState = GameLogic.createGame(initBoard = Some(exampleBoard))
    var nextState = GameLogic.placeDisc(initState, 3).getOrElse(initState)
    assertEquals(nextState.status, Win(Red))
    initState =
      GameLogic.createGame(initBoard = Some(exampleBoard), initPlayer = Yellow)
    nextState = GameLogic.placeDisc(initState, 3).getOrElse(initState)
    assertEquals(nextState.status, Win(Yellow))
  }
  property("Property: placing a disc should change the board") {
    forAll { column: Int =>
      val validColumn = Math.abs(column / 2) % 7
      val emptyGame = GameLogic.createGame()
      GameLogic
        .placeDisc(emptyGame, validColumn) match {
        case Left(err)         => false
        case Right(aGameState) =>
          aGameState.board != emptyGame.board
      }
    }
  }

  property(
    "Property: board should have exactly one more disc after valid move"
  ) {
    forAll { column: Int =>
      val validColumn = Math.abs(column / 2) % 7
      val emptyGame = GameLogic.createGame()
      GameLogic
        .placeDisc(emptyGame, validColumn) match {
        case Left(err)         => false
        case Right(aGameState) => countDiscs(aGameState.board) == 1
      }
    }
  }
  property("Property: single disc cannot be a winning move") {
    forAll { column: Int =>
      val validColumn = Math.abs(column / 2) % 7
      val emptyGame = GameLogic.createGame()
      GameLogic.placeDisc(emptyGame, validColumn) match {
        case Left(err)         => false
        case Right(aGameState) => aGameState.status == Ongoing
      }
    }
  }
  property("Property: full column returns error") {
    val fullColBoard = Board(
      Vector
        .tabulate(6, 7) { (r, c) =>
          if (c == 0) Some(Red) else None
        },
      6,
      7
    )
    val game = GameState(fullColBoard, Red, Ongoing)
    GameLogic.placeDisc(game, 0) match {
      case Left(err)        => err == ColumnFull
      case Right(someState) => false
    }
  }

  private def countDiscs(board: Board): Int =
    board.grid.flatten.count(_.isDefined)
}
