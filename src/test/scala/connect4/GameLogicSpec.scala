package connect4

import connect4.logic.GameLogic
import connect4.domain._

class GameLogicSpec extends munit.FunSuite {

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

  test("Util.checkLine") {
    val example = Vector(0, 0, 0, 0)
    val notExample = Vector(0, 0, 0, 1)
    assert(example.tail.forall(_ == example.head))
    assert(!notExample.tail.forall(_ == example.head))
  }

  test("Util.inBounds") {
    val nRows = 6
    val nCols = 7
    val nOutOfBounds = nRows + nCols + 1
    def inBounds(r: Int, c: Int) = r >= 0 && r < nRows && c >= 0 && c < nCols
    val coords = (0 to nRows)
      .map { i => (0 to nCols).map { j => (i, j) }.toVector }
      .toVector
      .flatten
      .distinct
    val testNumOutOfBounds =
      coords.filterNot(t => t match { case (x, y) => inBounds(x, y) }).length
    assertEquals(testNumOutOfBounds, nOutOfBounds)
  }

}
