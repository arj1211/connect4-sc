package connect4.ai

import connect4.domain._
import connect4.logic.GameLogic
import scala.annotation.tailrec

case class MinimaxAI(maxDepth: Int) extends PlayerEntity {
  override val name = "Minimax AI"

  override def chooseMove(gameState: GameState): Either[GameError, Int] = {
    if (gameState.status != Ongoing)
      Left(GameAlreadyOver)
    else {
      val validMoves = (0 until gameState.board.cols)
        .map { column =>
          GameLogic
            .placeDisc(gameState, column)
            .map(newState => (column, newState))
        }
        .collect { case Right((col, state)) =>
          (col, state)
        }

      if (validMoves.isEmpty) Left(ColumnFull)
      else {
        val evaluatedMoves = validMoves.map { case (col, state) =>
          val score = minimax(
            state,
            maxDepth - 1,
            maximizing = false,
            alpha = Int.MinValue,
            beta = Int.MaxValue
          )
          (col, score)
        }
        evaluatedMoves.maxBy(_._2) match {
          case (col, _) => Right(col)
        }
      }
    }
  }
  def minimax(
      state: GameState,
      depth: Int,
      maximizing: Boolean,
      alpha: Int = Int.MinValue,
      beta: Int = Int.MaxValue
  ): Int = {
    state.status match {
      case Win(winner) => {
        val rootPlayer =
          if (maximizing) state.currentPlayer else opponent(state.currentPlayer)
        if (winner == rootPlayer) 1000 + depth
        else -1000 - depth
      }
      case Draw                  => 0
      case Ongoing if depth == 0 => {
        val evalPlayer =
          if (maximizing) opponent(state.currentPlayer) else state.currentPlayer
        evaluateBoardHeuristic(state.board, evalPlayer)
      }
      case Ongoing =>
        if (maximizing) {
          var currentAlpha = alpha
          var maxEval = Int.MinValue
          for (column <- 0 until state.board.cols) {
            GameLogic.placeDisc(state, column) match {
              case Right(newState) =>
                val eval = minimax(
                  newState,
                  depth - 1,
                  maximizing = false,
                  currentAlpha,
                  beta
                )
                maxEval = math.max(maxEval, eval)
                currentAlpha = math.max(currentAlpha, eval)
                if (beta <= currentAlpha) {
                  return maxEval // beta cutoff
                }
              case Left(_) => // skip invalid moves
            }
          }
          maxEval
        } else {
          var currentBeta = beta
          var minEval = Int.MaxValue
          for (column <- 0 until state.board.cols) {
            GameLogic.placeDisc(state, column) match {
              case Right(newState) =>
                val eval = minimax(
                  newState,
                  depth - 1,
                  maximizing = true,
                  alpha,
                  currentBeta
                )
                minEval = math.min(minEval, eval)
                currentBeta = math.min(currentBeta, eval)
                if (currentBeta <= alpha) {
                  return minEval // alpha cutoff
                }
              case Left(_) => // skip invalid moves
            }
          }
          minEval
        }
    }
  }

  def evaluateLine(line: Vector[Board.Cell], player: Player): Int = {
    val targetCell = Some(player)
    val opponentCell = Some(if (player == Red) Yellow else Red)

    val playerCount = line.count(_ == targetCell)
    val opponentCount = line.count(_ == opponentCell)
    val emptyCount = 4 - playerCount - opponentCount

    // Pattern-based scoring
    (playerCount, opponentCount, emptyCount) match {
      case (4, 0, 0) => 1000 // Win (should be caught earlier)
      case (3, 0, 1) => 100 // Three in a row with one empty - immediate threat
      case (2, 0, 2) => 20 // Two in a row with two empties - good potential
      case (1, 0, 3) => 3 // One in a row with three empties - some potential
      case (0, 3, 1) =>
        -80 // Opponent has three in a row - urgent threat to block
      case (0, 2, 2) => -15 // Opponent has two in a row
      case (0, 1, 3) => -2 // Opponent has one in a row
      case (_, _, _) => 0 // Mixed or blocked line
    }
  }
  def calculateCenterBonus(
      board: Board,
      player: Player,
      opponent: Player
  ): Int = {
    val centerCol = board.cols / 2
    var bonus = 0

    // Center columns are more valuable
    for (
      col <- math
        .max(0, centerCol - 1) to math.min(board.cols - 1, centerCol + 1)
    ) {
      for (row <- 0 until board.rows) {
        board.grid(row)(col) match {
          case Some(`player`) =>
            // Bonus based on how close to center and how high (lower rows = better)
            val distanceFromCenter = math.abs(col - centerCol)
            val heightBonus = (board.rows - row) * 2 // Lower = better
            bonus += (3 - distanceFromCenter) * heightBonus

          case Some(`opponent`) =>
            val distanceFromCenter = math.abs(col - centerCol)
            val heightBonus = (board.rows - row) * 2
            bonus -= (3 - distanceFromCenter) * heightBonus

          case None => // Empty cell in center area - potential
            if (col == centerCol) bonus += 1
        }
      }
    }
    bonus
  }
  def evaluateBoardHeuristic(board: Board, player: Player): Int = {
    val opponentPlayer = if (player == Red) Yellow else Red

    // Generate all possible lines of 4 on the board
    val allLines = GameLogic.getAllLines(board)

    // Evaluate each line for both players
    val playerScore = allLines.map(evaluateLine(_, player)).sum
    val opponentScore = allLines.map(evaluateLine(_, opponentPlayer)).sum

    // Center control bonus (center columns are more valuable)
    val centerBonus = calculateCenterBonus(board, player, opponentPlayer)

    // Weight opponent threats more heavily (defensive play)
    playerScore - (opponentScore * 1.2).toInt + centerBonus
  }
  def opponent(player: Player): Player = player match {
    case Red    => Yellow
    case Yellow => Red
  }
}
