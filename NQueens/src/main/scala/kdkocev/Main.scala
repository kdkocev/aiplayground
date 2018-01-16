package kdkocev

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main extends App {

  val N = 200
  val queensPositions = ArrayBuffer.fill(N)(0) // for speedup

  def calculateConflicts(queenI: Int, queenJ: Int, board: ArrayBuffer[ArrayBuffer[Char]]): Int = {
    val verticalConflicts: Int = board.count(x => x(queenJ) == '*') - 1

    val leftConflicts = {
      val min = Math.min(queenI, queenJ)
      val startposI = queenI - min
      val startposJ = queenJ - min
      val max = Math.max(startposI, startposJ)

      (for (i <- 0 until (N - max))
        yield board(startposI + i)(startposJ + i) == '*').count(x => x) - 1
    }

    val rightConflicts: Int = {
      val min = Math.min(queenI, (N-1) - queenJ)
      val startposI = queenI - min
      val startposJ = queenJ + min
      val max = Math.max(N - startposI, startposJ)

      (for (i <- 0 until (N - max))
          yield board(startposI + i)(startposJ - i) == '*').count(x => x) - 1
    }

    verticalConflicts + leftConflicts + rightConflicts
  }

  def moveQueen(queenNum: Int, board: ArrayBuffer[ArrayBuffer[Char]]): ArrayBuffer[ArrayBuffer[Char]] = {
    // magic number
    val (_, queenPos) = (0 until N).foldLeft(500,0) {
      case ((min, index), qPos) =>
        val r = calculateConflicts(queenNum, qPos, board)
        if(r < min) (r, qPos)
        else (min, index)
    }

    board(queenNum) = ArrayBuffer.fill(N)('_')
    board(queenNum)(queenPos) = '*'
    queensPositions(queenNum) = queenPos

    board
  }

  @tailrec
  def iter(board: ArrayBuffer[ArrayBuffer[Char]], tries: Int): Boolean = {

    val (conflictsCount, queenIndex): (Int, Int) = (0 until N).foldLeft(0,0) {
      case ((max, index), queenNum) =>
        val r = calculateConflicts(queenNum, queensPositions(queenNum), board)
        if (r > max) (r, queenNum)
        else (max, index)
    }

    if(tries == 0) false
    else {
      if (conflictsCount != 0) {
        iter(moveQueen(queenIndex, board), tries - 1)
      } else {
        true
      }
    }
  }

  def solve(): Unit = {

    val board: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer.fill(N)(ArrayBuffer.fill(N)('_'))

    for (i <- 0 until N) {
      val n = Random.nextInt(N) // [0; N)
      board(i)(n) = '*'
      queensPositions(i) = n
    }

    if(!iter(board, 40)) {
      solve()
    } else
      board.foreach(println)
  }

  val startTime = System.currentTimeMillis
  solve()
  val endTime = System.currentTimeMillis
  println("Ended in " + (endTime - startTime))
}
