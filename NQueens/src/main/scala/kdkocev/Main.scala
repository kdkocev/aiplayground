package kdkocev

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main extends App {

  val N = 30

  // modify to return queen index as well as conflicts
  def calculateConflicts(queenNum: Int, board: ArrayBuffer[ArrayBuffer[Char]], ypoint: Option[Int] = None): Int = {
    val queenI: Int = queenNum
  // TODO fix this implementation to be cleaner
    val queenJ: Int =
      if(ypoint.isEmpty)
        (for((i, index) <- board(queenI).zipWithIndex if i == '*') yield index).head
      else
        ypoint.get

    val verticalConflicts: Int = board.count(x => x(queenJ) == '*') - 1

    val leftConflicts = {
      val min = List(queenI, queenJ).min
      val startposI = queenI - min
      val startposJ = queenJ - min

      (for (i <- 0 until N if startposI + i < N && startposJ + i < N)
        yield board(startposI + i)(startposJ + i) == '*').count(x => x) - 1
    }

    val rightConflicts: Int = {
      val min = List(queenI, (N-1) - queenJ).min
      val startposI = queenI - min
      val startposJ = queenJ + min

      (for (i <- 0 until N if startposI + i < N && startposJ - i >= 0)
          yield board(startposI + i)(startposJ - i) == '*').count(x => x) - 1
    }

    verticalConflicts + leftConflicts + rightConflicts
  }

  def moveQueen(queenNum: Int, board: ArrayBuffer[ArrayBuffer[Char]]): ArrayBuffer[ArrayBuffer[Char]] = {
    val v = (0 until N).map{
      x => calculateConflicts(queenNum, board, Some(x))
    }

    val pos = v.zipWithIndex.minBy{case (x, _) => x}

    board(queenNum) = board(queenNum).zipWithIndex.map {
      case (x, index) if index == pos._2 => '*'
      case _ => '_'
    }

    board
  }

  @tailrec
  def iter(board: ArrayBuffer[ArrayBuffer[Char]], tries: Int): Boolean = {

    val conflicts = 0 until N map (x => calculateConflicts(x, board))

    val maxConflictsQueen = conflicts.zipWithIndex.maxBy {case (x, _) => x}

    if(tries == 0) false
    else {
      if (maxConflictsQueen._1 != 0) {
        iter(moveQueen(maxConflictsQueen._2, board), tries - 1)
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
    }

    if(!iter(board, 100)) {
      solve()
    } else
      board.foreach(println)
  }

  val startTime = System.currentTimeMillis
  solve()
  val endTime = System.currentTimeMillis
  println("Ended in " + (endTime - startTime))
}
