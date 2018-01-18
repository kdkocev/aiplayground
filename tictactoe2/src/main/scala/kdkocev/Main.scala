package kdkocev

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

object Main extends App {

  val USER = 'O'
  val COMPUTER = 'X'
  var calculations = 0

  var matrix = ArrayBuffer(
    ArrayBuffer(' ', ' ', ' '),
    ArrayBuffer(' ', ' ', ' '),
    ArrayBuffer(' ', ' ', ' ')
  )

  def prettyPrint(matrix: ArrayBuffer[ArrayBuffer[Char]]): Unit = {
    matrix.foreach{ x =>
      println("-------------")
      println("| " + x.mkString(" | ") + " |")
    }
    println("-------------")
  }

  def boardIsFilled(board: ArrayBuffer[ArrayBuffer[Char]]): Boolean = {
    board.forall(x => !x.contains(' '))
  }

  def getWinner(matrix: ArrayBuffer[ArrayBuffer[Char]]): Option[Char] = {
    val horizontal =
      matrix
        .find(_.forall(_ == COMPUTER))
        .orElse(matrix.find(_.forall(_ == USER)))
    val vertical = (for {
      x <- 0 until 3
    } yield (
      matrix(0)(x), matrix(0)(x) != ' ' && matrix(0)(x) == matrix(1)(x) && matrix(1)(x) == matrix(2)(x)
    )).filter(_._2 == true)
    val left = matrix(0)(0) != ' ' && matrix(0)(0) == matrix(1)(1) && matrix(1)(1) == matrix(2)(2)
    val right = matrix(2)(0) != ' ' && matrix(2)(0) == matrix(1)(1) && matrix(1)(1) == matrix(0)(2)

    if(horizontal.isDefined) horizontal.map(_.head)
    else if(vertical.nonEmpty) vertical.headOption.map(_._1)
    else if(left) Some(matrix(1)(1))
    else if(right) Some(matrix(1)(1))
    else None
  }

  def gameIsFinished(): Boolean = {
    getWinner(matrix).nonEmpty || boardIsFilled(matrix)
  }

  def makeAMove(x: Int, y: Int): Unit = {
    matrix(x-1)(y-1) = USER
  }

  def calculatePrice(board: ArrayBuffer[ArrayBuffer[Char]], isMax: Boolean, isFirst: Boolean, depth: Int): (Int, ArrayBuffer[ArrayBuffer[Char]]) = {
    calculations += 1

    val g = getWinner(board)
    if(g.isDefined && g.contains(USER)) (-100 - depth, board)
    else if (g.isDefined && g.contains(COMPUTER)) (100 - depth, board)
    else if(boardIsFilled(board)) (0, board)
    else {
        val moves = for {
        el <- 0 until 9
        if board(el/3)(el%3) == ' '
      } yield {
        val symbol = if(isMax) {
          COMPUTER
        } else {
          USER
        }
        val b: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer(
          ArrayBuffer(board(0)(0), board(0)(1), board(0)(2)),
          ArrayBuffer(board(1)(0), board(1)(1), board(1)(2)),
          ArrayBuffer(board(2)(0), board(2)(1), board(2)(2)),
        )

        b(el/3)(el%3) = symbol

        calculatePrice(b, !isMax, false, depth + 1)
      }

      val move = if(isMax) {
        moves.maxBy(_._1)
      } else {
        moves.minBy(_._1)
      }

      if(isFirst) {
        move
      } else {
        (move._1, board)
      }
    }
  }

  def makeComputerMove(): Unit = {
    val move = calculatePrice(matrix, true, true, 0)
    matrix = move._2
  }

  val scanner = new Scanner(System.in)

  while(!gameIsFinished()) {
    prettyPrint(matrix)
    // User input
    val x = scanner.nextInt()
    val y = scanner.nextInt()

    if(x > 0 && x < 4 && y > 0 && y < 4 && matrix(x-1)(y-1) == ' ') {
      makeAMove(x, y)
      makeComputerMove()
    } else {
      println("Impossible move!")
    }

    println(s"Calculations = $calculations")
    calculations = 0
  }

  prettyPrint(matrix)
  println("Game Ended")
  getWinner(matrix) match {
    case Some(USER) => println("You Win!")
    case Some(COMPUTER) => println("You Lose! Better luck next time!")
    case _ => println("It's a Draw!")
  }
}
