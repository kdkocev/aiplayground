package kdkocev

import scala.annotation.tailrec

object Algo {
  // Just the start string
  def makeStart(count: Int): String = ">" * count + "_" + "<" * count

  // Just the end string
  def makeEnd(count: Int): String = "<" * count + "_" + ">" * count

  // Calculate the next possible states from `state` applying the list of rules
  // Note: >>_ to _>> is never a good rule so it's ignored along with _<< to <<_
  val rules = List((">_", "_>"), ("_<", "<_"), ("_><", "<>_"), ("><_", "_<>"))
  def calculatePossibleTransitions(state: String): List[String] =
    rules.collect {
      case (rule, replacement) if state.contains(rule) => state.replace(rule, replacement)
    }

  // Backtrack the parents list
  @tailrec
  def trackPath(current: String, parents: Map[String, String], path: List[String]): List[String] =
  parents.get(current) match {
    case Some(`current`) => current :: path
    case Some(x) => trackPath(x, parents, current :: path)
    case _ => sys.error(s"Parent for $current not found in parents list")
  }

  /**
    * The core logic
    * a.k.a BFS
    * @param queue with states to execute
    * @param parents parents map
    * @param endString the state we search for
    * @return the path
    */
  @tailrec
  def solveBFS(queue: List[String], parents: Map[String, String], endString: String): List[String] = queue match {
    case Nil => Nil // No nodes passed in
    case `endString` :: _  => trackPath(endString, parents, Nil)
    case head :: tail =>
      val children = calculatePossibleTransitions(head)
      solveBFS(tail ++ children, parents ++ children.map((_, head)), endString)
  }

  /**
    * The core logic
    * a.k.a DFS
    * @param stack with states to execute
    * @param parents parents map
    * @param endString the state we search for
    * @return the path
    */
  def solveDFS(stack: List[String], parents: Map[String, String], endString: String): List[String] = stack match {
    case Nil => Nil // No nodes passed in
    case x =>
      val last = x.last
      if (last == endString) trackPath(endString, parents, Nil)
      else {
        val children = calculatePossibleTransitions(last)
        solveDFS(stack.init ++ children, parents ++ children.map((_, last)), endString)
      }
  }

  def solveFor(frogsCount: Int): List[String] = {
    val startString = makeStart(frogsCount)
    val endString = makeEnd(frogsCount)

    // Add the start node in the queue and add it as its own parent
    solveDFS(startString :: Nil, Map(startString -> startString), endString)
  }
}

object Main extends App {
  // Note: Will throw an exception if something other than an Int is entered
  val frogsCount: Int = scala.io.StdIn.readLine("frogsCount = ").toInt

  // Run the algorithm and measure execution time
  val startTime = System.currentTimeMillis()
  val result = Algo.solveFor(frogsCount)
  val endTime = System.currentTimeMillis()

  // Print the results
  result.foreach(println)

  println(s"Elapsed Time = ${endTime - startTime} milliseconds")
}
