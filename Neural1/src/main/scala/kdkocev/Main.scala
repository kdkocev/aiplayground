package kdkocev

// a neuron should be a function that takes the prev layer
// and spits out a number between 0 and 1
// Every neuron has a bias, a weight for every connection to it
// and knows the results from the activation of the prev neurons that connect
// to it

object Main extends App {

  // two inputs 1 output

  val weights: List[List[List[Double]]] = List(
    // level 1
    List(
      List(0.5, 0.5),
      List(0.5, 0.5)
    ),
    // level 2
    List(
      List(-1.2, 1.0)
    )
  )

  val biases: List[List[Double]] = List(
    List(0.75, 0.1),
    List(0.1)
  )

  // binary activation function because we need a binary result
  def sig(x: Double): Double = {
    if(x >= 0) 1
    else 0
  }

  def calc(activations: List[Double], level: Int = 0): Unit = {
    val res = (for {
//      w <- weights(level)
      j <- 0 until weights(level).length
    } yield {
      val w = weights(level)(j)
      val b = biases(level)(j)
      val r = (for {
        i <- 0 until w.length
      } yield w(i) * activations(i)).sum - b

      sig(r)
    }).toList

//    println(res)

    if(level < weights.length - 1) {
      calc(res, level + 1)
    } else {
      println(res)
    }
  }

  calc(List(0,0))
  calc(List(0,1))
  calc(List(1,0))
  calc(List(1,1))


}
