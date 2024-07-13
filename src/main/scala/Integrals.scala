import scala.io.StdIn.readLine
import scala.math.BigDecimal.double2bigDecimal

object Integrals {
  // This function will be used while invoking "Summation" to compute
  // The area under the curve.
  def f(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    (coefficients zip powers).foldLeft(0.0)((sum, pair) => sum + pair._1 * Math.pow(x, pair._2))
  }

  // This function will be used while invoking "Summation" to compute
  // The Volume of revolution of the curve around the X-Axis
  // The 'Area' referred to here is the area of the circle obtained
  // By rotating the point on the curve (x,f(x)) around the X-Axis
  def area(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    //Fill Up this function body
    // To compute the area of the circle on revolving the point
    // (x,f(x)) around the X-Axis
    // For the given coefficients, powers and value of x
    Math.PI * Math.pow(f(coefficients, powers, x), 2)
  }

  // This is the part where the series is summed up
  // This function is invoked once with func = f to compute the area 	     // under the curve
  // Then it is invoked again with func = area to compute the volume
  // of revolution of the curve
  def summation(func: (List[Int], List[Int], Double) => Double, upperLimit: Int, lowerLimit: Int, coefficients: List[Int], powers: List[Int]): Double = {
    // Fill up this function
    (lowerLimit * 1d to upperLimit * 1d by 0.001d).map(x => 0.001d * func(coefficients, powers, x.toDouble)).sum
  }

  def displayAnswers(coefficients: List[Int], powers: List[Int], limits: List[Int]): List[String] = {
    List(f"${summation(f, limits.reverse.head, limits.head, coefficients, powers)}%.1f",
    f"${summation(area, limits.reverse.head, limits.head, coefficients, powers)}%.1f")
  }

}
