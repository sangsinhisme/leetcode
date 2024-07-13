import Utils._
import Integrals._
@main
def main(): Unit = {

  val listInput = readFile("01.txt")
  val coefficients: List[Int] = string2List[Int](elem => elem.toInt)(listInput.head)
  val powers: List[Int] = string2List[Int](elem => elem.toInt)(listInput.take(2).last)
  val limits: List[Int] = string2List[Int](elem => elem.toInt)(listInput.last)

  for (ans <- displayAnswers(coefficients, powers, limits)) println(ans)
}


//def main(args: Array[String]): Unit = {
//  /** Purely IO Section * */
//  displayAnswers(readLine().trim().split(" ").toList.map(_.toInt), readLine().trim().split(" ").toList.map(_.toInt), readLine().trim().split(" ").toList.map(_.toInt))
//}