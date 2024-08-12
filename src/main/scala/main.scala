import Matrix.*
import Arrays.*
@main
def main(): Unit = {

  val k = 3
  val nums = Array(4, 5, 8, 2)
  val `val` = 3
  val obj = new KthLargest(k, nums)
  val param_1 = obj.add(`val`)
  println(param_1)

  val param_2 = obj.add(5)
  println(param_2)

}


