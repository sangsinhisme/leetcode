import BinaryTree.*
import ValidMatrix.*
@main
def main(): Unit = {

  val rowSum = Array(5,7,10)
  val colSum = Array(8,6,8)
  print(restoreMatrix(rowSum, colSum).map(_.mkString(" ")).mkString("\n"))

}


