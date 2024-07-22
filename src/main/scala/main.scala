import BinaryTree.*
import Matrix.*
import Arrays.*
@main
def main(): Unit = {

  val names = Array("Alice","Bob","Bob")
  val heights = Array(155,185,150)
  println(sortPeople(names, heights).mkString("Array(", ", ", ")"))
}


