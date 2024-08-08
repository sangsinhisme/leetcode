import Matrix.*
@main
def main(): Unit = {

  val rows = 5
  val cols = 6
  val rStart = 1
  val cStart = 4
  spiralMatrixIII(rows, cols, rStart, cStart).foreach(
    elem => println(s"[${elem.mkString(", ")}]")
  )
}


