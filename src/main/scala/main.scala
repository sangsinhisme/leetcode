import Matrix.*
@main
def main(): Unit = {

  val rows = 1
  val cols = 1
  val rStart = 0
  val cStart = 0
  spiralMatrixIII(rows, cols, rStart, cStart).foreach(
    elem => println(s"[${elem.mkString(", ")}]")
  )
}


