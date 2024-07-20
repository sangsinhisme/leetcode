import scala.annotation.tailrec

object ValidMatrix {
  def restoreMatrix(rowSum: Array[Int], colSum: Array[Int]): Array[Array[Int]] = {
    @tailrec
    def help(rowSum: Array[Int], colSum: Array[Int], output: Array[Array[Int]]): Array[Array[Int]] = {
      val minRowValue = rowSum.filterNot(_ == 0) match
        case Array() => -1
        case arr => arr.min
      val minColValue: Int = colSum.filterNot(_ == 0) match
        case Array() => -1
        case arr => arr.min
      if (minColValue == -1 && minRowValue == -1) return output

      val minRowIndex = rowSum.indexOf(minRowValue)
      val minColIndex = colSum.indexOf(minColValue)

      output(minRowIndex)(minColIndex) = if (minRowValue < minColValue) minRowValue else minColValue
      rowSum(minRowIndex) = minRowValue - output(minRowIndex)(minColIndex)
      colSum(minColIndex) = minColValue - output(minRowIndex)(minColIndex)
      help(rowSum, colSum, output)
    }
    help(rowSum, colSum, Array.ofDim[Int](rowSum.length,colSum.length))
  }
}
