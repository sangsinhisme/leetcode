import scala.annotation.tailrec

object Matrix {
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

  def buildMatrix(k: Int, rowConditions: Array[Array[Int]], colConditions: Array[Array[Int]]): Array[Array[Int]] = {
    val nodes: List[Int] = (1 to k).toList

    def helper(conditions: Array[Array[Int]], nodes: List[Int]): List[Int] = {
      val unConditionNode = nodes.filter(
        elem => !conditions.exists(_.contains(elem))
      )
      if unConditionNode.nonEmpty then {
        if conditions.length == 0 then unConditionNode
        else unConditionNode ++ helper(conditions, nodes.filter(!unConditionNode.contains(_)))
      }
      else {
        val topElem = nodes.filter(
          elem => !conditions.exists(x => x(1) == elem)
        )
        if (topElem.isEmpty) return List.empty
        val newConditions = conditions.filter(x => !topElem.contains(x.head))
        val newNodes = nodes.filter(!topElem.contains(_))
        topElem ++ helper(newConditions, newNodes)
      }
    }

    val output = Array.ofDim[Int](k, k)
    val indexRow = helper(rowConditions, nodes)
    val indexCol = helper(colConditions, nodes)

    if (indexRow.isEmpty || indexCol.isEmpty || indexRow.length != k || indexCol.length != k ) return Array.empty
    for (elem <- nodes) {
      output(indexRow.indexOf(elem))(indexCol.indexOf(elem)) = elem
    }
    output
  }
}
