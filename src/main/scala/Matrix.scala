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

  def spiralMatrixIII(rows: Int, cols: Int, rStart: Int, cStart: Int): Array[Array[Int]] = {
    val totals = rows * cols

    @tailrec
    def helper(current: (Int, Int), visited: Array[(Int, Int)], valid: Array[(Int, Int)], vector: (Int, Int), maxRow: Int, maxCol: Int): Array[(Int, Int)] = {
      if (totals == 1) return visited
      if (valid.length == totals) return valid
      val nextVector = vector._1 match
        case 0 => (-1 * vector._2, -1 * vector._1)
        case _ => (vector._2, vector._1)
      var update = current
      var updateVisited = visited
      var updateValid = valid
      if (vector._2 == 0) {
        for (i <- 0 until maxRow) {
          update = (update._1 + vector._2, update._2 + vector._1)
          updateVisited = updateVisited :+ update
          if (update._1 < rows && update._2 < cols && update._1 > -1 && update._2 > -1)
            updateValid = updateValid :+ update
        }
        helper(update, updateVisited, updateValid, nextVector, maxRow + 1, maxCol)
      } else {
        for (i <- 0 until maxCol) {
          update = (update._1 + vector._2, update._2 + vector._1)
          updateVisited = updateVisited :+ update
          if (update._1 < rows && update._2 < cols && update._1 > -1 && update._2 > -1)
            updateValid = updateValid :+ update
        }
        helper(update, updateVisited, updateValid, nextVector, maxRow, maxCol + 1)
      }
    }

    helper((rStart, cStart), Array((rStart, cStart)), Array((rStart, cStart)), (1, 0), 1, 1).map(
      elem => Array(elem._1, elem._2)
    )
  }



}
