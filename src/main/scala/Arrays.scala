object Arrays {
  def sortPeople(names: Array[String], heights: Array[Int]): Array[String] = {
    def helper(curNames: Array[String], curHeights: Array[Int]): Array[String] = {
      if curNames.isEmpty then Array.empty
      else
        val maxIndex = curHeights.indexOf(curHeights.max)
        val length = curNames.length
        val newNames = curNames.take(maxIndex) ++ curNames.takeRight(length - maxIndex).tail
        val newHeights = curHeights.take(maxIndex) ++ curHeights.takeRight(length - maxIndex).tail
        Array(curNames(maxIndex)) ++ helper(newNames, newHeights)
    }

    helper(names, heights)
  }
}
