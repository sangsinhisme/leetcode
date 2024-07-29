import scala.annotation.tailrec
import scala.collection.mutable

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

  def frequencySort(nums: Array[Int]): Array[Int] = {
    val countMap = mutable.HashMap.empty[Int, Int]
    for (i <- nums) {
      countMap.get(i) match
        case Some(value) => countMap.put(i, value + 1)
        case _ => countMap.put(i, 1)
    }

    val sortSeq = countMap.toSeq.sortWith {
      case ((key1, value1), (key2, value2)) =>
        if (value1 != value2) value1 < value2
        else key1 > key2
    }


    def helper(map: Seq[(Int, Int)]): Array[Int] = {
      if map.isEmpty then Array.empty
      else map.head match
        case null => Array.empty
        case (key, value) => Array.fill(value)(key) ++ helper(map.tail)
    }

    helper(sortSeq)
  }
}
