import scala.annotation.tailrec
import scala.collection.mutable

object Combines {

  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    def backtrack(remaining: List[Int], path: List[Int], target: Int): List[List[Int]] = {
      if target < 0 then Nil
      else if target == 0 then List(path)
      else remaining match
        case Nil => Nil
        case x :: xs =>
          backtrack(xs, x :: path, target - x) :::
            backtrack(xs dropWhile (_ == x), path, target)
    }

    backtrack(candidates.sorted.toList, Nil, target)
  }

  def smallestDistancePair(nums: Array[Int], k: Int): Int = {
    // Sort the input array
    val sortedNums = nums.sorted

    // Define a helper function to count pairs with distance <= mid
    def countPairs(mid: Int): Int = {
      var count = 0
      var j = 0
      for (i <- sortedNums.indices) {
        while (j < sortedNums.length && sortedNums(j) - sortedNums(i) <= mid) {
          j += 1
        }
        count += (j - i - 1)
      }
      count
    }

    // Binary search on the distance
    var low = 0
    var high = sortedNums.last - sortedNums.head
    while (low < high) {
      val mid = (low + high) / 2
      if (countPairs(mid) >= k) {
        high = mid
      } else {
        low = mid + 1
      }
    }

    low
  }

}
