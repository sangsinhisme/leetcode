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
    val pq = mutable.PriorityQueue.empty[Int](Ordering.by(+_))

    for (i <- nums.indices; j <- i + 1 until nums.length) {
      val distance = Math.abs(nums(i) - nums(j))
      if (pq.size < k) {
        pq.enqueue(distance)
      } else if (distance < pq.head) {
        pq.dequeue()
        pq.enqueue(distance)
      }
    }
    pq.head
  }

}
