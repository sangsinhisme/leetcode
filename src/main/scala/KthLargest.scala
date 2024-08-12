import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

class KthLargest(_k: Int, _nums: Array[Int]) {

  // Min-heap to store the k largest elements
  private val pq = mutable.PriorityQueue.empty[Int](Ordering.by(-_)) // Min-heap

  // Initialize the heap with up to k elements from the input array
  _nums.foreach(add)

  def add(`val`: Int): Int = {
    if (pq.size < _k) {
      pq.enqueue(`val`)
    } else if (`val` > pq.head) {
      pq.dequeue()
      pq.enqueue(`val`)
    }
    pq.head
  }
}


/**
 * Your KthLargest object will be instantiated and called as such:
 * val obj = new KthLargest(k, nums)
 * val param_1 = obj.add(`val`)
 */