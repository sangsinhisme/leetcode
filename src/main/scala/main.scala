import GCDStrings.*
import Utils.*
import Integrals.*
import scala.util.control.Breaks._
@main
def main(): Unit = {

  val word1 = "LEET"
  val word2 = "CODE"
  println(gcdOfStrings(word1, word2))

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    // Map to store numbers and their indices
    val result: Array[Int] = Array.ofDim(2)
    val numMap = scala.collection.mutable.Map[Int, Int]()
    // Iterate through the array
    breakable {

      for (i <- nums.indices) {
        val complement = target - nums(i)

        // Check if complement exists in the map
        if (numMap.contains(complement)) {
          // Return the indices
          result(0) = numMap(complement)
          result(1) = i
          break
        }

        // Otherwise, store the current number and its index in the map
        numMap += (nums(i) -> i)
      }
    }

    // If no solution found, return an empty array (though problem assumes always a solution)
    Array()
  }
}


