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

  @tailrec
  def isSorted[T](list: List[T])(implicit ord: Ordering[T]): Boolean = list match {
    case Nil => true // an empty list is sorted
    case x :: Nil => true // a single-element list is sorted
    case x :: xs => ord.lteq(x, xs.head) && isSorted(xs) // if the first two elements are ordered and the rest are sorted, the full list is sorted too
  }

  @tailrec
  def isSortedDesc[T](list: List[T])(implicit ord: Ordering[T]): Boolean = list match {
    case Nil => true // an empty list is sorted
    case x :: Nil => true // a single-element list is sorted
    case x :: xs => ord.gteq(x, xs.head) && isSortedDesc(xs) // if the first two elements are ordered and the rest are sorted, the full list is sorted too
  }


  def numTeams(rating: Array[Int]): Int = {
    var count = 0
    val n = rating.length

    for (i <- 0 until n) {
      for (j <- i + 1 until n) {
        for (k <- j + 1 until n) {
          if ((rating(i) < rating(j) && rating(j) < rating(k)) ||
            (rating(i) > rating(j) && rating(j) > rating(k))) {
            count += 1
          }
        }
      }
    }

    count
  }

  def combinations(arr: Array[Int], k: Int): LazyList[Array[Int]] = {
    if (k == 0) return LazyList(Array())
    if (arr.length < k) return LazyList()

    val head = arr.head
    val tailCombinations = combinations(arr.tail, k - 1).map(head +: _)
    val withoutHeadCombinations = combinations(arr.tail, k)

    tailCombinations ++ withoutHeadCombinations
  }

  def countSeniors(details: Array[String]): Int = {
    var senior = 0
    for (passenger <- details){
      senior += (if passenger(11).asDigit * 10 + passenger(12).asDigit > 60 then 1 else 0)
    }
    senior
  }

  def minSwaps(nums: Array[Int]): Int = {
    val numFixed = nums.count(_ == 1)
    if (numFixed == 0) return 0

    val n = nums.length
    var currentOnes = nums.take(numFixed).count(_ == 1)
    var minSwaps = numFixed - currentOnes

    for (i <- 1 until n) {
      // Slide the window by removing the first element of the previous window
      // and adding the next element in the array (circularly)
      currentOnes = currentOnes - (if (nums(i - 1) == 1) 1 else 0) + (if (nums((i + numFixed - 1) % n) == 1) 1 else 0)
      val swapsNeeded = numFixed - currentOnes
      minSwaps = math.min(minSwaps, swapsNeeded)
    }
    minSwaps
  }

  def canBeEqual(target: Array[Int], arr: Array[Int]): Boolean = {
    target.sorted.sameElements(arr.sorted)
  }

  def rangeSum(nums: Array[Int], n: Int, left: Int, right: Int): Int = {
    val MOD = 1000000007
    val allSums = new collection.mutable.ArrayBuffer[Int]()
    for (i <- nums.indices) {
      var sum = 0
      for (j <- i until nums.length) {
        sum += nums(j)
        allSums.append(sum)
      }
    }
    val sortedSums = allSums.sorted
    var result = 0L
    for (i <- left - 1 until right) {
      result = (result + sortedSums(i)) % MOD
    }

    result.toInt
  }

  def kthDistinct(arr: Array[String], k: Int): String = {
    val countString = mutable.LinkedHashMap.empty[String, Int]
    for (i <- arr) {
      countString.get(i) match
        case Some(value) => countString.put(i, value + 1)
        case _ => countString.put(i, 1)
    }
    val filterCount = countString.filter((_, count) => count == 1)
    if (k > filterCount.size) ""
    else filterCount.toIndexedSeq(k - 1)._1
  }

}
