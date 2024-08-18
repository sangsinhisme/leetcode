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

  def numMagicSquaresInside(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length
    if (rows < 3 || cols < 3) return 0

    def helper(i: Int, j: Int): Boolean = {
      val nums: Set[Int] = (
        for {
          x <- i until i + 3
          y <- j until j + 3
        } yield grid(x)(y)).toSet
      if (nums.size != 9 || nums.exists(elem => elem < 1 || elem > 9)) return false

      val rows = (for (x <- i until i + 3) yield grid(x).slice(j, j + 3)).toList
      val cols = (for (y <- j until j + 3) yield (i until i + 3).map(x => grid(x)(y))).toList
      val diag1 = (0 until 3).map(d => grid(i + d)(j + d))
      val diag2 = (0 until 3).map(d => grid(i + d)(j + 2 - d))

      rows.forall(_.sum == 15) &&
        cols.forall(_.sum == 15) &&
        diag1.sum == 15 &&
        diag2.sum == 15
    }

    var count = 0
    for {
      i <- 0 until rows - 2
      j <- 0 until cols - 2
    } {
      if (helper(i, j)) count += 1
    }
    count
  }
  def regionsBySlashes(grid: Array[String]): Int = {
    val n = grid.length
    val m = grid(0).length
    val graph: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer.fill(n * 3, m * 3)(0)
    val visited = mutable.ArrayBuffer.fill(n * 3, m * 3)(false)

    for (i <- 0 until n) {
      for (j <- 0 until m) {
        if (grid(i)(j) == '/') {
          graph(i * 3)(j * 3 + 2) = 1
          graph(i * 3 + 1)(j * 3 + 1) = 1
          graph(i * 3 + 2)(j * 3) = 1
        }
        else if (grid(i)(j) == '\\') {
          graph(i * 3)(j * 3) = 1
          graph(i * 3 + 1)(j * 3 + 1) = 1
          graph(i * 3 + 2)(j * 3 + 2) = 1
        }
      }
    }

    def dfs(x: Int, y: Int): Unit = {
      val directions = Set((1, 0), (-1, 0), (0, 1), (0, -1))
      val stack = mutable.Stack((x, y))
      while (stack.nonEmpty) {
        val (cx, cy) = stack.pop()
        for ((dx, dy) <- directions) {
          val nx = cx + dx
          val ny = cy + dy
          if (nx >= 0 && nx < n * 3 && ny >= 0 && ny < m * 3 && graph(nx)(ny) == 0 && !visited(nx)(ny)) {
            visited(nx)(ny) = true
            stack.push((nx, ny))
          }
        }
      }
    }

    var regionCount = 0

    for (i <- 0 until n * 3; j <- 0 until m * 3) {
      if (graph(i)(j) == 0 && !visited(i)(j)) {
        visited(i)(j) = true
        dfs(i, j)
        regionCount += 1
      }
    }

    regionCount
  }


  def minDays(grid: Array[Array[Int]]): Int = {
    val n = grid.length
    val m = grid(0).length
    val visited = mutable.ArrayBuffer.fill(n, m)(0)

    def dfs(x: Int, y: Int): Unit = {
      val directions = Set((1, 0), (-1, 0), (0, 1), (0, -1))
      val stack = mutable.Stack((x, y))
      while (stack.nonEmpty) {
        val (cx, cy) = stack.pop()
        var linked = 0
        for ((dx, dy) <- directions) {
          val nx = cx + dx
          val ny = cy + dy
          if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid(nx)(ny) == 1) {
            if (visited(nx)(ny) > 0)
              linked += 1
            else
              visited(nx)(ny) = 1
              linked += 1
              stack.push((nx, ny))
          }
        }
        visited(cx)(cy) = linked
      }
    }

    var regionCount = 0

    for (i <- 0 until n ; j <- 0 until m) {
      if (grid(i)(j) == 1 && visited(i)(j) == 0) {
        visited(i)(j) = 1
        dfs(i, j)
        regionCount += 1
      }
    }
    if (visited.isEmpty) 0
    else if (visited.flatten.count(_ > 0) == 2) 2
    else {
      if (regionCount > 1) 0
      else {
        if (visited.flatten.min == 1) 1 else 2
      }
    }
  }

  def minDays2(grid: Array[Array[Int]]): Int = {
    val n = grid.length
    val m = grid(0).length

    if (n == 0 || m == 0) return 0

    def bfs(startX: Int, startY: Int, visited: Array[Array[Boolean]]): List[(Int, Int)] = {
      val directions = List((1, 0), (-1, 0), (0, 1), (0, -1))
      val queue = mutable.Queue((startX, startY))
      val component = mutable.ListBuffer[(Int, Int)]()
      visited(startX)(startY) = true
      component += ((startX, startY))

      while (queue.nonEmpty) {
        val (x, y) = queue.dequeue()
        for ((dx, dy) <- directions) {
          val nx = x + dx
          val ny = y + dy
          if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid(nx)(ny) == 1 && !visited(nx)(ny)) {
            visited(nx)(ny) = true
            queue.enqueue((nx, ny))
            component += ((nx, ny))
          }
        }
      }
      component.toList
    }

    def countComponents(): Int = {
      val visited = Array.fill(n, m)(false)
      var componentCount = 0
      for (i <- 0 until n; j <- 0 until m) {
        if (grid(i)(j) == 1 && !visited(i)(j)) {
          bfs(i, j, visited)
          componentCount += 1
        }
      }
      componentCount
    }

    def canDisconnectAllRegions: Boolean = {
      var flag = false
      var total = 0
      for (i <- 0 until n; j <- 0 until m) {
        if (grid(i)(j) == 1) {
          total += 1
          grid(i)(j) = 0
          if (countComponents() > 1) {
            grid(i)(j) = 1
            flag = true
          }
          grid(i)(j) = 1
        }
      }
      flag || (total == 1)
    }

    val originalCount = countComponents()
    if (originalCount == 0) return 0
    if (originalCount > 1) return 0
    if (canDisconnectAllRegions) return 1
    2
  }

  def lemonadeChange(bills: Array[Int]): Boolean = {
    var coin5 = 0
    var coin10 = 0
    var canGiveChange = true

    for (coin <- bills if canGiveChange) {
      coin match {
        case 5 => coin5 += 1
        case 10 =>
          if (coin5 == 0) canGiveChange = false
          else {
            coin5 -= 1
            coin10 += 1
          }
        case 20 =>
          if (coin10 > 0 && coin5 > 0) {
            coin10 -= 1
            coin5 -= 1
          } else if (coin5 >= 3) {
            coin5 -= 3
          } else {
            canGiveChange = false
          }
      }
    }
    canGiveChange
  }

  def maxDistance(arrays: List[List[Int]]): Int = {
    // Initialize with the first array's min and max
    var minVal = arrays.head.head
    var maxVal = arrays.head.last
    var maxDist = 0

    // Iterate over the rest of the arrays
    for (i <- 1 until arrays.length) {
      val currentMin = arrays(i).head
      val currentMax = arrays(i).last

      // Calculate the maximum distance using current array's values and global min/max
      maxDist = Math.max(maxDist, Math.max(Math.abs(currentMax - minVal), Math.abs(maxVal - currentMin)))

      // Update global min and max
      minVal = Math.min(minVal, currentMin)
      maxVal = Math.max(maxVal, currentMax)
    }

    maxDist
  }

  // Dimanche, 18 août 2024
  /**
   An ugly number is a positive integer whose prime factors are limited to 2, 3, and 5.
   Given an integer n, return the nth ugly number.
  */
  def nthUglyNumber(n: Int): Int = {
    val heap = mutable.PriorityQueue[Long]()(Ordering.by(-_)) // Min-heap
    val seen = mutable.HashSet[Long]()

    // Initial ugly number
    heap.enqueue(1)
    seen.add(1)

    var ugly = 1L

    for (_ <- 1 until n) {
      ugly = heap.dequeue()

      // Generate the next ugly numbers
      val nextUglyNumbers = List(ugly * 2, ugly * 3, ugly * 5)

      for (num <- nextUglyNumbers) {
        if (!seen.contains(num)) {
          heap.enqueue(num)
          seen.add(num)
        }
      }
    }

    heap.dequeue().toInt
  }
}
