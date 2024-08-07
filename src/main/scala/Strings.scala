import scala.util.parsing.json.JSON

import scala.annotation.tailrec
import scala.collection.mutable

import scala.language.postfixOps

object Strings {

  @tailrec
  def gcdOfStrings(str1: String, str2: String): String = {
    if (str1.length < str2.length) gcdOfStrings(str2, str1)
    else if (!str1.startsWith(str2)) ""
    else if (str2.isEmpty) str1
    else gcdOfStrings(str1.takeRight(str1.length - str2.length), str2)
  }

  def mergeAlternately(word1: String, word2: String): String = {
    val word1Length = word1.length
    val word2Length = word2.length

    val subLength = if (word1Length < word2Length) word2.substring(word1Length, word2Length)
    else if (word1Length > word2Length) word1.substring(word2Length, word1Length)
    else ""
    (word1.toList zip word2.toList).foldLeft("")(
      (x, y) => x + s"${y._1}" + s"${y._2}"
    ) + subLength
  }

  def minimumPushes(word: String): Int = {
    val countString = mutable.LinkedHashMap.empty[Char, Int]
    for (i <- word) {
      countString.get(i) match
        case Some(value) => countString.put(i, value + 1)
        case _ => countString.put(i, 1)
    }
    countString.values.toSeq.sortWith((x, y) => x > y).zipWithIndex.foldLeft(0)(
      (sum, values) => sum + values._1 * ((values._2 + 8) / 8))
  }

  def numberToWords(num: Int): String = {
    val jsonString =
      """{
      "0": "Zero",
      "1": "One",
      "2": "Two",
      "3": "Three",
      "4": "Four",
      "5": "Five",
      "6": "Six",
      "7": "Seven",
      "8": "Eight",
      "9": "Nine",
      "10": "Ten",
      "11": "Eleven",
      "12": "Twelve",
      "13": "Thirteen",
      "14": "Fourteen",
      "15": "Fifteen",
      "16": "Sixteen",
      "17": "Seventeen",
      "18": "Eighteen",
      "19": "Nineteen",
      "20": "Twenty",
      "30": "Thirty",
      "40": "Forty",
      "50": "Fifty",
      "60": "Sixty",
      "70": "Seventy",
      "80": "Eighty",
      "90": "Ninety",
      "100": "Hundred",
      "1000": "Thousand",
      "1000000": "Million",
      "1000000000": "Billion"
    }"""

    val parsedJson = JSON.parseFull(jsonString).get.asInstanceOf[Map[String, String]]
    val englishMap: Map[Int, String] = parsedJson.map { case (k, v) => (k.toInt, v) }

    def helper(nums: Int): String = {
      if (nums == 0) {
        ""
      } else if (nums <= 20) {
        englishMap(nums)
      } else if (nums < 100) {
        val digits = (nums / 10) * 10
        englishMap(digits) + " " + helper(nums % 10)
      } else {
        var numsZero = nums.toString.length - 1
        if (numsZero >= 9) numsZero = Math.pow(10, 9).toInt
        else if (numsZero >= 6) numsZero = Math.pow(10, 6).toInt
        else if (numsZero >= 3) numsZero = Math.pow(10, 3).toInt
        else numsZero = Math.pow(10, numsZero).toInt
        helper(nums / numsZero) + " " + englishMap(numsZero) + " " + helper(nums % numsZero)
      }

    }
    helper(num)
  }
}
