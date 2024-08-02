import scala.annotation.tailrec
import scala.util.matching.Regex

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
}
