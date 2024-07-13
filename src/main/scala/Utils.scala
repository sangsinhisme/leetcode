import scala.io.Source
import scala.reflect.ClassTag

object Utils {

  private val inputPath = "src/main/resources/input/"
  private val outputPath = "src/main/resources/output/"
  def readFile(fileName: String): List[String] = {
    val source = Source.fromFile(inputPath + fileName)
    var output: List[String] = List.empty
    for (line <- source.getLines())
      output = output :+ line
    output
  }

  def string2List[A:ClassTag](f: String => A)(input: String): List[A] = {
    input.split(" ").map(elem => f(elem)).toList
  }
}
