import java.io.File

import ammonite.ops._
import io.circe.Json
import io.circe.parser._

object Main extends App {

  val maxGap = 0.5

  val inPath = args.headOption
      .map(Path.expandUser(_))
      .filter(_.ext == "cast")
      .filter(exists)
      .getOrElse(throw new Exception("You need to specify an existing file"))

  val outPath = inPath/up/(inPath.baseName + "-fixed." + inPath.ext)

  val inLines = read.lines(inPath)

  val header = inLines.head

  val body = inLines.drop(1).map { line =>
    parse(line).toOption.flatMap(_.asArray).getOrElse(throw new Exception(s"can't parse $line"))
  }.sliding(2).foldLeft((0.0, Seq.empty[String])) {
    case ((removedGap, lines), Seq(prev, next)) =>
      val gap = next.head.asNumber.get.toDouble - prev.head.asNumber.get.toDouble
      val newRemovedGap = if (gap > maxGap) {
        removedGap + gap - maxGap
      }
      else {
        removedGap
      }

      val newTimestamp = next.head.asNumber.get.toDouble - newRemovedGap

      val newLine = next.updated(0, Json.fromDouble(newTimestamp).get)

      (newRemovedGap, lines :+ Json.arr(newLine:_*).noSpaces)
  }._2.dropRight(1)

  val outLines = inLines.head +: body

  write.over(outPath, outLines.mkString("\n"))

}
