package reposerecord

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, ZoneId}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object ReposeRecord {

  val INPUT_RESOURCE = "reposerecord/input.txt"

  private def getRotaList = Source.fromResource(INPUT_RESOURCE).getLines().toList

  private val DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val ID_PATTERN: Regex = "#[0-9]+".r
  val END_OF_TIMESTAMP = 17

  def parseDate(line : String) : (LocalDateTime, String) = {
    val (strDate, strAction) = line.splitAt(END_OF_TIMESTAMP)
    (LocalDateTime.parse(strDate.substring(1), DATE_FORMATTER), strAction)
  }

  def attempt(args: Array[String]): Unit = {
    val sleepTime = new mutable.HashMap[String, Long]()

    var guardId = new String

    var asleepTime = LocalDateTime.now()

    for (line <- getRotaList
      .map(parseDate)
      .sortBy(_._1.atZone(ZoneId.systemDefault()).toEpochSecond)) {
      val action = line._2
      if (action.contains("Guard")) {
        guardId = ID_PATTERN.findFirstIn(action).get
      } else if (action.contains("asleep")) {
        asleepTime = line._1
      } else if (action.contains("wakes")) {
        sleepTime(guardId) = ChronoUnit.MINUTES.between(asleepTime, line._1)
      }
    }

    println(sleepTime.toList.sortBy(_._2))
  }

  def main(args: Array[String]): Unit = {
    val sleepTime = new mutable.HashMap[String, mutable.HashMap[Int, Int]]()

    var guardId = new String

    var asleepTime = LocalDateTime.now()

    val total = 0

    for (line <- getRotaList
      .map(parseDate)
      .sortBy(_._1.atZone(ZoneId.systemDefault()).toEpochSecond)) {
      val action = line._2
      if (action.contains("Guard")) {
        guardId = ID_PATTERN.findFirstIn(action).get
      } else if (action.contains("asleep")) {
        asleepTime = line._1
      } else if (action.contains("wakes")) {
        while (asleepTime.isBefore(line._1)) {
          if (!sleepTime.contains(guardId)) sleepTime(guardId) = new mutable.HashMap[Int, Int]()
          if (sleepTime(guardId).contains(asleepTime.getMinute)) {
            sleepTime(guardId)(asleepTime.getMinute)+=1
          } else {
            sleepTime(guardId)(asleepTime.getMinute) = 1
          }
          asleepTime = asleepTime.plus(1, ChronoUnit.MINUTES)
        }
      }
    }
    //    "#1433" 50 mins

    println(sleepTime("#947").toList.sortBy(_._2).reverse)
    println(sleepTime.map(i => (i._1, i._2.values.toList.sorted.reverse.head)))
//    sleepTime.foreach(i => {
//      println(i._1, i._2.values.sum)
//    })

  }

}
