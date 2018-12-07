package dayone

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object ChronalCalibration {

  val INPUT_RESOURCE = "chronalcalibration/input.txt"

  private def getOperationsIterator = Source.fromResource(INPUT_RESOURCE).getLines()

  def calculateFreq(): Int = {
    var freq = 0

    getOperationsIterator
      .foreach(line => {
        val (operator, value) = line.splitAt(1)
        if (operator == "+") freq+=value.toInt else freq-=value.toInt
      })

    freq
  }

  def firstDuplicateFreq(): Option[Int] = {
    val freqs = ListBuffer(0)
    var result = -1

    var operations = getOperationsIterator

    while (true) {
      if (operations.hasNext) {
        val (operator, value) = operations.next.splitAt(1)
        if (operator == "+") {
          result = freqs.last+value.toInt
        } else {
          result = freqs.last-value.toInt
        }
        freqs.append(result)
      } else {
        val set = new mutable.HashSet[Int]()
        for (elem <- freqs) {
          if (set.contains(elem)) return Some(elem)
          set.add(elem)
        }
        operations = getOperationsIterator
      }
    }
    None
  }

  def main(args: Array[String]): Unit = {
    println(calculateFreq())
    println(firstDuplicateFreq())
  }

}