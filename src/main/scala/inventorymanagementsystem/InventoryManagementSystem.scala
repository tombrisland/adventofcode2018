package inventorymanagementsystem

import scala.collection.mutable
import scala.io.Source

object InventoryManagementSystem {

  val INPUT_RESOURCE = "inventorymanagementsystem/input.txt"

  private def getBoxesList = Source.fromResource(INPUT_RESOURCE).getLines().toList

  def calculateLetterCounts(identifier : String) : List[Int] = {
    val charMap = new mutable.HashMap[Char, Int]()
    identifier.foreach(c => {
      if (charMap.contains(c)) charMap(c)+=1 else charMap(c)=1
    })
    charMap.values.toList
  }

  def countTwosAndThrees(counts : List[Int]) : (Int, Int) = {
    (
      if (counts.contains(2)) 1 else 0,
      if (counts.contains(3)) 1 else 0
    )
  }

  private def sumTuples(two : (Int, Int), three : (Int, Int)) = (two._1 + three._1, two._2 + three._2)

  def intersect(x : String, y : String) : String = {
    var res = new String
    for (i <- 0 until x.length) {
      if (x(i)==y(i)) res+=x(i)
    }
    res
  }

  def calculateChecksum(): Long = {
    val (twos, threes) = getBoxesList
      .map(calculateLetterCounts)
      .map(countTwosAndThrees)
      .foldLeft(0, 0)(sumTuples)
    twos*threes
  }

  def findCloseDuplicates() : String = {
    getBoxesList.tails.flatMap {
      case x :: rest => rest.map { y => intersect(x, y) }
      case _ => None
    }
      .filter(s => s.length==25)
      .toList
      .head
  }

  def main(args: Array[String]): Unit = {
    println(calculateChecksum())
    println(findCloseDuplicates())
  }

}