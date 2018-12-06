package claims

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Claims {

  val INPUT_RESOURCE = "claims/input.txt"

  private def getClaimsList = Source.fromResource(INPUT_RESOURCE).getLines().toList
  private def CLAIM_PATTERN = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

  // generate list of every coordinate occupied from each line

  // flatmap and count
  def generateCoords(claim : String) : List[(Int, Int)] = {
    val found = CLAIM_PATTERN.findFirstMatchIn(claim).get

    val id = found.group(1)
    val coord = (found.group(2).toInt, found.group(3).toInt)
    val size = (found.group(4).toInt, found.group(5).toInt)
    val coords = ListBuffer[(Int, Int)]()

    (0 until size._1).foreach(x => {
      (0 until size._2).foreach(y => {
        coords.append((coord._1+x, coord._2+y))
      })
    })
    coords.toList
  }




  def main(args: Array[String]): Unit = {
    val l = getClaimsList.flatMap(generateCoords)

    val m = mutable.HashMap[(Int, Int), Int]()

    var c =0
    l.foreach(i => {
      if (m.contains(i)) m(i)+=1 else m(i)=1
      c+=1
    })

    getClaimsList.foreach(claim => {
      val coords = generateCoords(claim).map(coord => m(coord)==1)
      if (!coords.contains(false)) println(claim)
    })
    println(m.toList.count(i => i._2 > 1))
  }
}
