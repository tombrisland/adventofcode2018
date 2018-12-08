package dayeight

import scala.collection.mutable.ListBuffer
import scala.io.Source

object License {
  val INPUT_RESOURCE = "dayeight/input.txt"

  private def getNodes = Source
    .fromResource(INPUT_RESOURCE).mkString
    .split(" ")
    .filter(_!=" ")
    .map(_.toInt)
    .toList

  def sumMetadata(node : List[Int]) : (Int, Int) = {
    val header = (node.head, node(1))

    if (header._1 == 0) {
      return (2+header._2, node.slice(2, header._2+2).sum)
    }

    var offset = 0
    val children = ListBuffer[(Int, Int)]()

    for (_ <- 1 to header._1) {
      val n = valueRoot(node.slice(offset+2, node.size))
      offset += n._1
      children.append(n)
    }

    val baseSize = children.map(_._1).sum+2

    (baseSize+header._2, children.map(_._2).sum+node.slice(baseSize, baseSize+header._2).sum)
  }

  def valueRoot(node : List[Int]) : (Int, Int) = {
    val header = (node.head, node(1))

    if (header._1 == 0) {
      return (2+header._2, node.slice(2, header._2+2).sum)
    }

    var offset = 0
    val children = ListBuffer[(Int, Int)]()

    for (_ <- 1 to header._1) {
      val n = valueRoot(node.slice(offset+2, node.size))
      offset += n._1
      children.append(n)
    }

    val baseSize = children.map(_._1).sum+2

    val meta = node.slice(baseSize, baseSize+header._2)

    // remove children whose indexes aren't referenced
    val selectedChildren = meta
      .filter(c => children.size > c-1)
      .map(i => children(i-1))

    (baseSize+header._2, selectedChildren.map(_._2).sum)
  }

  def main(args: Array[String]): Unit = {
    println(sumMetadata(getNodes))
    println(valueRoot(getNodes))
  }


}
