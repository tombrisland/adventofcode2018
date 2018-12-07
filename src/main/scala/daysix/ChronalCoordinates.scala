package daysix

import scala.collection.mutable.ListBuffer
import scala.io.Source

object ChronalCoordinates {
  val INPUT_RESOURCE = "chronalcoordinates/input.txt"

  val COORD_PATTERN = "([0-9]+), ([0-9]+)".r

  private def getPointsList = Source.fromResource(INPUT_RESOURCE).getLines().toList.map(parseCoord)

  private def parseCoord(input : String): (Int, Int) = {
    val m = COORD_PATTERN.findFirstMatchIn(input).get
    (m.group(1).toInt, m.group(2).toInt)
  }

  private def manhattanDistance(one : (Int, Int), two : (Int, Int)) = {
    Math.abs(one._1 - two._1) + Math.abs(one._2 - two._2)
  }

  private def calculateCoords = {
    val points = getPointsList

    val minX = points.minBy(_._1)
    val maxX = points.maxBy(_._1)
    val minY = points.minBy(_._2)
    val maxY = points.maxBy(_._2)

    val res = ListBuffer[(Int, Int)]()

    (minX._1 to maxX._1).foreach(x => {
      (minY._2 to maxY._2).foreach(y => {
        res.append((x, y))
      })
    })

    res.toList
  }

  // could generate these way easier please don't meme me
  private def edgeCoords = {
    val points = getPointsList

    val minX = points.minBy(_._1)
    val maxX = points.maxBy(_._1)
    val minY = points.minBy(_._2)
    val maxY = points.maxBy(_._2)

    val res = ListBuffer[(Int, Int)]()

    (minX._1 to maxX._1).foreach(x => {
      res.append((x, minY._2))
      res.append((x, maxY._2))
    })

    (minY._2 to maxY._2).foreach(y => {
      res.append((minX._1, y))
      res.append((maxX._1, y))
    })

    res.toList
  }

  private def pointsEvalulated(coords : List[(Int, Int)]) = {
    coords
      .map(coord => {
        val distancesToPoints = getPointsList
          .map(point => (coord, point, manhattanDistance(point, coord)))
          .sortBy(_._3)

        val closest = distancesToPoints.head
        if (closest._3 == distancesToPoints(1)._3) null else closest

      }).filter(_!=null)
  }
  def main(args: Array[String]): Unit = {

    val pointsEvaluated = pointsEvalulated(calculateCoords)
    val edgesEvaluated = pointsEvalulated(edgeCoords)

    val partOne = pointsEvaluated
      .filter(i => !edgesEvaluated.map(_._2).distinct.contains(i._2))
      .groupBy(_._2)
      .mapValues(_.size)
      .toList
      .sortBy(_._2)
      .reverse

    val partTwo = calculateCoords
      .count(coord => getPointsList.map(manhattanDistance(coord, _)).sum < 10000)

    println(partOne.head)

    println(partTwo)
  }
}