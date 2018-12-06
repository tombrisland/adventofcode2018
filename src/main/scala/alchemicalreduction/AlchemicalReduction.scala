package alchemicalreduction

import scala.collection.mutable
import scala.io.Source


object AlchemicalReduction {

  val INPUT_RESOURCE = "alchemicalreduction/input.txt"
  private val EMPTY = ""

  private def getPolymerString = Source.fromResource(INPUT_RESOURCE).getLines().toList.head

  private def generateUnits = ('a' to 'z')
    .flatMap(c => List(
      c.toString + c.toUpper.toString,
      c.toUpper.toString + c.toString
    ))

  private def reactPolymer(input : String) : Int = {
    var polymer = input
    var len = polymer.length
    do {
      len = polymer.length
      generateUnits
        .foreach(unit => polymer = polymer.replace(unit, EMPTY))
    } while (polymer.length != len)
    len
  }

  def findShortestPolymer(input : String) : List[(String, Int)]= {
    val results = mutable.HashMap[String, Int]()
    generateUnits.foreach(unit => {
      var polymer = input
      val (one, two) = unit.splitAt(1)
      polymer = polymer.replace(one, EMPTY)
      polymer = polymer.replace(two, EMPTY)
      results.put(one, reactPolymer(polymer))
    })
    results.toMap.toList.sortBy(_._2)
  }

  def main(args: Array[String]): Unit = {
    println(reactPolymer(getPolymerString))
    println(findShortestPolymer(getPolymerString))
  }

}
