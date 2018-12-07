package dayseven

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Steps {

  val INPUT_RESOURCE = "dayseven/input.txt"

  private val STEP_REGEX = "Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.".r

  private def getStepList = Source.fromResource(INPUT_RESOURCE).getLines().toList

  private def parseStep(input : String) = {
    val m = STEP_REGEX.findFirstMatchIn(input).get

    (m.group(1), m.group(2))
  }

  def findUniqueLetters(steps: List[(String, String)]) : List[String] = {
    steps
      .flatMap(_.productIterator.toList)
      .distinct
      .asInstanceOf[List[String]]
  }

  def calculateDependencies(step : String, steps : List[(String, String)]) : (String, List[String]) = {
    (step, steps.filter(_._2==step).map(_._1))
  }

  private def calculateOrder(input : List[(String, List[String])]) = {

    var steps = input
    var ordered = ""

    while (steps.nonEmpty) {
      val chosen = steps.filter(_._2.isEmpty).map(_._1).min
      ordered+=chosen
      steps = steps.filter(_._1!=chosen).map(step => {(step._1, step._2.filter(_!=chosen))})
    }

    ordered
  }

  def calculateTime(step : String): Int = {
    ('A' to 'Z').filter(_==step.head).head-4
  }

  def main(args: Array[String]): Unit = {
    val steps = getStepList.map(parseStep)
    var order = calculateOrder(findUniqueLetters(steps).map(calculateDependencies(_, steps)))

    println(order)

    var time = 0
    var workers = ListBuffer[(String, Int)]()
    val completed = ListBuffer[String]()

    while (!order.isEmpty) {
      val availableSteps = order.filter(step => {
        val strStep = step.toString
        val deps = calculateDependencies(strStep, steps)._2
        val available = deps.isEmpty || deps.forall(completed.contains(_))
        available && workers.size < 5
      }).filter(step => !workers.map(_._1).contains(step.toString))

      if (!availableSteps.isEmpty) {
        availableSteps.foreach(step => {
          workers.append((step.toString, calculateTime(step.toString)))
          order.filter(_.toString!=step.toString)
        })
      }

      while (workers.minBy(_._2)._2 > 0) {
        workers = workers.map(w => (w._1, w._2-1))
        time+=1
      }

      workers.filter(_._2==0).foreach(f => {
        completed.append(f._1)
        order = order.filter(_.toString!=f._1)
      })
      workers = workers.filter(_._2!=0)

    }

    println(time)
  }

}
