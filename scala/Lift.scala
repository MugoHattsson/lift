import scala.util.Random
import scala.io.StdIn
object Lift {

  
  var floor = 0
  var direction = 1
  val liftColIndex = 4
  val liftWidth = 2
  var open = false
  var currentPassengers = 1
  var load = 0
  
  val numFloors = 7
  val floorLength = 10
  val maxPassengers = 10
  var none = -1
  var matrix = Array.fill[Int](numFloors, floorLength)(none)
  
  val openLeft = "O"
  val openRight = openLeft
  val closedLeft = "["
  val closedRight = "]"

  def main(args: Array[String]) = {

    val simulation = new Thread {
      override def run {
        while (currentPassengers > 0) {
          if (currentPassengers < maxPassengers) {
            addPassenger()
          }

          ridePassengers()
          moveLift()

          openDoors()
          movePassengers()
          exitPassengers()
          enterPassengers()
          exitPassengers()
          closeDoors()
        }
      }
    }

    simulation.start()

    StdIn.readLine()
    simulation.interrupt()
    simulation.join()
  }

  def addPassenger() = {
    currentPassengers += 1
    val from = Random.nextInt(7)
    var to = Random.nextInt(7)
    while (to == from) to = Random.nextInt(7)
    matrix(from)(0) = to
  }

  def movePassengers() = {
    matrix.map(xs => xs(xs.length - 1) = none)
    for (col <- (floorLength - 2) to 0 by -1) {
      for (row <- (numFloors - 1) to 0 by -1) {
        if (!(liftColIndex to liftColIndex + liftWidth contains col + 1)) {
          if (matrix(row)(col) != none && matrix(row)(col + 1) == none) {
            val pass = matrix(row)(col)
            matrix(row)(col) = none
            matrix(row)(col + 1) = pass
            drawLift()
          }
        }
      }
    }
  }

  def enterPassengers() = {
    val waitingIndex = liftColIndex - 1
    val liftEnd = waitingIndex + liftWidth
    var liftEnter = liftColIndex
    (0 until numFloors).foreach(row => {
      val pass = matrix(row)(waitingIndex)
      if (pass != none && floor == row && open && load < liftWidth) {
        load += 1
        (liftColIndex to liftEnd).foreach(pos =>
          if (matrix(row)(pos) == none) liftEnter = pos
        )
        matrix(row)(waitingIndex) = none
        matrix(row)(liftEnter) = pass
        drawLift()
      }
    })
  }

  def ridePassengers() = {
    val places = Array
      .fill[Int](liftWidth)(floor)
      .zip(liftColIndex until liftColIndex + liftWidth)
    for ((row, col) <- places) {
      if (matrix(row)(col) != none) {
        val pass = matrix(row)(col)
        matrix(row)(col) = none
        matrix(row + direction)(col) = pass
      }
    }
  }

  def exitPassengers() = {
    (liftColIndex until liftColIndex + liftWidth)
      .filter(c => matrix(floor)(c) == floor)
      .foreach(col => {
        val pass = matrix(floor)(col)
        val dest = nextFreeCol(matrix, floor, liftColIndex + liftWidth)
        matrix(floor)(col) = none
        matrix(floor)(dest) = pass
        load -= 1
        currentPassengers -= 1
        drawLift()
      })
  }

  def moveLift() = {
    val sleep = 500
    Thread.sleep(sleep)
    floor += direction
    if (floor == 0 || floor == 6) direction = -direction
    drawLift()
  }

  def formatMatrix(arr: Array[Array[Int]]): String = {
    arr
      .reverseMap(col =>
        col
          .map(c => {
            if (c == none) "_" else s"$c"
          })
          .mkString(" ")
      )
      .mkString("\n")
  }

  def drawLift(s: String = "") = {
    Thread.sleep(200)
    var pos = (numFloors - floor - 1) * (floorLength * 2) + liftColIndex * 2 - 1
    var result =
      if (open)
        formatMatrix(matrix)
          .patch(pos, openLeft, 1)
          .patch(pos + liftWidth * 2, openRight, 1)
      else
        formatMatrix(matrix)
          .patch(pos, closedLeft, 1)
          .patch(pos + liftWidth * 2, closedRight, 1)
    print("\033[2J\033[H")
    println(result)
  }

  def openDoors() {
    open = true
    drawLift()
  }

  def closeDoors() {
    open = false
    drawLift()
  }

  def debug(s: String = "") = {
    s" $floor: $open, $currentPassengers, $load, $s \n"
  }

  def nextFreeCol(arr: Array[Array[Int]], row: Int, from: Int): Int = {
    val range = from until floorLength
    range.filter(col => arr(row)(col) == none).head
  }
}
