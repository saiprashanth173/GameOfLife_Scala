package com.gameoflife.gameoflife

import com.gameoflife.gameoflife.CellOperations.{AliveCell, Cell}

import scala.io.StdIn

object GameOfLife {

  def main(args: Array[String]): Unit = {

    val aliveCells: Set[Cell] = getLiveCellsFormInput

    new Generation(aliveCells).transform.display()

  }

  // Getting Alive Cell from String input
  def getCellFromString(line: String): Cell = {
    val xAndy: Array[String] = line.trim.split(",")
    val x: Int = xAndy(0).toInt
    val y: Int = xAndy(1).toInt
    AliveCell(Coordinate(x, y))
  }

  def getLiveCellsFormInput: Set[Cell] = {
    var line: String = StdIn.readLine("Enter coordinates of live cell : ")
    var liveCells: Set[Cell] = Set()
    while (!line.trim.equals("")) {
      liveCells += getCellFromString(line)
      line = StdIn.readLine("Enter coordinates of live cell : ")
    }
    liveCells
  }

}
