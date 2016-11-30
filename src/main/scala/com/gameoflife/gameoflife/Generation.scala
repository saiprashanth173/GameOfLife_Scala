package com.gameoflife.gameoflife

import com.gameoflife.gameoflife.CellOperations.{AliveCell, Cell, DeadCell, getNeighbourCells}

class Generation(aliveCells: Set[Cell]) {

  def display(): Unit = {
    aliveCells.foreach((cell) => println(cell.coordinate.x + ", " + cell.coordinate.y))
  }

  def transform = {

    val neighboursOfLiveCells = aliveCells.toList
      .flatMap((cell) => getNeighbourCells(cell, aliveCells))

    val liveNeighbourCountMap: Map[Cell, Int] = neighboursOfLiveCells
      .map((cell) => (cell, 1)) // getting  format [(cell1,1), (cell2,1), (cell1,1), (cell3,1)]
      .groupBy(_._1) // grouping cells {cell1 = [(cell1, 1), (cell1, 1)], cell2 = [(cell2, 1)], cell3 = [(cell3,1)]}
      .map(cell => // Reduce phase final output {cell1 = 2, cell2 = 1, cell3 = 1}
      (cell._1, cell._2.foldLeft(0)(
        (sum, group) => sum + group._2)
        )
    )

    val liveCellsNextGen: Set[Cell] = liveNeighbourCountMap
      .map(
        (keySet) =>
          CellOperations.transformCell(keySet._1, keySet._2)
      )
      .filter(checkIfCellAlive)
      .toSet

    new Generation(liveCellsNextGen)
  }

  def checkIfCellAlive(cell: Cell): Boolean = {
    cell match {
      case AliveCell(coordinate) => true
      case DeadCell(coordinate) => false
    }

  }

}
