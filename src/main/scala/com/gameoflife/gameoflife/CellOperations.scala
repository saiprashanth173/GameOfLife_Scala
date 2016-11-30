package com.gameoflife.gameoflife


object CellOperations {


  sealed trait Cell {
    def coordinate: Coordinate
  }

  case class AliveCell(override val coordinate: Coordinate) extends Cell

  case class DeadCell(override val coordinate: Coordinate) extends Cell

  private def deadCells(aliveCells: Set[Cell], neighbourCoordinates: Set[Coordinate]): Set[Cell] = {
    val aliveCellCoordinates = aliveCells.map((cell) => cell.coordinate)

    neighbourCoordinates.diff(aliveCellCoordinates)
      .map(DeadCell)
  }

  private def liveCells(aliveCells: Set[Cell], neighbourCoordinated: Set[Coordinate]): Set[Cell] = {
    aliveCells.filter((cell) => neighbourCoordinated.contains(cell.coordinate))
  }

  private def hasLessThan2OrMoreThan3Neighbours(liveNeighbours: Int): Boolean = {
    liveNeighbours < 2 || liveNeighbours > 3
  }


  private def isDeadAndHasLessThan3LiveNeighbours(cell: Cell, liveNeighbours: Int): Boolean = {
    cell match {
      case AliveCell(coordinate) => false
      case DeadCell(coordinate) => liveNeighbours < 3
    }
  }

  def transformCell(cell: Cell, count: Int): Cell = {
    if (hasLessThan2OrMoreThan3Neighbours(count) || isDeadAndHasLessThan3LiveNeighbours(cell, count)) {
      return DeadCell(cell.coordinate)
    }
    AliveCell(cell.coordinate)
  }

  def getNeighbourCells(cell: Cell, aliveCells: Set[Cell]): Set[Cell] = {
    val neighbourCoordinated = cell.coordinate.getNeighbours
    var allNeighboursSet: Set[Cell] = Set()

    allNeighboursSet = allNeighboursSet ++ deadCells(aliveCells, neighbourCoordinated)
    allNeighboursSet = allNeighboursSet ++ liveCells(aliveCells, neighbourCoordinated)

    allNeighboursSet
  }


}
