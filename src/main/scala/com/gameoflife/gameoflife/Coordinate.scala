package com.gameoflife.gameoflife


case class Coordinate(x: Int, y: Int) {

  def getNeighbours: Set[Coordinate] = {
    var neighbours: Set[Coordinate] = Set()

    for (i <- x - 1 to x + 1; j <- y - 1 to y + 1) {
      if (i != x || j != y) {
        neighbours += Coordinate(i, j)
      }
    }
    neighbours
  }

}
