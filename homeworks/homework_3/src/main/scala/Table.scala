import scala.collection.mutable

case class Table(xLength: Int, yHigh: Int) {
  private val grid: mutable.Map[(Int, Int), Cell] = mutable.Map();
  private val emptyCell: EmptyCell = new EmptyCell;

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    grid += ((ix, iy) -> cell)
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= xLength || iy >= yHigh)
      None
    else {
      Some(grid.getOrElse((ix, iy), emptyCell))
    }
  }
}