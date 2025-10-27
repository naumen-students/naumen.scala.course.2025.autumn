
class Table(rows: Int, columns: Int) {
  private val Cells = Array.fill[Cell](rows, columns)(new EmptyCell)
  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix > Cells.length - 1 || iy > Cells(iy).length - 1) None
    else Some(Cells(ix)(iy))
  }
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < Cells.length && iy < Cells(iy).length) Cells(ix)(iy) = cell
  }
}