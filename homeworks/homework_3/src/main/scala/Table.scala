class Table(val width: Int, val height: Int) {

  private val cells = Array.fill[Cell](width * height)(new EmptyCell)

  private def indexInTable(ix: Int, iy: Int): Boolean = ix >= 0 && ix < width && iy >= 0 && iy < height

  private def getCellIndex(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (indexInTable(ix, iy)){
      Some(cells(getCellIndex(ix, iy)))
    }
    else{
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (indexInTable(ix, iy)){
      cells(getCellIndex(ix, iy)) = cell
    }
  }
}