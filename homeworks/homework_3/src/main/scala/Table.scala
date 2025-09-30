class Table(width: Int, height: Int) {
  require(width > 0 && height > 0, "Width and height must be positive")
  
  private val cells: Array[Cell] = Array.fill(width * height)(EmptyCell())
  
  private def indexOf(ix: Int, iy: Int): Int = ix + iy * width
  
  private def isValidIndex(ix: Int, iy: Int): Boolean = {
    ix >= 0 && ix < width && iy >= 0 && iy < height
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isValidIndex(ix, iy)) Some(cells(indexOf(ix, iy)))
    else None
  }
  
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isValidIndex(ix, iy)) {
      cells(indexOf(ix, iy)) = cell
    }
  }
}
