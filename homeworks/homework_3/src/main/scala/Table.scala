class Table(width: Int, height: Int) {
  private val data: Array[Array[Cell]] = Array.ofDim[Cell](width, height)
  for {i <- 0 until width
       j <- 0 until height
       } data(i)(j) = new EmptyCell

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (0 <= ix && ix < width && 0 <= iy && iy < height) {
      Some(data(ix)(iy))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (0 <= ix && ix < width && 0 <= iy && iy < height) {
      data(ix)(iy) = cell
    }
  }
}