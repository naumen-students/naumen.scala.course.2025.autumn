case class Table() {

  private var width: Int = 0
  private var height: Int = 0
  private var cells: Array[Cell] = null

  def this(width: Int,
           height: Int) {
    this()
    this.width = width
    this.height = height
    this.cells = new Array[Cell](width * height)

    fillTableWithEmptyCells()
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (!isCellInsideTable(ix, iy)) {
      return None
    }

    val index = getIndex(ix, iy)
    val cell = cells(index)

    return Some(cell)
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (!isCellInsideTable(ix, iy)) {
      return
    }

    val index = getIndex(ix, iy)
    cells(index) = cell
  }

  def isCellInsideTable(ix: Int, iy: Int): Boolean = {
    return ix >= 0 && iy >= 0 && getIndex(ix, iy) < width * height
  }
  
  private def fillTableWithEmptyCells(): Unit = {
    val tableSize = width * height

    for (i <- 0 until tableSize) {
      cells(i) = new EmptyCell()
    }
  }
  
  private def getIndex(ix: Int, iy: Int): Int = {
    return ix + width * iy;
  }
}