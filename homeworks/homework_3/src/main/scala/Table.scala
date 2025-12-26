class Table(columns: Int, rows: Int) {
  private val table: Array[Cell] =
    Array.fill(rows * columns)(new EmptyCell())

  def getCell(column: Int, row: Int): Option[Cell] = {
    if (column < 0 || column >= columns || row < 0 || row >= rows) {
      return None
    }

    Some(table(getIndex(column, row)))
  }

  def setCell(column: Int, row: Int, cell: Cell): Unit = {
    table(getIndex(column, row)) = cell
  }

  private def getIndex(column: Int, row: Int): Int = {
    row * columns + column
  }
}
