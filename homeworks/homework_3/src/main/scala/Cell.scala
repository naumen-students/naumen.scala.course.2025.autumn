package homework3

trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {

  override def toString: String = toString(Set.empty)

  private def toString(visited: Set[(Int, Int)]): String = {
    if (ix < 0 || iy < 0 || ix >= table.width || iy >= table.height) return "outOfRange"
    if (visited.contains((ix, iy))) return "cyclic"

    table.getCellSafe(ix, iy) match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) => cell.toString(visited + ((ix, iy)))
      case Some(cell) => cell.toString
    }
  }
}
