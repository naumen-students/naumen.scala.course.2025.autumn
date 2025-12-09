package homework3

trait Cell {
  def toString(): String
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

  override def toString: String = resolve(Set.empty)

  private def resolve(visited: Set[(Int, Int)]): String = {
    if (!table.inBounds(ix, iy)) "outOfRange"
    else if (visited.contains((ix, iy))) "cyclic"
    else {
      table.getCellSafe(ix, iy) match {
        case None => "outOfRange"
        case Some(ref: ReferenceCell) =>
          ref.resolve(visited + ((ix, iy)))
        case Some(cell) =>
          cell.toString
      }
    }
  }
}
