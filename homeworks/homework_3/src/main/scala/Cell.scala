trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = toString(Set.empty)
  def toString(visited: Set[Cell]): String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) =>
        if (visited.contains(cell)) {
          "cyclic"
        }
        else {
          cell.toString(visited + this)
        }
      case Some(anotherCell) => anotherCell.toString
    }
  }
}