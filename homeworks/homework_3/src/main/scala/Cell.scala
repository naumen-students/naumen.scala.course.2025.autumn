trait Cell {
  def toString(visited: Set[Cell] = Set.empty): String
  override def toString: String = toString()
}

class EmptyCell extends Cell {
  override def toString(visited: Set[Cell]): String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString(visited: Set[Cell]): String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString(visited: Set[Cell]): String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString(visited: Set[Cell]): String = {
    if (visited.contains(this)) "cyclic"
    else {
      table.getCell(ix, iy) match {
        case None => "outOfRange"
        case Some(target) => target.toString(visited + this)
      }
    }
  }
}
