trait Cell {
  def toString(): String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell{
  override def toString: String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = resolveReference(Set())

  def resolveReference(visited: Set[(Int, Int)]): String = {
    table.getStringValue(ix, iy, visited)
  }
}