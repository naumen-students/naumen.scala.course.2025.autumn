trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString: String = table.resolveToString(ix, iy, Set(this))
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString: String = text
}


