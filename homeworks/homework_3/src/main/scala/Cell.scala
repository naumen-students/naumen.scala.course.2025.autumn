package homework_3

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

class ReferenceCell(val refX: Int, val refY: Int, val table: Table) extends Cell {
  override def toString(visited: Set[Cell]): String = {
    val targetOpt = table.getCell(refX, refY)
    if (targetOpt.isEmpty) return "outOfRange"

    val target = targetOpt.get
    if (visited.contains(this)) return "cyclic"

    target.toString(visited + this)
  }
}
