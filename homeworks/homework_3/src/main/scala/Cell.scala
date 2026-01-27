// homeworks/homework_3/src/main/scala/Cell.scala

trait Cell {
  def toString(): String
}

class EmptyCell extends Cell {
  override def toString(): String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString(): String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString(): String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def toStringWithVisited(visited: Set[(Int, Int)]): String = {
    if (visited.contains((ix, iy))) return "cyclic"

    val cellOpt = table.getCell(ix, iy)
    if (cellOpt.isEmpty) return "outOfRange"

    val newVisited = visited + ((ix, iy))
    val cell = cellOpt.get

    cell match {
      case ref: ReferenceCell => ref.toStringWithVisited(newVisited)
      case _ => cell.toString
    }
  }

  override def toString(): String = {
    toStringWithVisited(Set.empty)
  }
}