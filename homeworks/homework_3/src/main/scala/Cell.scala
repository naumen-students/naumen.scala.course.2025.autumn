trait Cell {
  def repr(visited: Set[(Int, Int)]): String

  override def toString: String = repr(Set.empty)
}

class EmptyCell extends Cell {
  override def repr(visited: Set[(Int, Int)]): String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def repr(visited: Set[(Int, Int)]): String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def repr(visited: Set[(Int, Int)]): String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def repr(visited: Set[(Int, Int)]): String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(_) =>
        val coord = (ix, iy)
        if (visited.contains(coord)) "cyclic"
        else {
          val nextVisited = visited + coord
          table.getCell(ix, iy).get.repr(nextVisited)
        }
    }
  }
}
