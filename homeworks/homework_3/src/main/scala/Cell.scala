trait Cell {
    override def toString: String
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
    override def toString: String = render(Set.empty)
    
    private def render(visited: Set[ReferenceCell]): String =
        if (visited.contains(this)) "cyclic"
        else
            table.getCell(ix, iy) match {
                case None                     => "outOfRange"
                case Some(ref: ReferenceCell) => ref.render(visited + this)
                case Some(other)              => other.toString
            }
}
