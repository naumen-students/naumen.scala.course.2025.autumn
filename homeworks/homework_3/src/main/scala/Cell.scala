sealed trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(val string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = resolveReference()

  private def resolveReference(visited: Set[ReferenceCell] = Set.empty[ReferenceCell]): String = {
    table.getCell(ix, iy).map {
      case referenceCell: ReferenceCell =>
        if (visited.contains(referenceCell)) {
          "cyclic"
        } else {
          referenceCell.resolveReference(visited ++ Set(referenceCell))
        }
      case cell: Cell => cell.toString
    }.getOrElse("outOfRange")
  }
}