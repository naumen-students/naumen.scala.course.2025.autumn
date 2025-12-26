import scala.annotation.tailrec

sealed trait Cell {
  def toString(): String
}

class EmptyCell() extends Cell {
  override def toString(): String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString(): String = number.toString
}

class StringCell(string: String) extends Cell {
  override def toString(): String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString(): String = {
    getRefCellValue()
  }

  @tailrec
  private def getRefCellValue(visited: Set[(Int, Int)] = Set.empty): String = {
    val currentPos = (ix, iy)

    if (visited.contains(currentPos)) {
      return "cyclic"
    }

    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(refCell: ReferenceCell) =>
        refCell.getRefCellValue(visited + currentPos)
      case Some(cell: Cell) => cell.toString
    }
  }
}
