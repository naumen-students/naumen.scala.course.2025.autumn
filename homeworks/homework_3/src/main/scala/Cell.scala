sealed trait Cell

class EmptyCell extends Cell {
  override def toString : String = "empty"
}

class StringCell(str : String) extends Cell {
  override def toString : String = str
}

class NumberCell(num : Int) extends Cell {
  override def toString : String = num.toString
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {

  override def toString: String = toStringImpl()

  private def toStringImpl(visitedCells: Set[Cell] = Set.empty): String =
    table.getCell(ix, iy) match {
      case Some(targetCell) => formatTargetCell(targetCell, visitedCells)
      case None => "outOfRange"
    }

  private def formatTargetCell(targetCell: Cell, visitedCells: Set[Cell]): String =
    targetCell match {
      case refCell: ReferenceCell =>
        if (visitedCells.contains(this)) "cyclic"
        else refCell.toStringImpl(visitedCells + this)
      case cell: Cell => cell.toString
    }
}