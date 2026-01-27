abstract class Cell

case class EmptyCell() extends Cell() {
  override def toString: String = "empty"
}

case class NumberCell(value: Int) extends Cell() {
  override def toString: String = value.toString
}

case class StringCell(value: String) extends Cell() {
  override def toString: String = value
}

case class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString: String = getString(Set.empty)

  private def getString(visitedCells: Set[Cell]): String = {
    if (visitedCells.contains(this)) {
      return "cyclic"
    }

    val cell = table.getCell(ix, iy)
    
    return cell match {
      case Some(referenceCell: ReferenceCell) => referenceCell.getString(visitedCells + this)
      case Some(other) => other.toString
      case None => "outOfRange"
      case _ => ""
    }
  }
}
