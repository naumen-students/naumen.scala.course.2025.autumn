trait Cell {
  def toString: String
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}
class EmptyCell extends Cell {
  override def toString: String = "empty"
}
class StringCell(value: String) extends Cell {
  override def toString: String = value
}
class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def getX = ix
  private def getY = iy
  override def toString: String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(x) =>
        if (x.isInstanceOf[ReferenceCell] && table.getCell(x.asInstanceOf[ReferenceCell].getX,
          x.asInstanceOf[ReferenceCell].getY).get == this) "cyclic"
        else x.toString
    }
  }
}