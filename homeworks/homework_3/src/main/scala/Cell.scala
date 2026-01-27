import scala.collection.immutable.Queue

trait Cell {
  def toString(): String
}

case class EmptyCell() extends Cell{
  override def toString : String = "empty"
}

case class StringCell(value : String) extends Cell{
  override def toString : String = value
}

case class NumberCell(value : Int) extends Cell{
  override def toString : String = value.toString
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override def toString() : String = getValueRefCell(List.empty[ReferenceCell], this)

  def getValueRefCell(way : List[ReferenceCell], cell: ReferenceCell): String = {
    val valueFromTable = cell.table.getCell(cell.ix, cell.iy)
    valueFromTable match {
      case None => "outOfRange"
      case Some(v) => {
        if (way.contains(cell)) {
          "cyclic"
        } else {
          val newWay = cell :: way
          getValueCell(newWay, v)
        }
      }
    }
  }

  def getValueCell(way : List[ReferenceCell], cell : Cell): String =  cell match {
    case ceil : ReferenceCell  => getValueRefCell(way, ceil)
    case cell => cell.toString
  }

}

