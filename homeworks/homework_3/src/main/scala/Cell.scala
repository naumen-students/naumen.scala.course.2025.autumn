import scala.collection.mutable.{Set => MutableSet}

trait Cell {
  def toString(): String
}

object Cell {
  val EMPTY_VALUE = "empty"
  val OUT_OF_RANGE = "outOfRange"
  val CYCLIC = "cyclic"
}

class EmptyCell extends Cell {
  override def toString(): String = Cell.EMPTY_VALUE
}

object EmptyCell {
  private val instance = new EmptyCell
  def apply(): EmptyCell = instance
}

class NumberCell(number: Int) extends Cell {
  override def toString(): String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString(): String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString(): String = {
    table.getCell(ix, iy) match {
      case None => Cell.OUT_OF_RANGE
      case Some(cell) => 
        if (isCyclicReference(MutableSet.empty[ReferenceCell])) Cell.CYCLIC 
        else cell.toString()
    }
  }
  
  private def isCyclicReference(visited: MutableSet[ReferenceCell]): Boolean = {
    if (visited.contains(this)) true
    else {
      visited += this
      table.getCell(ix, iy) match {
        case Some(cell: ReferenceCell) => cell.isCyclicReference(visited)
        case _ => false
      }
    }
  }
}
