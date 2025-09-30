import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Cell {}

class ValueCell extends Cell {}

class EmptyCell extends ValueCell {
  override def toString: String = { "empty" }
}

class NumberCell(value: Int) extends ValueCell {
  override def toString: String = { value.toString }
}

class StringCell(value: String) extends ValueCell {
  override def toString: String = { value }
}

class ReferenceCell(val row: Int, val column: Int, val table: Table) extends Cell {
  @tailrec
  private def dereference(cell: ReferenceCell, prevCells: Set[ReferenceCell] = Set[ReferenceCell]()): Try[ValueCell] = {
    if (prevCells.contains(cell))
      return Failure(new StackOverflowError)

    cell.table.getCell(cell.row, cell.column) match {
      case Some(nextCell: ReferenceCell) => dereference(nextCell, prevCells + cell)
      case Some(nextCell: ValueCell) => Success(nextCell)
      case None => Failure(new IndexOutOfBoundsException)
    }
  }

  override def toString: String = {
    dereference(this) match {
      case Success(cell: ValueCell) => cell.toString
      case Failure(e: IndexOutOfBoundsException) => "outOfRange"
      case Failure(e: StackOverflowError) => "cyclic"
    }
  }
}

