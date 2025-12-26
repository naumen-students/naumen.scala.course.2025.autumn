import scala.annotation.tailrec
import scala.collection.mutable

trait Cell {}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = s"$value"
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(val column: Int, val row: Int, table: Table) extends Cell {
  override def toString: String =
    referenceToString(column, row, new mutable.HashSet[Cell]())

  @tailrec
  private def referenceToString(
      column: Int,
      row: Int,
      trace: mutable.HashSet[Cell]
  ): String = {
    val getCellResult = table.getCell(column, row)
    if (getCellResult.isEmpty) {
      return "outOfRange"
    }

    val cell = getCellResult.get
    if (trace.contains(cell)) {
      return "cyclic"
    }

    trace.add(cell)

    cell match {
      case referenceCell: ReferenceCell =>
        referenceToString(referenceCell.column, referenceCell.row, trace)
      case cell => cell.toString
    }
  }
}
