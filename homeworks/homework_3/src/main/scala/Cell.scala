import scala.annotation.tailrec
import scala.collection.immutable.HashSet

sealed trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

case class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

case class StringCell(string: String) extends Cell {
  override def toString: String = string
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {

  override def toString: String = {
    getReference(ix, iy, table, new HashSet[(Int, Int)])
  }

  @tailrec
  private def getReference(ix: Int, iy: Int, table: Table, visitedCells: Set[(Int, Int)]): String = {
    val option: Option[Cell] = table.getCell(ix, iy)
    if (option.isEmpty) {
      return "outOfRange"
    }

    val cell: Cell = option.get
    if (!cell.isInstanceOf[ReferenceCell]) {
      return cell.toString
    }

    val refCell: ReferenceCell = cell.asInstanceOf[ReferenceCell]
    if (visitedCells.contains((refCell.ix, refCell.iy))) {
      "cyclic"
    } else {
      getReference(refCell.ix, refCell.iy, table, visitedCells + ((refCell.ix, refCell.iy)))
    }
  }
}