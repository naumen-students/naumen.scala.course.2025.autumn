trait Cell{
  val data : Any
  def toString: String
}

class EmptyCell extends Cell {
  override val data: Option[Any] = None

  override def toString: String = "empty"
}

class NumberCell(num : Int) extends Cell{
  override val data: Int = num

  override def toString: String = s"$num"
}

class StringCell(str : String) extends Cell {
  override val data: String = str

  override def toString: String = str
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
  override val data: Cell = table.getCell(ix, iy) match {
    case Some(value) => value
    case None => new EmptyCell
  }

  override def toString: String = {
      if (ix < 0 || ix >= table.width || iy < 0 || iy >= table.length) {
        return "outOfRange"
      }

      references(Set()) match {
        case Some(result) => result
        case None => "cyclic"
      }
    }

    private def references(visited: Set[(Int, Int)]): Option[String] = {
      if (visited.contains((ix, iy))) {
        return None
      }

      table.getCell(ix, iy) match {
        case Some(cell: ReferenceCell) =>
          cell.references(visited + ((ix, iy)))
        case Some(otherCell) =>
          Some(otherCell.toString)
        case None =>
          Some("outOfRange")
      }
}}