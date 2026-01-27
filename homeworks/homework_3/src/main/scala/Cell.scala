// ячейка
sealed trait Cell {
  def toString(visited: Set[(Int, Int)]): String
  override def toString: String = this.toString(Set.empty)
}

// пустая ячейка
class EmptyCell extends Cell {
  override def toString(visited: Set[(Int, Int)]): String = "empty"
}

// ячейка с числом
class NumberCell(val number: Int) extends Cell {
  override def toString(visited: Set[(Int, Int)]): String = number.toString
}

// ячейка с текстом
class StringCell(val text: String) extends Cell {
  override def toString(visited: Set[(Int, Int)]): String = text
}

// ячейка-ссылка
class ReferenceCell(val targetIx: Int, val targetIy: Int, val table: Table) extends Cell {

  // метод для получения своих координат
  private var selfIx: Int = -1
  private var selfIy: Int = -1

  def setSelfCoordinates(ix: Int, iy: Int): Unit = {
    selfIx = ix
    selfIy = iy
  }

  override def toString(visited: Set[(Int, Int)]): String = {
    // если уже посещали ячейку, то она циклична
    if (visited.contains((targetIx, targetIy))) {
      "cyclic"
    } else if (targetIx < 0 || targetIx >= table.width || targetIy < 0 || targetIy >= table.height) {
      "outOfRange"
    } else {
      // добавление текущих координаты ячейки в set посещенных
      val newVisited = if (selfIx != -1 && selfIy != -1) visited + ((selfIx, selfIy)) else visited
      table.getCell(targetIx, targetIy) match {
        case Some(cell) => cell.toString(newVisited)
        case None => "outOfRange"
      }
    }
  }
}
