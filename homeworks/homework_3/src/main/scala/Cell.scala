// Базовый интерфейс ячейки
trait Cell {
  def toString: String
}

// Пустая ячейка
class EmptyCell extends Cell {
  override def toString: String = "empty"
}

// Ячейка с числом
class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

// Ячейка со строкой
class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

// Ячейка со ссылкой
class ReferenceCell(val refX: Int, val refY: Int, val table: Table) extends Cell {
  override def toString: String = {
    table.resolveReference(this, Set.empty)
  }
}