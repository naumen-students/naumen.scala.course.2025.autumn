import scala.collection.mutable

// Базовый интерфейс ячейки
sealed trait Cell {
    def toString: String
}

// Пустая ячейка
class EmptyCell extends Cell {
    override def toString: String = "empty"
}

// Ячейка с 32-битным целым числом
class NumberCell(number: Int) extends Cell {
    override def toString: String = number.toString
}

// Ячейка с текстом
class StringCell(text: String) extends Cell {
    override def toString: String = text
}

// Ячейка-ссылка
class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
    override def toString: String = {
        // Используем стек для отслеживания циклических ссылок
        val visited = mutable.Set[(Int, Int)]()

        def resolve(currentIx: Int, currentIy: Int): String = {
            if (!visited.add((currentIx, currentIy))) {
                return "cyclic"
            }

            table.getCell(currentIx, currentIy) match {
                case None =>
                    "outOfRange"
                case Some(cell: ReferenceCell) =>
                    resolve(cell.ix, cell.iy)
                case Some(cell) =>
                    cell.toString
            }
        }

        resolve(ix, iy)
    }
}