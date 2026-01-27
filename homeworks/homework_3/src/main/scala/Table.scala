import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  require(width > 0 && height > 0, "Width and height must be positive")

  private val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  def inBounds(ix: Int, iy: Int): Boolean =
    ix >= 0 && ix < width && iy >= 0 && iy < height

  private def index(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (inBounds(ix, iy)) Some(cells(index(ix, iy))) else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    if (inBounds(ix, iy)) cells(index(ix, iy)) = cell
    else throw new IndexOutOfBoundsException(s"Coordinates ($ix, $iy) are out of bounds")

  // Метод разрешения ссылок
  def resolveReference(ref: ReferenceCell, visited: Set[(Int, Int)]): String = {
    val targetX = ref.refX
    val targetY = ref.refY

    // Проверка выхода за границы
    if (!inBounds(targetX, targetY)) {
      return "outOfRange"
    }

    // Проверка циклических ссылок по координатам
    if (visited.contains((targetX, targetY))) {
      return "cyclic"
    }

    getCell(targetX, targetY) match {
      case Some(targetCell: ReferenceCell) =>
        // Рекурсивно разрешаем с обновленным visited
        resolveReference(targetCell, visited + ((targetX, targetY)))

      case Some(targetCell) =>
        targetCell.toString

      case None =>
        "outOfRange"
    }
  }
}