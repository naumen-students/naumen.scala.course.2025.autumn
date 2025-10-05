import scala.collection.mutable


class Table(val w: Int, val h: Int) {
  private val data: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(w * h)(new EmptyCell)

  private def inRange(ix: Int, iy: Int): Boolean =
    ix >= 0 && iy >= 0 && ix < w && iy < h

  private def idx(ix: Int, iy: Int): Int = ix + iy * w

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (inRange(ix, iy)) Some(data(idx(ix, iy))) else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (inRange(ix, iy)) {
      data(idx(ix, iy)) = cell
    }
  }
}
