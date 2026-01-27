
import scala.collection.mutable.HashMap

class Table(val width: Int, val height : Int) {
  private val data: HashMap[(Int, Int), Cell] = new HashMap()

   def getCell(ix: Int, iy: Int): Option[Cell] = {
     val isNotInBorders = ix < 0 || ix >= width || iy < 0 || iy >= height
     if (isNotInBorders) {
       None
     } else {
       data.get((ix, iy)).orElse(Some(new EmptyCell))
     }
   }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    val isInBorders = ix >= 0 && ix < width && iy >= 0 && iy < height
    if (isInBorders) {
      data((ix, iy)) = cell
    }
  }
}