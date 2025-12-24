import scala.collection.mutable
case class Table(width: Int, height: Int) {
    private val cells: mutable.ArrayBuffer[Cell] = mutable.ArrayBuffer.fill(width * height)(new EmptyCell);

    private def index(ix: Int, iy: Int): Int = ix + iy * width

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        if (ix < 0 || ix >= width || iy < 0 || iy >= height) {
            None
        } else{
            val cellIndex = index(ix,iy)
            Some(cells(cellIndex))
        }
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
            val cellIndex = index(ix,iy)
            cells(cellIndex) = cell
        }
        else{
            println(s"ix = $ix or iy = $iy is out of range")
        }
    }

}