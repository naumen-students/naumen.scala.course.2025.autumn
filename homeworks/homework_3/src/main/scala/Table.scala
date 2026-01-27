class Table(val width: Int, val height: Int) {

    private var cells: Vector[Cell] = Vector.fill(width * height)(new EmptyCell())

    private def index(ix: Int, iy: Int): Option[Int] =
        if (ix < 0 || iy < 0 || ix >= width || iy >= height) None
        else Some(ix + iy * width)

    def getCell(ix: Int, iy: Int): Option[Cell] = index(ix, iy).map(cells)

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        index(ix, iy) match {
            case Some(i) => cells = cells.updated(i, cell)
            case None =>
        }
    }
}
