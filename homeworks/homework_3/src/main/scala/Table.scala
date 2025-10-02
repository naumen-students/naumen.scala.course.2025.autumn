class Table(val width: Int, val height: Int) {
    private val cells: Array[Cell] =
        Array.fill(width * height)(new EmptyCell)
    
    def getCell(ix: Int, iy: Int): Option[Cell] =
        if (inBounds(ix, iy)) Some(cells(index(ix, iy))) else None
    
    def setCell(ix: Int, iy: Int, cell: Cell): Unit =
        if (inBounds(ix, iy)) cells(index(ix, iy)) = cell
    
    private def inBounds(ix: Int, iy: Int): Boolean =
        ix >= 0 && ix < width && iy >= 0 && iy < height
    
    private def index(ix: Int, iy: Int): Int =
        ix + iy * width
}
