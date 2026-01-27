import scala.collection.mutable.ListBuffer

class Table(length: Int, width: Int){
  var listOfCells: ListBuffer[Cell] = ListBuffer.fill(length * width)(new EmptyCell)
  def getCell(_ix: Int, _iy: Int): Option[Cell] = {
    if ((_iy<0) || (_iy >= length) || (_ix<0) || (_ix>=width)) {
      None
    }
    else
      Option(listOfCells(_ix + _iy * width))

  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit= {
    listOfCells(ix + iy * width) = cell
  }

  def resolveReference(ix: Int, iy: Int, visited: Set[(Int, Int)]): String = {
    if (ix < 0 || ix >= width || iy < 0 || iy >= length) "outOfRange"
    else if (visited.contains((ix, iy))) "cyclic"
    else {
      getCell(ix, iy) match {
        case Some(ref: ReferenceCell) =>
          val (newIx, newIy) = ref.data.asInstanceOf[(Int, Int)]
          resolveReference(newIx, newIy, visited + ((ix, iy)))
        case Some(other) => other.toString
        case None => "outOfRange"
      }
    }
  }
}