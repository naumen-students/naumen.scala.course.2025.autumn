class ReferenceCell(val cellRow: Int, val cellCol: Int, table: Table) extends Cell {
  override def toString: String = resolveRef(Set((cellRow, cellCol)))

  private def resolveRef(visited: Set[(Int, Int)]): String = {
    if (!table.isCellInTable(cellRow, cellCol))
      "outOfRange"
    else {
      val nextCell = table.getCell(cellRow, cellCol).get

      nextCell match {
        case refCell: ReferenceCell => processRefCell(refCell, visited)
        case _ => nextCell.toString
      }
    }
  }

  private def processRefCell(refCell: ReferenceCell, visited: Set[(Int, Int)]): String = {
    val refCellPos = (refCell.cellRow, refCell.cellCol)

    if (visited.contains(refCellPos))
      "cyclic"
    else
      refCell.resolveRef(visited + refCellPos)
  }
}
