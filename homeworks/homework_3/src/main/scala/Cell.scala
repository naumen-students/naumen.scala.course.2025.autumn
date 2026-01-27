trait Cell{
  val data: Any = None
  def toString: String
}
class EmptyCell extends Cell{
  override def toString: String = "empty"
}
class StringCell(value: String) extends Cell{
  override val data: Any = value
  override def toString: String = data.toString
}
class NumberCell(value : Int) extends Cell{
  override val data: Any = value
  override def toString: String =value.toString
}
class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override val data: Any = (ix, iy)
  override def toString: String = table.resolveReference(ix, iy, Set.empty)
}