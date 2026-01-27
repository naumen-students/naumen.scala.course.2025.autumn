import scala.:+
import scala.collection.mutable.ListBuffer

/*Таблица имеет фиксированную ширину и длину (параметры конструктора Table).

  Таблица должна быть представлена следующими типами ячеек (базовый интерфейс Cell):
— Пустая ячейка (класс EmptyCell),
— Ячейка с 32-битным целым числом (класс NumberCell),
— Ячейка с текстом (класс StringCell),
— Ячейка, содержащая ссылку на другую ячейку (класс ReferenceCell).

  По умолчанию, все ячейки таблицы являются пустыми (класс “EmptyCell”)

Таблица (класс Table) должна предоставлять следующие общедоступные методы:
— getCell(ix: Int, iy: Int): Option[Cell] (возвращает ячейку по индексам строки и столбца,
  либо “None”, если ix или iy вне границ таблицы),
— setCell(ix: Int, iy: Int, cell: Cell): Unit (устанавливает ячейку cell в указанные столбец и строку),
здесь ix — индекс колонки (ix>=0), iy — индекс строки (iy>=0),
cell — ячейка таблицы, представленная конкретной реализацией (EmptyCell/NumberCell/StringCell/ReferenceCell).

  Таблица хранит коллекцию ячеек. Так как определён метод “setCell”, можно использовать мутабельную коллекцию
  (package scala.collection.mutable).

Каждая ячейка должна предоставлять основной конструктор, посредством которого инициализируется значение ячейки.
  Например, class NumberCell(number: Int) extends Cell .
В случае “EmptyCell” какого-либо конструктора не требуется.
В случае “ReferenceCell”: class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell, где ix и iy —
индексы столбца и строки ячейки (на которую ведёт ссылка) таблицы table (которой принадлежит ячейка,
на которую ведёт ссылка).

  Каждая ячейка (реализация интерфейса Cell) должна предоставлять общедоступный метод toString(): String,
  который возвращает хранящееся в ней значение в виде строки (типа String).
  В случае “EmptyCell” метод “toString” должен возвращать значение “empty”
В случае “ReferenceCell” метод “toString” должен возвращать значение той ячейки, на которую определена ссылка.
В случае, если “ReferenceCell” ячейка ссылается на индекс ячейки, находящийся за границами таблицы,
метод “toString” должен возвращать значение “outOfRange”
В случае “циклических” ссылок (если, например, “ReferenceCell” ячейка ссылается на другую “ReferenceCell” ячейку,
которая вновь ссылается на первую), метод “toString” должен возвращать значение “cyclic”.

При реализации можно учесть, что двумерный список можно однозначно выразить одномерным:
  i = ix + iy * w,
где i — индекс одномерного списка, ix — индекс колонки двумерного списка,
iy — индекс строки двумерного списка, w — ширина (количество колонок) двумерного списка.*/

import scala.collection.mutable
class Table(val length : Int, val width : Int){
  val listCells: ListBuffer[Cell] = ListBuffer()
  def apply(length : Int, width : Int): Unit = {
    var i = 0
    while (i < length *width){
      listCells += new EmptyCell
      i += 1
    }
  }
  apply(length, width)
  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if ((ix > width) || (ix < 0) || (iy > length) || (iy < 0))
      None
    else
      Option(listCells(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if ((ix >=0) && (iy >= 0))
      listCells(ix + iy * width) = cell
  }
}