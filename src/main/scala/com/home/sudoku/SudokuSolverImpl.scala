package com.home.sudoku

import com.home.sudoku.SudokuSolverImpl.Cell._
import com.home.sudoku.SudokuSolverImpl._

class SudokuSolverImpl extends SudokuSolver {

  override def solve(input: Seq[String]): Seq[String] =
    fillField(parse(input)).map(render).getOrElse(Seq.empty)

  private def parse(input: Seq[String]): Field[Cell] = {
    val rows =
      input
        .map(_.map(char => if (char.isDigit) Filled(char.asDigit) else Empty))
        .map(_.toVector)
        .map(Row(_))

    Field(rows.toVector)
  }

  private def render(field: Field[Filled]): Seq[String] =
    field.rows.map(_.cells.map(_.value).mkString(""))

  private val possibleValues = (1 to 9).map(Filled).to(LazyList)

  private def isValueOk(value: Filled, cells: Vector[Cell]): Boolean =
    cells.collectFirst { case `value` => () }.isEmpty

  private def okByX(value: Filled, state: Field[Cell], indexX: Int): Boolean =
    isValueOk(value, state.rows.map(_.cells(indexX)))

  private def okByY(value: Filled, state: Field[Cell], indexY: Int): Boolean =
    isValueOk(value, state.rows(indexY).cells)

  private def okBySquare(value: Filled, state: Field[Cell], indexX: Int, indexY: Int): Boolean = {
    val xStart: Int = (indexX / 3) * 3
    val xEnd: Int = xStart + 3
    val yStart: Int = (indexY / 3) * 3
    val yEnd: Int = yStart + 3

    val cells = state.rows.slice(yStart, yEnd).flatMap(_.cells.slice(xStart, xEnd))

    isValueOk(value, cells)
  }

  private def isCellOk(cell: Filled, field: Field[Cell], indexX: Int, indexY: Int): Boolean =
    okByX(cell, field, indexX) && okByY(cell, field, indexY) && okBySquare(cell, field, indexX, indexY)

  private def fillField(field: Field[Cell]): Option[Field[Filled]] = {

    def fillRows(filledY: Vector[Row[Filled]], toBeFilledY: Vector[Row[Cell]]): LazyList[Field[Filled]] = {

      def fillCells(filledX: Vector[Filled], toBeFilledX: Vector[Cell]): LazyList[Row[Filled]] = {
        if (toBeFilledX.nonEmpty) {
          val cell = toBeFilledX.head

          val indexX = filledX.size
          val indexY = filledY.size
          val field = Field(filledY ++ (Row(filledX ++ toBeFilledX) +: toBeFilledY.tail))

          val possibleCells = cell match {
            case f: Filled => LazyList(f)
            case Empty => possibleValues.filter(value => isCellOk(value, field, indexX, indexY))
          }
          possibleCells.map(filledX :+ _).flatMap(fillCells(_, toBeFilledX.tail))
        } else {
          LazyList(Row(filledX))
        }
      }

      if (toBeFilledY.nonEmpty) {
        val row = toBeFilledY.head
        val possibleRows = fillCells(Vector.empty, row.cells)
        possibleRows.map(filledY :+ _).flatMap(fillRows(_, toBeFilledY.tail))
      } else {
        LazyList(Field(filledY))
      }
    }

    fillRows(Vector.empty, field.rows).headOption
  }
}

object SudokuSolverImpl {
  sealed trait Cell

  case object Cell {
    case object Empty extends Cell
    case class Filled(value: Int) extends Cell
  }

  case class Row[+T <: Cell](cells: Vector[T])
  case class Field[+T <: Cell](rows: Vector[Row[T]])
}
