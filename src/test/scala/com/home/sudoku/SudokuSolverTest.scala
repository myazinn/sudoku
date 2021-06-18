package com.home.sudoku

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.io.Source

class SudokuSolverTest extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  val targetSum: Int = (1 to 9).sum
  val filledFields: Seq[Seq[String]] = Source.fromResource("sudoku_fields.txt").getLines().grouped(9).toVector

  val solver: SudokuSolver = SudokuSolver()

  def fieldsGen: Gen[Seq[String]] = {
    val filledFieldsGen: Gen[Seq[String]] = Gen.oneOf(filledFields)
    val positionGen: Gen[(Int, Int)] =
      for {
        x <- Gen.choose(0, 8)
        y <- Gen.choose(0, 8)
      } yield (x, y)

    def hideNCells(field: Seq[String], n: Int): Gen[Seq[String]] =
      if (n < 1) Gen.const(field)
      else
        positionGen.map { case (x, y) =>
          field.updated(x, field(x).updated(y, '-'))
        }.flatMap(hideNCells(_, n - 1))

    for {
      filledField <- filledFieldsGen
      nToHide <- Gen.choose(0, 80)
      field <- hideNCells(filledField, nToHide)
    } yield field
  }

  def isFieldValid(field: Seq[String]): Boolean = {
    val fieldNumeric = field.map(_.map(_.asDigit))

    def areCellsOk(cellGroups: Seq[Seq[Int]]): Boolean =
      cellGroups.forall(group => group.sum == targetSum && group.distinct.size == group.size)

    def isOkByX = areCellsOk(fieldNumeric)

    def isOkByY = areCellsOk(fieldNumeric.transpose)

    def isOkBySquare = {
      val cellsToCheck = fieldNumeric.grouped(3).toVector.flatMap { threeRows =>
        threeRows.transpose.grouped(3).toVector.map(_.flatten)
      }
      areCellsOk(cellsToCheck)
    }

    isOkByX && isOkByY && isOkBySquare
  }

  "SudokuSolver" should {
    "solve sudoku" when {
      "there is a solution" in {
        import org.scalacheck.Shrink.shrinkAny
        forAll(fieldsGen, minSuccessful(1024)) { field =>
          val filledField = solver.solve(field)
          filledField should not be empty
          assert(isFieldValid(filledField))
        }
      }
      "there are no solutions" in {
        val field =
          Seq(
            "57----4--",
            "---27---1",
            "2-6-31---",
            "-25---9-6",
            "7-49--1--",
            "-----3--2",
            "-12394-78",
            "46852--19",
            "-----6-54",
          )
        val filledField = solver.solve(field)
        filledField should be(empty)
      }
    }
  }
}