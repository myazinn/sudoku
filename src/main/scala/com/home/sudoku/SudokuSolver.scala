package com.home.sudoku

trait SudokuSolver {
  def solve(input: Seq[String]): Seq[String]
}

object SudokuSolver {
  def apply(): SudokuSolver = new SudokuSolverImpl
}