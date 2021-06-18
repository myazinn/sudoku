package com.home.sudoku

import scala.io.StdIn

object SudokuApp extends App {

  println("Enter Sudoku field")

  val input = List.fill(9)(StdIn.readLine())
  val solver = SudokuSolver()
  val solution = solver.solve(input)
  val answer = if (solution.isEmpty) "Couldn't find a solution :(" else solution.mkString(System.lineSeparator)
  val delimiter = System.lineSeparator * 3

  println(delimiter + answer)

}
