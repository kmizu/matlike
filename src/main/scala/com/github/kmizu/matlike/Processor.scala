package com.github.kmizu.matlike

abstract class Processor[-In, +Out] {
  def name: String
  def process(input: In): Out
}
