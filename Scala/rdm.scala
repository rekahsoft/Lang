package ca.rekahsoft.rdm

/**
 * (C) Copyright Collin Doering @!@YEAR@!@
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * File: rdm.scala
 * Author: Collin J. Doering
 * Date: Jun 20, 2013
 */

def and(x: Boolean, y: => Boolean): Boolean = if (x) y else false

def or(x: Boolean, y: => Boolean): Boolean = if (x) true else y

def factorial(n: Int): Int = {
  def fact(x: Int, acc: Int): Int = if (x <= 1) acc else fact(x - 1, x * acc)
  fact(n,1)
}

// An implementation of a list (covariant)
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, var tail: List[T]) extends List[T]{
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new Error("Nil.head")
  def tail: Nothing = throw new Error("Nil.head")
}

// An implementation of Natural numbers
abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def + (x: Nat): Nat
  def - (x: Nat): Nat
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def pred = n
  def succ: Nat = new Succ(this)
  def + (x: Nat) = new Succ(n + x)
  def - (x: Nat) = if (x.isZero) this else n - x.pred
}

object Zero extends Nat {
  def isZero = true
  def pred = throw new Error("Zero.pred")
  def succ: Nat = new Succ(this)
  def + (x: Nat) = x
  def - (x: Nat) = if (x.isZero) this else throw new Error("negative number")
}

/* Note: the above use of var is shortform:
 *  class Test[T](var x: T)
 *  is transformed to:
 *  class Test[T](x: T) {
 *    var x = x
 *  }
 */

// Implement a BST
trait BTree[T] {
}

