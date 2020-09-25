/**
 * This file is part of Scala-ND.
 *
 * Copyright (C) 2020 Philipp Ruemmer <ph_r@gmx.net>
 *
 * Scala-ND is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Scala-ND is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Scala-ND.  If not, see <http://www.gnu.org/licenses/>.
 */

package nd

/**
 * Travelling Salesperson problem.
 */
object TSP extends App with BacktrackingSearch[(List[Int], Int)] {

  type WeightMatrix = IndexedSeq[IndexedSeq[Int]]
  type Route        = List[Int]

  def solve(g : WeightMatrix, w : Int) : Option[(Route, Int)] =
    search {
      findRoute(List(0), 0, g, w)                   // Start at node 0
    }

  private def findRoute(prefix       : Route,
                        prefixWeight : Int,
                        g            : WeightMatrix,
                        maxWeight    : Int) : Unit =
    if (prefix.size < g.size) {                     //** Incomplete route
      chooseInt(0 until g.size) {                   // Non-deterministic choice
        nextNode =>                                 // of next node

        assume(!(prefix contains nextNode))         // Assume node not visited

        val newWeight =
          prefixWeight + g(prefix.head)(nextNode)
        assume(newWeight <= maxWeight)              // Assume route not too long

        findRoute(nextNode :: prefix, newWeight,    // Recursive call
                  g, maxWeight)
      }
    } else {                                        //** Complete route
      val weight = prefixWeight + g(prefix.head)(0)
      assume(weight <= maxWeight)                   // Check final weight
      success(((0 :: prefix).reverse, weight))
    }

  val fourCities = Vector(Vector(0,  90,  42,  35),
                          Vector(90,  0,  30,  34),
                          Vector(42, 30,   0,  12),
                          Vector(35, 34,  12,   0))

  println(solve(fourCities, 145))

}
