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
 * Boolean SATisfiability problem.
 */
object SAT extends App with BacktrackingSearch[Map[String, Boolean]] {

  abstract sealed class For
  case class Prop(name : String)    extends For
  case class Not (f : For)          extends For
  case class And (f : For, g : For) extends For
  case class Or  (f : For, g : For) extends For

  type PartialModel = Map[String, Boolean]

  def propositions(f : For) : Set[String] = f match {
    case Prop(name) => Set(name)
    case Not (f)    => propositions(f)
    case And (f, g) => propositions(f) ++ propositions(g)
    case Or  (f, g) => propositions(f) ++ propositions(g)
  }

  def eval(f : For, m : PartialModel) : Boolean = f match {
    case Prop(name) => m(name)
    case Not (f)    => !eval(f, m)
    case And (f, g) => eval(f, m) && eval(g, m)
    case Or  (f, g) => eval(f, m) || eval(g, m)
  }

  def solve(f : For) : Option[PartialModel] =
    search {
      val props = propositions(f).toList.sorted
      solveRec(f, Map(), props)
    }

  private def solveRec(f : For, m : PartialModel, props : List[String]) : Unit =
    props match {
      case List() => {
        assume(eval(f, m))
        success(m)
      }
      case prop :: otherProps =>
        choose {
          (v : Boolean) =>
          solveRec(f, m + (prop -> v), otherProps)
        }
    }

  val f1 = And(Prop("p"), Or(Not(Prop("p")), Prop("q")))

  println("f1: " + solve(f1))

  val f2 = And(And(Prop("p"),
                   Or(Not(Prop("p")), Prop("q"))),
                   Or(Not(Prop("q")), Not(Prop("q"))))

  println("f2: " + solve(f2))

}
