package ch.overney.aoc2018.day13

object Part1 extends App {
  type Velocity = (Int, Int)

  val GoingSouth = (1, 0)
  val GoingNorth = (-1, 0)
  val GoingEast = (0, 1)
  val GoingWest = (0, -1)
  val TurnLeftMapping: Map[Velocity, Velocity] = Map(GoingSouth -> GoingEast, GoingEast -> GoingNorth,
    GoingNorth -> GoingWest, GoingWest -> GoingSouth)
  val TurnRightMapping: Map[Velocity, Velocity] = TurnLeftMapping.map { case (k, v) => (v, k)}

  sealed trait IntersectionChoices
  case object Left extends IntersectionChoices
  case object Straight extends IntersectionChoices
  case object Right extends IntersectionChoices
  object IntersectionChoices {
    val values = List(Left, Straight, Right)
  }

  sealed trait CircuitElement {
    val charRep: Char
  }
  trait Turn {
    val pairs: List[Set[Velocity]]
  }
  case object TurnA extends CircuitElement with Turn {
    val charRep = '\\'

    val pairs: List[Set[Velocity]] = List(Set(GoingNorth, GoingWest), Set(GoingSouth, GoingEast))
  }
  case object TurnB extends CircuitElement with Turn {
    val charRep = '/'

    val pairs: List[Set[Velocity]] = List(Set(GoingNorth, GoingEast), Set(GoingSouth, GoingWest))
  }
  case object HorizontalLine extends CircuitElement {
    val charRep = '-'
  }
  case object VerticalLine extends CircuitElement {
    val charRep = '|'
  }
  case object Intersection extends CircuitElement {
    val charRep = '+'
  }
  object CircuitElement {
    val values = List(TurnA, TurnB, HorizontalLine, VerticalLine, Intersection)
  }

  val CartGoingSouth = 'v' -> (GoingSouth, VerticalLine)
  val CartGoingNorth = '^' -> (GoingNorth, VerticalLine)
  val CartGoingWest = '<' -> (GoingWest, HorizontalLine)
  val CartGoingEast = '>' -> (GoingEast, HorizontalLine)
  val CartCharsMapping = Map(CartGoingNorth, CartGoingSouth, CartGoingEast, CartGoingWest)

  def infiniteIntersectionDecisionsStream: Stream[IntersectionChoices] =
    IntersectionChoices.values.toStream #::: infiniteIntersectionDecisionsStream

  case class Cart(initXY: String, x: Int, y: Int, velocity: Velocity,
      intersectChoices: Stream[IntersectionChoices] = infiniteIntersectionDecisionsStream) {

    def moveAndAdjustVelocity(terrainMap: Map[(Int, Int), CircuitElement]): Cart = {
      val newPos @ (newX, newY) = (x + velocity._1, y + velocity._2)
      terrainMap(newPos) match {
        case HorizontalLine | VerticalLine => copy(x = newX, y = newY)
        case t: Turn =>
          copy(x = newX, y = newY, velocity = (t.pairs.find(_.contains(velocity)).get - velocity).head)
        case Intersection =>
          val newVelocity = intersectChoices.head match {
            case Left     => TurnLeftMapping(velocity)
            case Right    => TurnRightMapping(velocity)
            case Straight => velocity
          }
          copy(x = newX, y = newY, velocity = newVelocity, intersectChoices = intersectChoices.tail)
      }
    }
  }

  val initialAcc = (Map[(Int, Int), CircuitElement](), List[Cart]())
  val (terrainMap, initialCarts) = Input.DataEntries(1).zipWithIndex.foldLeft(initialAcc) { case (acc, (row, x)) =>
    row.zipWithIndex.foldLeft(acc) { case (currAcc @ (terrainAcc, cartAcc), (char, y)) =>

      lazy val circuitElementOpt = CircuitElement.values.find(_.charRep == char)

      if (CartCharsMapping.contains(char)) {
        val (initV, ce) = CartCharsMapping(char)
        val newCart = Cart(s"$x$y", x, y, initV)
        (terrainAcc + ((x, y) -> ce), cartAcc :+ newCart)
      } else if (circuitElementOpt.nonEmpty) {
        (terrainAcc + ((x, y) -> circuitElementOpt.get), cartAcc)
      } else if (char == ' ') {
        currAcc
      } else {
        sys.error(s"Character '$char' is not recognized!")
      }
    }
  }

  println(initialCarts)

  def iterate(carts: List[Cart]): (Int, Int) = {
    val collisionOpt = carts.map(cart => (cart.x, cart.y)).groupBy(identity).find(_._2.size > 1)
    if (collisionOpt.nonEmpty) {
      collisionOpt.get._1
    } else {
      val updatedCarts = carts.map(_.moveAndAdjustVelocity(terrainMap))
      iterate(updatedCarts)
    }
  }

  // This doesn't give the right answer if the first crash is two carts "exchanging" spots!
  println(iterate(initialCarts))
}
