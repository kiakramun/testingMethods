package testing

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class IntAddGroupTest extends AnyFunSuite with ScalaCheckPropertyChecks:

  private val upperBound: Int = 500
  private val lowerBound: Int = -500;
  private def inClosedInterval(zs: Int*): Boolean = {
    zs.forall(z => z <= upperBound && z >= lowerBound)
  }
  private val randomValue = for {
    x <- Gen.choose(lowerBound, upperBound)
  } yield x

  private val threeRandomValues = for {
    x <- Gen.choose(lowerBound, upperBound)
    y <- Gen.choose(lowerBound, upperBound)
    z <- Gen.choose(lowerBound, upperBound)
  } yield (x,y,z)

  test("+ is associative"):
    forAll(threeRandomValues) { case(x,y,z) =>
      whenever(inClosedInterval(x,y,z)) {
        assert((x+y)+z == x+(y+z))
      }
    }


  test("there is a neutral element"):
    forAll(randomValue) { x=>
      whenever(inClosedInterval(x)) {
        assert(x+0==x)
      }
    }

  test("each element has an inverse"):
    forAll(randomValue) { x=>
      whenever(inClosedInterval(x)) {
        assert(x+(-x)==0)
      }
    }