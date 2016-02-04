package cbor.properties

import cbor.codec.NumberOps
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks
import shapeless.ops.coproduct.Inject

/**
  * Created by ytaras on 2/4/16.
  */
class NumbersOpsSuite extends FunSuite with PropertyChecks with NumberOps {
  // Just ot enforce implicit type conversion
  def injNC(v: => NumberChoice): NumberChoice = v

  test("Should not fail for big nums") {
    forAll { (x: BigInt) =>
      val r: NumberChoice = x
      val convertedBack: BigInt = r.map(toBigInt).unify
      convertedBack should be(x)
    }
  }
  test("Should encode byte smallest possible") {
    forAll { (x: Byte) =>
      val expected = Inject[NumberChoice, Byte].apply(x)
      injNC(x) should be(expected)
      injNC(x.toShort) should be(expected)
      injNC(x.toInt) should be(expected)
      injNC(x.toLong) should be(expected)
      injNC(BigInt(x)) should be(expected)
    }
  }
  test("Should encode short smallest possible") {
    forAll { (x: Short) =>
      whenever(x > Byte.MaxValue || x < Byte.MinValue) {
        val expected = Inject[NumberChoice, Short].apply(x)
        injNC(x) should be(expected)
        injNC(x.toInt) should be(expected)
        injNC(x.toLong) should be(expected)
        injNC(BigInt(x)) should be(expected)
      }
    }
  }
  test("Should encode int smallest possible") {
    forAll { (x: Int) =>
      whenever(x > Short.MaxValue || x < Short.MinValue) {
        val expected = Inject[NumberChoice, Int].apply(x)
        injNC(x) should be(expected)
        injNC(x.toLong) should be(expected)
        injNC(BigInt(x)) should be(expected)
      }
    }
  }
  test("Should encode long smallest possible") {
    forAll { (x: Long) =>
      whenever(x > Int.MaxValue || x < Int.MinValue) {
        val expected = Inject[NumberChoice, Long].apply(x)
        injNC(x) should be(expected)
        injNC(BigInt(x)) should be(expected)
      }
    }
  }
  test("Should encode bigint as bigint") {
    forAll { (x: BigInt) =>
      whenever(x > Long.MaxValue || x < Long.MinValue) {
        val expected = Inject[NumberChoice, BigInt].apply(x)
        injNC(x) should be(expected)
      }
    }
  }
}
