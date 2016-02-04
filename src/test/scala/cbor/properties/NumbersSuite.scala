package cbor.properties

import cbor.TestModel.withBuilder
import cbor.codec.Codecs
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks

/**
  * Created by ytaras on 2/3/16.
  */
class NumbersSuite extends FunSuite with PropertyChecks with ScodecHelpers {

  test("Should decode positive numbers") {
    forAll { (x: BigInt) =>
      whenever(0 <= x && x.bitLength <= 64) {
        val bytes = withBuilder(_.add(x.bigInteger))
        val result = decodeByNumber(Codecs.numberCodec)(bytes) map (_.value.fold(toNum))
        result should contain(x)
      }
    }
  }
  test("Should decode negative numbers") {
    forAll { (x: BigInt) =>
      whenever(0 > x && x.bitLength <= 64) {
        val bytes = withBuilder(_.add(x.bigInteger))
        val result = decodeByNegativeNumber(Codecs.negativeNumberCodec)(bytes) map (_.value.fold(toNum))
        result should contain(x)
      }
    }
  }
  test("Should provide compact representation") {
    import scodec.bits._
    val result = Codecs.utfStringCodec.encode("123")
    result should contain(hex"63 31 32 33".toBitVector)

  }
}
