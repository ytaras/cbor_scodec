package cbor.properties

import cbor.TestModel.withBuilder
import cbor.codec._
import org.scalacheck.Shapeless._
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks

/**
  * Created by ytaras on 1/30/16.
  */
class CodecsSuite extends FunSuite with PropertyChecks with ScodecHelpers {

  test("Should decode positive numbers") {
    forAll { (x: Long) =>
      whenever(0 <= x) {
        val bytes = withBuilder(_.add(x))
        val result = decode(Codecs.singleValueCodec)(bytes) map (_.value.fold(toNum))
        result should contain(x)
      }
    }
  }
}

