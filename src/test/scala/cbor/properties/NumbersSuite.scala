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
  test("Should encode bytes") {
    forAll { (x: Byte) =>
      whenever(0 <= x) {
        val bytes = withBuilder(_.add(x))
        val result = decodeByNumber(Codecs.byteCodec)(bytes)
        result should contain(x)
      }
    }
  }

}
