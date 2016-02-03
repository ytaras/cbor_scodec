package cbor.properties

import cbor.TestModel.withBuilder
import cbor.codec.Codecs
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks
import shapeless.Poly1

/**
  * Created by ytaras on 2/3/16.
  */
class NumbersSuite extends FunSuite with PropertyChecks with ScodecHelpers {

  object toNum extends Poly1 {
    implicit def caseNum[N: Numeric] = at[N](identity)

    implicit def caseLongLong = at[(Long, Long)] { case (o, t) =>
      (BigInt(o) << 32) + t
    }

  }
  test("Should encode bytes") {
    forAll { (x: Long) =>
      whenever(0 <= x) {
        val bytes = withBuilder(_.add(x))
        val result = decodeByNumber(Codecs.numberCodec)(bytes) map (_.value.fold(toNum))
        result should contain(x)
      }
    }
  }

}
