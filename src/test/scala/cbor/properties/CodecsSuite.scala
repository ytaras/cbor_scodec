package cbor.properties

import cbor.codec._
import cbor.{CInteger, CborValue, TestModel}
import org.scalacheck.Shapeless._
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks

/**
  * Created by ytaras on 1/30/16.
  */
class CodecsSuite extends FunSuite with PropertyChecks {

  import scodec._
  import scodec.bits._

  val maxUint64 = (BigInt(1) << 64) - 1

  ignore("Should decode encoded by self") {
    forAll { (x: CborValue) =>
      val bits: BitVector = Codec.encode(x)(Codecs.singleValueCodec).toOption.get
      val retValue: CborValue = Codec.decode[CborValue](bits)(Codecs.singleValueCodec).toOption.get.value
      retValue should be(x)
    }
  }
  ignore("Should decoded encoded by co.nstant.in") {
    forAll { (x: CInteger) =>
      whenever(x.i >= 0 && x.i < maxUint64) {
        val data = TestModel.serialize(x)
        val result = Codec.decode(ByteVector(data).toBitVector)(Codecs.singleValueCodec).toOption.get.value
        result should be(x)
      }
    }
  }
}

