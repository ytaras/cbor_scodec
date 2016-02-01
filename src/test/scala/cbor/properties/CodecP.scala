package cbor.properties

import cbor.TestModel._
import cbor._
import cbor.coded.{Codecs, Tags}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Shapeless._
import scodec.bits.ByteVector
import scodec.{Attempt, Codec, DecodeResult}
import shapeless.ops.coproduct.{Length => CoproductLength}
import shapeless.ops.hlist.{Length => HListLength}

/**
  * Created by ytaras on 1/30/16.
  */
class CodecP extends Properties("Codec") {

  implicit class ResultOps[V](inner: Attempt[DecodeResult[V]]) {
    def get: V = inner.toOption.get.value
  }

  property("Parses integer type") = forAll {
    (x: BigInteger) => x.i >= 0 ==> {
      val bytes = ByteVector(serialize(x))
      Codec.decode(bytes.toBitVector)(Codecs.majorType).get == Tags.UnsignedInteger
    }
  }
  property("Parses negative integer type") = forAll {
    (x: BigInteger) => x.i < 0 ==> {
      val bytes = ByteVector(serialize(x))
      Codec.decode(bytes.toBitVector)(Codecs.majorType).get == Tags.NegativeInteger
    }
  }
}

