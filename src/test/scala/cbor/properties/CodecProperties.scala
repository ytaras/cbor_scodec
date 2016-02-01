package cbor.properties

import cbor.TestModel._
import cbor._
import cbor.coded.{Codecs, Tags}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Shapeless._
import scodec.Codec
import scodec.bits.ByteVector
import shapeless.ops.coproduct.{Length => CoproductLength}
import shapeless.ops.hlist.{Length => HListLength}

/**
  * Created by ytaras on 1/30/16.
  */
class CodecP extends Properties("Codec") {
  property("Parses major type") = forAll { (x: CborTree) =>
    val bytes = ByteVector(serialize(x))
    Codec.decode(bytes.toBitVector)(Codecs.majorType) == Tags.UnsignedInteger
  }
}

