package cbor.codec

import scodec._
import scodec.bits.{BitVector, _}
import scodec.codecs._
import shapeless._
import shapeless.ops.coproduct.Mapper

import scala.language.higherKinds

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs extends NumberCodecs with StringCodecs {

  val singleValueCodec: Codec[NumberChoice] = choice(
    constant(bin"000") ~> numberCodec,
    constant(bin"001") ~> negativeNumberCodec
  )
}

trait StringCodecs {
  self: NumberCodecs =>
  val utfStringCodec: Codec[String] =
    variableSizeBytesLong(stringSize(bin"011"), utf8)
  val binaryDataCodec =
    variableSizeBytesLong(stringSize(bin"010"), bytes)

  // TODO Choose compact representation
  def stringSize(prefix: BitVector): Codec[Long] =
    (constant(prefix) ~> numberCodec).xmapc(_.map(toLong).unify)(x => Coproduct[NumberChoice](x))

  object toLong extends Poly1 {
    implicit def number[N: Numeric]: Case.Aux[N, Long] =
      at[N](implicitly[Numeric[N]].toLong)
  }

}

trait NumberCodecs extends NumberOps {
  val smallByteCodec = {
    val validate: Byte => Attempt[Byte] = {
      case x if 0 <= x && x <= 23 => Attempt.successful(x)
      case x => Attempt.failure(Err(s"$x is too large to fit into 5 bit cbor"))
    }
    ubyte(5).exmap(validate, validate)
  }
  val uint8Codec = prefixedCodec(24, ushort(8))
  val uint16Codec = prefixedCodec(25, uint(16))
  val uint32Codec = prefixedCodec(26, ulong(32))
  val uint64Codec = {
    def decoder: (Long ~ Long) => BigInt = {
      case (o ~ t) => (BigInt(o) << 32) + t
    }
    def encoder: BigInt => Attempt[Long ~ Long] = { x =>
      if (x.bitLength <= 64) {
        Attempt.failure(Err(s"$x doesnt fit into 64 bits"))
      } else {
        val lower = x.toLong
        val upper = (x >> 32).toLong
        Attempt.successful(upper ~ lower)
      }
    }
    prefixedCodec(27, ulong(32) ~ ulong(32)).widen[BigInt](decoder, encoder)
  }
  val numberCodec: Codec[NumberChoice] = (smallByteCodec :+: uint8Codec :+: uint16Codec :+: uint32Codec :+: uint64Codec).choice
  import Mapper._

  val negativeNumberCodec: Codec[NumberChoice] = numberCodec.xmapc(x => x.map(negate))(x => x.map(negate))

  def prefixedCodec[A](p: Byte, c: Codec[A]) = {
    val marker = BitVector.fromByte(p).drop(3)
    require(marker.toByte(signed = false) == p)
    require(marker.size == 5)
    constant(marker) ~> c
  }

  }

trait NumberOps {
  type NumberChoice = Byte :+: Short :+: Int :+: Long :+: BigInt :+: CNil

  object negate extends Poly1 {
    implicit def number[N: Numeric]: Case.Aux[N, N] = at[N] { x =>
      val instance = implicitly[Numeric[N]]
      val minusOne: N = instance.fromInt(-1)
      instance.minus(minusOne, x)
    }
  }
}

