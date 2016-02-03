package cbor.codec

import scodec._
import scodec.bits.{BitVector, _}
import scodec.codecs._

import scala.language.higherKinds

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs extends NumberCodecs {

  val singleValueCodec =
    constant(bin"000") ~> numberCodec
}

trait NumberCodecs {
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
  val numberCodec = (smallByteCodec :+: uint8Codec :+: uint16Codec :+: uint32Codec :+: uint64Codec).choice

  def prefixedCodec[A](p: Byte, c: Codec[A]) = {
    val marker = BitVector.fromByte(p).drop(3)
    require(marker.toByte(signed = false) == p)
    require(marker.size == 5)
    constant(marker) ~> c
  }

}

