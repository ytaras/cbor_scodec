package cbor.codec

import cbor.{CInteger, CborValue}
import scodec._
import scodec.bits.BitVector
import scodec.codecs._

import scala.language.higherKinds

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs extends NumberCodecs {

  val intValueCodec = new Codec[CInteger] {
    val sizeCodec = ubyte(5)
    // TODO - Maybe use disc union?
    val decoder: Decoder[Long ~ Long] = sizeCodec.asDecoder flatMap {
      case 24 => provide(0L) ~ ulong(8)
      case 25 => provide(0L) ~ ulong(16)
      case 26 => provide(0L) ~ ulong(32)
      case 27 => ulong(32) ~ ulong(32)
      case b => provide(0L ~ b.toLong)
    }

    override def encode(value: CInteger): Attempt[BitVector] = ???

    override def sizeBound: SizeBound = sizeCodec.sizeBound.atLeast

    override def decode(bits: BitVector): Attempt[DecodeResult[CInteger]] = decoder.map(CInteger(_)).decode(bits)
  }

  val singleValueCodec = (byte(3).unit(0) ~> intValueCodec).upcast[CborValue]


}

trait NumberCodecs {

  import scodec.bits._

  val smallByteCodec: Codec[Short] = {
    val validate: Short => Attempt[Short] = {
      case x if 0 <= x && x <= 23 => Attempt.successful(x)
      case x => Attempt.failure(Err(s"$x is too large to fit into 5 bit cbor"))
    }
    ushort(5).exmap(validate, validate)
  }
  val largerByteCodec: Codec[Short] = {
    val marker = bin"11000"
    require(marker.toShort(signed = false) == 24)
    require(marker.size == 5)
    constant(marker) ~> ushort(8)
  }
  val byteCodec: Codec[Short] = codecs.choice(smallByteCodec, largerByteCodec)

}

