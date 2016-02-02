package cbor.codec

import cbor.{CInteger, CborValue}
import scodec._
import scodec.bits.BitVector
import scodec.codecs._

import scala.language.higherKinds

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs {

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


