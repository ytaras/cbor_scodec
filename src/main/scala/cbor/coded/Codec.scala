package cbor.coded

import cbor.coded.Tags.MajorType
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineT
import scodec.GenCodec
import scodec.bits.BitVector
import scodec.codecs.bits
import shapeless.tag.@@

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs {
  val majorType: GenCodec[BitVector, Byte @@ MajorType] =
    bits(3).map { x =>
      refineT[MajorType](x.toByte(false))
    }

}

object Tags {
  type MajorType = Positive
}
