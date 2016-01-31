package cbor.coded

import cbor.coded.Tags.MajorType
import eu.timepit.refined.api.{RefType, Validate}
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineMT
import scodec.Codec
import shapeless.tag.@@

import scala.language.higherKinds

/**
  * Created by ytaras on 1/30/16.
  */
object Codecs {

  import eu.timepit.refined.scodec._

  val majorType = refTypeCodecCurried[@@, Byte, MajorType](scodec.codecs.byte(3))

  def refTypeCodecCurried[F[_, _], T, P](codec: Codec[T])
                                        (implicit refType: RefType[F], validate: Validate[T, P]) =
    refTypeCodec(codec, refType, validate)
}

object Tags {
  type MajorType = Positive
  val UnsignedInteger = refineMT[MajorType](1)
}
