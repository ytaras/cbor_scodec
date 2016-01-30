package cbor

/**
  * Created by ytaras on 1/30/16.
  */
object TestModel {

  sealed trait CborTree

  case class Integer(i: Int) extends CborTree

}
