package com.faacets

package object data {

  object all extends AllInstances

  object safeLong extends SafeLongInstances
  object rational extends RationalInstances
  object vecRational extends VecRationalInstances
/*

  object bigInt extends BigIntInstances
  object grp extends GrpInstances
  object perm extends PermInstances
  object listMap extends ListMapInstances
  object jsonObject extends JsonObjectInstances*/
  /*

  implicit class RichTraversable[T <: Traversable[_]](val lhs: T) extends AnyVal {
    def noneIfEmpty: Option[T] = if (lhs.isEmpty) None else Some(lhs)
  }
  class AllPaths(path: JsPath, sub: Seq[String]) {
    object json {
      def pruneTransformer: Reads[JsObject] = sub.map( str => (path \ str).json.prune ).reduce(_ andThen _)
      def jsObjectFormat: OFormat[JsObject] = OFormat[JsObject](Reads[JsObject](js => js.validate[JsObject]), OWrites[JsObject](identity))
      def prune: OFormat[JsObject] =
        if (sub.isEmpty) jsObjectFormat else OFormat(pruneTransformer, OWrites[JsObject](_.transform(pruneTransformer).get))
    }
  }
  implicit class RichJsPath(path: JsPath) {
    def \(sub: Seq[String]) = new AllPaths(path, sub)
  }
  implicit class WithCheck[A](val merge: Merged[A]) extends AnyVal {
   }*/
  

}
