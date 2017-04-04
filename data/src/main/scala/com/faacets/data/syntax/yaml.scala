package com.faacets.data
package syntax

import io.circe.Encoder
import io.circe.yaml.OrderPreservingPrinter


final class YamlOps[A](val lhs: A) extends AnyVal {

  def asYaml(implicit ev: Encoder[A]): String = OrderPreservingPrinter.spaces2.pretty(ev(lhs))

}

trait YamlSyntax {

  implicit def dataYaml[A:Encoder](lhs: A): YamlOps[A] = new YamlOps(lhs)

}
