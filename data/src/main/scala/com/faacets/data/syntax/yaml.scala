package com.faacets.data
package syntax

import io.circe.Encoder
import io.circe.yaml.Printer

final class YamlOps[A](val lhs: A) extends AnyVal {

  def asYaml(implicit ev: Encoder[A]): String =
    Printer(preserveOrder = true, dropNullKeys = true, sequenceStyle = Printer.FlowStyle.Flow).pretty(ev.apply(lhs))

}

trait YamlSyntax {

  implicit def dataYaml[A:Encoder](lhs: A): YamlOps[A] = new YamlOps(lhs)

}
