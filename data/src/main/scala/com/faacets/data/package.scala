package com.faacets

package object data {

  import io.circe.yaml.Printer
  val yamlPrinter = Printer(sequenceStyle = Printer.FlowStyle.Block)

}
