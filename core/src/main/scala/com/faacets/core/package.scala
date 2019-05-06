package com.faacets

import shapeless.Witness

/** Package dealing with Bell scenarios and associated objects (inequalities, correlations...). */
package object core {

  @inline def valueOf[S <: Singleton](implicit wS: Witness.Aux[S]): S = wS.value

}
