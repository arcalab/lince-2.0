package lince.frontend

import caos.frontend.Site.initSite
import lince.syntax.Lince
import lince.syntax.Lince.Program

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[Program](CaosConfig)
}