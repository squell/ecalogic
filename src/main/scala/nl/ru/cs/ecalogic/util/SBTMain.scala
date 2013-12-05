package nl.ru.cs.ecalogic
package util

class SBTMain extends xsbti.AppMain {

  case class Exit(code: Int) extends xsbti.Exit

  def run(configuration: xsbti.AppConfiguration): xsbti.MainResult = Exit(ECALogic.main_(configuration.arguments))

}
