 /*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package nl.ru.cs.ecalogic
package model

import nl.ru.cs.ecalogic.util.{DefaultErrorHandler, ErrorHandler, Polynomial}
import config.Options.{Model => Config}
import scala.util.{Success, Failure, Try}
import nl.ru.cs.ecalogic.ast.Import
import java.net.{URL, URLClassLoader}
import java.io.File

/**
 * @author Marc Schoolderman
 * @author Jascha Neutelings
 */
trait ComponentModel { model =>

  // Utility types
  type TDFunction  = (EACState, Polynomial) => Polynomial
  type LUBFunction = (EACState, EACState) => EACState
  type PHIFunction = CState => Polynomial
  type DFunction   = CState => CState
  type RVFunction  = (CState, Seq[ECAValue]) => ECAValue

  type CState <: ComponentState
  trait ComponentState extends PartiallyOrdered[CState] {

    val elements: Map[String, Polynomial]

    def tryCompareTo[B >: CState <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
      case that: ComponentState =>
        var sign = 0
        for (key <- elements.keys) {
          val cmp = elements(key) tryCompareTo that.elements(key) getOrElse(return None)
          if (sign*cmp < 0) return None // note: a*b<0 iff a,b have different signs
          else sign |= cmp
        }
        Some(sign)
      case _ => None
    }

    protected def update(newElements: Map[String, Polynomial]): CState

    private[ComponentModel] def update(newElements: Iterable[(String, Polynomial)]): CState = update(newElements.toMap)

    override def equals(that: Any) = that match {
      case that: ComponentState => tryCompareTo(that) == Some(0)
      case _                    => false
    }

    override def hashCode = elements.hashCode

    override def toString = elements.map {
      case (name, value) => s"$name = $value"
    }.mkString("(", ", ", ")")

  }

  case class EACState(state: CState, timestamp: Polynomial, energy: Polynomial) {

    def update(f: String, t1: Polynomial): (EACState, Polynomial) = {
      val e1 = energy + E(f)
      val t2 = t1 + T(f)
      val s1 = delta(f)(state)
      phiCheck(state, s1)

      if(s1 != state || Config.alwaysUpdate) {
        val upd = EACState(s1, t1, e1 + td(this,t1))
        if(Config.alwaysForwardTime)
          // not only update, but set it to the most recent
          (EACState(s1, t2, upd.energy + td(upd, t2)), t2)
        else
          (upd, t2)
      } else
        // do not update the timestamp if the state did not change
        (EACState(state, timestamp, e1), t2)
    }

    def forward(t1: Polynomial) =
      EACState(state, timestamp max t1, energy + td(this,t1))

    def reset = EACState(state, 0, 0)

    def update(timestamp: Polynomial, energy: Polynomial) = EACState(state, timestamp, energy)

    // why not define the lub here in the first place?
    def lub(that: ComponentModel#EACState) =
      // there has to be a better way?
      model.lub(this, that.asInstanceOf[EACState])

    /* checks if monotonicity of phi holds for s1 => s2 */
    def phiCheck(s1: CState, s2: CState) {
      val stOrder  = s1 tryCompareTo s2
      val phiOrder = phi(s1) tryCompareTo phi(s2)
      // check if the signs of the comparisons differ
      if(phiOrder.exists(ord=>stOrder.exists(_ * ord < 0)))
        throw new ECAException(s"$name::phi not monotone with respect to $s1 and $s2")
    }
  }

  val name: String

  val initialState: CState

  def E(f: String) = ECAValue.Zero

  def T(f: String) = ECAValue.Zero

  def initialEACState(timestamp: Polynomial = 0, energy: Polynomial = 0) =
    EACState(initialState, timestamp, energy)

  def lub(a: EACState, b: EACState): EACState = {
    val EACState(sa, ta, ea) = a
    val EACState(sb, tb, eb) = b

    EACState(
      sa.update(sa.elements.keys.map(key => key -> (sa.elements(key) max sb.elements(key)))),
      ta min tb,
      ea max eb
    )
  }

  // note: the check is not really necessary, since the GlobalState will
  // ensure td() will never be called with timestamps in the past.
  // but, it is better to be paranoid than wrong.
  def td(g: EACState, t: Polynomial): Polynomial =
    if (t >= g.timestamp) (t - g.timestamp) * phi(g.state) else throw new ECAException("$name::td attempt to rewind component")

  def delta(f: String)(s: CState) = s

  def phi(s: CState) = Polynomial(0)

  protected def rv(f: String)(s: CState, a: Seq[ECAValue]) = ECAValue.Zero

  def eval(f: String)(s: CState, a: Seq[ECAValue]): (CState, ECAValue) = (delta(f)(s), rv(f)(s, a))

  // None: this function does not exist
  // Some(x) where x >= 0: function exists and has arity x
  // Some(x) where < 0: function exists but arity is unknown (varargs?)
  def functionArity(f: String): Option[Int] = None

  def hasFunctionInfo = false

}

object ComponentModel {

  //TODO: Make this configurable
  lazy val ComponentPath = Seq(new File(new File(System.getProperty("ecalogic.home", ".")), "components"))

  lazy val ComponentLoader = new URLClassLoader(ComponentPath.map(_.getAbsoluteFile.toURI.toURL).toArray, getClass.getClassLoader)

  def fromImport(imprt: Import, errorHandler: ErrorHandler = new DefaultErrorHandler): ComponentModel = {
    val name = imprt.qualifiedName
    val ecmURL = Option(ComponentLoader.getResource(s"${name.replace('.', '/')}.ecm"))

    errorHandler.report {
      ecmURL.map(url => ECMModel.fromURL(url)) getOrElse {
        Try(Class.forName(name + "$", true, ComponentLoader)).map(clazz => clazz.getField("MODULE$").get(null).asInstanceOf[ComponentModel])         orElse
        Try(Class.forName(name      , true, ComponentLoader)).map(clazz => clazz.getMethod("getInstance").invoke(null).asInstanceOf[ComponentModel]) orElse
        Try(Class.forName(name      , true, ComponentLoader)).map(clazz => clazz.getField("INSTANCE").get(null).asInstanceOf[ComponentModel])        match {
          case Success(c) => c
          case Failure(e: ClassNotFoundException) => errorHandler.error(new ECAException(s"Unable to find component '$name'.", Some(imprt.position), Some(e))); null
          case Failure(e) => errorHandler.error(new ECAException(s"Error while loading component '$name': $e", Some(imprt.position), Some(e))); null
        }
      }
    }
  }

  def fromImports(imports: Map[String, Import], errorHandler: ErrorHandler = new DefaultErrorHandler): Map[String, ComponentModel] =
    imports.mapValues(fromImport(_, errorHandler)).view.force

}
