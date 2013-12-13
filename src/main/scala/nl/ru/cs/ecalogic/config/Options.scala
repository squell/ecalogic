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
package config

import util.{Positional, Position}
import scala.collection.mutable

/*
  Stub.
TODO: adds some code that reads cmdline flags here.
 */

object Options {
  def reset {
    noCPU = false;
    entryPoint = "program"
    terse = false
    aliasOverrides = Seq.empty[String]
    Model.alwaysUpdate = false
    Model.alwaysForwardTime = false
    Analysis.beforeSync = false
    Analysis.afterSync = false
    Analysis.fixPatience = 10000
    Analysis.techReport = false
  }

  /* Disable the simulated CPU; this switch should be deprecated in the future */
  @deprecated ("this variable is ignored by everybody in the world", "0.666xyzzy")
  var noCPU = false

  /* The "main" function to analyze; should change to 'main' in the future */
  var entryPoint = "program"

  /* Should the output of our program be to the point or nice to read? */
  var terse = false

  /* Override re-aliassing giving on the commandline */
  var aliasOverrides = Seq.empty[String]

  /* NOTE: there may be some interplay between these options; also, some
     options may be unnecessary if you enable other options. */

  object Model {
    /* Should a delta-function always update the energy-aware
       state-information even if the component state did not change?

       Technical report: false */
    var alwaysUpdate = false

    /* Should, on a energy state-update, the component-update also take
       into account the time the function itself takes? I.e. should the
       timestamp be set to the most recent time (true) or the last
       state change (false)

       Technical report: false */
    var alwaysForwardTime = false
  }

  object Analysis {
    /* Should all component states be forwarded just before a decision
       in the control flow? (if/while statement). Setting this to false
       results in over-estimations.

       Technical report: false */
    var beforeSync = false

    /* In the original document, at the exit of a while-loop, all timestamps
       of components get reset to the time before entering the while loop,
       while the global timestamp gets set to a time *after*. This is consistent
       with the if-statement (similar problem), but causes a factor two over-
       estimation in even simple cases.

       Setting this to true causes much tighter bounds on if and while's, but
       the question is, is it correct?

       Technical report: false */
    var afterSync = false

    /* How long should we attempt to find fixpoint? Note that 10000 is a high setting */
    var fixPatience = 10000

    /* Should we use the while-rule mentioned in the tech-report, or the simplified one
       mentioned in the paper?

       Technical report: true */
    var techReport = false

  }

  def apply(args: Array[String]): Array[String] = {
    import Analysis._
    import Model._

    val argHandler: mutable.Queue[String=>Unit] = mutable.Queue.empty
    val newArgs = Array.newBuilder[String]

    args.foreach {
      case "-0" | "--noCPU"
        => noCPU = true
      case "-t" | "--terse"
        => terse = true
      case "-e" | "--entry"
        => argHandler += (s => entryPoint = s)
      case "-tr" | "--techReport"
        => techReport = true
      case "-P" | "--fixPatience"
        => argHandler += (s => try fixPatience = s.toInt)
      case "-I" | "--import"
        => argHandler += (s => aliasOverrides = s+:aliasOverrides)
      case "-s0" | "--beforeSync"
        => beforeSync = true
      case "-s1" | "--afterSync"
        => afterSync = true
      case "-s" | "--sync"
        => beforeSync = true; afterSync = true
      case "-u0" | "--alwaysUpdate"
        => alwaysUpdate = true
      case "-u1" | "--alwaysForwardTime"
        => alwaysForwardTime = true
      case "-u" | "--update"
        => alwaysUpdate = true; alwaysForwardTime = true
      case "-h" | "-?" | "--help"
        => friendlyHelpMsg(); return Array.empty
      case s if s.startsWith("-")
        => throw new ECAException(s"unknown flag: $s")
      case s if argHandler.nonEmpty
        => argHandler.dequeue()(s)
      case s
        => newArgs += s
    }

    newArgs.result()
  }

  def friendlyHelpMsg() {
    println("""
      ... blah
""")
  }

  def main(args: Array[String]) =
    println(apply(args).reduce(_+", "+_))

}

