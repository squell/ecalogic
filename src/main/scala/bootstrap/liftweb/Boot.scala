package bootstrap.liftweb

import net.liftweb.http.{Html5Properties, LiftRules, Req}
import net.liftweb.sitemap.{Menu, SiteMap}
import net.liftmodules.JQueryModule
import net.liftweb.http.js.jquery.JQueryArtifacts

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("nl.ru.cs.ecalogic.web")
    // Build SiteMap
    def sitemap(): SiteMap = SiteMap(
      Menu.i("Home") / "index"
    )

    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))
  }
}
