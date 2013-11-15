package bootstrap.liftweb

/**
 * Created with IntelliJ IDEA.
 * User: User
 * Date: 15-11-13
 * Time: 16:26
 * To change this template use File | Settings | File Templates.
 */

import net.liftweb.http.{Html5Properties, LiftRules, Req}
import net.liftweb.sitemap.{Menu, SiteMap}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("nl.ru.cs.ecalogic")

    // Build SiteMap
    def sitemap(): SiteMap = SiteMap(
      Menu.i("Home") / "index"
    )

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))
  }
}
