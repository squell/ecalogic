seq(webSettings :_*)

name := "ecalogic-web"

libraryDependencies ++= {
  val liftVersion = "2.6-M2"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile" // Required for lift
    //,"net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile" // Required for database access, not used right now
    ,"net.liftmodules"   %% "lift-jquery-module_2.6" % "2.5" // Required for jquery
	,"net.liftmodules"   %% "textile_2.5" % "1.3" % "compile->default" // Required to parse text to html
    ,"org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test" // Required for lift
    ,"org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar") // Unsure why we need this, leaving it in for now, perhaps related to deployment?
    ,"ch.qos.logback"    % "logback-classic"     % "1.0.6" // Required for log messages, we need this.
    //,"org.specs2"        %% "specs2"             % "1.14"             % "test" // Could be used for testcases
    //,"com.h2database"    % "h2"                  % "1.3.167" // Could be used for database access
  )
}

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }
