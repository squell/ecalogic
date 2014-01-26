Build Instructions
==================

ECALogic uses the sbt build system (http://www.scala-sbt.org/).

Prerequisites
-------------

The following software has to be installed:
- JRE 6+
- sbt 0.13.1+
- git

Scala does not have to be installed, unless you want to build the software manually.

Build from source
-----------------

Build artifacts for the console application and web interface go into the `target/` and `web/target/`
folders respectively.

1. Get the source code:

		$ git clone https://github.com/squell/ecalogic.git
		$ cd ecalogic
		$ sbt

2. To build the console application and web interface as jar and war-files respectively:

		> compile

	To create a standalone jar of the console application that does not require Scala:

		> standalone

3. To create binary release archives of the console application:

		> distribute

Running the application
-----------------------

To run the console application from within sbt:

	> run <args>
	or
	> run-main <main-class> <args>

The main class for the console application is `nl.ru.cs.ecalogic.ECALogic`. The other main classes
are used for quick and dirty testing and may or may not function properly.

To start and stop a local web server containing the web application, from within sbt:

	> container:start
	and
	> container:stop

By default the web server can be accessed on port 8080 of the local machine.

Of course, instead of using the built-in web server, the web application can be deployed
to any web server that supports Java servlets.
