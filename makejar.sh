echo "This creates a .jar runnable by java -jar on systems without scala."

set -e

# so what to target?
select main in $(grep -l "main" `find src/main/scala/nl -name "*.scala"` | sed 's:/:.:g;s:^.*[.]nl:nl:;s:[.]scala$::'); do
	break
done


export CLASSPATH="$CLASSPATH:`pwd`:`pwd`/src/main/scala:`pwd`/src/test/scala"

# compile the main tree
echo "Compiling src/main/scala"
(
    cd src/main/scala
    scalac `find -name "*.scala" | grep -v SBT`
)

# compile and run the test cases
echo "Compiling src/test/scala"
(
    echo "import org.scalatest" > /tmp/test.scala
    if scalac /tmp/test.scala; then
	cd src/test/scala
	scalac `find -name "*.scala"`
	cd -

	if ! scala org.scalatest.tools.Runner -R src/test/scala -o; then
	    echo "scalatests failed -- aborting compilation"
	    exit 1
	fi
    else
	echo "ScalaTest dependency not found -- skipping self-test"
    fi
    rm -f /tmp/test.scala
)

# try to locate the scala-library
LIB=/usr/share/java/scala-library.jar
test -f $LIB || LIB=`which scala | grep -o '/.*scala[^\/]*/bin'`/../lib/scala-library.jar

if [ ! -f "$LIB" ]; then
    echo "Error: scala-library.jar not found!"
    exit 2
fi

# extract the entire scala runtime
(
    cd /tmp
    jar xf "$LIB" 
)

# create a jar archive of our project + the scala library
jar cef "$main" ecalogic.jar -C src/main/scala nl -C /tmp scala

# clean up
rm -rf /tmp/scala

