echo "This creates a .jar runnable by java -jar on systems without scala."

cd src/main/scala

# so what to target?
select main in $(grep -l "main" `find nl -name "*.scala"` | sed 's:/:.:g;s:[.]scala$::'); do
	break
done

# compile either linearly...
scalac `find -name "*.scala"`

# or by abusing the system for a bit
#for file in `find -name "*.scala"`; do scalac "$file" & done
wait

# try to locate the scala-library
LIB=/usr/share/java/scala-library.jar
test -f $LIB || LIB=`which scala | grep -o '/.*scala[^\/]*/bin'`/../lib/scala-library.jar

pushd /tmp >/dev/null
jar xf `which scala | grep -o '/.*scala[^\/]*/bin'`/../lib/scala-library.jar
popd >/dev/null
jar cef "$main" ../../../ecalogic.jar nl -C /tmp scala
rm -rf /tmp/scala
