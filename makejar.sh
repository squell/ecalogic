# so what to target?
select main in $(grep -l "main" `find nl -name "*.scala"` | sed 's:/:.:g;s:[.]scala$::'); do
	break
done

# compile either linearly...
scalac `find -name "*.scala"`

# or by abusing the system for a bit
#for file in `find -name "*.scala"`; do scalac "$file" & done
wait

pushd /tmp >/dev/null
jar xf "`which scala | grep -o '/tmp/[^\/]*/'`lib"/scala-library.jar
popd >/dev/null
jar cef "$main" ecalogic.jar nl -C /tmp scala
rm -rf /tmp/scala
