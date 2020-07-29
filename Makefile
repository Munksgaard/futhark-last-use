test: tests/alias.actual tests/psum.actual
	diff -u tests/alias.expected tests/alias.actual
	diff -u tests/psum.expected tests/psum.actual

tests/psum.actual: tests/psum.fut LastUse.hs Main.hs
	cabal run futhark-last-use -- tests/psum.fut > tests/psum.actual

tests/alias.actual: tests/alias.fut LastUse.hs Main.hs
	cabal run futhark-last-use -- tests/alias.fut > tests/alias.actual

clean:
	rm tests/*.actual
