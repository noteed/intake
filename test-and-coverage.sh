#! /bin/sh
cabal clean || exit 1
cabal configure --enable-library-coverage --enable-tests || exit 1
cabal build || exit 1

./dist/build/intake/intake run -f tests/invoked-echo-a.txt || exit 1
./dist/build/intake/intake run -f tests/invoked-invoked-true.txt || exit 1
./dist/build/intake/intake run -f tests/invoked-true.txt || exit 1
runghc -Idist/build/ tests/invoked-true.hs || exit 1
# TODO intake run -f tests/invoked-false.txt || exit 1
runghc -Idist/build/ tests/invoked-false.hs || exit 1
# TODO intake run -f tests/invoked-invoked-false.txt || exit 1
runghc -Idist/build/ tests/invoked-invoked-false.hs || exit 1

rm -rf tix
mkdir tix
# With the HPCTIXDIR environment variable set, an HPC-instrumented process
# will use its own .tix file. This is necessary here as Intake is run
# concurrently with itself: a server and multiple clients.
# `hpc combine` can then be used to aggregate the .tix files in a single
# new .tix file, used as usual with `hpc report`, `hpc markup` or
# `covered markup`.
export HPCTIXDIR=tix

./dist/build/intake/intake || exit 1
./dist/build/intake/intake show || exit 1

./dist/build/intake/intake serve &
SERVER=$!
sleep 2

./dist/build/intake/intake run -f tests/intake-curl-text-workflows.txt || exit 1
./dist/build/intake/intake run -f tests/intake-curl-json-workflows.txt || exit 1
curl -H "Accept: text/plain" http://127.0.0.1:7001/workflows/a -d ''
curl -H "Accept: application/json" http://127.0.0.1:7001/workflows/a -d ''

runghc -Idist/build/ tests/intake-run-00.hs || exit 1

# ./dist/build/intake/intake status x # no such build
mkdir ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
mkdir ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbb/
# ./dist/build/intake/intake status a # more than one build
rm -r ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
rm -r ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbb

ID=$(./dist/build/intake/intake run ab | cut -f 3 -d ' ')
echo $ID
# ./dist/build/intake/intake run xxx # no such workflow

kill -INT $SERVER
sleep 2

# Combine all the .tix files in a single intake.tix file.
TIX=$(ls tix/*.tix | head -n 1)
cp $TIX intake.tix
TIXS=$(ls tix/*.tix | tail -n +2)
for i in $TIXS ; do
  hpc combine --union $i intake.tix --output new.tix
  mv new.tix intake.tix
done

rm -r coverage
mkdir -p coverage
hpc markup intake.tix --hpcdir dist/hpc/mix/intake-0.0.0 --destdir coverage
hpc report intake.tix --hpcdir dist/hpc/mix/intake-0.0.0
covered markup --hpcdir dist/hpc/mix/intake-0.0.0/ intake
rm intake.tix
rm -rf tix
cabal haddock --internal --hyperlink-source
