#! /bin/sh
cabal clean && cabal configure --enable-library-coverage && cabal build || exit 1

rm -rf tix
mkdir tix
export HPCTIXDIR=tix

./dist/build/intake/intake || exit 1
./dist/build/intake/intake show || exit 1

./dist/build/intake/intake serve &
SERVER=$!
sleep 2

curl -H "Accept: text/plain" http://127.0.0.1:7001/workflows
curl -H "Accept: application/json" http://127.0.0.1:7001/workflows
curl -H "Accept: text/plain" http://127.0.0.1:7001/workflows/a -d ''
curl -H "Accept: application/json" http://127.0.0.1:7001/workflows/a -d ''

ID=$(./dist/build/intake/intake run a | cut -f 3 -d ' ')
echo $ID
./dist/build/intake/intake show $ID
./dist/build/intake/intake status $ID

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

rm -r coverage
mkdir -p coverage
TIX=$(ls tix/*.tix | head -n 1)
cp $TIX intake.tix
TIXS=$(ls tix/*.tix | tail -n +2)
for i in $TIXS ; do
  hpc combine --union $i intake.tix --output new.tix
  mv new.tix intake.tix
done
hpc markup intake.tix --hpcdir dist/hpc/mix/intake-0.0.0 --destdir coverage
hpc report intake.tix --hpcdir dist/hpc/mix/intake-0.0.0
rm intake.tix
rm -rf tix
cabal haddock --internal --hyperlink-source
