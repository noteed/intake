#! /bin/sh
rm -f intake.tix
cabal clean && cabal configure --enable-library-coverage && cabal build || exit 1
./dist/build/intake/intake || exit 1
./dist/build/intake/intake show
./dist/build/intake/intake run a > RUN-A
ID=`tail -n1 RUN-A`
rm RUN-A
echo $ID
./dist/build/intake/intake show $ID
./dist/build/intake/intake status $ID
echo Completed > ~/.intake/$ID/0/state
./dist/build/intake/intake status $ID
rm -r ~/.intake/$ID
./dist/build/intake/intake status x # no such build
mkdir ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
mkdir ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbb/
./dist/build/intake/intake status a # more than one build
rm -r ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
rm -r ~/.intake/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbb
./dist/build/intake/intake run ab > RUN-AB
ID=`tail -n1 RUN-AB`
rm RUN-AB
echo $ID
./dist/build/intake/intake run xxx # no such workflow
rm -r ~/.intake/$ID
rm -r coverage
mkdir -p coverage
hpc markup intake --hpcdir dist/hpc/mix/intake-0.0.0 --destdir coverage
hpc report intake --hpcdir dist/hpc/mix/intake-0.0.0
