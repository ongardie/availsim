set -ex
BIN=./main
COMMON="--servers=5"
make opt

for timing in LAN BadRecv P1 P2; do
    mkdir -p $timing
    (
        cd $timing
        ../$BIN $COMMON --timing=$timing --samples=1000000
        Rscript ../plots.R
    )
done
