ghc --make -O2 sum.hs
./sum < $1 > sum.inp
tplot +dk lines -w 1920 -h 1080 -tf '%H:%M:%S' -if sum.inp -o sum.png
qiv sum.png
