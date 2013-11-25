ghc --make -O2 minmax.hs
./minmax < $1 > minmax.inp
splot -colorscheme pale=grey -tickInterval 60000 -w 1920 -h 1080 -tf '%H:%M:%S' -if minmax.inp -o minmax.png
qiv minmax.png
