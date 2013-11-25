#!/bin/bash
set -e
ghc -rtsopts --make -O2 Qlen.hs
display(){
  case "$1" in
    (*.png)
      if [ "$DISPLAY" = '' ]; then
        echo "\$DISPLAY is empty, skipping display..." >>/dev/stderr
      elif [ -x /usr/bin/qiv ]; then
        /usr/bin/qiv "$1"
      else
        echo "Cannot run /usr/bin/qiv..." >>/dev/stderr
      fi;;
    (*.json)
      if [ -x /usr/bin/jq ]; then
        /usr/bin/jq . "$1" | less
      else
        echo "Cannot run /usr/bin/jq..." >>/dev/stderr
      fi;;
    (*)
      echo "No rule to display \`$1'" >>/dev/stderr;;
  esac
}

qlen(){
  time ./Qlen +RTS -h -RTS "$@"
}

case "$1" in
  (macinfo|minmax)
    echo "Writing $2.$1.inp..." >>/dev/stderr
    qlen "$1" < "$2" > "$2.$1".inp
    echo "Writing $2.$1.png..." >>/dev/stderr
    splot -colorscheme pale=grey \
          -tickInterval 60000 \
          -w 1920 -h 1080 \
          -tf '%H:%M:%S' \
          '-if' "$2.$1".inp \
          -o "$2.$1".png
    display "$2.$1".png;;
  (sum)
    echo 'Writing sum.inp...' >>/dev/stderr
    qlen sum < "$2" > "$2.$1".inp
    echo 'Writing sum.png...' >>/dev/stderr
    tplot +dk lines -w 1920 -h 1080 \
          -tf '%H:%M:%S' \
          '-if' "$2.$1".inp -o "$2.$1".png
    display "$2.$1".png;;
  (xpose)
    echo "Writing $1.json..." >>/dev/stderr
    qlen "$1" < "$2" > "$2.$1".json
    display "$2.$1".json;;
  (*)
    echo "Unexpected argument \`$1'";;
esac
