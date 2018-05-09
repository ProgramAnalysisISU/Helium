#!/bin/bash

SCRIPT=$(readlink -f "$0")
DIR=$(dirname "$SCRIPT")

echo "
#!/bin/sh
# file: hc
clang -Xclang -load -Xclang $DIR/build/lib/libhelium.so \"\$@\"
" > /usr/local/bin/hcc

chmod a+x /usr/local/bin/hcc
echo "export PATH=$DIR:\$PATH" >> $HOME/.bashrc
