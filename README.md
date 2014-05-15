hedge-alg
=========
sudo apt-get install libghc-executable-path-dev
sudo apt-get install libghc-control-monad-loop-dev 
sudo apt-get install ibghc-monad-loops-dev 
sudo apt-get install libghc-readline-dev 
sudo apt-get install libghc-gtk-dev
sudo apt-get install libghc-glade-dev
~/.cabal/bin/graphmod -i ./ -i /usr/lib/haskell-packages/ghc/lib/ -a testground.hs | dot -Grankdir=LR -Tpng > dep.png
