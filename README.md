# qua-view
Web app written in Haskell (GHCJS) for visualizing building geometry.
This is a part of [qua-kit](https://github.com/achirkin/qua-kit) project - **Quick Urban Analysis Kit** -
that is being developed under the scope of [ADvISE](http://www.ia.arch.ethz.ch/advise/) reseach project.
Qua-view can be compiled into a standalone javascript application, independent of the main project.
Other parts are responsible for a server side.

Compiled application currently is available on http://qua-kit.ethz.ch/

Supports mouse and finger control. Works best on chrome (desktop or mobile), but most other browsers working too.


## Developer setup

First, we need to install GHCJS. We install GHCJS via stack, but before that some dependencies need to be installed.
Refer to [GHCJS documentation](https://github.com/ghcjs/ghcjs/tree/ghc-8.0) to check, which are needed.
In particular, the following command installs everything needed on Ubuntu:

    sudo apt-get install libtinfo-dev nodejs nodejs-legacy npm

(`nodejs-legacy is only needed for the alias from `node` to `nodejs`.)

Next step is to install haskell dependencies. At this moment we use stack snapshot `lts-8.21`:

    stack install alex happy haddock-2.17.4 haddock-api-2.17.4 hscolour hoogle --resolver=lts-8.21

Then, clone this repository and setup GHCJS using stack:

```
git clone -b reflex https://github.com/achirkin/qua-view
cd qua-view
ln -s `stack path --compiler-bin`/ghcjs-pkg `stack path --compiler-bin`/ghc-pkg
stack setup
```

### Build documentation

Another important step is to correctly build haddocks, because our crucial dependencies are not in hackage or stackage.
Run haddock in the `qua-view` project folder (do this before running `stack build` the first time).

```
stack haddock
```

The last thing is to run hoogle to make it easier to lookup function names.

```
stack hoogle generate -- --local
stack hoogle -- server --port=8080 --local
```

Also have a look at the following file for `GHCJS` and `JavaScript` module docs

    ~/.ghcjs/XXX/ghcjs/doc/html/index.html

and this [Reflex tutorial](https://github.com/reflex-frp/reflex-platform#tutorial)
and [function reference](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md).

#### Hoogle for both qua-server and qua-view

```
cd [root qua-kit folder]
stack install hoogle
pushd apps/hs/qua-server
stack haddock
popd
pushd apps/hs/qua-view
stack haddock
popd
stack exec hoogle -- generate \
                  --local=apps/hs/qua-server/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/doc \
                  --local=apps/hs/qua-view/.stack-work/install/x86_64-linux/lts-8.21/ghcjs-0.2.1.9008021_ghc-8.0.2/doc \
                  --local=/home/achirkin/.stack/snapshots/x86_64-linux-nopie/lts-8.21/8.0.2/doc \
                  --local=/home/achirkin/.stack/snapshots/x86_64-linux/lts-8.21/ghcjs-0.2.1.9008021_ghc-8.0.2/doc
stack exec hoogle -- server --port=8080 --local
```

### Build the project

Finally, you can generate the website into the `/web` directory:

    stack build --file-watch

To view the result, you need to run a small web server due to browsers' same-origin policy, e.g.

    cd qua-view/web
    python -m SimpleHTTPServer
