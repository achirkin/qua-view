# qua-view
Web app written in Haskell (GHCJS) for visualizing building geometry.
This is a part of [qua-kit](https://github.com/achirkin/qua-kit) project - **Quick Urban Analysis Kit** -
that is being developed under the scope of [ADvISE](http://www.ia.arch.ethz.ch/advise/) reseach project.
Qua-view can be compiled into a standalone javascript application, independent of the main project.
Other parts are responsible for a server side.

Compiled application currently is available on http://qua-kit.ethz.ch/

Supports mouse and finger control. Works best on chrome (desktop or mobile), but most other browsers working too.


## developing qua-view

First, we need to install GHCJS. We install GHCJS via stack, but before that some dependencies need to be installed.
Refer to [GHCJS documentation](https://github.com/ghcjs/ghcjs/tree/ghc-8.0) to check, which dependencies we need to install.
In particular, the following command installs everything needed on Ubuntu:
```
sudo apt-get install nodejs libtinfo-dev
```
Note, you might need to make an alias `node` to `nodejs` to make GHCJS recognize the toolname correctly.

Next step is to install haskell dependencies. At this moment we use stack snapshot `lts-8.21`:
```
stack install alex happy haddock-2.17.4 haddock-api-2.17.4 hoogle --resolver=lts-8.21
```

Then, clone this repository and setup GHCJS using stack:
```
git clone -b reflex https://github.com/achirkin/qua-view
cd qua-view
stack setup
```

Another important step is to correctly build haddocks, because our crucial dependencies are not in hackage or stackage.
Run haddock in the `qua-view` project folder
```
stack haddock
```

The last thing is to run hoogle to make it easier to lookup function names.
```
stack hoogle -- server --port=8080
```
