# qua-view
Web app written in Haskell (GHCJS) for visualizing building geometry.
This is a part of [qua-kit](https://github.com/achirkin/qua-kit) project - **Quick Urban Analysis Kit** -
that is being developed under the scope of [ADvISE](http://www.ia.arch.ethz.ch/advise/) reseach project.
Qua-view can be compiled into a standalone javascript application, independent of the main project.
Other parts are responsible for a server side.

Note that qua-view depends on
[ghcjs-webgl](https://github.com/achirkin/ghcjs-webgl), 
[fastvec](https://github.com/achirkin/fastvec), and
[ghcjs-base-alt](https://github.com/achirkin/ghcjs-base-alt),
which are available only via github.

Compiled application currently is available on http://qua-kit.ethz.ch/

Supports mouse and finger control. Works best on chrome (desktop or mobile), but most other browsers working too.

### GHCJS + WebGL issues

* It would be nice to unify interface of WebGL and OpenGL libraries. Now the differencies are significant.
* It also would be nice to connect ghcjs-dom and ghcjs-webgl to be able to compile the same app using both - ghcjs and ghc.
