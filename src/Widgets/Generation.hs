{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI #-}
module Widgets.Generation
    ( setInnerHTML, getElementById, makeElementFromHtml
    , qhtml, qcss, qjs
    , newVar, returnVars
    , hamlet, cassius, julius
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import qualified GHCJS.DOM.Element as Element (js_setInnerHTML, Element)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddTopDecls)
import GHCJS.Nullable

import Text.Hamlet (HtmlUrl,hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Julius (JavascriptUrl, renderJavascriptUrl, julius)
import Text.Cassius (cassius)
import Text.Internal.Css (CssUrl, renderCss)
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.IO.Class
import Reflex.Class (Reflex)
import Reflex.Dom ( Element (..), GhcjsDomSpace, GhcjsDomSpace, Widget, ElementConfig
                  , EventResult, wrapRawElement, extractRawElementConfig, placeRawElement)


import System.IO (FilePath)
import System.FilePath ((</>), (<.>))



-- | Set content of a given element to a static JSString.
--   Useful together with qhtml splice.
setInnerHTML :: MonadIO m => Element er GhcjsDomSpace t -> JSString -> m ()
setInnerHTML e x = liftIO $ Element.js_setInnerHTML (_element_raw e) x


-- | Generate HTML into a JSString at compile time.
--
--   > setInnerHTML e $(qhtml [shamlet| <div>Hello World! |])
--
--   This is a preferred method to put moderate chunks of html code into a widget,
--   because it allows to store generated html as a plain JavaScript string.
qhtml :: HtmlUrl url -> Q Exp
qhtml h = do
    fName <- newGlobalUnique >>= newName
    qAddTopDecls [ForeignD (ImportF JavaScript Unsafe funCode fName (ConT ''JSString) )]
    return $ VarE fName
  where
    funCode = "$r = " ++ show (renderHtml $ h (const $ const SText.empty)) ++ ";"


-- | Generate CSS into a .css file at compile time.
--
--   This function does no return an expression, so you have to use `returnVars` function inside a splice.
--
--   > let myClassId = $(do
--   >         myClass <- newVar
--   >         qcss [cassius|
--   >                .#{myClass}
--   >                    width: 80%
--   >                    color: #01FF9A
--   >              |]
--   >         returnVars [myClass]
--   >       )
--
--   This is a preferred method to put moderate chunks of css code into a widget,
--   because it allows to keep css separately from js.
--
--   NOTE: this code relies on an assumption that all TH splices in a single file run in a single
--         environment (i.e. they share IORefs created by `unsafePerformIO newIORef`).
qcss :: CssUrl url -> Q ()
qcss css = do
    mName <- getModuleName
    let fNameFull = cssGenPath </> mName <.> "css"
        content = JSString.pack . LText.unpack $ renderCss (css undefined)
    writeCss fNameFull content


-- | Generate JavaScript into a JSString at compile time and execute it as an action.
--   The content of JavaScript script is transformed into a string and evaluated using function 'eval'.
--   This means you should not put top-level function declarations there.
--
--   > $(qjs [julius| console.log("hello!") |] :: MonadIO m => m ()
--
--   Please do not (ab)use this unless strictly necessary!
qjs :: JavascriptUrl url -> Q Exp
qjs j = do
    fName <- newGlobalUnique >>= newName
    qAddTopDecls [ForeignD (ImportF JavaScript Unsafe funCode fName (ConT ''IO `AppT` ConT ''()) )]
    return $ VarE 'liftIO `AppE` VarE fName
  where
    funCode = "eval(" ++ show (renderJavascriptUrl (const $ const SText.empty) j) ++ ");"




-- | Use this to get values of unique vars inside a splice.
--   It returns n-tuple consisting of literal text values.
--   You can use it in let or case binding, or  as a return value.
--
--   > let (class1, id1, id2) = $(returnVars ["classname1", "idname1", "anotheridname"])
returnVars :: [Text] -> Q Exp
returnVars [] = return (ConE '())
returnVars [name] = return (LitE (StringL $ SText.unpack name) `SigE` ConT ''Text)
returnVars names  = pure $ foldl (\t n -> t `AppE` (LitE (StringL $ SText.unpack n) `SigE` ConT ''Text))
                              (ConE $ tupleDataName (length names)) names

-- | Generate new unique variable to use it in css or js as a class or element identifier
newVar :: Q Text
newVar = SText.pack <$> newGlobalUnique



----------------------------------------------------------------------------------------------------
-- * Generating unique names
----------------------------------------------------------------------------------------------------

-- | Write css code to a new file if it should not exist by now,
--   or append css code to an existing file (if there were splices writing css above in the module).
writeCss :: String -> JSString -> Q ()
writeCss mname c = runIO $ do
    isNew <- atomicModifyIORef' cssStarted
                   $ \s -> if Set.member mname s then (s, False)
                                                 else (Set.insert mname s, True)
    if isNew then js_writeFile  (JSString.pack mname) c
             else js_appendFile (JSString.pack mname) c


-- | Set {module name}
--   A set of modules which have a css file bound.
--   Don't expect this set to have all css files.
--   The idea behind this variable is to track if current module has started its css file.
cssStarted :: IORef (Set String)
cssStarted  = unsafePerformIO (newIORef Set.empty)


-- | Write JSString into file.
--   Use this only in TH environment!
--
--   We have to use this function instead of normal haskell writeFile,
--   because we use "-DDGHCJS_BROWSER" option for efficiency reasons.
--   This option removes all code related to filesystem interaction, even though wee need it fo TH.
foreign import javascript unsafe "require('fs').writeFileSync($1, $2);" js_writeFile :: JSString -> JSString -> IO ()

-- | Append JSString into file.
--   Use this only in TH environment!
--
--   We have to use this function instead of normal haskell writeFile,
--   because we use "-DDGHCJS_BROWSER" option for efficiency reasons.
--   This option removes all code related to filesystem interaction, even though wee need it fo TH.
foreign import javascript unsafe "require('fs').appendFileSync($1, $2);" js_appendFile :: JSString -> JSString -> IO ()


-- | cssGenPath is an absolute path to build CssGen folder, where TH generates a list of css files
--   to be aggregated in qua-view.css file that is required by the generated JS code.
--   Normally, cabal setup pre-build hook defines it via CPP definition '-DCSS_GEN_PATH=".."'.
cssGenPath :: FilePath
#ifdef CSS_GEN_PATH
cssGenPath = CSS_GEN_PATH
#else
cssGenPath = "CssGen"
{-# WARNING cssGenPath "CPP definition CSS_GEN_PATH is not found! This means I don't know where to write generated css code." #-}
#endif

----------------------------------------------------------------------------------------------------
-- * Generating unique names
----------------------------------------------------------------------------------------------------

getModuleName :: Q String
getModuleName = (>>= \c -> if c == '.' then "zi" else [c]) . loc_module <$> location

getModuleUnique :: String -> Q String
getModuleUnique mName =
    runIO $ encodeIntU <$> js_module_unique (JSString.pack modulesUniqPath) (JSString.pack mName)

newLocalUnique :: String -> Q String
newLocalUnique moduleName = runIO $ atomicModifyIORef' mUniqSource f
    where
      f m = let m' = Map.alter sureAdd moduleName m in (m', encodeIntU $ m' Map.! moduleName)
      sureAdd Nothing = Just 1
      sureAdd (Just i) = Just (i+1)

newGlobalUnique :: Q String
newGlobalUnique = do
    mName <- getModuleName
    (\m l -> m ++ 'z':l) <$> getModuleUnique mName <*> newLocalUnique mName


encodeIntU :: Int -> String
encodeIntU = map (toEnum . (49+) . fromEnum) . show

-- | Map {module name} -> {unique local id}
--   I am using it only in TH splices to generate unique names within a single module.
mUniqSource :: IORef (Map String Int)
mUniqSource = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE mUniqSource #-}


-- | This function gets a unique order number for haskell module.
--   It does so by keeping module names in a file separated by a special character.
foreign import javascript unsafe
  "var fs = require('fs'), l = fs.readFileSync($1, {encoding: 'utf-8'}).split('|'), i = l.indexOf($2); if(i >= 0){$r = i;} else {fs.appendFileSync($1,$2.concat('|'));$r = l.length - 1;}"
  js_module_unique :: JSString -> JSString -> IO Int


-- | A path to modules.unique file in the build folder.
--   This file contains a list of modules that use TH unique identifiers.
--   That is, it keeps short unique ids for each module that needs them.
--   Normally, cabal setup pre-build hook defines it via CPP definition '-DMODULES_UNIQUE_PATH=".."'.
modulesUniqPath :: FilePath
#ifdef MODULES_UNIQUE_PATH
modulesUniqPath = MODULES_UNIQUE_PATH
#else
modulesUniqPath = "modules.unique"
{-# WARNING cssGenPath "CPP definition MODULES_UNIQUE_PATH is not found! This means I don't know where to look for unique modules identifiers." #-}
#endif


----------------------------------------------------------------------------------------------------
-- * Getting document elements
----------------------------------------------------------------------------------------------------

-- | Get existing element by its ids.
--   Use this function with care: a good use case is accessing an element that is hardcoded into html page.
getElementById :: Reflex t
               => ElementConfig EventResult t GhcjsDomSpace
               -> JSString -> Widget x (Maybe (Element EventResult GhcjsDomSpace t))
getElementById cfg elId = do
    me <- liftIO $ nullableToMaybe <$> js_getElementById elId
    case me of
      Just e -> Just <$> wrapRawElement e (extractRawElementConfig cfg)
      Nothing -> return Nothing

-- | Create an element directly from html code.
--   The html code must have exactly one root element;
--   otherwise the function fails at runtime.
makeElementFromHtml :: Reflex t
                    => ElementConfig EventResult t GhcjsDomSpace
                    -> JSString -> Widget x (Element EventResult GhcjsDomSpace t)
makeElementFromHtml cfg content = do
    me <- liftIO $ nullableToMaybe <$> js_createElement content
    case me of
      Just e -> placeRawElement e >> wrapRawElement e (extractRawElementConfig cfg)
      Nothing -> error "hamlet splice must have exactly one root html element."


foreign import javascript unsafe "$r = document.getElementById($1);"
    js_getElementById :: JSString -> IO (Nullable Element.Element)
foreign import javascript unsafe "var d = document.createElement('div');d.innerHTML = $1;$r = d.firstChild;"
    js_createElement :: JSString -> IO (Nullable Element.Element)



