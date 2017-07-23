{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Widgets.Generation
    ( setInnerHTML, qhtml, qcss
    , newVar, returnVars
    , hamlet, cassius
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import qualified GHCJS.DOM.Element as Element (js_setInnerHTML)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddTopDecls)

import Text.Hamlet (HtmlUrl,hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Cassius (cassius)
import Text.Internal.Css (CssUrl, renderCss)
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText

import Control.Monad.IO.Class
import Reflex.Dom (Element (..), GhcjsDomSpace)

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
--   TODO: there is a problem with this approach; during development of a single module,
--         number of unique ids varies, so generated css files may duplicate.
--         As a workaround, we can `stack clean` the project from time to time.
qcss :: CssUrl url -> Q ()
qcss css = do
    curLoc <- location
    let fNameBase = (>>= \c -> if c == '.' then "zi" else [c]) $ loc_module curLoc
    fUniqPart <- newLocalUnique
    let fNameFull = JSString.pack $ cssGenPath </> (fNameBase ++ 'z':fUniqPart) <.> "css"
        content = JSString.pack . LText.unpack $ renderCss (css undefined)
    runIO $ js_writeFile fNameFull content


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

-- | Write JSString into file.
--   Use this only in TH environment!
--
--   We have to use this function instead of normal haskell writeFile,
--   because we use "-DDGHCJS_BROWSER" option for efficiency reasons.
--   This option removes all code related to filesystem interaction, even though wee need it fo TH.
foreign import javascript unsafe "require('fs').writeFileSync($1, $2);" js_writeFile :: JSString -> JSString -> IO ()


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

getModuleUnique :: Q String
getModuleUnique = do
    mName <- JSString.pack . (>>= \c -> if c == '.' then "zi" else [c]) . loc_module <$> location
    runIO $ encodeIntU <$> js_module_unique (JSString.pack modulesUniqPath) mName

newLocalUnique :: Q String
newLocalUnique = runIO $ atomicModifyIORef' mUniqSource (\(ModuleUnique i) -> (ModuleUnique $! i + 1, encodeIntU i) )

newGlobalUnique :: Q String
newGlobalUnique = (\m l -> m ++ 'z':l) <$> getModuleUnique <*> newLocalUnique


encodeIntU :: Int -> String
encodeIntU = map (toEnum . (49+) . fromEnum) . show

-- | An abstract unique object.
--   I am using it only in TH splices to generate unique names within a single module.
newtype ModuleUnique = ModuleUnique Int
mUniqSource :: IORef ModuleUnique
mUniqSource = unsafePerformIO (newIORef (ModuleUnique 0))
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

