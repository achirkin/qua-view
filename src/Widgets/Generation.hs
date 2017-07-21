{-# LANGUAGE TemplateHaskell #-}
module Widgets.Generation
    ( setInnerHTML, qhtml, qcss
    ) where

import qualified GHCJS.DOM.Element as Element (js_setInnerHTML)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddTopDecls)

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Internal.Css (Css, renderCss)
import Data.JSString (JSString)
import Data.Text.Lazy (unpack)

import Control.Monad.IO.Class
import Reflex.Dom (Element (..), GhcjsDomSpace)

import Data.FileEmbed (makeRelativeToProject)
import System.IO (FilePath)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))


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
qhtml :: Html -> Q Exp
qhtml h = do
    fldrname <- folderCssGen
    runIO $ do
      putStrLn "Hello world!"
      putStrLn $ "folder: " ++ fldrname
    fName <- newName "js_innerHTML"
    qAddTopDecls [ForeignD (ImportF JavaScript Unsafe funCode fName (ConT ''JSString) )]
    return $ VarE fName
  where
    funCode = "$r = " ++ show (renderHtml h) ++ ";"


qcss :: Css -> Q Exp
qcss = litE . StringL . unpack . renderCss



-- | Folder output folder for html, js, and css
folderWeb :: Q FilePath
folderWeb = do
    webf <- makeRelativeToProject "web"
    runIO $ createDirectoryIfMissing True webf
    return webf

-- | Folder where all generated css stays before it is aggregated in the postBuild hook
folderCssGen :: Q FilePath
folderCssGen = do
    f <- makeRelativeToProject ("web" </> "CssGen")
    runIO $ createDirectoryIfMissing True f
    return f



