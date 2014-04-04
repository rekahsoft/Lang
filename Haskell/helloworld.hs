-- File: helloworld.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: May 31, 2012
-- Description: Example code from O'Reilly Yesod book

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|
<p>this is a test paragraph. And here is some variable interpolation:<br/>
#{testvar}
|]
  where testvar = "here is a var!" :: String

main :: IO ()
main = warpDebug 3000 HelloWorld
