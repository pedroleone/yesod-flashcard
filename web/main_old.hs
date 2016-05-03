{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET POST
/link LinkR GET
|]

instance Yesod HelloWorld
-- defaultLayout: transforma widget em Handler
-- whamlet: Widget Hamlet

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet|
      <h1> Hello World!
      <button onclick="ola()">Teste
|] >> toWidget [lucius|
    h1 {
        color: red; 
    }
|] >> toWidgetHead [julius|
    function ola() {
        alert("Ola Javascript do haskell");
    }
|]

postHomeR :: Handler Html
postHomeR = defaultLayout $ [whamlet|
       <p> Ola do POST
|] >> toWidget [cassius|
    p 
        color: red;
|] 

getLinkR :: Handler Html
getLinkR = defaultLayout $ [whamlet|
      <a href=@{HomeR}> Hello World!
|]



main :: IO ()
main = warp 8080 HelloWorld

-- stack build: compila
-- stack exec -- web : roda
-- curl -v -X POST <url>
-- parar aplicação: control+c
