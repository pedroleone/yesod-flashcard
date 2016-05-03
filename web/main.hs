{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina
-- 15, 16, 17, 21 e 22
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Produto json
   nome Text
   valor Double
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/cadastro UserR GET POST
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

getUserR :: Handler Html
getUserR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  [whamlet| 
    <div class="container">
        <h1>Cadastro de Produto
        <form>
            <div class="form-group">
                <p>Nome: <input type="text" id="usuario">
                <p>Valor: <input type="text" id="valor">
                <p><button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{UserR}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#usuario").val(), "valor":$("#valor").val().parseDouble()}),
                 success: function(data) {
                     alert(data.resp);
                     $("#usuario").val("");
                     $("#valor").val("");
                 }
            })
         });
     }
  |]

postUserR :: Handler ()
postUserR = do
    produto <- requireJsonBody :: Handler Produto
    runDB $ insert produto
    sendResponse (object [pack "resp" .= pack "CREATED"])
    -- Linha 60: Le o json {nome:"Teste"} e converte para
    -- Clientes "Teste". 
    -- O comando runDB $ insert (Clientes "Teste")
    -- Insere o registro "Teste" no banco
    -- {resp:"CREATED"}
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Ola Mundo
|] 

connStr = "dbname=d21dkdr13uffb2 host=ec2-54-204-41-74.compute-1.amazonaws.com user=dledruvwcfkhki password=juUgSHrxGrDOKeQBH4gcc9ZVwI port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


