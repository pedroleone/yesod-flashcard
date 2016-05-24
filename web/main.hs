{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Text.Julius
import Control.Monad.Logger (runStdoutLoggingT)
import Text.Blaze.Html.Renderer.String (renderHtml)

data Pagina = Pagina{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    nome Text
    login Text
    senha Text
    deriving Show
   
FlashCard json
    nome Text
    descricao Text
    deriving Show
    
FlashCardDetail json
    cardid FlashCardId
    frente Text
    verso Text
    comentario Text
    deriving Show

UserFlashCard json
    userid UserId
    cardid FlashCardId
    UniqueUserFlashCard userid cardid
    deriving Show

|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/login LoginR GET POST
/erro ErroR GET
/usuario UsuarioR GET POST
/perfil/#UserId PerfilR GET
/admin AdminR GET
/logout LogoutR GET
/cadastro CadastraUsuarioR GET POST
/flashcard MeusFlashCardsR GET
/lista ListaFlashcardsR GET
|]

instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized CadastraUsuarioR _ = return Authorized
    isAuthorized ListaFlashcardsR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formUser :: Form User
formUser = renderDivs $ User <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UserId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{userNome user}
          <p><b> Login: #{userLogin user}
      |]
          


postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

--------------------
widgetLoginLogout = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing ->  [hamlet|<a href="@{LoginR}"><i class="fa fa-sign-in fa-fw" aria-hidden="true"></i> Login|]
        Just _ ->  [hamlet|<a href="@{LogoutR}"><i class="fa fa-sign-out fa-fw" aria-hidden="true"></i> Logout|]    

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
                wd <- widgetLoginLogout
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/index.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]                



getLoginR :: Handler Html
getLoginR =  do 
             (widget, enctype) <- generateFormPost formLogin
             defaultLayout $ do
                wd <- widgetLoginLogout
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/login.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]


getCadastraUsuarioR :: Handler Html
getCadastraUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout $ do
                wd <- widgetLoginLogout
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/cadastro.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]
                    

postCadastraUsuarioR :: Handler Html
postCadastraUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR



getMeusFlashCardsR :: Handler Html
getMeusFlashCardsR = do
      -- logica: buscar o ID do usuario que esta logado (rota deve ser AuthenticationRequired)
      -- depois de obter o ID, fazer uma query com join na UserFlashCard + 
      
      -- mu <- lookupSession "_ID"
      -- fcs <- runDB $ selectList [UserFlashCardUserid .= show $ mu] [] 
      defaultLayout $ do
        wd <- widgetLoginLogout
        toWidget $ $(luciusFile "templates/style.lucius")
        $(whamletFile "templates/meusfc.hamlet")
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
        toWidgetHead
            [hamlet|
                <meta charset="UTF-8">  
            |]
        toWidget[julius|
            $(document).ready(function() {
                $('#flashcard').DataTable({
                "language": { "url": "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json" }
                });
            } );            
        |]


getListaFlashcardsR = do
      defaultLayout $ do
        wd <- widgetLoginLogout
        toWidget $ $(luciusFile "templates/style.lucius")
        $(whamletFile "templates/listafc.hamlet")
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
        toWidgetHead
            [hamlet|
                <meta charset="UTF-8">  
            |]
        toWidget[julius|
            $(document).ready(function() {
                $('#flashcard').DataTable({
                "language": { "url": "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json" }
                });
            } );            
        |]            

--------------
                
getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]


postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect HomeR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UserLogin ==. login, UserSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (HomeR) --(PerfilR uid)

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     redirect (HomeR)

---------------------------

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
