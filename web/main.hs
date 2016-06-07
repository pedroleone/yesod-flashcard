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
import Yesod.Form.Bootstrap3

data Pagina = Pagina{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    nome Text
    login Text
    senha Text
    deriving Show
   
FlashCard json
    nome Text
    descricao Text
    ownerid UserId
    deriving Show
    
FlashCardDetail json
    cardid FlashCardId
    frente Textarea
    verso Textarea
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
/admin AdminR GET
/logout LogoutR GET
/cadastro CadastraUsuarioR GET POST
/flashcard MeusFlashCardsR GET
/lista ListaFlashcardsR GET
/flashcard/novo CriaFlashCardR GET POST
/flashcard/add/#FlashCardId AdicionarCardR GET POST
/flashcard/estudo/#FlashCardId EstudaR GET 
/flashcard/favoritar/#FlashCardId FavoritaR GET
/faq FaqR GET
/admin/deletar/#FlashCardId ApagaflashR GET 
/admin/deletar/card/#FlashCardDetailId ApagacardR GET

|]



instance Yesod Pagina where
    authRoute _ = Just LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized CadastraUsuarioR _ = return Authorized
    isAuthorized ListaFlashcardsR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized (EstudaR _) _ = return Authorized
    isAuthorized FaqR _ = return Authorized
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
        Just "0" -> Authorized 
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
formUser = renderBootstrap3 BootstrapBasicForm $ User <$>
           areq textField (bfs ("Nome" :: Text)) Nothing <*>
           areq textField (bfs ("Login" :: Text)) Nothing <*>
           areq passwordField (bfs ("Senha" :: Text)) Nothing

formLogin :: Form (Text,Text)
formLogin = renderBootstrap3 BootstrapBasicForm $ (,) <$>
           areq textField (bfs ("Login" :: Text)) Nothing <*>
           areq passwordField (bfs ("Senha" :: Text)) Nothing

formFlashCard :: Form (Text, Text)
formFlashCard = renderBootstrap3 BootstrapBasicForm $ (,) <$>
            areq textField (bfs ("Nome do Conjunto" :: Text)) Nothing <*>
            areq textField (bfs ("Descrição" :: Text)) Nothing

formFlashCardDetail :: Form (Textarea, Textarea)
formFlashCardDetail = renderBootstrap3 BootstrapBasicForm $ (,) <$>
            areq textareaField (bfs ("Frente" :: Text)) Nothing <*>
            areq textareaField (bfs ("Verso" :: Text)) Nothing 


getCriaFlashCardR :: Handler Html
getCriaFlashCardR = do
                (widget, enctype) <- generateFormPost formFlashCard
                defaultLayout $ do
                    menu <- widgetMenu
                    toWidget $ $(luciusFile "templates/style.lucius")
                    $(whamletFile "templates/criafc.hamlet")
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                    toWidgetHead
                        [hamlet|
                            <meta charset="UTF-8">  
                        |]                
                    

postCriaFlashCardR :: Handler Html
postCriaFlashCardR = do
                      mu <- lookupSession "_ID"
                      case mu of 
                        Nothing -> redirect LoginR
                        Just uid -> do 
                            ((result, _), _) <- runFormPost formFlashCard
                            case result of 
                                FormSuccess (nome,descricao) -> do 
                                        runDB $ insert $ FlashCard nome descricao (toSqlKey $ read $ unpack $ uid)
                                        redirect MeusFlashCardsR



--------------------




getHomeR :: Handler Html
getHomeR = defaultLayout $ do
                menu <- widgetMenu
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
                menu <- widgetMenu
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
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/cadastro.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]
                    

postCadastraUsuarioR :: Handler Html
postCadastraUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (MeusFlashCardsR)
               _ -> redirect ErroR



getMeusFlashCardsR :: Handler Html
getMeusFlashCardsR = do
      mu <- lookupSession "_ID"
      case mu of 
        Nothing -> redirect LoginR
        Just uid -> do
                        fcs <- runDB $ selectList [FlashCardOwnerid ==. (toSqlKey $ read $ unpack $ uid)] [] 
                        favfcs <- runDB $ (rawSql (pack $ "SELECT ??, ?? FROM flash_card INNER JOIN user_flash_card ON flash_card.id = user_flash_card.cardid WHERE user_flash_card.userid = " ++ (unpack $ uid)) []) :: Handler [(Entity FlashCard, Entity UserFlashCard)]
                        defaultLayout $ do
                        menu <- widgetMenu
                        toWidget $ $(luciusFile "templates/style.lucius")
                        $(whamletFile "templates/meusfc.hamlet")
                        addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                        addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
                        addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
                        addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
                        toWidgetHead
                            [hamlet|
                                <meta charset="UTF-8">  
                            |]
                        toWidget[julius|
                            $(document).ready(function() {
                                $('table').DataTable({
                                "language": { "url": "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json" }
                            });
                        } );            
                    |]


getListaFlashcardsR = do
      fcs <- runDB $ selectList [] [Asc FlashCardNome]
      defaultLayout $ do
        menu <- widgetMenu
        toWidget $ $(luciusFile "templates/style.lucius")
        $(whamletFile "templates/listafc.hamlet")
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
        addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
        toWidgetHead
            [hamlet|
                <meta charset="UTF-8">  
            |]
        toWidget[julius|
            $(document).ready(function() {
                $('table').DataTable({
                "language": { "url": "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json" }
                });

            } );            
        |]
        toWidgetHead [lucius|
        div.dataTables_wrapper {
                margin-bottom: 3em;
            }        
        |]

getAdicionarCardR :: FlashCardId -> Handler Html
getAdicionarCardR fid = do
            (widget, enctype) <- generateFormPost formFlashCardDetail
            defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/criacards.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]                

postAdicionarCardR :: FlashCardId -> Handler Html
postAdicionarCardR fid = do
                    ((result, _), _) <- runFormPost formFlashCardDetail
                    case result of
                        FormSuccess (frente,verso) -> do
                            runDB $ insert $ FlashCardDetail fid frente verso 
                            redirect MeusFlashCardsR


getEstudaR :: FlashCardId -> Handler Html
getEstudaR fid = do
      cards <- runDB $ selectList [FlashCardDetailCardid ==. fid] []
      fc <- runDB $ get404 fid
      user <- runDB $ get404 (flashCardOwnerid fc)
      defaultLayout $ do
        menu <- widgetMenu
        toWidget $ $(luciusFile "templates/style.lucius")
        $(whamletFile "templates/estudo.hamlet")
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        toWidgetHead
            [hamlet|
                <meta charset="UTF-8">  
            |]


getFaqR :: Handler Html
getFaqR = defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/faq.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]    

getFavoritaR :: FlashCardId -> Handler Html
getFavoritaR fid = do
                mu <- lookupSession "_ID"
                case mu of
                    Nothing -> redirect LoginR
                    Just uid -> do
                        favfc <- runDB $ selectFirst [UserFlashCardUserid ==. (toSqlKey $ read $ unpack $ uid), UserFlashCardCardid ==. fid] []
                        case favfc of
                            Nothing -> do 
                                runDB $ insert $ UserFlashCard (toSqlKey $ read $ unpack $ uid) fid
                                redirect MeusFlashCardsR
                            Just (Entity key fav) -> do 
                                runDB $ delete $ key
                                redirect MeusFlashCardsR

--------------
                
getAdminR :: Handler Html
getAdminR = do       
        fcs <- runDB $ selectList [] [Asc FlashCardNome] 
        cards <- runDB $ selectList [] [Asc FlashCardDetailCardid]
        defaultLayout $ do
            menu <- widgetMenu
            toWidget $ $(luciusFile "templates/style.lucius")
            $(whamletFile "templates/admin.hamlet")
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
            addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
            addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
            addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
            toWidgetHead
                [hamlet|
                    <meta charset="UTF-8">  
                |]
            toWidget[julius|
                $(document).ready(function() {
                    $('table').DataTable({
                    "language": { "url": "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json" }
                    });
                } );            
            |]


getApagaflashR :: FlashCardId -> Handler Html
getApagaflashR fid = do
                        runDB $ deleteCascade fid
                        mu <- lookupSession "_ID"
                        if mu == (Just "0")
                            then redirect AdminR
                            else redirect MeusFlashCardsR

getApagacardR :: FlashCardDetailId -> Handler Html
getApagacardR cid = do
                        runDB $ delete cid
                        redirect AdminR


postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "0" >> redirect HomeR
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

widgetMenu = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing ->  [hamlet|
            <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{MeusFlashCardsR}"><i class="fa fa-file-o fa-fw" aria-hidden="true"></i> Meus Flashcards</a>
                    <li><a href="@{ListaFlashcardsR}"> <i class="fa fa-search fa-fw" aria-hidden="true"></i> Procurar Flashcards</a>
                    <li><a href="@{FaqR}"> <i class="fa fa-question-circle fa-fw" aria-hidden="true"></i> FAQ</a>
                    <li><a href="@{LoginR}"><i class="fa fa-sign-in fa-fw" aria-hidden="true"></i> Login</a>
                    |]
        Just "0" -> [hamlet|
            <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{ListaFlashcardsR}"> <i class="fa fa-search fa-fw" aria-hidden="true"></i> Procurar Flashcards</a>
                    <li><a href="@{FaqR}"> <i class="fa fa-question-circle fa-fw" aria-hidden="true"></i> FAQ</a>
                    <li><a href="@{LogoutR}"><i class="fa fa-sign-out fa-fw" aria-hidden="true"></i> Logout</a>
                    <li><a href="@{AdminR}"><i class="fa fa-lock fa-fw" aria-hidden="true"></i> Administração</a>
                    |]
        Just _ -> [hamlet|
            <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{MeusFlashCardsR}"><i class="fa fa-file-o fa-fw" aria-hidden="true"></i> Meus Flashcards</a>
                    <li><a href="@{ListaFlashcardsR}"> <i class="fa fa-search fa-fw" aria-hidden="true"></i> Procurar Flashcards</a>
                    <li><a href="@{FaqR}"> <i class="fa fa-question-circle fa-fw" aria-hidden="true"></i> FAQ</a>
                    <li><a href="@{LogoutR}"><i class="fa fa-sign-out fa-fw" aria-hidden="true"></i> Logout</a>
                    |]


---------------------------

connStr = "dbname=dcq1g11l0nlarg host=ec2-54-243-202-110.compute-1.amazonaws.com user=ghagzcmagsinsp password=dhCA-0uFwZx4qXgqiwSOnKiZNB port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigrationUnsafe migrateAll) pool
       warp 8080 (Pagina pool)

