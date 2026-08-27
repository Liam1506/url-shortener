{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

import Web.Scotty
    ( formParam,
      get,
      html,
      middleware,
      pathParam,
      post,
      redirect,
      scotty,
      setHeader,
      status,
      text,
      liftIO,
      ActionM )
import Network.Wai.Middleware.HttpAuth
import Network.Wai (Request, pathInfo, remoteHost, Application, Middleware)
import Data.ByteString (ByteString)
import Network.HTTP.Types (status404)
import Data.Aeson (KeyValue ((.=)))
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy as TL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Network.Wai (remoteHost)
import Network.Socket (SockAddr)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple (Only(..))
import Data.IORef
import qualified Data.Aeson as Aeson
import Text.Mustache


data TestField = TestField String String deriving (Show)
instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
   
    conn <- open "links.db"

    execute_ conn
        "CREATE TABLE IF NOT EXISTS links (key TEXT PRIMARY KEY, target TEXT NOT NULL)"

    close conn

    conn <- open "sessions.db"

    execute_ conn
        "CREATE TABLE IF NOT EXISTS sessions (\
        \    key TEXT PRIMARY KEY DEFAULT (lower(hex(randomblob(16)))),\
        \    user_cookie TEXT NOT NULL,\
        \    created_at TEXT DEFAULT CURRENT_TIMESTAMP\
        \)"
    
    close conn

    entries <- liftIO loadDB
    let initialMap = generateHashMap entries
    mapBox <- newIORef initialMap


    scotty 3001 $ do

        middleware $ basicAuth checkCreds "url-shortener" `guardPath` isProtected

        get "/reload" $ do
            entries <- liftIO loadDB
            let newMap = generateHashMap entries
            liftIO $ atomicWriteIORef mapBox newMap
            text "done"

        get "/dashboard" $ do
            entries <- liftIO loadDB
            let newMap = generateHashMap entries
            liftIO $ atomicWriteIORef mapBox newMap
            page <- liftIO $ renderDashboard newMap
            html page

        post "/add" $ do
            key    <- formParam "key"    :: ActionM String
            target <- formParam "target" :: ActionM String
            currentMap <- liftIO $ readIORef mapBox
            let unique = isUnique currentMap key
            if unique
                then do
                    liftIO $ addEntry key target
                    _ <- liftIO $ atomicModifyIORef' mapBox $ \oldMap ->
                        let newMap = Map.insert key target oldMap
                        in (newMap, newMap)
                    redirect "/dashboard"
                else
                    text "Not unique"

        get "/delete/:key" $ do
            key <- pathParam "key" :: ActionM String
            liftIO $ deleteEntry key
            liftIO $ atomicModifyIORef' mapBox $ \m -> (Map.delete key m, ())
            redirect "/dashboard"

        get "/edit/:key" $ do
            key <- pathParam "key" :: ActionM String
            currentMap <- liftIO $ readIORef mapBox
            case Map.lookup key currentMap of
                Nothing     -> redirect "/dashboard"
                Just target -> html $ TL.pack $
                    "<form action='/edit/" ++ key ++ "' method='POST'>\
                    \<label>Target: <input name='target' value='" ++ target ++ "' required /></label>\
                    \<button type='submit'>Save</button></form>"

        post "/edit/:key" $ do
            key    <- pathParam "key"    :: ActionM String
            target <- formParam "target" :: ActionM String
            liftIO $ updateEntry key target
            liftIO $ atomicModifyIORef' mapBox $ \m -> (Map.insert key target m, ())
            redirect "/dashboard"

        get "/" $ do
            redirect "https://liamwittig.de"

        get "/:path" $ do
            path <- pathParam "path"
            currentMap <- liftIO $ readIORef mapBox
            case Map.lookup path currentMap of
                Just target -> do
                    setHeader "Cache-Control" "public, max-age=3600"
                    redirect $ pack target
                Nothing     -> redirect $ pack ("https://liamwittig.de/" ++ path)


notFoundError :: ActionM ()
notFoundError  = do
    status status404
    text $ "404 not found"

checkCreds :: CheckCreds
checkCreds u p = return $ u == "liam" && p == "1234"

isProtected :: Request -> Bool
isProtected req = case pathInfo req of
    ("dashboard":_) -> True
    ("add":_)       -> True
    ("reload":_)    -> True
    ("delete":_)    -> True
    ("edit":_)      -> True
    _               -> False

guardPath :: Middleware -> (Request -> Bool) -> Middleware
guardPath mw predicate app req respond
    | predicate req = mw app req respond
    | otherwise     = app req respond


loadDB :: IO [(String, String)]
loadDB  = do
    conn <- open "links.db"


    r <- query_ conn "SELECT * from links" :: IO [TestField]
    mapM_ print r
    close conn
    let rows = map (\(TestField key target) -> (key, target)) r
    return rows

addEntry :: String -> String -> IO ()
addEntry key target = do
    conn <- open "links.db"

    execute conn "INSERT INTO links (key, target) VALUES (?, ?)"
        (key :: String, target :: String)

    close conn

deleteEntry :: String -> IO ()
deleteEntry key = do
    conn <- open "links.db"
    execute conn "DELETE FROM links WHERE key = ?" (Only (key :: String))
    close conn

updateEntry :: String -> String -> IO ()
updateEntry key target = do
    conn <- open "links.db"
    execute conn "UPDATE links SET target = ? WHERE key = ?" (target :: String, key :: String)
    close conn

isUnique :: HashMap String String -> String -> Bool
isUnique mapToProve key = not $ Map.member key mapToProve

-- 2. Fixed the type signature (removed 'List' and added 'Ord String')
generateHashMap :: [(String, String)] -> HashMap String String
generateHashMap list = Map.fromList list


mapToContext :: HashMap String String -> Aeson.Value
mapToContext m =
    let entries = map (\(k, v) -> Aeson.object ["key" .= k, "target" .= v])
                      (Map.toList m)
    in Aeson.object ["entries" .= entries]

renderDashboard :: HashMap String String -> IO TL.Text
renderDashboard m = do
    compiled <- automaticCompile ["templates"] "dashboard.html"
    case compiled of
        Left err       -> return $ TL.pack $ "Template error: " ++ show err
        Right template -> return $ TL.fromStrict $ substitute template (mapToContext m)