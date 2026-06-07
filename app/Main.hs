{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

import Web.Scotty
    ( formParam,
      get,
      headers,
      html,
      pathParam,
      post,
      redirect,
      request,
      scotty,
      status,
      text,
      liftIO,
      ActionM )
import Network.Wai.Middleware.HttpAuth
import Data.ByteString (ByteString)
import Data.SecureMem -- for constant-time comparison
import Network.HTTP.Types (status401, status404)
import GHC.Generics
import Data.Aeson (FromJSON, object, KeyValue ((.=)))
import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as TL
import qualified GHC.List as T
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Hashable (hash)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (remoteHost)
import Network.Socket (SockAddr)
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.IORef
import System.Posix.Internals (c_close)
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

    entries <- liftIO loadDB
    let initialMap = generateHashMap entries
    mapBox <- newIORef initialMap


    scotty 3000 $ do

        get "/go/:path" $ do
            target <- pathParam "path"
            allHeaders <- headers

            liftIO $ print allHeaders
            currentMap <- liftIO $ readIORef mapBox
            let result = Map.lookup target currentMap 

            req <- request
            let clientAddr = remoteHost req :: SockAddr
            liftIO $ print clientAddr
                
            case result of
                Just foundTarget -> redirect $ pack foundTarget
                Nothing          -> notFoundError


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
        get "/" $ do 
            redirect "https://liamwittig.de"

        get "/:path" $ do 
            path    <- pathParam "path"
            redirect $ pack ("https://liamwittig.de/" ++ path)


notFoundError :: ActionM ()
notFoundError  = do 
    status status404
    text $ "404 not found"


loadDB ::  Ord String => IO [(String, String)]
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

isUnique :: Map String String -> String -> Bool
isUnique mapToProve key = Map.notMember key mapToProve

-- 2. Fixed the type signature (removed 'List' and added 'Ord String')
generateHashMap :: Ord String => [(String, String)] -> Map String String
generateHashMap list = Map.fromList list


mapToContext :: Map String String -> Aeson.Value
mapToContext m =
    let entries = map (\(k, v) -> Aeson.object ["key" .= k, "target" .= v])
                      (Map.toList m)
    in Aeson.object ["entries" .= entries]

renderDashboard :: Map String String -> IO TL.Text
renderDashboard m = do
    compiled <- automaticCompile ["templates"] "dashboard.html"
    case compiled of
        Left err       -> return $ TL.pack $ "Template error: " ++ show err
        Right template -> return $ TL.fromStrict $ substitute template (mapToContext m)