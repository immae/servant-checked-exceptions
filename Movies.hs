{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Movies where

import GHC.Generics               ( Generic )
import Data.Aeson                 ( FromJSON (..), ToJSON (..), Value )
import Data.Proxy                 ( Proxy(..) )
import Network.Wai.Handler.Warp   ( run )

import Servant                    ( NamedRoutes
                                  , Handler, serve, HasServer (..) )
import Servant.API                (Capture, Delete, Get, Put, QueryParam, ReqBody
                                  , JSON, NoContent (..)
                                  , FromHttpApiData (..),ToHttpApiData(..)
                                  , (:>) )
import Servant.API.Generic        ( (:-), ToServantApi, GenericMode )

import Servant.Client.Generic     ()

import Servant.API.Alternative    ((:<|>))
import Servant.Server             ( Application)
import Servant.Server.Generic     ( AsServer )
import Data.Aeson.Types (Parser)
import "servant-checked-exceptions" Servant.Checked.Exceptions (ErrStatus (..), Status, Throws, Envelope, pureSuccEnvelope, pureErrEnvelope, Throwing)
import Data.Aeson (withText)
import Text.Read (readMaybe)
import Data.Text (unpack)
import Network.HTTP.Types (status404)
import Data.Kind (Type)

data AsEnveloppedServerT (es :: [Type]) (m :: Type -> Type)
instance GenericMode (AsEnveloppedServerT es m) where
  type AsEnveloppedServerT es m :- api = ServerT (Throwing es :> api) m
type AsEnveloppedServer es = AsEnveloppedServerT es Handler

instance HasServer (api (AsEnveloppedServerT es m)) context => HasServer (Throwing es :> NamedRoutes api) context where
  type ServerT (Throwing es :> NamedRoutes api) m = api (AsEnveloppedServerT es m)
  route _ ctx del = route (Proxy :: Proxy (api (AsEnveloppedServerT es m))) ctx (_ del)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy (Throwing es :> NamedRoutes api))

data Movie = Movie
    { movieId :: MovieId
    , title :: Title
    , year :: Year
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

type MovieId = String
type Title = String
type Year = Int

data API mode = API
    { version :: mode :- Throws ApiError :> "version" :> Get '[JSON] Version
    , movies :: mode :- "movies" :> NamedRoutes MoviesAPI
    } deriving stock Generic

type Version = String

data MoviesAPI mode = MoviesAPI
    { list :: mode :- "list" :> QueryParam "SortBy" SortBy :> Get '[JSON] [Movie]
    , movie :: mode :- Throws ApiError :> Capture "movieId" MovieId :> NamedRoutes MovieAPI
    } deriving stock Generic

data SortBy = Year | Title

instance ToHttpApiData SortBy where
  toQueryParam Year = "year"
  toQueryParam Title = "title"

instance FromHttpApiData SortBy where
  parseQueryParam "year" = Right Year
  parseQueryParam "title" = Right Title
  parseQueryParam param = Left $ param <> " is not a valid value"

data MovieAPI mode = MovieAPI
  { get :: mode :- Get '[JSON] (Maybe Movie)
  , update :: mode :- ReqBody '[JSON] Movie :> Put '[JSON] NoContent
  , delete :: mode :- Delete '[JSON] NoContent
  } deriving stock Generic

type MovieCatalogAPI = NamedRoutes API

data ApiError = ApiError deriving (Eq, Read, Show)

instance ToJSON ApiError where
  toJSON :: ApiError -> Value
  toJSON = toJSON . show

instance FromJSON ApiError where
  parseJSON :: Value -> Parser ApiError
  parseJSON = withText "ApiError" $
    maybe (fail "could not parse as ApiError") pure . readMaybe . unpack

instance ErrStatus ApiError where
  toErrStatus :: ApiError -> Status
  toErrStatus _ = status404

versionHandler :: Handler (Envelope '[ApiError] Version)
--versionHandler = pureSuccEnvelope "0.0.1"
versionHandler = pureErrEnvelope ApiError

movieListHandler :: Maybe SortBy -> Handler [Movie]
movieListHandler _ = pure moviesDB

moviesDB :: [Movie]
moviesDB =
  [ Movie "1" "Se7en" 1995
  , Movie "2" "Minority Report" 2002
  , Movie "3" "The Godfather" 1972
  ]

getMovieHandler :: MovieId -> Handler (Envelope '[ApiError] (Maybe Movie))
getMovieHandler requestMovieId = go moviesDB
  where
    go [] = pureSuccEnvelope Nothing
    go (movie:_) | movieId movie == requestMovieId = pureSuccEnvelope $ Just movie
    go (_:ms) = go ms

updateMovieHandler :: MovieId -> Movie -> Handler (Envelope '[ApiError] NoContent)
updateMovieHandler _ _ =
   -- update the movie list in the database...
   pureSuccEnvelope NoContent

deleteMovieHandler :: MovieId -> Handler (Envelope '[ApiError] NoContent)
deleteMovieHandler _ =
   -- delete the movie from the database...
   pureSuccEnvelope NoContent

server ::  API AsServer
server =
  API
    { version = versionHandler
    , movies = moviesHandler
    }

moviesHandler :: MoviesAPI AsServer
moviesHandler =
  MoviesAPI
    { list = movieListHandler
    , movie = movieHandler
    }

movieHandler :: MovieId -> MovieAPI (AsEnveloppedServer '[ApiError])
movieHandler movieId = MovieAPI
    { get = getMovieHandler movieId
    , update = updateMovieHandler movieId
    , delete = deleteMovieHandler movieId
    }

api :: Proxy MovieCatalogAPI
api = Proxy

main :: IO ()
main = run 8081 app

app :: Application
app = serve api server
