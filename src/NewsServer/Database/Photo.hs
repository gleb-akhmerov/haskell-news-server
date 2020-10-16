module NewsServer.Database.Photo where


import Data.ByteString (ByteString)
import Data.Int (Int32)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres

import NewsServer.Database.BeamSchema


createPhoto :: ByteString -> Pg Int32
createPhoto photoBytes = do
  [photo] <- runInsertReturningList $ insert (dbPhoto newsDb) $
    insertExpressions
      [ Photo { photoId      = default_
              , photoContent = val_ photoBytes
              }
      ]
  pure (photoId photo)


getPhoto :: Int32 -> Pg (Maybe ByteString)
getPhoto gPhotoId = do
  mPhoto <- runSelectReturningOne $ select $
              filter_ (\p -> photoId p ==. val_ gPhotoId)
                      (all_ (dbPhoto newsDb))
  pure (fmap photoContent mPhoto)
