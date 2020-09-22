module Queries.Photo where


import Database.Beam
import Database.Beam.Postgres

import BeamSchema


deleteOrphanedPhotos :: Pg ()
deleteOrphanedPhotos =
  let usedPhotoIds =
        (fmap draftMainPhotoId (all_ (dbDraft newsDb)))
        `union_`
        (fmap postMainPhotoId (all_ (dbPost newsDb)))
        `union_`
        (fmap draftAdditionalPhotoPhotoId (all_ (dbDraftAdditionalPhoto newsDb)))
        `union_`
        (fmap postAdditionalPhotoPhotoId (all_ (dbPostAdditionalPhoto newsDb)))
  in runDelete $ delete (dbPhoto newsDb)
       (\p -> not_ (photoId p `in_` [subquery_ usedPhotoIds]))
