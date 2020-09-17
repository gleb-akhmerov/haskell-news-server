{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}

module BeamSchema where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Types (PGArray)

data UsrT f = Usr
  { _usrId        :: C f Int32
  , _usrFirstName :: C f Text
  , _usrLastName  :: C f Text
  , _usrAvatar    :: C f ByteString
  , _usrCreatedAt :: C f UTCTime
  , _usrIsAdmin   :: C f Bool
  }
  deriving (Generic, Beamable)

data AuthorT f = Author
  { _authorId               :: PrimaryKey UsrT f
  , _authorShortDescription :: C f Text
  }
  deriving (Generic, Beamable)

data CategoryT f = Category
  { _categoryId       :: C f Int32
  , _categoryParentId :: PrimaryKey CategoryT (Nullable f)
  , _categoryName     :: C f Text
  }
  deriving (Generic, Beamable)

data PhotoT f = Photo
  { _photoId      :: C f Int32
  , _photoContent :: C f ByteString
  }
  deriving (Generic, Beamable)

data DraftT f = Draft
  { _draftId          :: C f Int32
  , _draftShortName   :: C f Text
  , _draftCreatedAt   :: C f UTCTime
  , _draftAuthorId    :: PrimaryKey AuthorT f
  , _draftCategoryId  :: PrimaryKey CategoryT f
  , _draftTextContent :: C f Text
  , _draftMainPhotoId :: PrimaryKey PhotoT f
  }
  deriving (Generic, Beamable)

data PostT f = Post
  { _postId          :: PrimaryKey DraftT f
  , _postShortName   :: C f Text
  , _postPublishedAt :: C f UTCTime
  , _postAuthorId    :: PrimaryKey AuthorT f
  , _postCategoryId  :: PrimaryKey CategoryT f
  , _postTextContent :: C f Text
  , _postMainPhotoId :: PrimaryKey PhotoT f
  }
  deriving (Generic, Beamable)

data TagT f = Tag
  { _tagId :: C f Int32
  , _tagName :: C f Text
  }
  deriving (Generic, Beamable)

data PostTagT f = PostTag
  { _postTagTagId  :: PrimaryKey TagT f
  , _postTagPostId :: PrimaryKey PostT f
  }
  deriving (Generic, Beamable)

data DraftTagT f = DraftTag
  { _draftTagTagId   :: PrimaryKey TagT f
  , _draftTagDraftId :: PrimaryKey DraftT f
  }
  deriving (Generic, Beamable)

data PostAdditionalPhotoT f = PostAdditionalPhoto
  { _postAdditionalPhotoPhotoId :: PrimaryKey PhotoT f
  , _postAdditionalPhotoPostId  :: PrimaryKey PostT f
  }
  deriving (Generic, Beamable)

data DraftAdditionalPhotoT f = DraftAdditionalPhoto
  { _draftAdditionalPhotoPhotoId :: PrimaryKey PhotoT f
  , _draftAdditionalPhotoDraftId :: PrimaryKey DraftT f
  }
  deriving (Generic, Beamable)

data CommentaryT f = Commentary
  { _commentaryId      :: C f Int32
  , _commentaryUserId  :: PrimaryKey UsrT f
  , _commentaryPostId  :: PrimaryKey PostT f
  , _commentaryContent :: C f Text
  }
  deriving (Generic, Beamable)

instance Table UsrT where
  data PrimaryKey UsrT f = UsrId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = UsrId . _usrId

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (PrimaryKey UsrT f)
    deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId { unCategoryId :: (C f Int32) }
    deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryId

instance Table PhotoT where
  data PrimaryKey PhotoT f = PhotoId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PhotoId . _photoId

instance Table DraftT where
  data PrimaryKey DraftT f = DraftId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftId . _draftId

instance Table PostT where
  data PrimaryKey PostT f = PostId (PrimaryKey DraftT f)
    deriving (Generic, Beamable)
  primaryKey = PostId . _postId

instance Table TagT where
  data PrimaryKey TagT f = TagId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TagId . _tagId

instance Table PostTagT where
  data PrimaryKey PostTagT f = PostTagId (PrimaryKey TagT f) (PrimaryKey PostT f)
    deriving (Generic, Beamable)
  primaryKey = PostTagId <$> _postTagTagId <*> _postTagPostId

instance Table DraftTagT where
  data PrimaryKey DraftTagT f = DraftTagId (PrimaryKey TagT f) (PrimaryKey DraftT f)
    deriving (Generic, Beamable)
  primaryKey = DraftTagId <$> _draftTagTagId <*> _draftTagDraftId

instance Table PostAdditionalPhotoT where
  data PrimaryKey PostAdditionalPhotoT f = PostAdditionalPhotoId (PrimaryKey PhotoT f) (PrimaryKey PostT f)
    deriving (Generic, Beamable)
  primaryKey = PostAdditionalPhotoId <$> _postAdditionalPhotoPhotoId <*> _postAdditionalPhotoPostId

instance Table DraftAdditionalPhotoT where
  data PrimaryKey DraftAdditionalPhotoT f = DraftAdditionalPhotoId (PrimaryKey PhotoT f) (PrimaryKey DraftT f)
    deriving (Generic, Beamable)
  primaryKey = DraftAdditionalPhotoId <$> _draftAdditionalPhotoPhotoId <*> _draftAdditionalPhotoDraftId

instance Table CommentaryT where
  data PrimaryKey CommentaryT f = CommentaryId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = CommentaryId . _commentaryId

--type Usr = UsrT Identity
--type Author = AuthorT Identity
type Category = CategoryT Identity
--type Photo = PhotoT Identity
--type Draft = DraftT Identity
type Post = PostT Identity

--deriving instance Show Usr
--deriving instance Show Author
deriving instance Show Category
--deriving instance Show Photo
--deriving instance Show Draft
deriving instance Show Post

deriving instance Show (PrimaryKey UsrT Identity)
deriving instance Show (PrimaryKey AuthorT Identity)
deriving instance Show (PrimaryKey PhotoT Identity)
deriving instance Show (PrimaryKey DraftT Identity)
deriving instance Show (PrimaryKey CategoryT Identity)
deriving instance Show (PrimaryKey CategoryT (Nullable Identity))

data NewsDb f = NewsDb
  { _dbUsr                  :: f (TableEntity UsrT)
  , _dbAuthor               :: f (TableEntity AuthorT)
  , _dbCategory             :: f (TableEntity CategoryT)
  , _dbPhoto                :: f (TableEntity PhotoT)
  , _dbDraft                :: f (TableEntity DraftT)
  , _dbPost                 :: f (TableEntity PostT)
  , _dbTag                  :: f (TableEntity TagT)
  , _dbPostTag              :: f (TableEntity PostTagT)
  , _dbDraftTag             :: f (TableEntity DraftTagT)
  , _dbPostAdditionalPhoto  :: f (TableEntity PostAdditionalPhotoT)
  , _dbDraftAdditionalPhoto :: f (TableEntity DraftAdditionalPhotoT)
  , _dbCommentary           :: f (TableEntity CommentaryT)
  }
  deriving (Generic, Database be)

newsDb :: DatabaseSettings be NewsDb
newsDb = defaultDbSettings `withDbModification`
  dbModification
    { _dbAuthor = modifyTableFields tableModification
        { _authorId = UsrId "id" }
    , _dbCategory = modifyTableFields tableModification
        { _categoryParentId = CategoryId "parent_id" }
    , _dbDraft = modifyTableFields tableModification
        { _draftAuthorId = AuthorId (UsrId "author_id")
        , _draftCategoryId = CategoryId "category_id"
        , _draftMainPhotoId = PhotoId "main_photo_id" }
    , _dbPost = modifyTableFields tableModification
        { _postId = DraftId "id"
        , _postAuthorId = AuthorId (UsrId "author_id")
        , _postCategoryId = CategoryId "category_id"
        , _postMainPhotoId = PhotoId "main_photo_id" }
    , _dbPostTag = modifyTableFields tableModification
        { _postTagTagId = TagId "tag_id"
        , _postTagPostId = PostId (DraftId "post_id") }
    , _dbDraftTag = modifyTableFields tableModification
        { _draftTagTagId = TagId "tag_id"
        , _draftTagDraftId = DraftId "draft_id" }
    , _dbPostAdditionalPhoto = modifyTableFields tableModification
        { _postAdditionalPhotoPhotoId = PhotoId "photo_id"
        , _postAdditionalPhotoPostId = PostId (DraftId "post_id") }
    , _dbDraftAdditionalPhoto = modifyTableFields tableModification
        { _draftAdditionalPhotoPhotoId = PhotoId "photo_id"
        , _draftAdditionalPhotoDraftId = DraftId "post_id" }
    , _dbCommentary = modifyTableFields tableModification
        { _commentaryUserId = UsrId "user_id"
        , _commentaryPostId = PostId (DraftId "post_id") }
    }

allUsers :: Q Postgres NewsDb s (UsrT (QExpr Postgres s))
allUsers = all_ (_dbUsr newsDb)

allUsersFiltered :: Q Postgres NewsDb s (UsrT (QExpr Postgres s))
allUsersFiltered = do
  user <- allUsers
  guard_ (_usrId user ==. val_ 1)
  pure user

categoryWithParentsImpl :: (Monoid a, IsString a) => a -> a
categoryWithParentsImpl categoryId =
  "(with recursive category_tree as (\
  \  select c1.name, c1.parent_id from category c1\
  \  where id = " <> categoryId <>
  "    union all\
  \  select c2.name, c2.parent_id from category c2\
  \  join category_tree on category_tree.parent_id = c2.id\
  \)\
  \select array_agg(name) from category_tree)"

categoryWithParents :: C (QExpr Postgres s) Int32 -> Q Postgres NewsDb s (QGenExpr e Postgres s (PGArray Text))
categoryWithParents =
  let expr = customExpr_ categoryWithParentsImpl :: C (QExpr Postgres s) Int32 -> QGenExpr e Postgres s (PGArray Text)
  in pure . expr

postsWithCategories :: Q Postgres NewsDb s (PostT (QExpr Postgres s), QGenExpr e Postgres s (PGArray Text))
postsWithCategories = do
  post <- all_ (_dbPost newsDb)
  cats <- categoryWithParents (unCategoryId (_postCategoryId post))
  pure (post, cats)
