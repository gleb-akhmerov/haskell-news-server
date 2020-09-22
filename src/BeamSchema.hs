{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module BeamSchema where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam

data UserT f = User
  { userId        :: C f Int32
  , userFirstName :: C f Text
  , userLastName  :: C f Text
  , userAvatar    :: C f ByteString
  , userCreatedAt :: C f LocalTime
  , userIsAdmin   :: C f Bool
  }
  deriving (Generic, Beamable)

data AuthorT f = Author
  { authorId               :: C f Int32
  , authorShortDescription :: C f Text
  }
  deriving (Generic, Beamable)

data CategoryT f = Category
  { categoryId       :: C f Int32
  , categoryParentId :: C f (Maybe Int32)
  , categoryName     :: C f Text
  }
  deriving (Generic, Beamable)

data PhotoT f = Photo
  { photoId      :: C f Int32
  , photoContent :: C f ByteString
  }
  deriving (Generic, Beamable)

data DraftT f = Draft
  { draftId          :: C f Int32
  , draftShortName   :: C f Text
  , draftCreatedAt   :: C f LocalTime
  , draftAuthorId    :: C f Int32
  , draftCategoryId  :: C f Int32
  , draftTextContent :: C f Text
  , draftMainPhotoId :: C f Int32
  }
  deriving (Generic, Beamable)

data PostT f = Post
  { postId          :: C f Int32
  , postShortName   :: C f Text
  , postPublishedAt :: C f LocalTime
  , postAuthorId    :: C f Int32
  , postCategoryId  :: C f Int32
  , postTextContent :: C f Text
  , postMainPhotoId :: C f Int32
  }
  deriving (Generic, Beamable)

data TagT f = Tag
  { tagId :: C f Int32
  , tagName :: C f Text
  }
  deriving (Generic, Beamable)

data PostTagT f = PostTag
  { postTagTagId  :: C f Int32
  , postTagPostId :: C f Int32
  }
  deriving (Generic, Beamable)

data DraftTagT f = DraftTag
  { draftTagTagId   :: C f Int32
  , draftTagDraftId :: C f Int32
  }
  deriving (Generic, Beamable)

data PostAdditionalPhotoT f = PostAdditionalPhoto
  { postAdditionalPhotoPhotoId :: C f Int32
  , postAdditionalPhotoPostId  :: C f Int32
  }
  deriving (Generic, Beamable)

data DraftAdditionalPhotoT f = DraftAdditionalPhoto
  { draftAdditionalPhotoPhotoId :: C f Int32
  , draftAdditionalPhotoDraftId :: C f Int32
  }
  deriving (Generic, Beamable)

data CommentaryT f = Commentary
  { commentaryId      :: C f Int32
  , commentaryUserId  :: C f Int32
  , commentaryPostId  :: C f Int32
  , commentaryContent :: C f Text
  }
  deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . userId

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = AuthorId . authorId

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = CategoryId . categoryId

instance Table PhotoT where
  data PrimaryKey PhotoT f = PhotoId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PhotoId . photoId

instance Table DraftT where
  data PrimaryKey DraftT f = DraftId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftId . draftId

instance Table PostT where
  data PrimaryKey PostT f = PostId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostId . postId

instance Table TagT where
  data PrimaryKey TagT f = TagId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TagId . tagId

instance Table PostTagT where
  data PrimaryKey PostTagT f = PostTagId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostTagId <$> postTagTagId <*> postTagPostId

instance Table DraftTagT where
  data PrimaryKey DraftTagT f = DraftTagId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftTagId <$> draftTagTagId <*> draftTagDraftId

instance Table PostAdditionalPhotoT where
  data PrimaryKey PostAdditionalPhotoT f = PostAdditionalPhotoId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostAdditionalPhotoId <$> postAdditionalPhotoPhotoId <*> postAdditionalPhotoPostId

instance Table DraftAdditionalPhotoT where
  data PrimaryKey DraftAdditionalPhotoT f = DraftAdditionalPhotoId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftAdditionalPhotoId <$> draftAdditionalPhotoPhotoId <*> draftAdditionalPhotoDraftId

instance Table CommentaryT where
  data PrimaryKey CommentaryT f = CommentaryId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = CommentaryId . commentaryId

type User = UserT Identity
type Author = AuthorT Identity
type Category = CategoryT Identity
type Photo = PhotoT Identity
type Draft = DraftT Identity
type Post = PostT Identity
type Tag = TagT Identity
type PostTag = PostTagT Identity
type DraftTag = DraftTagT Identity
type PostAdditionalPhoto = PostAdditionalPhotoT Identity
type DraftAdditionalPhoto = DraftAdditionalPhotoT Identity
type Commentary = CommentaryT Identity

deriving instance Show User
deriving instance Show Author
deriving instance Show Category
deriving instance Show Photo
deriving instance Show Draft
deriving instance Show Post
deriving instance Show Tag
deriving instance Show PostTag
deriving instance Show DraftTag
deriving instance Show PostAdditionalPhoto
deriving instance Show DraftAdditionalPhoto
deriving instance Show Commentary

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey AuthorT Identity)
deriving instance Show (PrimaryKey CategoryT Identity)
deriving instance Show (PrimaryKey CategoryT (Nullable Identity))
deriving instance Show (PrimaryKey PhotoT Identity)
deriving instance Show (PrimaryKey DraftT Identity)
deriving instance Show (PrimaryKey PostT Identity)
deriving instance Show (PrimaryKey TagT Identity)
deriving instance Show (PrimaryKey PostTagT Identity)
deriving instance Show (PrimaryKey DraftTagT Identity)
deriving instance Show (PrimaryKey PostAdditionalPhotoT Identity)
deriving instance Show (PrimaryKey DraftAdditionalPhotoT Identity)
deriving instance Show (PrimaryKey CommentaryT Identity)

data NewsDb f = NewsDb
  { dbUser                 :: f (TableEntity UserT)
  , dbAuthor               :: f (TableEntity AuthorT)
  , dbCategory             :: f (TableEntity CategoryT)
  , dbPhoto                :: f (TableEntity PhotoT)
  , dbDraft                :: f (TableEntity DraftT)
  , dbPost                 :: f (TableEntity PostT)
  , dbTag                  :: f (TableEntity TagT)
  , dbPostTag              :: f (TableEntity PostTagT)
  , dbDraftTag             :: f (TableEntity DraftTagT)
  , dbPostAdditionalPhoto  :: f (TableEntity PostAdditionalPhotoT)
  , dbDraftAdditionalPhoto :: f (TableEntity DraftAdditionalPhotoT)
  , dbCommentary           :: f (TableEntity CommentaryT)
  }
  deriving (Generic, Database be)

newsDb :: DatabaseSettings be NewsDb
newsDb = defaultDbSettings `withDbModification`
  dbModification
    { dbUser = setEntityName "usr"
    , dbPostTag = modifyTableFields tableModification
        { postTagTagId = "tag_id"
        , postTagPostId = "post_id" }
    , dbDraftTag = modifyTableFields tableModification
        { draftTagTagId = "tag_id"
        , draftTagDraftId = "draft_id" }
    , dbPostAdditionalPhoto = modifyTableFields tableModification
        { postAdditionalPhotoPhotoId = "photo_id"
        , postAdditionalPhotoPostId = "post_id" }
    , dbDraftAdditionalPhoto = modifyTableFields tableModification
        { draftAdditionalPhotoPhotoId = "photo_id"
        , draftAdditionalPhotoDraftId = "draft_id" }
    }
