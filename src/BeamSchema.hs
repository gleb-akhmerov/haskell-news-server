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
  { _userId        :: C f Int32
  , _userFirstName :: C f Text
  , _userLastName  :: C f Text
  , _userAvatar    :: C f ByteString
  , _userCreatedAt :: C f LocalTime
  , _userIsAdmin   :: C f Bool
  }
  deriving (Generic, Beamable)

data AuthorT f = Author
  { _authorId               :: C f Int32
  , _authorShortDescription :: C f Text
  }
  deriving (Generic, Beamable)

data CategoryT f = Category
  { _categoryId       :: C f Int32
  , _categoryParentId :: C f (Maybe Int32)
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
  , _draftCreatedAt   :: C f LocalTime
  , _draftAuthorId    :: C f Int32
  , _draftCategoryId  :: C f Int32
  , _draftTextContent :: C f Text
  , _draftMainPhotoId :: C f Int32
  }
  deriving (Generic, Beamable)

data PostT f = Post
  { _postId          :: C f Int32
  , _postShortName   :: C f Text
  , _postPublishedAt :: C f LocalTime
  , _postAuthorId    :: C f Int32
  , _postCategoryId  :: C f Int32
  , _postTextContent :: C f Text
  , _postMainPhotoId :: C f Int32
  }
  deriving (Generic, Beamable)

data TagT f = Tag
  { _tagId :: C f Int32
  , _tagName :: C f Text
  }
  deriving (Generic, Beamable)

data PostTagT f = PostTag
  { _postTagTagId  :: C f Int32
  , _postTagPostId :: C f Int32
  }
  deriving (Generic, Beamable)

data DraftTagT f = DraftTag
  { _draftTagTagId   :: C f Int32
  , _draftTagDraftId :: C f Int32
  }
  deriving (Generic, Beamable)

data PostAdditionalPhotoT f = PostAdditionalPhoto
  { _postAdditionalPhotoPhotoId :: C f Int32
  , _postAdditionalPhotoPostId  :: C f Int32
  }
  deriving (Generic, Beamable)

data DraftAdditionalPhotoT f = DraftAdditionalPhoto
  { _draftAdditionalPhotoPhotoId :: C f Int32
  , _draftAdditionalPhotoDraftId :: C f Int32
  }
  deriving (Generic, Beamable)

data CommentaryT f = Commentary
  { _commentaryId      :: C f Int32
  , _commentaryUserId  :: C f Int32
  , _commentaryPostId  :: C f Int32
  , _commentaryContent :: C f Text
  }
  deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (C f Int32)
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
  data PrimaryKey PostT f = PostId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostId . _postId

instance Table TagT where
  data PrimaryKey TagT f = TagId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TagId . _tagId

instance Table PostTagT where
  data PrimaryKey PostTagT f = PostTagId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostTagId <$> _postTagTagId <*> _postTagPostId

instance Table DraftTagT where
  data PrimaryKey DraftTagT f = DraftTagId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftTagId <$> _draftTagTagId <*> _draftTagDraftId

instance Table PostAdditionalPhotoT where
  data PrimaryKey PostAdditionalPhotoT f = PostAdditionalPhotoId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PostAdditionalPhotoId <$> _postAdditionalPhotoPhotoId <*> _postAdditionalPhotoPostId

instance Table DraftAdditionalPhotoT where
  data PrimaryKey DraftAdditionalPhotoT f = DraftAdditionalPhotoId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = DraftAdditionalPhotoId <$> _draftAdditionalPhotoPhotoId <*> _draftAdditionalPhotoDraftId

instance Table CommentaryT where
  data PrimaryKey CommentaryT f = CommentaryId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = CommentaryId . _commentaryId

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
  { _dbUser                 :: f (TableEntity UserT)
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
    { _dbUser = setEntityName "usr"
    , _dbPostTag = modifyTableFields tableModification
        { _postTagTagId = "tag_id"
        , _postTagPostId = "post_id" }
    , _dbDraftTag = modifyTableFields tableModification
        { _draftTagTagId = "tag_id"
        , _draftTagDraftId = "draft_id" }
    , _dbPostAdditionalPhoto = modifyTableFields tableModification
        { _postAdditionalPhotoPhotoId = "photo_id"
        , _postAdditionalPhotoPostId = "post_id" }
    , _dbDraftAdditionalPhoto = modifyTableFields tableModification
        { _draftAdditionalPhotoPhotoId = "photo_id"
        , _draftAdditionalPhotoDraftId = "post_id" }
    }
