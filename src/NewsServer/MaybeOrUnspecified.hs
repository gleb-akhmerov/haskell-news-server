module NewsServer.MaybeOrUnspecified where


data MaybeOrUnspecified a
  = Specified (Maybe a)
  | Unspecified
  deriving Show
