module JoinList where
import Sized (Sized (size), getSize)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m 
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ a)
    | i == 0 = Just a
    | otherwise = Nothing
indexJ i (Append m l1 l2)
    | i < 0 || i > size0 = Nothing
    | i < size1 = indexJ i l1
    | otherwise = indexJ (i-size1) l2
        where size0 = getSize . size $ m
              size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l@Single{}
    | n <= 0 = l
dropJ n l@(Append m l1 l2)
    | n >= size0 = Empty
    | n >= size1 = dropJ (n-size1) l2
    | n > 0 = dropJ n l1 +++ l2
    | otherwise = l
        where size0 = getSize . size $ m
              size1 = getSize . size . tag $ l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l@Single{}
    | n > 0 = l
takeJ n l@(Append m l1 l2)
    | n >= size0 = l
    | n >= size1 = l1 +++ takeJ (n-size1) l2
    | n > 0 = takeJ n l1
        where size0 = getSize . size $ m
              size1 = getSize . size . tag $ l1
takeJ _ _ = Empty
