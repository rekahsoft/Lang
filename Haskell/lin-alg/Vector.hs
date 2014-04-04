-- (C) Copyright Collin Doering 2014
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: Vector.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Feb  5, 2014

-- Inspired by Coursera class "Coding the Matrix"

-- | This Module represents Vectors
module Vector where

import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe,fromJust)

-- | Vector
data Vector a = Vector { unvector :: M.Map Integer a }

instance Show a => Show (Vector a) where
  show (Vector a) = "Vector " ++ M.foldr (\x y -> show x ++ " " ++ y) "" a

instance Functor Vector where
  fmap f (Vector v) = Vector $ M.map f v

instance Foldable Vector where
  foldr f i (Vector v) = M.foldr f i v

-- class Field a => Vector a where
--   (+) = udnefined
--   (-) = undefined
--   (*) = undefined

-- The Num class doesn't exactly fit for Vectors
-- instance Num a => Num (Vector a) where
--   (+) = addVector
--   (-) = subVector
--   (*) = scalarMultVector
--   negate v = fmap (*(-1)) v
--   abs v = undefined
--   fromInteger a = fromList [a]

-- | Returns the dimension of the given Vector
dimension :: Num a => Vector a -> Integer
dimension (Vector v) = toInteger . M.size $ v

-- | Given a list, returns it represented as a Vector
fromList :: Num a => [a] -> Vector a
fromList xs = Vector $ M.fromList $ zip [1..] xs

-- | 
scalarMultVector :: Num a => a -> Vector a -> Vector a
scalarMultVector a (Vector v) = Vector $ M.map (a*) v

-- | 
binOpVector :: Num a => (Int -> Int -> Bool) -> String -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
binOpVector p e f (Vector v1) (Vector v2)
  | p (M.size v1) (M.size v2) = Vector $ M.foldrWithKey (\k x y -> M.insert k (f x (fromMaybe (error "Internal error!") $ M.lookup k v2)) y) M.empty v1
  | otherwise = error e

-- | Add two given Vectors
addVector :: Num a => Vector a -> Vector a -> Vector a
addVector = binOpVector (==) "Error! Can only add vectors of the same dimension." (+)

-- | Subtract two given Vectors
subVector :: Num a => Vector a -> Vector a -> Vector a
subVector = binOpVector (==) "Error! Can only subtract vectors of the same dimension." (-)

-- | Apply the dot product to two given vectors
dotVector :: Num a => Vector a -> Vector a -> a
dotVector x@(Vector v1) y@(Vector v2) = M.foldr (+) 0 $ unvector $ binOpVector (==) "Error! Can only perform dot product on Vectors of equal size." (*) x y

(<.>) :: Num a => Vector a -> Vector a -> a
(<.>) = dotVector
