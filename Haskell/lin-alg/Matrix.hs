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

-- File: Matrix.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Feb 5, 2014

-- Inspired by Coursera class "Coding the Matrix"

-- | This modules represents a Matrix
module Matrix where

import Vector
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe,fromJust)

-- | A data structure that represents a Matrix
data Matrix a = Matrix { unmatrix :: M.Map Integer (Vector a) }

instance Show a => Show (Matrix a) where
  show (Matrix m) = M.foldr (\x y -> drop 7 (show x) ++ "\n" ++ y) "" m

instance Functor Matrix where
  fmap f (Matrix m) = Matrix $ M.foldrWithKey (\k v y -> M.insert k (fmap f v) y) M.empty m
                           
-- | 
matrixFromList :: Num a => [[a]] -> Matrix a
matrixFromList xs = Matrix $ foldr (\(k,v) y -> if length v == dim then
                                                  M.insert k (fromList v) y
                                                else error "Error! The rows of the matrix must be the same length.") M.empty $ zip [1..] xs
  where dim = length . head $ xs

-- | 
matrixElemAt :: Num a => Integer -> Integer -> Matrix a -> Maybe a
matrixElemAt i j (Matrix m)
  | i >= 1 &&
    j >= 1 &&
    i <= toInteger (M.size m) = M.lookup j (unvector $ fromJust $ M.lookup i m)
  | otherwise = Nothing

-- |
-- columnVector :: Num a => Integer -> Matrix a -> Vector a
columnVector j m@(Matrix m')   
  | j <= snd (matrixSize m) = Vector $ M.foldrWithKey (\k x y -> M.insert k (fromJust $ M.lookup j (unvector x)) y) M.empty m'
  | otherwise = error "Internal Error"

-- matrixSize :: Num a => Matrix a -> (Integer,Integer)
matrixSize (Matrix m) = (toInteger $ M.size m, toInteger $ M.size $ unvector $ fromJust $ M.lookup 1 m)
  
-- | 
-- binOpMatrix :: Num a => ((Int,Int) -> (Int,Int) -> Bool) -> (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
binOpMatrix p e f (Matrix m1) (Matrix m2)
  | p (fromMaybe (error "Internal Error") $ M.lookup 1 m1, M.size m1)
      (fromMaybe (error "Internal Error") $ M.lookup 1 m2, M.size m2) = undefined
  | otherwise = error e

-- |
transposeMatrix :: Num a => Matrix a -> Matrix a
-- transposeMatrix m@(Matrix m1) = Matrix $ M.foldrWithKey (\k _ y -> M.insert k (columnVector k m) y) M.empty m1
transposeMatrix m@(Matrix m') = let (_,j) = matrixSize m
                                in Matrix $ foldr (\k y -> M.insert k (columnVector k m) y) M.empty [1..j]
  
-- | 
scalarMultMatrix :: Num a => a -> Matrix a -> Matrix a
scalarMultMatrix a m = fmap (scalarMultVector a) m

-- | 
-- multMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
multMatrix (Matrix m1) n@(Matrix m2)
  | (M.size . unvector $
     fromMaybe (error "Error! Cannot multiply matrices with given dimension.") $
     M.lookup 1 m1) == M.size m2 = Matrix $
                                   M.foldrWithKey (\k x y -> M.insert k (Vector $
                                                                         M.foldrWithKey (\k' x' y' -> M.insert k' (dotVector x x') y') M.empty (unmatrix $ transposeMatrix n)) y) M.empty m1
  | otherwise = error "Internal Error"

-- multMatrix m@(Matrix m') n@(Matrix n') = let mSize = matrixSize m
--                                              nSize = matrixSize m
--                                              canMult (_,a) (b,_) = a == b
                                             
                
-- multMatrix = binOpMatrix (\(_,a) (b,_) -> a == b) (\(Matri

-- | 
crossProductMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
crossProductMatrix = undefined

-- | 
addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrix = undefined
-- addMatrix = binOpMatrix (\(a,b) (c,d) -> a == c && b == d) "Error! Cannot add matrices with different dimensions." (+)

(<+>) :: Num a => Matrix a -> Matrix a -> Matrix a
(<+>) = addMatrix

-- | 
subMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
subMatrix = undefined

(<->) :: Num a => Matrix a -> Matrix a -> Matrix a
(<->) = subMatrix

-- | 
rowEchelonForm :: Num a => Matrix a -> Matrix a
rowEchelonForm = undefined

-- | 
reducedRowEchelonForm :: Num a => Matrix a -> Matrix a
reducedRowEchelonForm = undefined
