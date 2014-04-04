-- (C) Copyright Collin Doering 2011
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

-- File: polynomials.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Aug  7, 2011

data Floating a = Poly a
                | PolyL [a]
                deriving (Eq, Show)

data Posn2D x y = Posn2D Double Double
                deriving (Eq, Show, Ord)

instance Ord Posn2D where
  (>) (Posn2D x1 y1) (Posn2D x2 y2) = distFromOrigin x1 y1 > distFromOrigin x2 y2
    where distFromOrigin a b = sqrt (a**2 + b**2)

--addPolynomial :: Polynomial a -> Polynomial a -> Polynomial a
