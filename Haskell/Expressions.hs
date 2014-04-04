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

-- File: Expressions.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Nov 11, 2011

type Symbol = Char

data Expr a = Const a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Pow (Expr a) (Expr a)
            | Var Symbol
            deriving (Show)

-- this has to be done with a parser..

-- simplifyExpr :: Num a => Expr a -> Expr a
-- simplifyExpr (Const a) = Const a
-- simplifyExpr (Add x y) = addExpr x y
-- simplifyExpr (Mult x y) = multExpr x y
-- simplifyExpr (Pow x y) = powExpr x y
-- simplifyExpr (Var x) = Var x
--   where addExpr (Const x) (Const y) = Const (x + y)
--         addExpr x y = Add (simplifyExpr x) (simplifyExpr y)
--         multExpr (Const x) (Const y) = Const (x * y)
--         multExpr (Const x) (Var y) = ..
-- dead end..stuck..don't know how to do this :(
