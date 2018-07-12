# (C) Copyright Collin Doering 2011
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# File: rdm.py
# Author: Collin J. Doering <rekahsoft@gmail.com>
# Date: Jul  7, 2011def factorial (n):

import os

# Only works for python 2.6.x since in python 3 print is a function
def factorial (n):
    if n < 0:
        print("Sorry can't take the factorial of a number n < 0!")
        return
    elif n == 1:
        return 1;
    else:
        for i in range (2, n):
            ret *= i
        return ret
