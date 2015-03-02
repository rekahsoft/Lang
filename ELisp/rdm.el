;; (C) Copyright Collin J. Doering 2014
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; File: rdm.el
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Nov  6, 2014

(defun hi-there (arg)
  "Says 'Hi there!' in the message buffer. If called with the prefix
argument, prompts the user to enter a name and then replies 'Hi there, name'"
  (interactive "P")
  (if (equal arg '(4))
      (let ((name (read-from-minibuffer "Enter your name: ")))
        (message "Hi there, %s." name))
    (message "Hi there!")))

