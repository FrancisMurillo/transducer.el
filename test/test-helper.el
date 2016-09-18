;;; test-helper.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: test-helper.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Fri Sep  9 10:18:18 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(require 'dash)

(defun get-required-file (name url)
  "Get required NAME and URL."
  (condition-case ex
    (require name)
  ('error
   (progn
     (require 'url)
     (let ((req-file (make-temp-file "req-")))
       (url-copy-file url req-file)
       (load-file req-file)
       (delete-file req-file))))))

(get-required-file 'stream "https://raw.githubusercontent.com/FrancisMurillo/stream.el/master/stream.el")


(require 'transducer (expand-file-name "transducer" "./"))

(defun list-equal (equality xs ys)
  "Check if a list is equal with EQUALITY, XS and YS."
  (if (/= (length xs) (length ys))
      nil
    (if (and (null xs) (null ys))
        t
      (-any-p
       (lambda (pair)
         (funcall equality (car pair) (cdr pair)))
       (-zip-pair xs ys)))))

(defun pair-equal (equality xp yp)
  (and (consp xp)
     (consp yp)
     (funcall equality (car xp) (car yp))
     (funcall equality (cdr xp) (cdr yp))))

(defun unroll-pairs (pairs)
  (-reduce
   #'append
   (-map
    (lambda (pair)
      (list (car pair) (cdr pair)))
    pairs)))


(provide 'test-helper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test-helper.el ends here
