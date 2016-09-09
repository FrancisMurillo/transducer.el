;;; transducer-test.el ---
;;
;; Filename: transducer-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Fri Sep  9 10:17:53 2016 (+0800)
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

(ert-deftest transducer-test/map ()
  (let* ((mapper #'1+)
      (xs (list 1 2 3))
      (ys (-map mapper xs))
      (zs
       (transducer-transduce
        (transducer-map mapper)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((mapper (-partial #'* 2))
      (xs (list))
      (ys (-map mapper xs))
      (zs
       (transducer-transduce
        (transducer-map mapper)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs))))

(ert-deftest transducer-test/filter ()
  (let* ((filterer #'oddp)
      (xs (list 1 2 3))
      (ys (-filter filterer xs))
      (zs
       (transducer-transduce
        (transducer-filter filterer)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((filterer #'evenp)
      (xs (list))
      (ys (-filter filterer xs))
      (zs
       (transducer-transduce
        (transducer-filter filterer)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs))))

(ert-deftest transducer-test/composes ()
  (let* ((mapper #'1+)
      (filterer #'oddp)
      (xs (list 1 2 3 4 5))
      (ys (-filter
           filterer
           (-map mapper xs)))
      (zs
       (transducer-transduce
        (transducer-composes
         (transducer-map mapper)
         (transducer-filter filterer))
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((mapper #'1+)
      (filterer #'oddp)
      (xs (list))
      (ys (-filter
           filterer
           (-map mapper xs)))
      (zs
       (transducer-transduce
        (transducer-composes
         (transducer-map mapper)
         (transducer-filter filterer))
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((mapper #'1+)
      (filterer #'oddp)
      (remapper #'number-to-string)
      (xs (list 1 2 3 4 5))
      (ys (-map
           remapper
           (-filter
            filterer
            (-map mapper xs))))
      (zs
       (transducer-transduce
        (transducer-composes
         (transducer-composes
          (transducer-map mapper)
          (transducer-filter filterer))
         (transducer-map remapper))
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'string-equal ys zs))))

(ert-deftest transducer-test/distinct ()
  (let* ((xs (list 1 2 3 1 2 3 4 5 0))
      (ys (-distinct xs))
      (zs
       (transducer-transduce
        (transducer-distinct)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((xs (list))
      (ys (-distinct xs))
      (zs
       (transducer-transduce
        (transducer-distinct)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs))))

(ert-deftest transducer-test/first ()
  (let* ((filterer #'oddp)
      (xs (list 2 4 5 6 8))
      (ys (list (-first filterer xs)))
      (zs
       (transducer-transduce
        (transducer-first filterer)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs)))

  (let* ((filterer #'evenp)
      (xs (list))
      (ys (-first filterer xs))
      (zs
       (transducer-transduce
        (transducer-first filterer)
        (transducer-list-reducer)
        xs)))
    (should (list-equal #'= ys zs))))


(provide 'transducer-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer-test.el ends here
