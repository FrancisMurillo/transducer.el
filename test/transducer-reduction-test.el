;;; transducer-reduction-test.el --- Test for reduction
;;
;; Filename: transducer-reduction-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Fri Sep  9 17:15:11 2016 (+0800)
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

(ert-deftest transducer-reduction-test/reduced ()
  (should (transducer-reduced-value-p
           (transducer-reduced-value 'meow)))
  (let ((value 'meow))
    (should (eq value
                (transducer-reduced-get-value
                 (transducer-reduced-value value))))))
  (let ((value 'purr))
    (should (eq value
                (transducer-reduced-get-value
                 (transducer-reduced-value
                  (transducer-reduced-value value))))))


(ert-deftest transducer-reduction-test/list ()
  (let ((xs (list 1 2 3 4 5)))
    (should
     (list-equal
      #'=
      xs
      (transducer-transduce
       (transducer-identity)
       (transducer-list-reducer)
       xs)))))

(ert-deftest transducer-reduction-test/stream ()
  (let* ((xs  (list 1 2 3 4 5))
      (stream-to-list
       (transducer-transduce-stream
        (transducer-identity)
        (stream-from-list xs))))
    (should
     (list-equal
      #'=
      xs
      (stream-to-list
       (transducer-transduce-stream
        (transducer-identity)
        (stream-from-list xs)))))))


(provide 'transducer-reduction-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer-reduction-test.el ends here
