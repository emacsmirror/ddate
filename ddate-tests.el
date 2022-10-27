;;; ddate-tests.el --- Tests for ddate.el                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Erik L. Arneson

;; Author: Erik L. Arneson <earneson@arnesonium.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run the tests.

;;; Code:

(require 'ert)
(require 'ddate)

(ert-deftest ddate-standard ()
  (should (string= (ddate 5 5 -30) "Setting Orange, Discord 52, 1137 YOLD"))
  (should (string= (ddate 12 11 1975) "Sweetmorn, The Aftermath 24, 3141 YOLD"))
  (should (string= (ddate 12 11 -5000) "Sweetmorn, The Aftermath 24, -3833 YOLD")))

(ert-deftest ddate-immediate ()
  (should (string-match (rx bol "Today is " (one-or-more ascii) " YOLD")
                        (ddate))))

(ert-deftest ddate-holidays ()
  (should (string= (ddate 5 1 1985) "Setting Orange, Chaos 5, 3151 YOLD, Mungday"))
  (should (string= (ddate 19 2 1982) "Setting Orange, Chaos 50, 3148 YOLD, Chaoflux"))
  (should (string= (ddate 19 3 1979) "Pungenday, Discord 5, 3145 YOLD, Mojoday"))
  (should (string= (ddate 15 7 1968) "Sweetmorn, Confusion 50, 3134 YOLD, Confuflux"))
  (should (string= (ddate 12 8 1913) "Prickle-Prickle, Bureaucracy 5, 3079 YOLD, Zaraday"))
  (should (string= (ddate 26 9 1898) "Prickle-Prickle, Bureaucracy 50, 3064 YOLD, Bureflux")))

(ert-deftest ddate-st-tib ()
  (should (string= (ddate 29 2 2004) "St. Tib's Day, 3170 YOLD"))
  (should (string= (ddate 29 2 1912) "St. Tib's Day, 3078 YOLD"))
  (should-error (ddate 29 2 2001)))

(ert-deftest ddate-invalid ()
  (should-error (ddate 0 0 0))
  (should-error (ddate 45 1 1))
  (should-error (ddate 9 39 (- 0 10))))

(ert-deftest ddate-no-ddate ()
  (let ((ddate-command "/dev/null"))
    (should-error (ddate)))
  (let ((ddate-command "/dev/null"))
    (should-error (ddate 5 5 30)))
  (let ((ddate-command "date"))
    (should-error (ddate 6 6 31))))

;;; ddate-tests.el ends here
