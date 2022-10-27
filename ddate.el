;;; ddate.el --- manage Discordian dates in Emacs                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Erik L. Arneson

;; Author: Erik L. Arneson <earneson@arnesonium.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;; This package uses the ddate command
;; (https://github.com/bo0ts/ddate) and then munges its output to
;; produce something pretty. With faces.

;;; Code:

(defgroup ddate nil 
  "Options for handling and displaying the Discordian date"
  :prefix "ddate-"
  :group 'applications)

(defcustom ddate-command "ddate"
  "Command to run to get Discordian date"
  :type 'string
  :group 'ddate)

(defcustom ddate-format "Today is %{%A, the %e day of %B%} in the YOLD %Y%N: Celebrate %H!"
  "Default format for ddate. Try to keep it all on one line."
  :type ':string
  :group 'ddate)

(defvar ddate-day-colors '(("Sweetmorn" . "white")
                           ("Boomtime" . "red")
                           ("Pungenday" . "yellow")
                           ("Prickle-Prickle" . "green")
                           ("Setting Orange" . "orange"))
  "Colors associated with the days of the week")

(defvar ddate-holiday-names '("Mungday"
                              "Chaoflux"
                              "St. Tib's Day"
                              "Mojoday"
                              "Discoflux"
                              "Syaday"
                              "Confuflux"
                              "Zaraday"
                              "Bureflux"
                              "Maladay"
                              "Afflux")
  "Names of the Discordian holidays")

(defun ddate (&optional day month year)
  "Return the Discordian date as a string."
  (interactive)
  (let ((shell-command (format "%s +\"%s\"" ddate-command ddate-format)))
    (if year
        (setq shell-command (concat shell-command
                                    (format " %d %d %d" day month year))))
    (string-trim-right (shell-command-to-string shell-command))))

(defun ddate-pretty (&optional day month year)
  "Return the Discordian date as a propertized string"
  (let ((ddate-string (ddate day month year))
        (day-match-rx (rx (group (eval (cons 'or (mapcar #'car ddate-day-colors))))))
        (holiday-match-rx (rx (group (eval (cons 'or ddate-holiday-names)))))
        day-name)
    ;; Apply colors to the days of the week
    (if (string-match day-match-rx ddate-string)
        (progn
          (setq day-name (match-string 1 ddate-string))
          (add-text-properties (match-beginning 1) (match-end 1)
                               (list 'font-lock-face (list ':foreground (cdr (assoc day-name ddate-day-colors))))
                               ddate-string)))
    ;; Is it a holiday? Let's celebrate.
    (if (string-match holiday-match-rx ddate-string)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(font-lock-face (:foreground "cyan" :weight bold))
                             ddate-string))
    ddate-string))

(provide 'ddate)
;;; test.el ends here
