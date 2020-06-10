;;; currency-convert.el --- Currency converter -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-currency-convert
;; Package-Requires: ((emacs "24.4"))
;; Package-Version: 0.1.0
;; Keywords: comm convenience i18n
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Convert amounts of money from one currency to another in Emacs.
;;
;; Exchange rates are downloaded from exchangeratesapi.io. They ought
;; to be accurate enough for everyday purposes. It goes without saying
;; that you should not rely on this package for investment or business
;; decisions.
;;
;;; Code:

(require 'json)
(require 'url)

(defvar currency-convert--rates nil
  "Exchange rates for all known currencies.")

(defun currency-convert--rates-file ()
  "Internal helper to get local exchange rates file name."
  (concat (file-name-as-directory user-emacs-directory)
          "currency-convert-rates.json"))

(defun currency-convert--load-rates ()
  "Internal helper to load exchange rates from local file."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents (currency-convert--rates-file))
        (setq currency-convert--rates (json-read)))
    ((file-missing file-error end-of-file json-error)
     (error "Please do M-x currency-convert-update-rates"))))

(defun currency-convert--ensure-rates ()
  "Internal helper to ensure exchange rates are loaded."
  (unless currency-convert--rates
    (currency-convert--load-rates)))

(defun currency-convert-update-rates ()
  "Get the latest exchange rates from the internet.

The rates are saved into a local file in `user-emacs-directory'
so they don't need to be updated on future Emacs runs.  However,
it is okay to update at any time to get more recent rates.

The rates are downloaded from the free site exchangeratesapi.io.
They may lag a few days behind the latest rates.  Downloads of
up-to-the-minute rates are only offered by paid services."
  (interactive)
  (let* ((url-show-status nil) (url-mime-accept-string "application/json"))
    (with-temp-buffer
      (url-insert-file-contents "https://api.exchangeratesapi.io/latest")
      (write-region nil nil (currency-convert--rates-file))))
  (currency-convert--load-rates)
  (let ((date (cdr (assoc 'date currency-convert--rates))))
    (message "Now using exchange rates from %s" date)
    date))

(defun currency-convert--currency-names ()
  "Internal helper to list all known currency names."
  (sort (cons (cdr (assoc 'base currency-convert--rates))
              (mapcar (lambda (pair) (symbol-name (car pair)))
                      (cdr (assoc 'rates currency-convert--rates))))
        #'string<))

(defun currency-convert--currency-rate (currency)
  "Internal helper to get the exchange rate for CURRENCY."
  (if (equal currency (cdr (assoc 'base currency-convert--rates))) 1
    (cdr (or (assoc currency (cdr (assoc 'rates currency-convert--rates))
                    (lambda (a b) (equal (symbol-name a) b)))
             (if (equal currency "")
                 (error "No currency given")
               (error "No such currency: %s" currency))))))

(defun currency-convert--display-alist (alist)
  "Internal helper to display ALIST of currency-amount pairs."
  (with-current-buffer-window
   "*Currency*" nil nil
   (let ((inhibit-read-only t))
     (erase-buffer)
     (special-mode)
     (dolist (pair alist (current-buffer))
       (let* ((currency (car pair)) (amount (cdr pair)))
         (insert (format "%10.2f %s\n" amount currency)))))))

(defun currency-convert--parse-amount (string)
  "Internal helper to parse STRING as an amount of money."
  (save-match-data
    (if (string-match "^-?[0-9]+\\(\\.[0-9][0-9]\\)?$" string)
        (string-to-number string)
      (error "Amount should be of the form [-]123.45"))))

;;;###autoload
(defun currency-convert (amount from-currency)
  "Convert AMOUNT from FROM-CURRENCY to other known currencies.

Due to inaccuracies in exchange rate data and floating point
arithmetic, the conversion is only suitable for everyday
purposes.  Do not use it for business or investment decisions.

When used as an interactive command, AMOUNT and FROM-CURRENCY are
input into the minibuffer.  The conversion is displayed in the
*Currency* buffer.  If that buffer already exists, its contents
are replaced with the new conversion.

When called from Lisp, AMOUNT is an integer or floating point
number.  FROM-CURRENCY is the uppercase three-letter currency as a
string.  The return value is a list of (CURRENCY . AMOUNT) pairs."
  (interactive
   (progn (currency-convert--ensure-rates)
          (let* ((amount (currency-convert--parse-amount
                          (read-string "Amount: ")))
                 (from-currency
                  (completing-read
                   "Currency: " (currency-convert--currency-names) nil t)))
            (list amount from-currency))))
  (let* ((from-rate (currency-convert--currency-rate from-currency))
         (base-amount (/ amount from-rate))
         (alist
          (mapcar
           (lambda (to-currency)
             (let* ((to-rate (currency-convert--currency-rate to-currency))
                    (to-amount (* base-amount to-rate)))
               (cons to-currency to-amount)))
           (currency-convert--currency-names))))
    (when (called-interactively-p 'interactive)
      (currency-convert--display-alist alist))
    alist))

(provide 'currency-convert)

;;; currency-convert.el ends here
