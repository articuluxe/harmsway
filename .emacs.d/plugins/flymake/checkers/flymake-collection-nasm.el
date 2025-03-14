;;; flymake-collection-nasm.el --- NASM diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2024 Abdelhak Bougouffa

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; `flymake' checker for Assembly using "nasm".
;; Inspired by `flymake-nasm' - https://github.com/juergenhoetzel/flymake-nasm

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-nasm-format 'elf64
  "The NASM output format.
You can list the supported formats by running \"nasm -hf\"q."
  :type 'symbol
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-nasm "flymake-collection-nasm")
(flymake-collection-define-rx flymake-collection-nasm
  "Assembly checker using the Netwide Assembler (NASM)."
  :title "nasm"
  :pre-let ((nasm-exec (executable-find "nasm")))
  :pre-check (unless nasm-exec (error "Not found nasm on PATH"))
  :write-type 'file
  :command `(,nasm-exec ,(format "-f%s" flymake-collection-nasm-format) ,flymake-collection-temp-file)
  :regexps
  ((error bol (file-name) ":" line ": error: " (message) eol)
   (warning bol (file-name) ":" line ": warning: " (message) eol)
   (note bol (file-name) ":" line ": note: " (message) eol)))


(provide 'flymake-collection-nasm)

;;; flymake-collection-nasm.el ends here
