;;; helm-cscope.el --- Use xcscope with helm!

;; Authors: Hikaru Tooyama (http://github.com/vexus2/)
;;          Sergey Pashaev <sergey.pashaev@gmail.com>
;; Maintainer: Sergey Pashaev <sergey.pashaev@gmail.com>
;; Created: 30th November 2013
;; Keywords: helm, cscope

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; Source code can be found here:
;;   http://github.com/sergey-pashaev/helm-cscope

;;; Commentary:
;;
;; This is pretty straightforward port of anything-cscope.el from:
;; https://github.com/vexus2/dotfiles/blob/master/.elisp/anything-cscope.el
;; So all credits goes to Mr. vexus2

;;; Usage:
;;
;; Put helm-cscope.el in your load path:
;; (add-to-list 'load-path "/<path to helm-cscope dir here>/")
;;
;; Add (require 'helm-cscope) to your .emacs
;;
;; Example configuration:

;; ;; Enable helm-cscope-mode
;; (add-hook 'c-mode-hook 'helm-cscope-mode)
;; (add-hook 'c++-mode-hook 'helm-cscope-mode)
;; ;; Set key bindings
;; (eval-after-load "helm-cscope"
;;   '(progn
;;      (define-key helm-cscope-mode-map (kbd "M-t") 'helm-cscope-find-symbol)
;;      (define-key helm-cscope-mode-map (kbd "M-r") 'helm-cscope-find-global-definition)
;;      (define-key helm-cscope-mode-map (kbd "M-g M-c") 'helm-cscope-find-called-function)
;;      (define-key helm-cscope-mode-map (kbd "M-g M-p") 'helm-cscope-find-calling-this-funtcion)
;;      (define-key helm-cscope-mode-map (kbd "M-s") 'helm-cscope-select)))


(require 'helm)
(require 'xcscope)

(defvar helm-cscope-db-directory nil)

(defvar helm-c-source-cscope-c-cymbol
  '((name . "cscope : c-cymbol")
    (candidates-process . (lambda ()
                            (helm-cscope-candidates "-0")))
    (action . helm-c-source-cscope-action)
    (candidate-number-limit . 20)
    (type . file)
    (delayed)))

;; todo: fix action for this source. it creates empty file instead
;; openning existing
(defvar helm-c-source-cscope-global-definition
  '((name . "cscope : global-definition")
    
    (candidates-process . (lambda ()
                            (helm-cscope-candidates "-1")))
    (action . helm-c-source-cscope-action)
    (requires-pattern . 3)
    (candidate-number-limit . 10)
    (delayed)))

(defvar helm-c-source-cscope-called-function
  '((name . "cscope : called-function")
    
    (candidates-process . (lambda ()
                            (helm-cscope-candidates "-2")))
    (action . helm-c-source-cscope-action)
    (requires-pattern . 3)
    (delayed)))

(defvar helm-c-source-cscope-calling-this-function
  '((name . "cscope : calling-this-function")
    (candidates-process . (lambda ()
                            (helm-cscope-candidates "-3")))
    (action . helm-c-source-cscope-action)
    (requires-pattern . 3)
    (delayed)))

;; todo: how it should work?
(defvar helm-c-source-cscope-text-string
  '((name . "cscope : text-string")
    (candidates-process . (lambda ()
                            (helm-cscope-candidates "-4")))
    (action . helm-c-source-cscope-action)
    (requires-pattern . 3)
    (delayed)))

(defun helm-cscope-candidates (search-type-arg)
  (let ((cscp-dir nil)
        (base-database-file-name)
        (next-item)
        (options)
        (cscope-directory)
        (database-file)
        (done)
        )
    (save-excursion
      (with-current-buffer helm-current-buffer
        (setq cscp-dir (cscope-canonicalize-directory
                        (or cscope-initial-directory default-directory)))
        )
      (setq default-directory cscp-dir
            cscope-search-list (cscope-find-info cscp-dir) 
            cscope-searched-dirs nil
            cscope-command-args (list search-type-arg (concat helm-pattern ".*"))
            cscope-first-match nil
            cscope-first-match-point nil
            cscope-stop-at-first-match-dir-meta (memq t cscope-search-list)
            )
      
      (catch 'finished
        (setq options '("-L"))
        (while (and (not done) cscope-search-list)
          (setq next-item (car cscope-search-list)
                cscope-search-list (cdr cscope-search-list)
                base-database-file-name cscope-database-file
                )
          (if (listp next-item)
              (progn
                (setq cscope-directory (car next-item))
                (if (not (stringp cscope-directory))
                    (setq cscope-directory
                          (cscope-search-directory-hierarchy
                           default-directory)))
                (if (file-regular-p cscope-directory)
                    (progn
                      ;; Handle the case where `cscope-directory' is really
                      ;; a full path name to a cscope database.
                      (setq base-database-file-name
                            (file-name-nondirectory cscope-directory)
                            cscope-directory
                            (file-name-directory cscope-directory))
                      ))
                (setq cscope-directory 
                      (file-name-as-directory cscope-directory))
                (if (not (member cscope-directory cscope-searched-dirs))
                    (progn
                      (setq cscope-searched-dirs (cons cscope-directory
                                                       cscope-searched-dirs)
                            done t)
                      ))
                )
            (progn
              (if (and cscope-first-match
                       cscope-stop-at-first-match-dir
                       cscope-stop-at-first-match-dir-meta)
                  (throw 'finished nil))
              ))
          )
        (if (not done)
            (throw 'finished nil))
        (if (car (cdr next-item))
            (let (newopts)
              (setq newopts (car (cdr next-item)))
              (if (not (listp newopts))
                  (error (format "Cscope options must be a list: %s" newopts)))
              (setq options (append options newopts))
              ))
        (if cscope-command-args
            (setq options (append options cscope-command-args)))
        (setq database-file (concat cscope-directory base-database-file-name)
              cscope-searched-dirs (cons cscope-directory
                                         cscope-searched-dirs)
              )

        ;; The database file and the directory containing the database file
        ;; must both be writable.
        (if (or (not (file-writable-p database-file))
                (not (file-writable-p (file-name-directory database-file)))
                cscope-do-not-update-database)
            (setq options (cons "-d" options)))

        (setq options (cons base-database-file-name options))
        (setq options (cons "-f" options))
        (setq default-directory cscope-directory)
        (setq helm-cscope-db-directory cscope-directory)
        (setq options (cons cscope-program options))
        (apply 'start-process (concat "helm-cscope" search-type-arg) nil
               options)
        ))))

(defun helm-c-source-cscope-action (line)
  (let* (
         (lines (split-string line " "))
         (file-name (car lines))
         (line-number (string-to-number (car (cdr (cdr lines)))))
         )
    (find-file (concat helm-cscope-db-directory file-name))
    (goto-line line-number)))

;;;###autoload
(defun helm-cscope-select ()
  (interactive)
  (helm :sources '(
                   helm-c-source-cscope-c-cymbol
                   helm-c-source-cscope-global-definition
                   helm-c-source-cscope-called-function
                   helm-c-source-cscope-calling-this-function
                   )
        :buffer "*helm cscope*"))

;;;###autoload
(defun helm-cscope-find-symbol ()
  (interactive)
  (helm :sources '(helm-c-source-cscope-c-cymbol)
        :buffer "*helm cscope*"))

;;;###autoload
(defun helm-cscope-find-global-definition ()
  (interactive)
  (helm :sources '(helm-c-source-cscope-global-definition)
        :buffer "*helm cscope*"))

;;;###autoload
(defun helm-cscope-find-called-function ()
  (interactive)
  (helm :sources '(helm-c-source-cscope-called-function)
        :buffer "*helm cscope*"))

;;;###autoload
(defun helm-cscope-find-calling-this-funtcion ()
  (interactive)
  (helm :sources '(helm-c-source-cscope-calling-this-function)
        :buffer "*helm cscope*"))

(defvar helm-cscope-mode-name " Helm cscope")
(defvar helm-cscope-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-cscope-mode ()
  "Enable for helm-cscope"
  :group      'helm-cscope
  :init-value nil
  :global     nil
  :keymap     helm-cscope-mode-map
  :lighter    helm-cscope-mode-name
  (if helm-cscope-mode
      (run-hooks 'helm-cscope-mode-hook)))

(provide 'helm-cscope)

;;; helm-cscope.el ends here
