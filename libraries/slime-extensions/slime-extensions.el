;;; babel-mode.el --- 

;; Keywords: convenience, babel2, programming, auto-complete

;;; Commentary:

;; Hooks into and extends lisp-mode, provided by slime.
;; Made and tested for programming in Babel2 with Emacs and CCL via slime.
;; Put the following text on the first line of a file in order to enable
;; the Babel major-mode: ;;; -*-babel-*-" ... when inserted the first time,
;; reloading the buffer (e.g. M-x revert-buffer) will enable babel-mode!

;; Babel-helper establishes the babel-mode mainly for auto-completion.
;; But there are some further convenience options and settings that can
;; help novice users configure a nice (slime) lisp environment.
;; Currently they are disabled by default, but please feel free to
;; check the (via the menu-bar, or via M-x) customize-group
;; Programming/Babel Helper ... you can configure the settings there.
;; Of course you can also add them manually to the custom-set-variables
;; section of your .emacs file.

;;; Howto:

;; PART 1:
;; If you want to enable slime-extensions with default settings, add the
;; following lines BEFORE the custom-set-variables section of your
;; .emacs file *keep the order! and remove the first two ;; of every line*
;; and adapt the first path so it matches your Babel folder:
;; ----
;; ;; adapt this path to include your babel folder
;; (setf slime-extensions-path "~/Babel2/sharing/slime-extensions")
;;
;; ;; Load the babel-mode / slime-extension configurations ... also an emacs
;; ;; customization group in "Programming/"
;; (add-to-list 'load-path slime-extensions-path)
;; (require 'slime-extensions-config)
;;
;; ;; Due to complications in the auto-complete-config this needs
;; ;; to be executed after slime-extensions-config, but before
;; ;; custom-set-variables!
;; (add-to-list 'load-path (concat slime-extensions-path "/utils/ac"))
;; (require 'auto-complete-config)

;; PART 2:
;; Add this line immediately AFTER the custom-set-variables section
;; of your .emacs file:
;; ----
;; (require 'slime-extensions)

;;; Code:

(require 'slime-extensions-config)

(when se-use-se-slime-defs
  (add-to-list 'load-path se-slime-path)
  (require 'slime)
  (slime-setup '(slime-repl slime-autodoc slime-fancy-inspector)))

(when se-use-se-lisp-defs
  (setq inferior-lisp-program se-inferior-lisp-path)
  (command-execute 'slime))

;; Create babel-mode, derived from lisp-mode.
;; Utils running on lisp-mode will also run on babel-mode
;; (which is considered as a lisp dialect).
;; Put the following text on the first line of a file in order
;; to enable the Babel major-mode: ;;; -*-babel-*-"
(define-derived-mode babel-mode lisp-mode
  "Derived major mode for editing lisp in a slime-based environment, optimized for the Babel framework. \\{babel-mode-map}"
  ;(setq font-lock-keywords '("\\<foo\\>" print "print"))
  (setq mode-name "Babel")
)

;; Activate yasnippet for automatic snippet insertion...
;; Yasnippet can also serve as a source for finding
;; auto-completion candidates.
;; http://code.google.com/p/yasnippet/
(when se-use-yasnippet
  (add-to-list 'load-path (concat se-base-path "/utils/yas"))
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory (concat se-base-path "/utils/yas/snippets")))

;; Activate after installing auto-complete from:
;; http://cx4a.org/software/auto-complete/manual.html
(add-to-list 'load-path (concat se-base-path "/utils/ac"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat se-base-path "/utils/ac/ac-dict"))
(add-to-list 'load-path (concat se-base-path "/utils/ac/popup"))
(add-to-list 'load-path (concat se-base-path "/utils/ac/ac-dict"))

;(ac-config-default)

(defun se-slime-ac-candidates ()
  "Complete symbol at the Point of candidates."
  (if (memq major-mode se-slime-ac-modes)
      (let* ((end (point))
	     (beg (slime-symbol-start-pos))
	     (prefix (buffer-substring-no-properties beg end))
	     (result (slime-simple-completions prefix)))
	(destructuring-bind (completions partial) result completions))))

(defun se-slime-ac-documentation (symbol)
  "Lookup the symbol documentation via swank."
  (ignore-errors
    (message "%s" (slime-arglist (string-to-multibyte symbol)))
    (slime-eval
     `(swank:documentation-symbol ,(string-to-multibyte symbol)))))

(defun se-slime-ac-action ()
  (let* ((end (point))
         (beg (slime-symbol-start-pos))
         (symb (buffer-substring-no-properties beg end)))
         (message "%s" (slime-arglist symb))))

(ac-define-source slime
  '((candidates . se-slime-ac-candidates)
    ;(action . se-slime-ac-action)
    (document . se-slime-ac-documentation)
    (candidate-face . se-slime-ac-candidate-face)
    (selection-face . se-slime-ac-selection-face)
    (requires . 2)
    (symbol . "l")))

(defun ac-babel-mode-setup ()
"Adds sources for the completion-lookup in files that are in babel-mode"
   (when ac-sources
     (setq ac-sources se-slime-ac-sources)))
  ;(setq ac-sources (append se-slime-ac-sources ac-sources)))

(unless se-use-custom-ac-config
  (ac-config-default))

(when se-use-custom-ac-config
  (setq-default ac-sources se-global-ac-sources)
  (when se-use-standard-ac-modes
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup))
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (if se-use-sources-in-lisp-modes
      (add-hook 'lisp-mode-hook 'ac-babel-mode-setup)
      (add-hook 'babel-mode-hook 'ac-babel-mode-setup))
  (global-auto-complete-mode t))

(when ac-modes
  (setf ac-modes (append se-add-to-ac-modes ac-modes)))

;;(partial-completion-mode)

(setq abbrev-file-name (concat se-base-path "/abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(set-default 'abbrev-mode t)

(add-to-list 'load-path (concat se-base-path "/utils"))


(require 'cparen)
(defun cparen-activate-babel ()
  (interactive)
;  (mapcar (lambda (mode)
;            (font-lock-add-keywords mode cparen-font-lock-keywords))
;          '(scheme-mode inferior-scheme-mode))
    (mapcar (lambda (mode)
            (font-lock-add-keywords mode cparen-mini-font-lock-keywords))
          '(emacs-lisp-mode lisp-interaction-mode
                            inferior-lisp-mode lisp-mode babel-mode)))

(when se-cparen-lisp-modes
  (cparen-activate-babel))

(when se-global-line-numbers
  (require 'linum)
  (global-linum-mode))

(provide 'slime-extensions)
;;; babel-mode.el ends here