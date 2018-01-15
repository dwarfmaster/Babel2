;;; babel-mode-config.el --- 

;; Keywords: convenience, babel2, programming, auto-complete

;;; Commentary:

;; This file provides the default settings and the customization options
;; for the slime-extensions.

;;; Code:

;; create babel-mode group of settings
;; allows visual settings interface
;; (e.g. via M-x [ENTER] customize-group [ENTER] "babel-mode"

(defgroup slime-extensions nil
  "slime-extensions configuration. Please see `auto-complete', `yasnippet' and `linum' for more options. Put the following text on the first line of a file in order to enable the Babel major-mode: ;;; -*-babel-*-"
  :group 'programming
  :prefix "se-")

(defcustom se-base-path "~/Babel2/sharing/slime-extensions"
  "The path to your slime-extensions installation. Restart emacs after update!"
  :type 'string
  :group 'slime-extensions)

(defcustom se-use-se-slime-defs nil
  "If true, slime-extensions configures the slime-definitions for you. Otherwise you have to fix your own (e.g. in your .emacs file) and load them before loading slime-extensions. Restart emacs after update!"
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-slime-path "~/slime"
  "The path to your slime installation. Restart emacs after update!"
  :type 'string
  :group 'slime-extensions)

(defcustom se-use-se-lisp-defs nil
  "If true, slime-extensions configures the inferior-lisp-definitions for you. Otherwise you have to fix your own (e.g. in your .emacs file) and load them before loading slime-extensions. Restart emacs after update!"
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-inferior-lisp-path "~/ccl/dx86cl"
  "The path to your inferior lisp. Use dx86cl for OS X, dx86cl64 for OS X 64-Bit, wx86cl / wx86cl64 for windows, or lx86cl / lx86cl64 for linux systems. Restart slime after update!"
  :type 'string
  :group 'slime-extensions)

(defcustom se-use-custom-ac-config t
  "Determine whether to use the default ac-configurations, or the custom ones (following 6 settings) ... defaults to t, so that the custom settings from slime-extensions apply."
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-global-ac-sources '(ac-source-abbrev)
  "Specify a list of ac-sources that are used for all major modes. Defaults to nil in this customization. Safe sources (recommended for programmers) are: ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-words-in-all-buffer"
  :type '(repeat symbol)
  :group 'slime-extensions)

(defcustom se-use-standard-ac-modes nil
  "Define whether to use the standard ac-modes. Normally these are emacs-mode, c-mode, ruby-mode and css-mode... if this is nil, you can still enable specific modes by adding code of the following scheme to your .emacs file: (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)"
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-use-sources-in-lisp-modes t
  "Define whether to use the ac-sources from the se-slime-ac-sources (below) in all lisp-modes (t), or only in babel-mode (nil)."
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-add-to-ac-modes
  '(lisp-mode babel-mode lisp-interaction-mode inferior-lisp-mode)
  "Enables auto-complete for these additional major modes... The entries are added to the original list of `ac-modes', which can be edited separately in the auto-complete customization group!"
  :type '(repeat symbol)
  :group 'slime-extensions)

(defcustom se-slime-ac-modes
  '(lisp-mode babel-mode lisp-interaction-mode inferior-lisp-mode)
  "Major modes `se-slime-ac' (slime-fed auto-complete) can run on. Requires restart!"
  :type '(repeat symbol)
  :group 'slime-extensions)

(defface se-slime-ac-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for slime-ac candidates."
  :group 'slime-extensions)

(defface se-slime-ac-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the slime-ac selected candidate."
  :group 'slime-extensions)

(defcustom se-slime-ac-sources
  '(ac-source-slime)
  "Auto-complete sources for `se-slime-ac'. For novices this is limited by default to use only sources from the currently evaluated symbols. Programmers should consider adding any of the following: ac-source-dictionary ac-source-files-in-current-dir ac-source-features ac-source-functions ac-source-variables ac-source-yasnippet ac-source-symbols ac-source-slime ... The order plays a role here. Later results replace earlier ones. Ac-source-slime is added last because it delivers the nicest documentation."
  :type '(repeat symbol)
  :group 'slime-extensions)

(defcustom se-use-yasnippet nil
  "Define whether to use yasnippet for automatic snippet insertion on auto-complete. This is needed if you want to use ac-source-yasnippet."
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-global-line-numbers t
  "Turn the global line-numbering on, or off. Requires restart!"
  :type 'boolean
  :group 'slime-extensions)

(defcustom se-cparen-lisp-modes t
  "Turn the parenthesis-highlighting for lisp-modes on, or off. Requires restart!"
  :type 'boolean
  :group 'slime-extensions)

(provide 'slime-extensions-config)


