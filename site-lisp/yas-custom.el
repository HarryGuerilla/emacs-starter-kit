;;; yasnippet-bundle.el --- Yet another snippet extension (Auto compiled bundle)
;;; Yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;;           2009 pluskid, joaotavora

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.6.1
;; Package-version: 0.6.1c
;; X-URL: http://code.google.com/p/yasnippet/
;; Keywords: convenience, emulation
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;
;;   1. In your .emacs file:
;;        (add-to-list 'load-path "/dir/to/yasnippet.el")
;;        (require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;        (setq yas/root-directory "~/.emacs/snippets")
;;        (yas/load-directory yas/root-directory)
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
;;
;;   Interesting variables are:
;;
;;       `yas/root-directory'
;;
;;           The directory where user-created snippets are to be
;;           stored. Can also be a list of directories that
;;           `yas/reload-all' will use for bulk-reloading snippets. In
;;           that case the first directory the default for storing new
;;           snippets.
;;
;;       `yas/mode-symbol'
;;
;;           A local variable that you can set in a hook to override
;;           snippet-lookup based on major mode. It is a a symbol (or
;;           list of symbols) that correspond to subdirectories of
;;           `yas/root-directory' and is used for deciding which
;;           snippets to consider for the active buffer.
;;
;;   Major commands are:
;;
;;       M-x yas/expand
;;
;;           Try to expand snippets before point.  In `yas/minor-mode',
;;           this is bound to `yas/trigger-key' which you can customize.
;;
;;       M-x yas/load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas/insert-snippet
;;
;;           Prompts you for possible snippet expansion if that is
;;           possible according to buffer-local and snippet-local
;;           expansion conditions.  With prefix argument, ignore these
;;           conditions.
;;
;;       M-x yas/find-snippets
;;
;;           Lets you find the snippet files in the correct
;;           subdirectory of `yas/root-directory', according to the
;;           active major mode (if it exists) like
;;           `find-file-other-window'.
;;
;;       M-x yas/visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas/insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas/new-snippet
;;
;;           Lets you create a new snippet file in the correct
;;           subdirectory of `yas/root-directory', according to the
;;           active major mode.
;;
;;       M-x yas/load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas/tryout-snippet
;;
;;           When editing a snippet, this opens a new empty buffer,
;;           sets it to the appropriate major mode and inserts the
;;           snippet there, so you can see what it looks like.  This is
;;           bound to "C-c C-t" while in `snippet-mode'.
;;
;;   The `dropdown-list.el' extension is bundled with YASnippet, you
;;   can optionally use it the preferred "prompting method", puting in
;;   your .emacs file, for example:
;;
;;       (require 'dropdown-list)
;;       (setq yas/prompt-functions '(yas/dropdown-prompt
;;                                    yas/ido-prompt
;;                                    yas/completing-prompt))
;;
;;   Also check out the customization group
;;
;;        M-x customize-group RET yasnippet RET
;;
;;   If you use the customization group to set variables
;;   `yas/root-directory' or `yas/global-mode', make sure the path to
;;   "yasnippet.el" is present in the `load-path' *before* the
;;   `custom-set-variables' is executed in your .emacs file.
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(require 'cl)
(require 'assoc)
(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables


(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

;;;###autoload
(defcustom yas/root-directory nil
  "Root directory that stores the snippets for each major mode.

If you set this from your .emacs, can also be a list of strings,
for multiple root directories. If you make this a list, the first
element is always the user-created snippets directory. Other
directories are used for bulk reloading of all snippets using
`yas/reload-all'"
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'yasnippet
  :require 'yasnippet
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             (unless (or (not (fboundp 'yas/reload-all))
                         (equal old new))
               (yas/reload-all)))))

(defcustom yas/prompt-functions '(yas/x-prompt
                                  yas/dropdown-prompt
                                  yas/completing-prompt
                                  yas/ido-prompt
                                  yas/no-prompt)
  "Functions to prompt for keys, templates, etc interactively.

These functions are called with the following arguments:

- PROMPT: A string to prompt the user

- CHOICES: a list of strings or objects.

- optional DISPLAY-FN : A function that, when applied to each of
the objects in CHOICES will return a string.

The return value of any function you put here should be one of
the objects in CHOICES, properly formatted with DISPLAY-FN (if
that is passed).

- To signal that your particular style of prompting is
unavailable at the moment, you can also have the function return
nil.

- To signal that the user quit the prompting process, you can
signal `quit' with

  (signal 'quit \"user quit!\")."
  :type '(repeat function)
  :group 'yasnippet)

(defcustom yas/indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

- `fixed' Indent the snippet to the current column;

- `auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indendation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto))
  :group 'yasnippet)

(defcustom yas/also-auto-indent-first-line nil
  "Non-nil means also auto indent first line according to mode.

Naturally this is only valid when `yas/indent-line' is `auto'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/snippet-revival t
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key bound to `yas/expand' when function `yas/minor-mode' is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet
  :set #'(lambda (symbol key)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol key)
             ;; On very first loading of this defcustom,
             ;; `yas/trigger-key' is *not* loaded.
             (if (fboundp 'yas/trigger-key-reload)
                 (yas/trigger-key-reload old)))))
  
(defcustom yas/next-field-key '("TAB" "<tab>")
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))
           

(defcustom yas/prev-field-key '("<backtab>" "<S-tab>")
  "The key to navigate to previous field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/skip-and-clear-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/triggers-in-field nil
  "If non-nil, `yas/next-field-key' can trigger stacked expansions.

Otherwise, `yas/next-field-key' just tries to move on to the next
field"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/fallback-behavior 'call-other-command
  "How to act when `yas/trigger-key' does *not* expand a snippet.

- `call-other-command' means try to temporarily disable YASnippet
    and call the next command bound to `yas/trigger-key'.

- nil or the symbol `return-nil' mean do nothing. (and
  `yas/expand-returns' nil)

- A lisp form (apply COMMAND . ARGS) means interactively call
  COMMAND, if ARGS is non-nil, call COMMAND non-interactively
  with ARGS as arguments."
  :type '(choice (const :tag "Call previous command"  call-other-command)
                 (const :tag "Do nothing"             return-nil))
  :group 'yasnippet)
(make-variable-buffer-local 'yas/fallback-behavior)

(defcustom yas/choose-keys-first nil
  "If non-nil, prompt for snippet key first, then for template.

Otherwise prompts for all possible snippet names.

This affects `yas/insert-snippet' and `yas/visit-snippet-file'."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/choose-tables-first nil
  "If non-nil, and multiple eligible snippet tables, prompts user for tables first.

Otherwise, user chooses between the merging together of all
eligible tables.

This affects `yas/insert-snippet', `yas/visit-snippet-file'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu 'real-modes
  "Display a YASnippet menu in the menu bar.

When non-nil, submenus for each snippet table will be listed
under the menu \"Yasnippet\".

- If set to `real-modes' only submenus whose name more or less
corresponds to a major mode are listed.

- If set to `abbreviate', only the current major-mode
menu and the modes set in `yas/mode-symbol' are listed.

Any other non-nil value, every submenu is listed."
  :type '(choice (const :tag "Full"  t)
                 (const :tag "Real modes only" real-modes)
                 (const :tag "Abbreviate" abbreviate))
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/wrap-around-region nil
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker.  This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace t
  "If non-nil, don't raise errors in inline elisp evaluation.

An error string \"[yas] error\" is returned instead."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/ignore-filenames-as-triggers nil
  "If non-nil, don't derive tab triggers from filenames.

This means a snippet without a \"# key:'\ directive wont have a
tab trigger."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/visit-from-menu nil
  "If non-nil visit snippets's files from menu, instead of expanding them.

This cafn only work when snippets are loaded from files."
  :type 'boolean
  :group 'yasnippet)

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the currently active field of a snippet"
  :group 'yasnippet)

(defface yas/field-debug-face
  '()
  "The face used for debugging some overlays normally hidden"
  :group 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User can also customize the next defvars
(defun yas/define-some-keys (keys keymap definition)
  "Bind KEYS to DEFINITION in KEYMAP, read with `read-kbd-macro'."
  (let ((keys (or (and (listp keys) keys)
                  (list keys))))
    (dolist (key keys)
      (define-key keymap (read-kbd-macro key) definition))))

(defvar yas/keymap
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (yas/define-some-keys (car binding) map (cdr binding)))
          `((,yas/next-field-key     . yas/next-field-or-maybe-expand)
            (,yas/prev-field-key     . yas/prev-field)
            ("C-g"                   . yas/abort-snippet)
            (,yas/skip-and-clear-key . yas/skip-and-clear-or-delete-char)))
    map)
  "The keymap active while a snippet expansion is in progress.")

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.

The hooks will be run in an environment where some variables bound to
proper values:

`yas/snippet-beg' : The beginning of the region of the snippet.

`yas/snippet-end' : Similar to beg.

Attention: These hooks are not run when exiting nested/stackd snippet expansion!")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run just before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal 'font-lock-comment-face
                       (get-char-property (1- (point))
                                          'face))
                (equal 'font-lock-string-face
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Snippet expanding condition.

This variable is a lisp form:

    * If it evaluates to nil, no snippets can be expanded.

    * If it evaluates to the a cons (require-snippet-condition
      . REQUIREMENT)

       * Snippets bearing no \"# condition:\" directive are not
         considered

       * Snippets bearing conditions that evaluate to nil (or
         produce an error) won't be onsidered.

       * If the snippet has a condition that evaluates to non-nil
         RESULT:

          * If REQUIREMENT is t, the snippet is considered

          * If REQUIREMENT is `eq' RESULT, the snippet is
            considered

          * Otherwise, the snippet is not considered.

    * If it evaluates to the symbol 'always, all snippets are
      considered for expansion, regardless of any conditions.

    * If it evaluates to t or some other non-nil value

       * Snippet bearing no conditions, or conditions that
         evaluate to non-nil, are considered for expansion. 

       * Otherwise, the snippet is not considered.

Here's an example preventing snippets from being expanded from
inside comments, in `python-mode' only, with the exception of
snippets returning the symbol 'force-in-comment in their
conditions.

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))

The default value is similar, it filters out potential snippet
expansions inside comments and string literals, unless the
snippet itself contains a condition that returns the symbol
`force-in-comment'.")
(make-variable-buffer-local 'yas/buffer-local-condition)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables

(defvar yas/version "0.6.1b")

(defvar yas/menu-table (make-hash-table)
  "A hash table of MAJOR-MODE symbols to menu keymaps.")

(defvar yas/active-keybindings nil
  "A list of cons (KEYMAP . KEY) setup from defining snippets.")

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} )
  "List of characters which *might* need to be escaped.")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`lisp-expression`\" expression." )

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([ \t\n]*([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode stuff

;; XXX: `last-buffer-undo-list' is somehow needed in Carbon Emacs for MacOSX
(defvar last-buffer-undo-list nil)

(defvar yas/minor-mode-menu nil
  "Holds the YASnippet menu")

(defun yas/init-minor-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define yas/minor-mode-menu
      map
      "Menu used when YAS/minor-mode is active."
      '("YASnippet"
        "----"
        ["Expand trigger" yas/expand
         :help "Possibly expand tab trigger before point"]
        ["Insert at point..." yas/insert-snippet
         :help "Prompt for an expandable snippet and expand it at point"]
        ["New snippet..." yas/new-snippet
         :help "Create a new snippet in an appropriate directory"]
        ["Visit snippet file..." yas/visit-snippet-file
         :help "Prompt for an expandable snippet and find its file"]
        ["Find snippets..." yas/find-snippets
         :help "Invoke `find-file' in the appropriate snippet directory"] 
        "----"
        ("Snippet menu behaviour"
         ["Visit snippets" (setq yas/visit-from-menu t)
          :help "Visit snippets from the menu"
          :active t :style radio   :selected yas/visit-from-menu]
         ["Expand snippets" (setq yas/visit-from-menu nil)
          :help "Expand snippets from the menu"
          :active t :style radio :selected (not yas/visit-from-menu)]
         "----"
         ["Show \"Real\" modes only" (setq yas/use-menu 'real-modes)
          :help "Show snippet submenus for modes that appear to be real major modes"
          :active t :style radio   :selected (eq yas/use-menu 'real-modes)]
         ["Show all modes" (setq yas/use-menu 't)
          :help "Show one snippet submenu for each loaded table"
          :active t :style radio   :selected (eq yas/use-menu 't)]
         ["Abbreviate according to current mode" (setq yas/use-menu 'abbreviate)
          :help "Show only snippet submenus for the current active modes"
          :active t :style radio   :selected (eq yas/use-menu 'abbreviate)])
        ("Indenting"
         ["Auto" (setq yas/indent-line 'auto)
          :help "Indent each line of the snippet with `indent-according-to-mode'"
          :active t :style radio   :selected (eq yas/indent-line 'auto)]
         ["Fixed" (setq yas/indent-line 'fixed)
          :help "Indent the snippet to the current column"
          :active t :style radio   :selected (eq yas/indent-line 'fixed)]
         ["None" (setq yas/indent-line 'none)
          :help "Don't apply any particular snippet indentation after expansion"
          :active t :style radio   :selected (not (member yas/indent-line '(fixed auto)))]
         "----"
         ["Also auto indent first line" (setq yas/also-auto-indent-first-line
                                              (not yas/also-auto-indent-first-line))
          :help "When auto-indenting also, auto indent the first line menu"
          :active (eq yas/indent-line 'auto)
          :style toggle :selected yas/also-auto-indent-first-line]
         )
        ("Prompting method"
         ["System X-widget" (setq yas/prompt-functions
                                  (cons 'yas/x-prompt
                                        (remove 'yas/x-prompt
                                                yas/prompt-functions)))
          :help "Use your windowing system's (gtk, mac, windows, etc...) default menu"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/x-prompt)]
         ["Dropdown-list" (setq yas/prompt-functions
                                (cons 'yas/dropdown-prompt
                                      (remove 'yas/dropdown-prompt
                                              yas/prompt-functions)))
          :help "Use a special dropdown list"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/dropdown-prompt)]
         ["Ido" (setq yas/prompt-functions
                      (cons 'yas/ido-prompt
                            (remove 'yas/ido-prompt
                                    yas/prompt-functions)))
          :help "Use an ido-style minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/ido-prompt)]
         ["Completing read" (setq yas/prompt-functions
                                  (cons 'yas/completing-prompt
                                        (remove 'yas/completing-prompt-prompt
                                                yas/prompt-functions)))
          :help "Use a normal minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/completing-prompt-prompt)]
         )
        ("Misc"
         ["Wrap region in exit marker" 
          (setq yas/wrap-around-region
                (not yas/wrap-around-region))
          :help "If non-nil automatically wrap the selected text in the $0 snippet exit"
          :style toggle :selected yas/wrap-around-region]
         ["Allow stacked expansions " 
          (setq yas/triggers-in-field
                (not yas/triggers-in-field))
          :help "If non-nil allow snippets to be triggered inside other snippet fields"
          :style toggle :selected yas/triggers-in-field]
         ["Revive snippets on undo " 
          (setq yas/snippet-revival
                (not yas/snippet-revival))
          :help "If non-nil allow snippets to become active again after undo"
          :style toggle :selected yas/snippet-revival]
         ["Good grace " 
          (setq yas/good-grace
                (not yas/good-grace))
          :help "If non-nil don't raise errors in bad embedded eslip in snippets"
          :style toggle :selected yas/good-grace]
         ["Ignore filenames as triggers" 
          (setq yas/ignore-filenames-as-triggers
                (not yas/ignore-filenames-as-triggers))
          :help "If non-nil don't derive tab triggers from filenames"
          :style toggle :selected yas/ignore-filenames-as-triggers]
         )
        "----"
        ["Load snippets..."  yas/load-directory
         :help "Load snippets from a specific directory"]
        ["Reload everything" yas/reload-all
         :help "Cleanup stuff, reload snippets, rebuild menus"]
        ["About"            yas/about
         :help "Display some information about YASsnippet"]))
    ;; Now for the stuff that has direct keybindings
    ;;
    (define-key map "\C-c&\C-s" 'yas/insert-snippet)
    (define-key map "\C-c&\C-n" 'yas/new-snippet)
    (define-key map "\C-c&\C-v" 'yas/visit-snippet-file)
    (define-key map "\C-c&\C-f" 'yas/find-snippets)
    map))

(defvar yas/minor-mode-map (yas/init-minor-keymap)
  "The keymap used when `yas/minor-mode' is active.")

(defun yas/trigger-key-reload (&optional unbind-key)
  "Rebind `yas/expand' to the new value of `yas/trigger-key'.

With optional UNBIND-KEY, try to unbind that key from
`yas/minor-mode-map'."
  (when (and unbind-key
             (stringp unbind-key)
             (not (string= unbind-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro unbind-key) nil)) 
  (when  (and yas/trigger-key
              (stringp yas/trigger-key)
              (not (string= yas/trigger-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro yas/trigger-key) 'yas/expand)))

;;;###autoload
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}"
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet
  (when yas/minor-mode
    (yas/trigger-key-reload)
    ;; load all snippets definitions unless we still don't have a
    ;; root-directory or some snippets have already been loaded.
    (unless (or (null yas/root-directory)
                (> (hash-table-count yas/snippet-tables) 0))
      (yas/reload-all))))

(defvar yas/dont-activate #'(lambda ()
                              (and yas/root-directory
                                   (null (yas/get-snippet-tables))))
  "If non-nil don't let `yas/minor-mode-on' active yas for this buffer.

`yas/minor-mode-on' is usually called by `yas/global-mode' so
this effectively lets you define exceptions to the \"global\"
behaviour.")
(make-variable-buffer-local 'yas/dont-activate)


(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode.

Do this unless `yas/dont-activate' is t or the function
`yas/get-snippet-tables' (which see), returns an empty list."
  (interactive)
  (unless (or (and (functionp yas/dont-activate)
                   (funcall yas/dont-activate))
              (and (not (functionp yas/dont-activate))
                   yas/dont-activate))
    (yas/minor-mode 1)))

(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))

(define-globalized-minor-mode yas/global-mode yas/minor-mode yas/minor-mode-on
  :group 'yasnippet
  :require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode stuff
;;
(defvar yas/font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
          lisp-font-lock-keywords
          lisp-font-lock-keywords-1
          lisp-font-lock-keywords-2
          '(("$\\([0-9]+\\)"
             (0 font-lock-keyword-face)
             (1 font-lock-string-face t))
            ("${\\([0-9]+\\):?"
             (0 font-lock-keyword-face)
             (1 font-lock-warning-face t))
            ("${" font-lock-keyword-face)
            ("$[0-9]+?" font-lock-preprocessor-face)
            ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
            ("}"
             (0 font-lock-keyword-face)))))

(defun yas/init-major-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil
      map
      "Menu used when snippet-mode is active."
      (cons "Snippet"
            (mapcar #'(lambda (ent)
                        (when (third ent)
                          (define-key map (third ent) (second ent)))
                        (vector (first ent) (second ent) t))
                    (list
                     (list "Load this snippet" 'yas/load-snippet-buffer "\C-c\C-c")
                     (list "Try out this snippet" 'yas/tryout-snippet "\C-c\C-t")))))
    map))

(defvar snippet-mode-map
  (yas/init-major-keymap)
  "The keymap used when `snippet-mode' is active")


(define-derived-mode snippet-mode text-mode "Snippet"
  "A mode for editing yasnippets"
  (set-syntax-table (standard-syntax-table))
  (setq font-lock-defaults '(yas/font-lock-keywords))
  (set (make-local-variable 'require-final-newline) nil)
  (use-local-map snippet-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal structs for template management

(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition expand-env file keybinding)))
  "A template for a snippet."
  content
  name
  condition
  expand-env
  file
  keybinding)

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of MAJOR-MODE symbols to `yas/snippet-table' objects.")

(defstruct (yas/snippet-table (:constructor yas/make-snippet-table (name)))
  "A table to store snippets for a particular mode.

Has the following fields:

`yas/snippet-table-name'

  A symbol normally corresponding to a major mode, but can also be
  a pseudo major-mode to be referenced in `yas/mode-symbol', for
  example.

`yas/snippet-table-hash'

  A hash table the key is a string (the snippet key) and the
  value is yet another hash of (NAME TEMPLATE), where NAME is the
  snippet name and TEMPLATE is a `yas/template' object name.

`yas/snippet-table-parents'

  A list of tables considered parents of this table: i.e. when
  searching for expansions they are searched as well."
  name
  (hash (make-hash-table :test 'equal))
  (parents nil))

(defvar yas/better-guess-for-replacements nil
  "If non-nil `yas/store' better guess snippet replacements.")

(defun yas/store (table name key template)
  "Store a snippet template in the TABLE."

  ;; This is dones by searching twice:
  ;;
  ;; * Try to get the existing namehash from TABLE using key.
  ;;
  ;; * Try to get the existing namehash from by searching the *whole*
  ;; snippet table for NAME. This is becuase they user might have
  ;; changed the key and that can no longer be used to locate the
  ;; previous `yas/template-structure'.
  ;;
  ;; * If that returns nothing, oh well...
  ;;
  (dolist (existing-namehash (remove nil (list (gethash key (yas/snippet-table-hash table))
                                               (when yas/better-guess-for-replacements
                                                 (let (a)
                                                   (maphash #'(lambda (key namehash)
                                                                (when (gethash name namehash)
                                                                  (setq a namehash)))
                                                            (yas/snippet-table-hash table))
                                                   a)))))
    (let ((existing-template (gethash name existing-namehash)))
      (when existing-template
        ;; Remove the existing keybinding
        (when (yas/template-keybinding existing-template)
          (define-key
            (symbol-value (first (yas/template-keybinding existing-template)))
            (second (yas/template-keybinding existing-template))
            nil)
          (setq yas/active-keybindings
                (delete (yas/template-keybinding existing-template)
                        yas/active-keybindings)))
        ;; Remove the (name . template) mapping from existing-namehash.
        (remhash name existing-namehash))))
  ;; Now store the new template independent of the previous steps.
  ;;
  (puthash name
           template
           (or (gethash key
                        (yas/snippet-table-hash table))
               (puthash key
                        (make-hash-table :test 'equal)
                        (yas/snippet-table-hash table)))))

(defun yas/fetch (table key)
  "Fetch a snippet binding to KEY from TABLE."
  (let* ((keyhash (yas/snippet-table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas/filter-templates-by-condition
       (let (alist)
         (maphash #'(lambda (k v)
                      (push (cons k v) alist))
                  namehash)
         alist)))))


;; Filtering/condition logic

(defun yas/eval-condition (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas] error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))


(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas/template' structure.

This function implements the rules described in
`yas/buffer-local-condition'.  See that variables documentation."
  (let ((requirement (yas/require-template-specific-condition-p)))
    (if (eq requirement 'always)
        templates
      (remove-if-not #'(lambda (pair)
                         (yas/template-can-expand-p (yas/template-condition (cdr pair)) requirement))
                     templates))))

(defun yas/require-template-specific-condition-p ()
  "Decides if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas/buffer-local-condition)
      'always
    (let ((local-condition (or (and (consp yas/buffer-local-condition)
                                    (yas/eval-condition yas/buffer-local-condition))
                               yas/buffer-local-condition)))
      (when local-condition
        (if (eq local-condition t)
            t
          (and (consp local-condition)
               (eq 'require-snippet-condition (car local-condition))
               (symbolp (cdr local-condition))
               (cdr local-condition)))))))

(defun yas/template-can-expand-p (condition &optional requirement)
  "Evaluates CONDITION and REQUIREMENT and returns a boolean"
  (let* ((requirement (or requirement
                          (yas/require-template-specific-condition-p)))
         (result (or (null condition)
                     (yas/eval-condition
                      (condition-case err
                          (read condition)
                        (error (progn
                                 (message (format "[yas] error reading condition: %s"
                                                  (error-message-string err))))
                               nil))))))
    (cond ((eq requirement t)
           result)
          (t
           (eq requirement result)))))

(defun yas/snippet-table-get-all-parents (table)
  (let ((parents (yas/snippet-table-parents table)))
    (when parents
      (append (copy-list parents)
              (mapcan #'yas/snippet-table-get-all-parents parents)))))

(defun yas/snippet-table-templates (table)
  (when table
    (let ((acc (list)))
      (maphash #'(lambda (key namehash)
                   (maphash #'(lambda (name template)
                                (push (cons name template) acc))
                            namehash))
               (yas/snippet-table-hash table))
      (yas/filter-templates-by-condition acc))))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax
        done
        templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (mapcan #'(lambda (table)
                        (yas/fetch table (buffer-substring-no-properties start end)))
                    (yas/get-snippet-tables)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))


(defun yas/snippet-table-all-keys (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
                   (when (yas/filter-templates-by-condition templates)
                     (push key acc)))
               (yas/snippet-table-hash table))
      acc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/read-and-eval-string (string)
  ;; TODO: This is a possible optimization point, the expression could
  ;; be stored in cons format instead of string,
  "Evaluate STRING and convert the result to string."
  (let ((retval (catch 'yas/exception
                  (condition-case err
                      (save-excursion
                        (save-restriction
                          (save-match-data
                            (widen)
                            (let ((result (eval (read string))))
                              (when result
                                (format "%s" result))))))
                    (error (if yas/good-grace
                               "[yas] elisp error!"
                             (error (format "[yas] elisp error: %s"
                                            (error-message-string err)))))))))
    (when (and (consp retval)
               (eq 'yas/exception (car retval)))
      (error (cdr retval)))
    retval))

(defvar yas/mode-symbol nil
  "If non-nil, lookup snippets using this instead of `major-mode'.")
(make-variable-buffer-local 'yas/mode-symbol)

(defun yas/snippet-table-get-create (mode)
  "Get the snippet table corresponding to MODE.

Optional DIRECTORY gets recorded as the default directory to
search for snippet files if the retrieved/created table didn't
already have such a property."
  (let ((table (gethash mode
                        yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table (symbol-name mode)))
      (puthash mode table yas/snippet-tables))
    table))

(defun yas/get-snippet-tables (&optional mode-symbol dont-search-parents)
  "Get snippet tables for current buffer.

Return a list of 'yas/snippet-table' objects indexed by mode.

The modes are tried in this order: optional MODE-SYMBOL, then
`yas/mode-symbol', then `major-mode' then, unless
DONT-SEARCH-PARENTS is non-nil, the guessed parent mode of either
MODE-SYMBOL or `major-mode'.

Guessing is done by looking up the MODE-SYMBOL's
`derived-mode-parent' property, see also `derived-mode-p'."
  (let ((mode-tables
         (mapcar #'(lambda (mode)
                     (gethash mode yas/snippet-tables))
                 (append (list mode-symbol)
                         (if (listp yas/mode-symbol)
                             yas/mode-symbol
                           (list yas/mode-symbol))
                         (list major-mode
                               (and (not dont-search-parents)
                                    (get (or mode-symbol major-mode)
                                         'derived-mode-parent))))))
        (all-tables))
    (dolist (table (remove nil mode-tables))
      (push table all-tables)
      (nconc all-tables (yas/snippet-table-get-all-parents table)))
    (remove-duplicates all-tables)))

(defun yas/menu-keymap-get-create (mode)
  "Get the menu keymap correspondong to MODE."
  (or (gethash mode yas/menu-table)
      (puthash mode (make-sparse-keymap) yas/menu-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template-related and snippet loading functions

(defun yas/parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP VARS FILE KEYBINDING)

If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition
 * key
 * group
 * expand-env

#name: #include \"...\"
# --
#include \"$1\""
  ;;
  ;;
  (goto-char (point-min))
  (let* ((name (and file
                    (file-name-nondirectory file)))
         (key (unless yas/ignore-filenames-as-triggers
                (and name
                     (file-name-sans-extension name))))
         template
         bound
         condition
         (group (and file
                     (yas/calculate-group file)))
         expand-env
         binding)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (match-string-no-properties 2)))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "expand-env" (match-string-no-properties 1))
                   (setq expand-env (match-string-no-properties 2)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))
                 (when (string= "binding" (match-string-no-properties 1))
                   (setq binding (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group expand-env file binding)))

(defun yas/calculate-group (file)
  "Calculate the group for snippet file path FILE."
  (let* ((dominating-dir (locate-dominating-file file
                                                 ".yas-make-groups"))
         (extra-path (and dominating-dir
                          (replace-regexp-in-string (concat "^"
                                                            (expand-file-name dominating-dir))
                                                    ""
                                                    (expand-file-name file))))
         (extra-dir (and extra-path
                         (file-name-directory extra-path)))
         (group (and extra-dir
                     (replace-regexp-in-string "/"
                                               "."
                                               (directory-file-name extra-dir)))))
    group))

;; (defun yas/glob-files (directory &optional recurse-p append)
;;   "Returns files under DIRECTORY ignoring dirs and hidden files.

;; If RECURSE in non-nil, do that recursively."
;;   (let (ret
;;         (default-directory directory))
;;     (dolist (entry (directory-files "."))
;;       (cond ((or (string-match "^\\."
;;                                (file-name-nondirectory entry))
;;                  (string-match "~$"
;;                                (file-name-nondirectory entry)))
;;              nil)
;;             ((and recurse-p
;;                   (file-directory-p entry))
;;              (setq ret (nconc ret
;;                               (yas/glob-files (expand-file-name entry)
;;                                               recurse-p
;;                                               (if append
;;                                                   (concat append "/" entry)
;;                                                 entry)))))
;;             ((file-directory-p entry)
;;              nil)
;;             (t
;;              (push (if append
;;                        (concat append "/" entry)
;;                      entry) ret))))
;;     ret))

(defun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
                   (string-match "~$"
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  `(lambda () (interactive) (yas/expand-or-visit-from-menu ,template)))

(defun yas/expand-or-visit-from-menu (template)
  (if yas/visit-from-menu
      (yas/visit-snippet-file-1 template)
    (let ((where (if mark-active
                     (cons (region-beginning) (region-end))
                   (cons (point) (point)))))
      (yas/expand-snippet (yas/template-content template)
                          (car where)
                          (cdr where)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popping up for keys and templates
;;
(defun yas/prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas/template'."
  (when templates
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet: ")
                       templates
                       #'yas/template-name))
          yas/prompt-functions)))

(defun yas/prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS."
  (when keys
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet key: ") keys))
          yas/prompt-functions)))

(defun yas/prompt-for-table (tables &optional prompt)
  (when tables
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet table: ")
                       tables
                       #'yas/snippet-table-name))
          yas/prompt-functions)))

(defun yas/x-prompt (prompt choices &optional display-fn)
  (when (and window-system choices)
    (let ((keymap (cons 'keymap
                        (cons
                         prompt
                         (mapcar (lambda (choice)
                                   (list choice
                                         'menu-item
                                         (if display-fn
                                             (funcall display-fn choice)
                                           choice)
                                         t))
                                 choices)))))
      (when (cdr keymap)
        (car (x-popup-menu (if (fboundp 'posn-at-point)
                               (let ((x-y (posn-x-y (posn-at-point (point)))))
                                 (list (list (+ (car x-y) 10)
                                             (+ (cdr x-y) 20))
                                       (selected-window)))
                             t)
                           keymap))))))

(defun yas/ido-prompt (prompt choices &optional display-fn)
  (when (and (featurep 'ido)
             ido-mode)
    (let* ((formatted-choices (or (and display-fn
                                       (mapcar display-fn choices))
                                  choices))
           (chosen (and formatted-choices
                        (ido-completing-read prompt
                                             formatted-choices
                                             nil
                                             'require-match
                                             nil
                                             nil))))
      (when chosen
        (nth (position chosen formatted-choices :test #'string=) choices)))))

(eval-when-compile (require 'dropdown-list nil t))
(defun yas/dropdown-prompt (prompt choices &optional display-fn)
  (when (featurep 'dropdown-list)
    (let* ((formatted-choices (or (and display-fn
                                       (mapcar display-fn choices))
                                  choices))
           (chosen (and formatted-choices
                        (nth (dropdown-list formatted-choices)
                             choices))))
      chosen)))

(defun yas/completing-prompt (prompt choices &optional display-fn)
  (let* ((formatted-choices (or (and display-fn
                                     (mapcar display-fn choices))
                                choices))
         (chosen (and formatted-choices
                      (completing-read prompt
                                       formatted-choices
                                       nil
                                       'require-match
                                       nil
                                       nil))))
    (when chosen
      (nth (position chosen formatted-choices :test #'string=) choices))))

(defun yas/no-prompt (prompt choices &optional display-fn)
  (first choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;;
(defun yas/load-directory-1 (directory &optional parents no-hierarchy-parents making-groups-sym)
  "Recursively load snippet templates from DIRECTORY."
  ;; TODO: Rewrite this horrible, horrible monster I created
  (unless (file-exists-p (concat directory "/" ".yas-skip"))
    (let* ((major-mode-and-parents (unless making-groups-sym
                                     (yas/compute-major-mode-and-parents (concat directory "/dummy")
                                                                         nil
                                                                         no-hierarchy-parents)))
           (yas/ignore-filenames-as-triggers (or yas/ignore-filenames-as-triggers
                                                 (file-exists-p (concat directory "/" ".yas-ignore-filenames-as-triggers"))))
           (mode-sym (and major-mode-and-parents
                          (car major-mode-and-parents)))
           (parents (if making-groups-sym
                        parents
                      (rest major-mode-and-parents)))
           (snippet-defs nil)
           (make-groups-p (or making-groups-sym
                              (file-exists-p (concat directory "/" ".yas-make-groups")))))
      (with-temp-buffer
        (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
          (when (file-readable-p file)
            (insert-file-contents file nil nil nil t)
            (push (yas/parse-template file)
                  snippet-defs))))
      (yas/define-snippets (or mode-sym
                               making-groups-sym)
                           snippet-defs
                           parents)
      (dolist (subdir (yas/subdirs directory))
        (if make-groups-p
            (yas/load-directory-1 subdir parents 't (or mode-sym
                                                        making-groups-sym))
          (yas/load-directory-1 subdir (list mode-sym)))))))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.

Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (unless yas/root-directory
    (setq yas/root-directory directory))
  (dolist (dir (yas/subdirs directory))
    (yas/load-directory-1 dir nil 'no-hierarchy-parents))
  (when (interactive-p)
    (message "done.")))

(defun yas/kill-snippet-keybindings ()
  "Remove the all active snippet keybindings."
  (interactive)
  (dolist (keybinding yas/active-keybindings)
    (define-key (symbol-value (first keybinding)) (second keybinding) nil))
  (setq yas/active-keybindings nil))

(defun yas/reload-all (&optional reset-root-directory)
  "Reload all snippets and rebuild the YASnippet menu. "
  (interactive "P")
  ;; Turn off global modes and minor modes, save their state though
  ;;
  (let ((restore-global-mode (prog1 yas/global-mode
                               (yas/global-mode -1)))
        (restore-minor-mode (prog1 yas/minor-mode
                              (yas/minor-mode -1))))
    ;; Empty all snippet tables and all menu tables
    ;;
    (setq yas/snippet-tables (make-hash-table))
    (setq yas/menu-table (make-hash-table))

    ;; Init the `yas/minor-mode-map', taking care not to break the
    ;; menu....
    ;;
    (setf (cdr yas/minor-mode-map)
          (cdr (yas/init-minor-keymap)))

    ;; Now, clean up the other keymaps we might have cluttered up.
    (yas/kill-snippet-keybindings)

    (when reset-root-directory
      (setq yas/root-directory nil))

    ;; Reload the directories listed in `yas/root-directory' or prompt
    ;; the user to select one.
    ;;
    (if yas/root-directory
        (if (listp yas/root-directory)
            (dolist (directory yas/root-directory)
              (yas/load-directory directory))
          (yas/load-directory yas/root-directory))
      (call-interactively 'yas/load-directory))

    ;; Restore the mode configuration
    ;;
    (when restore-minor-mode
      (yas/minor-mode 1))
    (when restore-global-mode
      (yas/global-mode 1))

    (message "[yas] Reloading everything... Done.")))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet Bundle

(defun yas/initialize ()
  "For backward compatibility, enable `yas/minor-mode' globally"
  (yas/global-mode 1))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code dropdown)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.

YASNIPPET is the yasnippet.el file path.

YASNIPPET-BUNDLE is the output file of the compile result.

SNIPPET-ROOTS is a list of root directories that contains the
snippets definition.

CODE is the code to be placed at the end of the generated file
and that can initialize the YASnippet bundle.

Last optional argument DROPDOWN is the filename of the
dropdown-list.el library.

Here's the default value for all the parameters:

  (yas/compile-bundle \"yasnippet.el\"
                      \"yasnippet-bundle.el\"
                      \"snippets\")
                      \"(yas/initialize-bundle)
                        ### autoload
                        (require 'yasnippet-bundle)`\"
                      \"dropdown-list.el\")
"
  (interactive "ffind the yasnippet.el file: \nFTarget bundle file: \nDSnippet directory to bundle: \nMExtra code? \nfdropdown-library: ")
  
  (let* ((yasnippet (or yasnippet
                        "yasnippet.el"))
         (yasnippet-bundle (or yasnippet-bundle
                               "./yasnippet-bundle.el"))
         (snippet-roots (or snippet-roots
                            "snippets"))
         (dropdown (or dropdown
                       "dropdown-list.el"))
         (code (or (and code
                        (condition-case err (read code) (error nil))
                        code)
                   (concat "(yas/initialize-bundle)"
                           "\n;;;###autoload" ; break through so that won't
                           "(require 'yasnippet-bundle)")))
         (dirs (or (and (listp snippet-roots) snippet-roots)
                   (list snippet-roots)))
         (bundle-buffer nil))
    (with-temp-file yasnippet-bundle
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert "\n")
      (when dropdown
        (insert-file-contents dropdown))
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\"")
      (flet ((yas/define-snippets
              (mode snippets &optional parent-or-parents)
              (insert ";;; snippets for " (symbol-name mode) "\n")
              (let ((literal-snippets (list)))
                (dolist (snippet snippets)
                  (let ((key                    (first   snippet))
                        (template-content       (second  snippet))
                        (name                   (third   snippet))
                        (condition              (fourth  snippet))
                        (group                  (fifth   snippet))
                        (expand-env             (sixth   snippet))
                        ;; Omit the file on purpose
                        (file                   nil) ;; (seventh snippet)) 
                        (binding                (eighth  snippet)))
                    (push `(,key
                            ,template-content
                            ,name
                            ,condition
                            ,group
                            ,expand-env
                            ,file
                            ,binding)
                          literal-snippets)))
                (insert (pp-to-string `(yas/define-snippets ',mode ',literal-snippets ',parent-or-parents)))
                (insert "\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/subdirs dir))
            (yas/load-directory-1 subdir nil 'no-hierarchy-parents))))

      (insert (pp-to-string `(yas/global-mode 1)))
      (insert ")\n\n" code "\n")

      ;; bundle-specific provide and value for yas/dont-activate
      (let ((bundle-feature-name (file-name-nondirectory
                                  (file-name-sans-extension
                                   yasnippet-bundle)))) 
        (insert (pp-to-string `(set-default 'yas/dont-activate
                                            #'(lambda ()
                                                (and (or yas/root-directory
                                                         (featurep ',(make-symbol bundle-feature-name)))
                                                     (null (yas/get-snippet-tables)))))))
        (insert (pp-to-string `(provide ',(make-symbol bundle-feature-name)))))
      
      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n"))))

(defun yas/compile-textmate-bundle ()
  (interactive)
  (yas/compile-bundle "yasnippet.el"
                      "./yasnippet-textmate-bundle.el"
                      "extras/imported/"
                      (concat "(yas/initialize-bundle)"
                              "\n;;;###autoload" ; break through so that won't
                              "(require 'yasnippet-textmate-bundle)")
                      "dropdown-list.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some user level functions
;;;

(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>/joaotavora <joaotavora@gmail.com>")))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define SNIPPETS for MODE.

SNIPPETS is a list of snippet definitions, each taking the
following form:

 (KEY TEMPLATE NAME CONDITION GROUP EXPAND-ENV FILE KEYBINDING)

Within these, only TEMPLATE is actually mandatory.

All the elelements are strings, including CONDITION, EXPAND-ENV
and KEYBINDING which will be `read' and eventually `eval'-ed.

FILE is probably of very little use if you're programatically
defining snippets.

You can use `yas/parse-template' to return such lists based on
the current buffers contents.

Optional PARENT-MODE can be used to specify the parent tables of
MODE. It can be a mode symbol of a list of mode symbols. It does
not need to be a real mode."
  (let ((snippet-table (yas/snippet-table-get-create mode))
        (parent-tables (mapcar #'yas/snippet-table-get-create
                               (if (listp parent-mode)
                                   parent-mode
                                 (list parent-mode))))
        (keymap (if yas/use-menu
                    (yas/menu-keymap-get-create mode)
                  nil)))
    ;; Setup the menu
    ;;
    (when parent-tables
      (setf (yas/snippet-table-parents snippet-table)
            parent-tables)
      (when yas/use-menu
        (let ((parent-menu-syms-and-names
               (if (listp parent-mode)
                   (mapcar #'(lambda (sym)
                               (cons sym (concat "parent mode - " (symbol-name sym))))
                           parent-mode)
                 '((parent-mode . "parent mode")))))
          (mapc #'(lambda (sym-and-name)
                    (define-key keymap
                      (vector (intern (replace-regexp-in-string " " "_" (cdr sym-and-name))))
                      (list 'menu-item (cdr sym-and-name)
                            (yas/menu-keymap-get-create (car sym-and-name)))))
                (reverse parent-menu-syms-and-names)))))
    (when yas/use-menu
      (define-key yas/minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap
                    :visible (yas/show-menu-p ',mode))))
    ;; Iterate the recently parsed snippets definition
    ;;
    (dolist (snippet snippets)
      (let* ((file (seventh snippet))
             (key (or (car snippet)
                      (unless yas/ignore-filenames-as-triggers
                        (and file 
                             (file-name-sans-extension (file-name-nondirectory file))))))
             (name (or (third snippet)
                       (and file
                            (file-name-directory file))))
             (condition (fourth snippet))
             (group (fifth snippet))
             (keybinding (eighth snippet))
             (template nil))
        ;; Read the snippet's "binding :" expression
        ;;
        (condition-case err
            (when keybinding
              (setq keybinding (read (eighth snippet)))
              (let* ((this-mode-map-symbol (intern (concat (symbol-name mode) "-map")))
                     (keys (or (and (consp keybinding)
                                    (read-kbd-macro (cdr keybinding)))
                               (read-kbd-macro keybinding)))
                     (keymap-symbol (or (and (consp keybinding)
                                             (car keybinding))
                                        this-mode-map-symbol)))
                (if (and (boundp keymap-symbol)
                         (keymapp (symbol-value keymap-symbol)))
                    (setq keybinding (list keymap-symbol
                                           keys
                                           name))
                  (error (format "keymap \"%s\" does not (yet?) exist" keymap-symbol)))))
          (error
           (message "[yas] warning: keybinding \"%s\" invalid for snippet \"%s\" since %s."
                    keybinding name (error-message-string err))
           (setf keybinding nil)))

        ;; Create the `yas/template' object and store in the
        ;; appropriate snippet table. This only done if we have found
        ;; a key and a name for the snippet, because that is what
        ;; indexes the snippet tables
        ;;
        (setq template (yas/make-template (second snippet)
                                          (or name key)
                                          condition
                                          (sixth snippet)
                                          (seventh snippet)
                                          keybinding))
        (when (and key
                   name)
          (yas/store snippet-table
                     name
                     key
                     template))
        ;; If we have a keybinding, register it if it does not
        ;; conflict!
        ;;
        (when keybinding
	  (let ((lookup (lookup-key (symbol-value (first keybinding)) (second keybinding))))
	    (if (and lookup
		     (not (numberp lookup)))
		(message "[yas] warning: won't overwrite keybinding \"%s\" for snippet \"%s\" in `%s'"
			 (key-description (second keybinding)) name (first keybinding))
	      (define-key
		(symbol-value (first keybinding))
		(second keybinding)
		`(lambda (&optional yas/prefix)
		   (interactive "P")
		   (when (yas/template-can-expand-p ,(yas/template-condition template))
		     (yas/expand-snippet ,(yas/template-content template)
					 nil
					 nil
					 ,(yas/template-expand-env template)))))
	      (add-to-list 'yas/active-keybindings keybinding))))

        ;; Setup the menu groups, reorganizing from group to group if
        ;; necessary
        ;;
        (when yas/use-menu
          (let ((group-keymap keymap))
            ;; Delete this entry from another group if already exists
            ;; in some other group. An entry is considered as existing
            ;; in another group if its name string-matches.
            ;;
            (yas/delete-from-keymap group-keymap name)

            ;; ... then add this entry to the correct group
            (when (and (not (null group))
                       (not (string= "" group)))
              (dolist (subgroup (mapcar #'make-symbol
                                        (split-string group "\\.")))
                (let ((subgroup-keymap (lookup-key group-keymap
                                                   (vector subgroup))))
                  (when (null subgroup-keymap)
                    (setq subgroup-keymap (make-sparse-keymap))
                    (define-key group-keymap (vector subgroup)
                      `(menu-item ,(symbol-name subgroup)
                                  ,subgroup-keymap)))
                  (setq group-keymap subgroup-keymap))))
            (define-key group-keymap (vector (gensym))
              `(menu-item ,(yas/template-name template)
                          ,(yas/make-menu-binding template)
                          :help ,name
                          :keys ,(when (and key name)
                                   (concat key yas/trigger-symbol))))))))))

(defun yas/show-menu-p (mode)
  (cond ((eq yas/use-menu 'abbreviate)
         (find mode
               (mapcar #'(lambda (table)
                           (intern (yas/snippet-table-name table)))
                       (yas/get-snippet-tables))))
        ((eq yas/use-menu 'real-modes)
         (yas/real-mode? mode))
        (t
         t))) 

(defun yas/delete-from-keymap (keymap name)
  "Recursively delete items name NAME from KEYMAP and its submenus.

Skip any submenus named \"parent mode\""
  ;; First of all, recursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;;
  (mapc #'(lambda (item)
            (when (and (keymapp (fourth item))
                       (stringp (third item))
                       (not (string-match "parent mode" (third item))))
              (yas/delete-from-keymap (fourth item) name)))
        (rest keymap))
  ;;
  (when (keymapp keymap)
    (let ((pos-in-keymap))
      (while (setq pos-in-keymap
                   (position-if #'(lambda (item)
                                    (and (listp item)
                                         (or
                                          ;; the menu item we want to delete
                                          (and (eq 'menu-item (second item))
                                               (third item)
                                               (and (string= (third item) name)))
                                          ;; a stale subgroup
                                          (and (keymapp (fourth item))
                                               (not (and (stringp (third item))
                                                         (string-match "parent mode"
                                                                       (third item))))
                                               (null (rest (fourth item)))))))
                                keymap))
        (setf (nthcdr pos-in-keymap keymap)
              (nthcdr (+ 1 pos-in-keymap) keymap))))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.

NAME is a description to this template.  Also update the menu if
`yas/use-menu' is `t'.  CONDITION is the condition attached to
this snippet.  If you attach a condition to a snippet, then it
will only be expanded when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (undo 1)
    nil))

(defun yas/expand ()
  "Expand a snippet before point.

If no snippet expansion is possible, fall back to the behaviour
defined in `yas/fallback-behavior'"
  (interactive)
  (yas/expand-1))

(defun yas/expand-1 (&optional field)
  "Actually fo the work for `yas/expand'"
  (multiple-value-bind (templates start end) (if field
                                                 (save-restriction
                                                   (narrow-to-region (yas/field-start field) (yas/field-end field))
                                                   (yas/current-key))
                                               (yas/current-key))
    (if templates
        (let ((template (or (and (rest templates) ;; more than one
                                 (yas/prompt-for-template (mapcar #'cdr templates)))
                            (cdar templates))))
          (when template
            (yas/expand-snippet (yas/template-content template)
                                start
                                end
                                (yas/template-expand-env template))))
      (cond ((eq yas/fallback-behavior 'return-nil)
             ;; return nil
             nil)
            ((eq yas/fallback-behavior 'call-other-command)
             (let* ((yas/minor-mode nil)
                    (keys-1 (this-command-keys-vector))
                    (keys-2 (and yas/trigger-key
                                 (stringp yas/trigger-key)
                                 (read-kbd-macro yas/trigger-key))) 
                    (command-1 (and keys-1 (key-binding keys-1)))
                    (command-2 (and keys-2 (key-binding keys-2)))
                    (command (or (and (not (eq command-1 'yas/expand))
                                      command-1)
                                 command-2)))
               (when (and (commandp command)
                          (not (eq 'yas/expand command)))
                 (setq this-command command)
                 (call-interactively command))))
            ((and (listp yas/fallback-behavior)
                  (cdr yas/fallback-behavior)
                  (eq 'apply (car yas/fallback-behavior)))
             (if (cddr yas/fallback-behavior)
                 (apply (cadr yas/fallback-behavior)
                        (cddr yas/fallback-behavior))
               (when (commandp (cadr yas/fallback-behavior))
                 (setq this-command (cadr yas/fallback-behavior))
                 (call-interactively (cadr yas/fallback-behavior)))))
            (t
             ;; also return nil if all the other fallbacks have failed
             nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet development

(defun yas/all-templates (tables)
  "Return all snippet tables applicable for the current buffer.

Honours `yas/choose-tables-first', `yas/choose-keys-first' and
`yas/buffer-local-condition'"
  (when yas/choose-tables-first
    (setq tables (list (yas/prompt-for-table tables))))
  (mapcar #'cdr
          (if yas/choose-keys-first
              (let ((key (yas/prompt-for-keys
                          (mapcan #'yas/snippet-table-all-keys tables))))
                (when key
                  (mapcan #'(lambda (table)
                              (yas/fetch table key))
                          tables)))
            (mapcan #'yas/snippet-table-templates tables))))

(defun yas/insert-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas/prompt-function'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (let* ((yas/buffer-local-condition (or (and no-condition
                                              'always)
                                         yas/buffer-local-condition))
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (template (and templates
                        (or (and (rest templates) ;; more than one template for same key
                                 (yas/prompt-for-template templates))
                            (car templates))))
         (where (if mark-active
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (if template
        (yas/expand-snippet (yas/template-content template)
                            (car where)
                            (cdr where)
                            (yas/template-expand-env template))
      (message "[yas] No snippets can be inserted here!"))))

(defun yas/visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas/insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas/buffer-local-condition 'always)
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (template (and templates
                        (or (and (rest templates) ;; more than one template for same key
                                 (yas/prompt-for-template templates
                                                          "Choose a snippet template to edit: "))
                            (car templates)))))

    (when template
      (yas/visit-snippet-file-1 template))))

(defun yas/visit-snippet-file-1 (template)
  (let ((file (yas/template-file template)))
    (cond ((and file (file-exists-p file))
           (find-file-other-window file)
           (snippet-mode))
          (file
           (message "Original file %s no longer exists!" file))
          (t
           (message "This snippet was not loaded from a file!")))))

(defun yas/guess-snippet-directories-1 (table &optional suffix)
  "Guesses possible snippet subdirsdirectories for TABLE."
  (unless suffix
    (setq suffix (yas/snippet-table-name table))) 
  (cons suffix
        (mapcan #'(lambda (parent)
                    (yas/guess-snippet-directories-1
                     parent
                     (concat (yas/snippet-table-name parent) "/" suffix)))
                (yas/snippet-table-parents table))))

(defun yas/guess-snippet-directories ()
  "Try to guess suitable directories based on the current active
tables.

Returns a a list of options alist TABLE -> DIRS where DIRS are
all the possibly directories where snippets of table might be
lurking."
  (let ((main-dir (or (and (listp yas/root-directory)
                           (first yas/root-directory))
                      yas/root-directory
                      (setq yas/root-directory "~/.emacs.d/snippets")))
        (tables (yas/get-snippet-tables)))
    ;; HACK! the snippet table created here is a dummy table that
    ;; holds the correct name so that `yas/make-directory-maybe' can
    ;; work. The real table, if it does not exist in
    ;; yas/snippet-tables will be created when the first snippet for
    ;; that mode is loaded.
    ;; 
    (unless (gethash major-mode yas/snippet-tables)
      (setq tables (cons (yas/make-snippet-table (symbol-name major-mode))
                         tables)))
    
    (mapcar #'(lambda (table)
                (cons table
                      (mapcar #'(lambda (subdir)
                                  (concat main-dir "/" subdir))
                              (yas/guess-snippet-directories-1 table))))
            tables)))

(defun yas/make-directory-maybe (table-and-dirs &optional main-table-string)
  "Returns a dir inside  TABLE-AND-DIRS, prompts for creation if none exists."
  (or (some #'(lambda (dir) (when (file-directory-p dir) dir)) (cdr table-and-dirs))
      (let ((candidate (first (cdr table-and-dirs))))
        (if (y-or-n-p (format "Guessed directory (%s) for%s%s table \"%s\" does not exist! Create? "
                              candidate
                              (if (gethash (intern (yas/snippet-table-name (car table-and-dirs)))
                                           yas/snippet-tables)
                                  ""
                                " brand new")
                              (or main-table-string
                                  "")
                              (yas/snippet-table-name (car table-and-dirs))))
            (progn
              (make-directory candidate 'also-make-parents)
              ;; create the .yas-parents file here...
              candidate)))))

(defun yas/new-snippet (&optional choose-instead-of-guess)
  ""
  (interactive "P")
  (let* ((guessed-directories (yas/guess-snippet-directories))
         (option (or (and choose-instead-of-guess
                          (some #'(lambda (fn)
                                    (funcall fn "Choose a snippet table: "
                                             guessed-directories
                                             #'(lambda (option)
                                                 (yas/snippet-table-name (car option)))))
                                yas/prompt-functions))
                     (first guessed-directories)))
         (chosen))
    (setq chosen (yas/make-directory-maybe option (unless choose-instead-of-guess
                                                    " main")))
    (unless (or chosen
                choose-instead-of-guess)
      (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                            (mapcar #'(lambda (table-and-dirs)
                                        (yas/snippet-table-name (car table-and-dirs)))
                                    (rest guessed-directories))))
          (setq chosen (some #'yas/make-directory-maybe
                             (rest guessed-directories)))))
    (unless (or chosen
                choose-instead-of-guess)
      (when (y-or-n-p "Having trouble... use snippet root dir? ")
        (setq chosen (if (listp yas/root-directory)
                         (first yas/root-directory)
                       yas/root-directory))))
    (if chosen
        (let ((default-directory chosen)
              (name (read-from-minibuffer "Enter a snippet name: ")))
          (find-file-other-window (concat name
                                          ".yasnippet"))
          (snippet-mode)
          (unless (and choose-instead-of-guess
                       (not (y-or-n-p "Insert a snippet with useful headers? ")))
            (yas/expand-snippet (format 
                                 "\
# -*- mode: snippet -*-
# name: %s
# key: $1${2:
# binding: \"${3:keybinding}\"}${4:
# expand-env: ((${5:some-var} ${6:some-value}))}
# --
$0" name))))
      (message "[yas] aborted snippet creation."))))

(defun yas/find-snippets (&optional same-window )
  "Look for user snippets in guessed current mode's directory.

Calls `find-file' interactively in the guessed directory.

With prefix arg SAME-WINDOW opens the buffer in the same window.

Because snippets can be loaded from many different locations,
this has to guess the correct directory using
`yas/guess-snippet-directories', which returns a list of
options. 

If any one of these exists, it is taken and `find-file' is called
there, otherwise, proposes to create the first option returned by
`yas/guess-snippet-directories'."
  (interactive "P")
  (let* ((guessed-directories (yas/guess-snippet-directories))
         (chosen)
         (buffer))
    (setq chosen (yas/make-directory-maybe (first guessed-directories) " main"))
    (unless chosen
      (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                            (mapcar #'(lambda (table-and-dirs)
                                        (yas/snippet-table-name (car table-and-dirs)))
                                    (rest guessed-directories))))
          (setq chosen (some #'yas/make-directory-maybe
                             (rest guessed-directories)))))
    (unless chosen
      (when (y-or-n-p "Having trouble... go to snippet root dir? ")
        (setq chosen (if (listp yas/root-directory)
                         (first yas/root-directory)
                       yas/root-directory))))
    (if chosen
        (let ((default-directory chosen))
          (setq buffer (call-interactively (if same-window
                                               'find-file
                                             'find-file-other-window)))
          (when buffer
            (save-excursion
              (set-buffer buffer)
              (when (eq major-mode 'fundamental-mode)
                (snippet-mode)))))
      (message "Could not guess snippet dir!"))))

(defun yas/compute-major-mode-and-parents (file &optional prompt-if-failed no-hierarchy-parents)
  (let* ((file-dir (and file
                        (directory-file-name (or (locate-dominating-file file ".yas-make-groups")
                                                 (directory-file-name (file-name-directory file))))))
         (major-mode-name (and file-dir
                               (file-name-nondirectory file-dir)))
         (parent-file-dir (and file-dir
                               (directory-file-name (file-name-directory file-dir))))
         (parent-mode-name (and parent-file-dir
                                (not no-hierarchy-parents)
                                (file-name-nondirectory parent-file-dir)))
         (major-mode-sym (or (and major-mode-name
                                  (intern major-mode-name))
                             (when prompt-if-failed
                               (read-from-minibuffer
                                "[yas] Cannot auto-detect major mode! Enter a major mode: "))))
         (parent-mode-sym (and parent-mode-name
                               (intern parent-mode-name)))
         (extra-parents-file-name (concat file-dir "/.yas-parents"))
         (more-parents (when (file-readable-p extra-parents-file-name)
                         (mapcar #'intern
                                 (split-string
                                  (with-temp-buffer
                                    (insert-file-contents extra-parents-file-name)
                                    (buffer-substring-no-properties (point-min)
                                                                    (point-max))))))))
    (when major-mode-sym
      (remove nil (append (list major-mode-sym parent-mode-sym)
                          more-parents)))))

(defun yas/load-snippet-buffer (&optional kill)
  "Parse and load current buffer's snippet definition.

With optional prefix argument KILL quit the window and buffer."
  (interactive "P")
  (if buffer-file-name
      (let ((major-mode-and-parent (yas/compute-major-mode-and-parents buffer-file-name)))
        (if major-mode-and-parent
            (let* ((parsed (yas/parse-template buffer-file-name))
                   (name (and parsed
                              (third parsed))))
              (when name
                (let ((yas/better-guess-for-replacements t))
                  (yas/define-snippets (car major-mode-and-parent)
                                       (list parsed)
                                       (cdr major-mode-and-parent)))
                (when (and (buffer-modified-p)
                           (y-or-n-p "Save snippet? "))
                  (save-buffer))
                (if kill
                    (quit-window kill)
                  (message "[yas] Snippet \"%s\" loaded for %s."
                           name
                           (car major-mode-and-parent)))))
          (message "[yas] Cannot load snippet for unknown major mode")))
    (message "Save the buffer as a file first!")))

(defun yas/tryout-snippet (&optional debug)
  "Test current buffers's snippet template in other buffer."
  (interactive "P")
  (let* ((major-mode-and-parent (yas/compute-major-mode-and-parents buffer-file-name))
         (parsed (yas/parse-template))
         (test-mode (or (and (car major-mode-and-parent)
                             (fboundp (car major-mode-and-parent))
                             (car major-mode-and-parent))
                        (intern (read-from-minibuffer "[yas] please input a mode: "))))
         (template (and parsed
                        (fboundp test-mode)
                        (yas/make-template (second parsed)
                                           (third parsed)
                                           nil
                                           (sixth parsed)
                                           nil
                                           nil))))
    (cond (template
           (let ((buffer-name (format "*YAS TEST: %s*" (yas/template-name template))))
             (set-buffer (switch-to-buffer buffer-name))
             (erase-buffer)
             (setq buffer-undo-list nil)
             (funcall test-mode)
             (yas/expand-snippet (yas/template-content template)
                                 (point-min)
                                 (point-max)
                                 (yas/template-expand-env template))
             (when debug
               (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local))))
          (t
           (message "[yas] Cannot test snippet for unknown major mode")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User convenience functions, for using in snippet definitions

(defvar yas/modified-p nil
  "Non-nil if field has been modified by user or transformation.")

(defvar yas/moving-away-p nil
  "Non-nil if user is about to exit field.")

(defvar yas/text nil
  "Contains current field text.")

(defun yas/substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas/choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (some #'(lambda (fn)
              (funcall fn "Choose: " possibilities))
          yas/prompt-functions)))

(defun yas/key-to-value (alist)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
        (or (cdr (find key alist :key #'car :test #'string=))
            key)))))

(defun yas/throw (text)
  "Throw a yas/exception with TEXT as the reason."
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away-p (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
    (yas/throw (format "[yas] field only allows %s" possibilities))))

(defun yas/field-value (number)
  (let* ((snippet (car (yas/snippets-at-point)))
         (field (and snippet
                     (yas/snippet-find-field snippet number))))
    (when field
      (yas/field-text-for-display field))))

(defun yas/default-from-field (number)
  (unless yas/modified-p
    (yas/field-value number)))

(defun yas/inside-string ()
  (equal 'font-lock-string-face (get-char-property (1- (point)) 'face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet expansion and field management

(defvar yas/active-field-overlay nil
  "Overlays the currently active field.")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current active field ")

(defconst yas/prefix nil
  "A prefix argument for expansion direct from keybindings")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted on the last snippet expansion.")

(defvar yas/start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas/active-field-overlay)
(make-variable-buffer-local 'yas/field-protection-overlays)
(make-variable-buffer-local 'yas/deleted-text)

(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (fields '())
  (exit nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field
  force-exit)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)

(defstruct (yas/mirror (:constructor yas/make-mirror (start end transform)))
  "A mirror."
  start end
  (transform nil)
  next)

(defstruct (yas/exit (:constructor yas/make-exit (marker)))
  marker
  next)

(defun yas/apply-transform (field-or-mirror field)
  "Calculate the value of the field/mirror. If there's a transform
for this field, apply it. Otherwise, returned nil."
  (let* ((yas/text (yas/field-text-for-display field))
         (text yas/text)
         (yas/modified-p (yas/field-modified-p field))
         (yas/moving-away-p nil)
         (transform (if (yas/mirror-p field-or-mirror)
                        (yas/mirror-transform field-or-mirror)
                      (yas/field-transform field-or-mirror)))
         (start-point (if (yas/mirror-p field-or-mirror)
                          (yas/mirror-start field-or-mirror)
                        (yas/field-start field-or-mirror)))
         (transformed (and transform
                           (save-excursion
                             (goto-char start-point)
                             (yas/read-and-eval-string transform)))))
    transformed))

(defsubst yas/replace-all (from to &optional text)
  "Replace all occurance from FROM to TO.

With optional string TEXT do it in that string."
  (if text
      (replace-regexp-in-string (regexp-quote from) to text t t)
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to t t text))))

(defun yas/snippet-find-field (snippet number)
  (find-if #'(lambda (field)
               (eq number (yas/field-number field)))
           (yas/snippet-fields snippet)))

(defun yas/snippet-sort-fields (snippet)
  "Sort the fields of SNIPPET in navigation order."
  (setf (yas/snippet-fields snippet)
        (sort (yas/snippet-fields snippet)
              '(lambda (field1 field2)
                 (yas/snippet-field-compare field1 field2)))))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the field's start point"
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (< n1 n2)
          t)
      (if n2
          nil
        (< (yas/field-start field1)
           (yas/field-start field2))))))

(defun yas/field-probably-deleted-p (snippet field)
  "Guess if SNIPPET's FIELD should be skipped."
  (and (zerop (- (yas/field-start field) (yas/field-end field)))
       (or (yas/field-parent-field field)
           (and (eq field (car (last (yas/snippet-fields snippet))))
                (= (yas/field-start field) (overlay-end (yas/snippet-control-overlay snippet)))))))

(defun yas/snippets-at-point (&optional all-snippets)
  "Return a sorted list of snippets at point, most recently
inserted first."
  (sort
   (remove nil (remove-duplicates (mapcar #'(lambda (ov)
                                              (overlay-get ov 'yas/snippet))
                                          (if all-snippets
                                              (overlays-in (point-min) (point-max))
                                            (overlays-at (point))))))
   #'(lambda (s1 s2)
       (<= (yas/snippet-id s2) (yas/snippet-id s1)))))

(defun yas/next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point, otherwise
delegate to `yas/next-field'."
  (interactive)
  (if yas/triggers-in-field
      (let ((yas/fallback-behavior 'return-nil)
            (active-field (overlay-get yas/active-field-overlay 'yas/field)))
        (when active-field
          (unless (yas/expand-1 active-field)
            (yas/next-field))))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
         (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'(lambda (field)
                                     (and (not (eq field active-field))
                                          (yas/field-probably-deleted-p snippet field)))
                                 (yas/snippet-fields snippet)))
         (active-field-pos (position active-field live-fields))
         (target-pos (and active-field-pos (+ arg active-field-pos)))
         (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;;
    (when (and active-field
               (yas/field-transform active-field))
      (let* ((yas/moving-away-p t)
             (yas/text (yas/field-text-for-display active-field))
             (text yas/text)
             (yas/modified-p (yas/field-modified-p active-field)))
        ;; primary field transform: exit call to field-transform
        (yas/read-and-eval-string (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD"
  (yas/make-move-field-protection-overlays snippet field)
  (yas/make-move-active-field-overlay snippet field))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (setf (yas/snippet-active-field snippet) field)
  (yas/place-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  ;; primary field transform: first call to snippet transform
  (unless (yas/field-modified-p field)
    (if (yas/field-update-display field snippet)
        (let ((inhibit-modification-hooks t))
          (yas/update-mirrors snippet))
      (setf (yas/field-modified-p field) nil))))

(defun yas/prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field -1))

(defun yas/abort-snippet (&optional snippet)
  (interactive)
  (let ((snippet (or snippet
                     (car (yas/snippets-at-point)))))
    (when snippet
      (setf (yas/snippet-force-exit snippet) t))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET."
  (interactive)
  (setf (yas/snippet-force-exit snippet) t)
  (goto-char (if (yas/snippet-exit snippet)
                 (yas/exit-marker (yas/snippet-exit snippet))
               (overlay-end (yas/snippet-control-overlay snippet)))))

(defun yas/exit-all-snippets ()
  "Exit all snippets."
  (interactive)
  (mapc #'(lambda (snippet)
            (yas/exit-snippet snippet)
            (yas/check-commit-snippet))
        (yas/snippets-at-point)))


;;; Apropos markers-to-points:
;;;
;;; This was found useful for performance reasons, so that an
;;; excessive number of live markers aren't kept around in the
;;; `buffer-undo-list'. However, in `markers-to-points', the
;;; set-to-nil markers can't simply be discarded and replaced with
;;; fresh ones in `points-to-markers'. The original marker that was
;;; just set to nil has to be reused.
;;;
;;; This shouldn't bring horrible problems with undo/redo, but it
;;; you never know
;;;

(defun yas/markers-to-points (snippet)
  "Convert all markers in SNIPPET to a cons (POINT . MARKER)
where POINT is the original position of the marker and MARKER is
the original marker object with the position set to nil."
  (dolist (field (yas/snippet-fields snippet))
    (let ((start (marker-position (yas/field-start field)))
          (end (marker-position (yas/field-end field))))
      (set-marker (yas/field-start field) nil)
      (set-marker (yas/field-end field) nil)
      (setf (yas/field-start field) (cons start (yas/field-start field)))
      (setf (yas/field-end field) (cons end (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (let ((start (marker-position (yas/mirror-start mirror)))
            (end (marker-position (yas/mirror-end mirror))))
        (set-marker (yas/mirror-start mirror) nil)
        (set-marker (yas/mirror-end mirror) nil)
        (setf (yas/mirror-start mirror) (cons start (yas/mirror-start mirror)))
        (setf (yas/mirror-end mirror) (cons end (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (let ((exit (marker-position (yas/exit-marker snippet-exit))))
        (set-marker (yas/exit-marker snippet-exit) nil)
        (setf (yas/exit-marker snippet-exit) (cons exit (yas/exit-marker snippet-exit)))))))

(defun yas/points-to-markers (snippet)
  "Convert all cons (POINT . MARKER) in SNIPPET to markers. This
is done by setting MARKER to POINT with `set-marker'."
  (dolist (field (yas/snippet-fields snippet))
    (setf (yas/field-start field) (set-marker (cdr (yas/field-start field))
                                              (car (yas/field-start field))))
    (setf (yas/field-end field) (set-marker (cdr (yas/field-end field))
                                            (car (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (setf (yas/mirror-start mirror) (set-marker (cdr (yas/mirror-start mirror))
                                                  (car (yas/mirror-start mirror))))
      (setf (yas/mirror-end mirror) (set-marker (cdr (yas/mirror-end mirror))
                                                (car (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (setf (yas/exit-marker snippet-exit) (set-marker (cdr (yas/exit-marker snippet-exit))
                                                       (car (yas/exit-marker snippet-exit)))))))

(defun yas/commit-snippet (snippet &optional no-hooks)
  "Commit SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text.

Return a buffer position where the point should be placed if
exiting the snippet.

NO-HOOKS means don't run the `yas/after-exit-snippet-hook' hooks."

  (let ((control-overlay (yas/snippet-control-overlay snippet))
        yas/snippet-beg
        yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay))

    (let ((inhibit-modification-hooks t))
      (when yas/active-field-overlay
        (delete-overlay yas/active-field-overlay))
      (when yas/field-protection-overlays
        (mapc #'delete-overlay yas/field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas/snippet-end'...
    ;;
    (let ((previous-field (yas/snippet-previous-active-field snippet)))
      (when (and yas/snippet-end previous-field)
        (yas/advance-end-maybe previous-field yas/snippet-end)))

    ;; Convert all markers to points,
    ;;
    (yas/markers-to-points snippet)

    ;; Take care of snippet revival
    ;;
    (if yas/snippet-revival
        (push `(apply yas/snippet-revive ,yas/snippet-beg ,yas/snippet-end ,snippet)
              buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas/take-care-of-redo'....
      (setf (yas/snippet-fields snippet) nil))

    ;; XXX: `yas/after-exit-snippet-hook' should be run with
    ;; `yas/snippet-beg' and `yas/snippet-end' bound. That might not
    ;; be the case if the main overlay had somehow already
    ;; disappeared, which sometimes happens when the snippet's messed
    ;; up...
    ;;
    (unless no-hooks (run-hooks 'yas/after-exit-snippet-hook)))

  (message "[yas] snippet exited."))

(defun yas/check-commit-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up."
  (let* ((snippets (yas/snippets-at-point 'all-snippets))
         (snippets-left snippets))
    (dolist (snippet snippets)
      (let ((active-field (yas/snippet-active-field snippet)))
        (cond ((or (prog1 (yas/snippet-force-exit snippet)
                     (setf (yas/snippet-force-exit snippet) nil))
                   (not (and active-field (yas/field-contains-point-p active-field))))
               (setq snippets-left (delete snippet snippets-left))
               (yas/commit-snippet snippet snippets-left))
              ((and active-field
                    (or (not yas/active-field-overlay)
                        (not (overlay-buffer yas/active-field-overlay))))
               ;;
               ;; stacked expansion: this case is mainly for recent
               ;; snippet exits that place us back int the field of
               ;; another snippet
               ;;
               (save-excursion
                 (yas/move-to-field snippet active-field)
                 (yas/update-mirrors snippet)))
              (t
               nil))))
    (unless snippets-left
      (remove-hook 'post-command-hook 'yas/post-command-handler 'local)
      (remove-hook 'pre-command-hook 'yas/pre-command-handler 'local))))

(defun yas/field-contains-point-p (field &optional point)
  (let ((point (or point
                   (point))))
    (and (>= point (yas/field-start field))
         (<= point (yas/field-end field)))))

(defun yas/field-text-for-display (field)
  "Return the propertized display text for field FIELD.  "
  (buffer-substring (yas/field-start field) (yas/field-end field)))

(defun yas/undo-in-progress ()
  "True if some kind of undo is in progress"
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas/make-control-overlay (snippet start end)
  "Creates the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil
                               t)))
    (overlay-put overlay 'keymap yas/keymap)
    (overlay-put overlay 'yas/snippet snippet)
    overlay))

(defun yas/skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (interactive)
  (let ((field (or field
                   (and yas/active-field-overlay
                        (overlay-buffer yas/active-field-overlay)
                        (overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
                (not (yas/field-modified-p field))
                (eq (point) (marker-position (yas/field-start field))))
           (yas/skip-and-clear field)
           (yas/next-field 1))
          (t
           (call-interactively 'delete-char)))))

(defun yas/skip-and-clear (field)
  "Deletes the region of FIELD and sets it modified state to t"
  (setf (yas/field-modified-p field) t)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas/active-field-overlay
           (overlay-buffer yas/active-field-overlay))
      (move-overlay yas/active-field-overlay
                    (yas/field-start field)
                    (yas/field-end field))
    (setq yas/active-field-overlay
          (make-overlay (yas/field-start field)
                        (yas/field-end field)
                        nil nil t))
    (overlay-put yas/active-field-overlay 'priority 100)
    (overlay-put yas/active-field-overlay 'face 'yas/field-highlight-face)
    (overlay-put yas/active-field-overlay 'yas/snippet snippet)
    (overlay-put yas/active-field-overlay 'modification-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-in-front-hooks
                 '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-behind-hooks
                 '(yas/on-field-overlay-modification))))

(defun yas/on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and it point it
at field start. This hook doesn't do anything if an undo is in
progress."
  (unless (yas/undo-in-progress)
    (let ((field (overlay-get yas/active-field-overlay 'yas/field)))
      (cond (after?
             (yas/advance-end-maybe field (overlay-end overlay))
;;; primary field transform: normal calls to expression
             (let ((saved-point (point)))
               (yas/field-update-display field (car (yas/snippets-at-point)))
               (goto-char saved-point))
             (yas/update-mirrors (car (yas/snippets-at-point))))
            (field
             (when (and (not after?)
                        (not (yas/field-modified-p field))
                        (eq (point) (if (markerp (yas/field-start field))
                                        (marker-position (yas/field-start field))
                                      (yas/field-start field))))
               (yas/skip-and-clear field))
             (setf (yas/field-modified-p field) t))))))

;;; Apropos protection overlays:
;;;
;;; These exist for nasty users who will try to delete parts of the
;;; snippet outside the active field. Actual protection happens in
;;; `yas/on-protection-overlay-modification'.
;;;
;;; Currently this signals an error which inhibits the command. For
;;; commands that move point (like `kill-line'), point is restored in
;;; the `yas/post-command-handler' using a global
;;; `yas/protection-violation' variable.
;;;
;;; Alternatively, I've experimented with an implementation that
;;; commits the snippet before actually calling `this-command'
;;; interactively, and then signals an eror, which is ignored. but
;;; blocks all other million modification hooks. This presented some
;;; problems with stacked expansion.
;;;

(defun yas/make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas/field-start field))
        (end (yas/field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;;
    (when (< (buffer-size) end)
      (save-excursion
        (let ((inhibit-modification-hooks t))
          (goto-char (point-max))
          (newline))))
    ;; go on to normal overlay creation/moving
    ;;
    (cond ((and yas/field-protection-overlays
                (every #'overlay-buffer yas/field-protection-overlays))
           (move-overlay (first yas/field-protection-overlays) (1- start) start)
           (move-overlay (second yas/field-protection-overlays) end (1+ end)))
          (t
           (setq yas/field-protection-overlays
                 (list (make-overlay (1- start) start nil t nil)
                       (make-overlay end (1+ end) nil t nil)))
           (dolist (ov yas/field-protection-overlays)
             (overlay-put ov 'face 'yas/field-debug-face)
             (overlay-put ov 'yas/snippet snippet)
             ;; (overlay-put ov 'evaporate t)
             (overlay-put ov 'modification-hooks '(yas/on-protection-overlay-modification)))))))

(defvar yas/protection-violation nil
  "When non-nil, signals attempts to erronesly exit or modify the snippet.

Functions in the `post-command-hook', for example
`yas/post-command-handler' can check it and reset its value to
nil. The variables value is the point where the violation
originated")

(defun yas/on-protection-overlay-modification (overlay after? beg end &optional length)
  "Signals a snippet violation, then issues error.

The error should be ignored in `debug-ignored-errors'"
  (cond ((not (or after?
                  (yas/undo-in-progress)))
         (setq yas/protection-violation (point))
         (error "Exit the snippet first!"))))

(add-to-list 'debug-ignored-errors "^Exit the snippet first!$")


;;; Apropos stacked expansion:
;;;
;;; the parent snippet does not run its fields modification hooks
;;; (`yas/on-field-overlay-modification' and
;;; `yas/on-protection-overlay-modification') while the child snippet
;;; is active. This means, among other things, that the mirrors of the
;;; parent snippet are not updated, this only happening when one exits
;;; the child snippet.
;;;
;;; Unfortunately, this also puts some ugly (and not fully-tested)
;;; bits of code in `yas/expand-snippet' and
;;; `yas/commit-snippet'. I've tried to mark them with "stacked
;;; expansion:".
;;;
;;; This was thought to be safer in in an undo/redo perpective, but
;;; maybe the correct implementation is to make the globals
;;; `yas/active-field-overlay' and `yas/field-protection-overlays' be
;;; snippet-local and be active even while the child snippet is
;;; running. This would mean a lot of overlay modification hooks
;;; running, but if managed correctly (including overlay priorities)
;;; they should account for all situations...
;;;

(defun yas/expand-snippet (template &optional start end expand-env)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)

  ;; If a region is active, set `yas/selected-text'
  (setq yas/selected-text
        (when mark-active
          (prog1 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
            (unless start (setq start (region-beginning))
                    (unless end (setq end (region-end)))))))

  (when start
    (goto-char start))

  ;; stacked expansion: shoosh the overlay modification hooks
  ;;
  (let ((to-delete (and start end (buffer-substring-no-properties start end)))
        (start (or start (point)))
        (end (or end (point)))
        (inhibit-modification-hooks t)
        (column (current-column))
        snippet)

    ;; Delete the region to delete, this *does* get undo-recorded.
    ;;
    (when (and to-delete
               (> end start))
      (delete-region start end)
      (setq yas/deleted-text to-delete))

    ;; Narrow the region down to the template, shoosh the
    ;; `buffer-undo-list', and create the snippet, the new snippet
    ;; updates its mirrors once, so we are left with some plain text.
    ;; The undo action for deleting this plain text will get recorded
    ;; at the end of this function.
    (save-restriction
      (narrow-to-region start start)
      (let ((buffer-undo-list t))
        ;; snippet creation might evaluate users elisp, which
        ;; might generate errors, so we have to be ready to catch
        ;; them mostly to make the undo information
        ;;
        (setq yas/start-column (save-restriction (widen) (current-column)))
        (insert template)

        (setq snippet
              (if expand-env
                  (let ((read-vars (condition-case err
                                       (read expand-env)
                                     (error nil))))
                    (eval `(let ,read-vars
                             (yas/snippet-create (point-min) (point-max)))))
                (yas/snippet-create (point-min) (point-max))))))

    ;; stacked-expansion: This checks for stacked expansion, save the
    ;; `yas/previous-active-field' and advance its boudary.
    ;;
    (let ((existing-field (and yas/active-field-overlay
                               (overlay-buffer yas/active-field-overlay)
                               (overlay-get yas/active-field-overlay 'yas/field))))
      (when existing-field
        (setf (yas/snippet-previous-active-field snippet) existing-field)
        (yas/advance-end-maybe existing-field (overlay-end yas/active-field-overlay))))

    ;; Exit the snippet immediately if no fields
    ;;
    (unless (yas/snippet-fields snippet)
      (yas/exit-snippet snippet))

    ;; Push two undo actions: the deletion of the inserted contents of
    ;; the new snippet (without the "key") followed by an apply of
    ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
    ;;
    (let ((start (overlay-start (yas/snippet-control-overlay snippet)))
          (end (overlay-end (yas/snippet-control-overlay snippet))))
      (push (cons start end) buffer-undo-list)
      (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
            buffer-undo-list))
    ;; Now, move to the first field
    ;;
    (let ((first-field (car (yas/snippet-fields snippet))))
      (when first-field
        (yas/move-to-field snippet first-field))))
  (message "[yas] snippet expanded."))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas/snippet-fields snippet)
    (yas/commit-snippet snippet 'no-hooks)))

(defun yas/snippet-revive (beg end snippet)
  "Revives the SNIPPET and creates a control overlay from BEG to
END.

BEG and END are, we hope, the original snippets boudaries. All
the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas/take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  ;;
  (yas/points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  ;;
  (let ((target-field (or (yas/snippet-active-field snippet)
                          (car (yas/snippet-fields snippet)))))
    (when target-field
      (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet beg end))
      (overlay-put (yas/snippet-control-overlay snippet) 'yas/snippet snippet)

      (yas/move-to-field snippet target-field)

      (add-hook 'post-command-hook 'yas/post-command-handler nil t)
      (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

      (push `(apply yas/take-care-of-redo ,beg ,end ,snippet)
            buffer-undo-list))))

(defun yas/snippet-create (begin end)
  "Creates a snippet from an template inserted between BEGIN and END.

Returns the newly created snippet."
  (let ((snippet (yas/make-snippet)))
    (goto-char begin)
    (yas/snippet-parse-create snippet)

    ;; Sort and link each field
    (yas/snippet-sort-fields snippet)

    ;; Create keymap overlay for snippet
    (setf (yas/snippet-control-overlay snippet)
          (yas/make-control-overlay snippet (point-min) (point-max)))

    ;; Move to end
    (goto-char (point-max))

    ;; Setup hooks
    (add-hook 'post-command-hook 'yas/post-command-handler nil t)
    (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

    snippet))


;;; Apropos adjacencies: Once the $-constructs bits like "$n" and
;;; "${:n" are deleted in the recently expanded snippet, we might
;;; actually have many fields, mirrors (and the snippet exit) in the
;;; very same position in the buffer. Therefore we need to single-link
;;; the fields-or-mirrors-or-exit, which I have called "fom",
;;; according to their original positions in the buffer.
;;;
;;; Then we have operation `yas/advance-end-maybe' and
;;; `yas/advance-start-maybe', which conditionally push the starts and
;;; ends of these foms down the chain.
;;;
;;; This allows for like the printf with the magic ",":
;;;
;;;   printf ("${1:%s}\\n"${1:$(if (string-match "%" text) "," "\);")}  \
;;;   $2${1:$(if (string-match "%" text) "\);" "")}$0
;;;

(defun yas/fom-start (fom)
  (cond ((yas/field-p fom)
         (yas/field-start fom))
        ((yas/mirror-p fom)
         (yas/mirror-start fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-end (fom)
  (cond ((yas/field-p fom)
         (yas/field-end fom))
        ((yas/mirror-p fom)
         (yas/mirror-end fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-next (fom)
  (cond ((yas/field-p fom)
         (yas/field-next fom))
        ((yas/mirror-p fom)
         (yas/mirror-next fom))
        (t
         (yas/exit-next fom))))

(defun yas/calculate-adjacencies (snippet)
  "Calculate adjacencies for fields or mirrors of SNIPPET.

This is according to their relative positions in the buffer, and
has to be called before the $-constructs are deleted."
  (flet ((yas/fom-set-next-fom (fom nextfom)
                               (cond ((yas/field-p fom)
                                      (setf (yas/field-next fom) nextfom))
                                     ((yas/mirror-p fom)
                                      (setf (yas/mirror-next fom) nextfom))
                                     (t
                                      (setf (yas/exit-next fom) nextfom))))
         (yas/compare-fom-begs (fom1 fom2)
                               (> (yas/fom-start fom2) (yas/fom-start fom1)))
         (yas/link-foms (fom1 fom2)
                        (yas/fom-set-next-fom fom1 fom2)))
    ;; make some yas/field, yas/mirror and yas/exit soup
    (let ((soup))
      (when (yas/snippet-exit snippet)
        (push (yas/snippet-exit snippet) soup))
      (dolist (field (yas/snippet-fields snippet))
        (push field soup)
        (dolist (mirror (yas/field-mirrors field))
          (push mirror soup)))
      (setq soup
            (sort soup
                  #'yas/compare-fom-begs))
      (when soup
        (reduce #'yas/link-foms soup)))))

(defun yas/advance-end-maybe (fom newend)
  "Maybe advance FOM's end to NEWEND if it needs it.

If it does, also:

* call `yas/advance-start-maybe' on FOM's next fom.

* in case FOM is field call `yas/advance-end-maybe' on its parent
  field"
  (when (and fom (< (yas/fom-end fom) newend))
    (set-marker (yas/fom-end fom) newend)
    (yas/advance-start-maybe (yas/fom-next fom) newend)
    (if (and (yas/field-p fom)
             (yas/field-parent-field fom))
        (yas/advance-end-maybe (yas/field-parent-field fom) newend))))

(defun yas/advance-start-maybe (fom newstart)
  "Maybe advance FOM's start to NEWSTART if it needs it.

If it does, also call `yas/advance-end-maybe' on FOM."
  (when (and fom (< (yas/fom-start fom) newstart))
    (set-marker (yas/fom-start fom) newstart)
    (yas/advance-end-maybe fom newstart)))

(defvar yas/dollar-regions nil
  "When expanding the snippet the \"parse-create\" functions add
  cons cells to this var")

(defun yas/snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((parse-start (point)))
    ;; Reset the yas/dollar-regions
    ;;
    (setq yas/dollar-regions nil)
    ;; protect escaped quote, backquotes and backslashes
    ;;
    (yas/protect-escapes nil '(?\\ ?` ?'))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas/replace-backquotes)
    ;; protect escapes again since previous steps might have generated
    ;; more characters needing escaping
    ;;
    (goto-char parse-start)
    (yas/protect-escapes)
    ;; parse fields with {}
    ;;
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors and fields
    ;;
    (goto-char parse-start)
    (yas/simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas/transform-mirror-parse-create snippet)
    ;; calculate adjacencies of fields and mirrors
    ;;
    (yas/calculate-adjacencies snippet)
    ;; Delete $-constructs
    ;;
    (yas/delete-regions yas/dollar-regions)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas/restore-escapes)
    ;; update mirrors for the first time
    ;;
    (yas/update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent-according-to-mode (snippet-markers)
  "Indent current line according to mode, preserving
SNIPPET-MARKERS."
  ;; XXX: Here seems to be the indent problem:
  ;;
  ;; `indent-according-to-mode' uses whatever
  ;; `indent-line-function' is available. Some
  ;; implementations of these functions delete text
  ;; before they insert. If there happens to be a marker
  ;; just after the text being deleted, the insertion
  ;; actually happens  after the marker, which misplaces
  ;; it.
  ;;
  ;; This would also happen if we had used overlays with
  ;; the `front-advance' property set to nil.
  ;;
  ;; This is why I have these `trouble-markers', they are the ones at
  ;; they are the ones at the first non-whitespace char at the line
  ;; (i.e. at `yas/real-line-beginning'. After indentation takes place
  ;; we should be at the correct to restore them to. All other
  ;; non-trouble-markers have been *pushed* and don't need special
  ;; attention.
  ;;
  (goto-char (yas/real-line-beginning))
  (let ((trouble-markers (remove-if-not #'(lambda (marker)
                                            (= marker (point)))
                                        snippet-markers)))
    (save-restriction
      (widen)
      (condition-case err
          (indent-according-to-mode)
        (error (message "[yas] warning: yas/indent-according-to-mode habing problems running %s" indent-line-function)
               nil)))
    (mapc #'(lambda (marker)
              (set-marker marker (point)))
          trouble-markers)))

(defun yas/indent (snippet)
  (let ((snippet-markers (yas/collect-snippet-markers snippet)))
    ;; Look for those $>
    (save-excursion
      (while (re-search-forward "$>" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (when (not (eq yas/indent-line 'auto))
          (yas/indent-according-to-mode snippet-markers))))
    ;; Now do stuff for 'fixed and 'auto
    (save-excursion
      (cond ((eq yas/indent-line 'fixed)
             (while (and (zerop (forward-line))
                         (zerop (current-column)))
               (indent-to-column column)))
            ((eq yas/indent-line 'auto)
             (let ((end (set-marker (make-marker) (point-max)))
                   (indent-first-line-p yas/also-auto-indent-first-line))
               (while (and (zerop (if indent-first-line-p
                                      (prog1
                                          (forward-line 0)
                                        (setq indent-first-line-p nil))
                                    (forward-line 1)))
                           (not (eobp))
                           (<= (point) end))
                 (yas/indent-according-to-mode snippet-markers))))
            (t
             nil)))))

(defun yas/collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (dolist (field (yas/snippet-fields snippet))
      (push (yas/field-start field) markers)
      (push (yas/field-end field) markers)
      (dolist (mirror (yas/field-mirrors field))
        (push (yas/mirror-start mirror) markers)
        (push (yas/mirror-end mirror) markers)))
    (let ((snippet-exit (yas/snippet-exit snippet)))
      (when (and snippet-exit
                 (marker-buffer (yas/exit-marker snippet-exit)))
        (push (yas/exit-marker snippet-exit) markers)))
    markers))

(defun yas/real-line-beginning ()
  (let ((c (char-after (line-beginning-position)))
        (n (line-beginning-position)))
    (while (or (eql c ?\ )
               (eql c ?\t))
      (incf n)
      (setq c (char-after n)))
    n))

(defun yas/escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas/protect-escapes (&optional text escaped)
  "Protect all escaped characters with their numeric ASCII value.

With optional string TEXT do it in string instead of buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (concat "\\" (char-to-string escaped))
                                     (yas/escape-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/restore-escapes (&optional text escaped)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead of the buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (yas/escape-string escaped)
                                     (char-to-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
  with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
    (let ((transformed (yas/read-and-eval-string (yas/restore-escapes (match-string 1)))))
      (goto-char (match-end 0))
      (when transformed (insert transformed))
      (delete-region (match-beginning 0) (match-end 0)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
        (scan-sexps from count))
    (error
     nil)))

(defun yas/make-marker (pos)
  "Create a marker at POS with `nil' `marker-insertion-type'"
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse most field expressions, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;

* \"${n: text$(expression)}, the same with a lisp expression;
  this is caught with the curiously named `yas/multi-dollar-lisp-expression-regexp'

* the same as above but unnumbered, (no N:) and number is calculated automatically.

When multiple expressions are found, only the last one counts."
  ;;
  (save-excursion
    (while (re-search-forward yas/field-regexp nil t)
      (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
             (number (and (match-string-no-properties 1)
                          (string-to-number (match-string-no-properties 1))))
             (brand-new-field (and real-match-end-0
                                   ;; break if on "$(" immediately
                                   ;; after the ":", this will be
                                   ;; caught as a mirror with
                                   ;; transform later.
                                   (not (save-match-data
                                          (eq (string-match "$[ \t\n]*("
                                                            (match-string-no-properties 2)) 0)))
                                   (not (and number (zerop number)))
                                   (yas/make-field number
                                                   (yas/make-marker (match-beginning 2))
                                                   (yas/make-marker (1- real-match-end-0))
                                                   parent-field))))
        (when brand-new-field
          (goto-char real-match-end-0)
          (push (cons (1- real-match-end-0) real-match-end-0)
                yas/dollar-regions)
          (push (cons (match-beginning 0) (match-beginning 2))
                yas/dollar-regions)
          (push brand-new-field (yas/snippet-fields snippet))
          (save-excursion
            (save-restriction
              (narrow-to-region (yas/field-start brand-new-field) (yas/field-end brand-new-field))
              (goto-char (point-min))
              (yas/field-parse-create snippet brand-new-field)))))))
  ;; if we entered from a parent field, now search for the
  ;; `yas/multi-dollar-lisp-expression-regexp'. THis is used for
  ;; primary field transformations
  ;; 
  (when parent-field
    (save-excursion
      (while (re-search-forward yas/multi-dollar-lisp-expression-regexp nil t)
        (let* ((real-match-end-1 (yas/scan-sexps (match-beginning 1) 1)))
          ;; commit the primary field transformation if we don't find
          ;; it in yas/dollar-regions (a subnested field) might have
          ;; already caught it.
          (when (and real-match-end-1
                     (not (member (cons (match-beginning 0)
                                        real-match-end-1)
                                  yas/dollar-regions)))
            (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1)
                                                                          real-match-end-1)))
              (setf (yas/field-transform parent-field) (yas/restore-escapes lisp-expression-string)))
            (push (cons (match-beginning 0) real-match-end-1)
                  yas/dollar-regions)))))))

(defun yas/transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations."
  (while (re-search-forward yas/transform-mirror-regexp nil t)
    (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
           (number (string-to-number (match-string-no-properties 1)))
           (field (and number
                       (not (zerop number))
                       (yas/snippet-find-field snippet number))))
      (when (and real-match-end-0
                 field)
        (push (yas/make-mirror (yas/make-marker (match-beginning 0))
                               (yas/make-marker (match-beginning 0))
                               (yas/restore-escapes
                                (buffer-substring-no-properties (match-beginning 2)
                                                                (1- real-match-end-0))))
              (yas/field-mirrors field))
        (push (cons (match-beginning 0) real-match-end-0) yas/dollar-regions)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" mirrors and the exit-marker."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
    (let ((number (string-to-number (match-string-no-properties 1))))
      (cond ((zerop number)

             (setf (yas/snippet-exit snippet)
                   (yas/make-exit (yas/make-marker (match-end 0))))
             (save-excursion
               (goto-char (match-beginning 0))
               (when yas/wrap-around-region
                 (cond (yas/selected-text
                        (insert yas/selected-text))
                       ((and (eq yas/wrap-around-region 'cua)
                             cua-mode
                             (get-register ?0))
                        (insert (prog1 (get-register ?0)
                                  (set-register ?0 nil))))))
               (push (cons (point) (yas/exit-marker (yas/snippet-exit snippet)))
                     yas/dollar-regions)))
            (t
             (let ((field (yas/snippet-find-field snippet number)))
               (if field
                   (push (yas/make-mirror (yas/make-marker (match-beginning 0))
                                          (yas/make-marker (match-beginning 0))
                                          nil)
                         (yas/field-mirrors field))
                 (push (yas/make-field number
                                       (yas/make-marker (match-beginning 0))
                                       (yas/make-marker (match-beginning 0))
                                       nil)
                       (yas/snippet-fields snippet))))
             (push (cons (match-beginning 0) (match-end 0))
                   yas/dollar-regions))))))

(defun yas/delete-regions (regions)
  "Sort disjuct REGIONS by start point, then delete from the back."
  (mapc #'(lambda (reg)
            (delete-region (car reg) (cdr reg)))
        (sort regions
              #'(lambda (r1 r2)
                  (>= (car r1) (car r2))))))

(defun yas/update-mirrors (snippet)
  "Updates all the mirrors of SNIPPET."
  (save-excursion
    (dolist (field (yas/snippet-fields snippet))
      (dolist (mirror (yas/field-mirrors field))
        ;; stacked expansion: I added an `inhibit-modification-hooks'
        ;; here, for safety, may need to remove if we the mechanism is
        ;; altered.
        ;;
        (let ((inhibit-modification-hooks t))
          (yas/mirror-update-display mirror field)
          ;; `yas/place-overlays' is needed if the active field and
          ;; protected overlays have been changed because of insertions
          ;; in `yas/mirror-update-display'
          ;;
          (when (eq field (yas/snippet-active-field snippet))
            (yas/place-overlays snippet field)))))))

(defun yas/mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."
  (let ((reflection (or (yas/apply-transform mirror field)
                        (yas/field-text-for-display field))))
    (when (and reflection
               (not (string= reflection (buffer-substring-no-properties (yas/mirror-start mirror)
                                                                        (yas/mirror-end mirror)))))
      (goto-char (yas/mirror-start mirror))
      (insert reflection)
      (if (> (yas/mirror-end mirror) (point))
          (delete-region (point) (yas/mirror-end mirror))
        (set-marker (yas/mirror-end mirror) (point))
        (yas/advance-start-maybe (yas/mirror-next mirror) (point))))))

(defun yas/field-update-display (field snippet)
  "Much like `yas/mirror-update-display', but for fields"
  (when (yas/field-transform field)
    (let ((inhibit-modification-hooks t)
          (transformed (yas/apply-transform field field))
          (point (point)))
      (when (and transformed
                 (not (string= transformed (buffer-substring-no-properties (yas/field-start field)
                                                                           (yas/field-end field)))))
        (setf (yas/field-modified-p field) t)
        (goto-char (yas/field-start field))
        (insert transformed)
        (if (> (yas/field-end field) (point))
            (delete-region (point) (yas/field-end field))
          (set-marker (yas/field-end field) (point))
          (yas/advance-start-maybe (yas/field-next field) (point)))
        t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre- and post-command hooks
;;
(defun yas/pre-command-handler () )

(defun yas/post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (cond (yas/protection-violation
         (goto-char yas/protection-violation)
         (setq yas/protection-violation nil))
        ((eq 'undo this-command)
         ;;
         ;; After undo revival the correct field is sometimes not
         ;; restored correctly, this condition handles that
         ;;
         (let* ((snippet (car (yas/snippets-at-point)))
                (target-field (and snippet
                                   (find-if-not #'(lambda (field)
                                                    (yas/field-probably-deleted-p snippet field))
                                                (remove nil
                                                        (cons (yas/snippet-active-field snippet)
                                                              (yas/snippet-fields snippet)))))))
           (when target-field
             (yas/move-to-field snippet target-field))))
        ((not (yas/undo-in-progress))
         ;; When not in an undo, check if we must commit the snippet (use exited it).
         (yas/check-commit-snippet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug functions.  Use (or change) at will whenever needed.
;;
;; some useful debug code for looking up snippet tables
;;
;; (insert (pp
;; (let ((shit))
;;   (maphash #'(lambda (k v)
;;             (push k shit))
;;         (yas/snippet-table-hash (gethash 'ruby-mode yas/snippet-tables)))
;;   shit)))
;;

(defun yas/debug-tables ()
  (interactive)
  (with-output-to-temp-buffer "*YASnippet tables*"
    (dolist (symbol (remove nil (append (list major-mode)
                                        (if (listp yas/mode-symbol)
                                            yas/mode-symbol
                                          (list yas/mode-symbol)))))
      (princ (format "Snippet table hash keys for %s:\n\n" symbol))
      (let ((keys))
        (maphash #'(lambda (k v)
                     (push k keys))
                 (yas/snippet-table-hash (gethash symbol yas/snippet-tables)))
        (princ keys))

      (princ (format "Keymap for  %s:\n\n" symbol))
      (princ (gethash symbol yas/menu-table)))))

(defun yas/debug-snippet-vars ()
  "Debug snippets, fields, mirrors and the `buffer-undo-list'."
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
    (princ "Interesting YASnippet vars: \n\n")

    (princ (format "\nPost command hook: %s\n" post-command-hook))
    (princ (format "\nPre  command hook: %s\n" pre-command-hook))

    (princ (format "%s live snippets in total\n" (length (yas/snippets-at-point (quote all-snippets)))))
    (princ (format "%s overlays in buffer:\n\n" (length (overlays-in (point-min) (point-max)))))
    (princ (format "%s live snippets at point:\n\n" (length (yas/snippets-at-point))))


    (dolist (snippet (yas/snippets-at-point))
      (princ (format "\tsid: %d control overlay from %d to %d\n"
                     (yas/snippet-id snippet)
                     (overlay-start (yas/snippet-control-overlay snippet))
                     (overlay-end (yas/snippet-control-overlay snippet))))
      (princ (format "\tactive field: %d from %s to %s covering \"%s\"\n"
                     (yas/field-number (yas/snippet-active-field snippet))
                     (marker-position (yas/field-start (yas/snippet-active-field snippet)))
                     (marker-position (yas/field-end (yas/snippet-active-field snippet)))
                     (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet)))))
      (when (yas/snippet-exit snippet)
        (princ (format "\tsnippet-exit: at %s next: %s\n"
                       (yas/exit-marker (yas/snippet-exit snippet))
                       (yas/exit-next (yas/snippet-exit snippet)))))
      (dolist (field (yas/snippet-fields snippet))
        (princ (format "\tfield: %d from %s to %s covering \"%s\" next: %s\n"
                       (yas/field-number field)
                       (marker-position (yas/field-start field))
                       (marker-position (yas/field-end field))
                       (buffer-substring-no-properties (yas/field-start field) (yas/field-end field))
                       (yas/debug-format-fom-concise (yas/field-next field))))
        (dolist (mirror (yas/field-mirrors field))
          (princ (format "\t\tmirror: from %s to %s covering \"%s\" next: %s\n"
                         (marker-position (yas/mirror-start mirror))
                         (marker-position (yas/mirror-end mirror))
                         (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror))
                         (yas/debug-format-fom-concise (yas/mirror-next mirror)))))))

    (princ (format "\nUndo is %s and point-max is %s.\n"
                   (if (eq buffer-undo-list t)
                       "DISABLED"
                     "ENABLED")
                   (point-max)))
    (unless (eq buffer-undo-list t)
      (princ (format "Undpolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
      (let ((first-ten (subseq buffer-undo-list 0 19)))
        (dolist (undo-elem first-ten)
          (princ (format "%2s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 70))))))))

(defun yas/debug-format-fom-concise (fom)
  (when fom
    (cond ((yas/field-p fom)
           (format "field %d from %d to %d"
                   (yas/field-number fom)
                   (marker-position (yas/field-start fom))
                   (marker-position (yas/field-end fom))))
          ((yas/mirror-p fom)
           (format "mirror from %d to %d"
                   (marker-position (yas/mirror-start fom))
                   (marker-position (yas/mirror-end fom))))
          (t
           (format "snippet exit at %d"
                   (marker-position (yas/fom-start fom)))))))


(defun yas/exterminate-package ()
  (interactive)
  (yas/global-mode -1)
  (yas/minor-mode -1)
  (yas/kill-snippet-keybindings)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory (or (and (listp yas/root-directory)
                               (first yas/root-directory))
                          yas/root-directory
                          "~/Source/yasnippet/snippets/"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapc #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (setq undo-in-progress nil)
  (snippet-mode)
  (yas/minor-mode 1)
  (let ((abbrev))
    (setq abbrev "$f")
    (insert abbrev))
  (unless quiet
    (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `locate-dominating-file' is added for compatibility in emacs < 23
(unless (or (eq emacs-major-version 23)
            (fboundp 'locate-dominating-file))
  (defvar locate-dominating-stop-dir-regexp
    "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
    "Regexp of directory names which stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file' which will stop its search
when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which filenames are interpreted as hostnames,
or mount points potentially requiring authentication as a different user.")

  (defun locate-dominating-file (file name)
    "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found."
    ;; We used to use the above locate-dominating-files code, but the
    ;; directory-files call is very costly, so we're much better off doing
    ;; multiple calls using the code in here.
    ;;
    ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
    ;; `name' in /home or in /.
    (setq file (abbreviate-file-name file))
    (let ((root nil)
          (prev-file file)
          ;; `user' is not initialized outside the loop because
          ;; `file' may not exist, so we may have to walk up part of the
          ;; hierarchy before we find the "initial UID".
          (user nil)
          try)
      (while (not (or root
                      (null file)
                      ;; FIXME: Disabled this heuristic because it is sometimes
                      ;; inappropriate.
                      ;; As a heuristic, we stop looking up the hierarchy of
                      ;; directories as soon as we find a directory belonging
                      ;; to another user.  This should save us from looking in
                      ;; things like /net and /afs.  This assumes that all the
                      ;; files inside a project belong to the same user.
                      ;; (let ((prev-user user))
                      ;;   (setq user (nth 2 (file-attributes file)))
                      ;;   (and prev-user (not (equal user prev-user))))
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq try (file-exists-p (expand-file-name name file)))
        (cond (try (setq root file))
              ((equal file (setq prev-file file
                                 file (file-name-directory
                                       (directory-file-name file))))
               (setq file nil))))
      root)))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
             (dolist (k '(":" ">" ";" "<" "{" "}"))
               (define-key (symbol-value (make-local-variable 'yas/keymap))
                 k 'self-insert-command))))


;;; yasnippet.el ends here

;;; auto-complete.el --- Auto Completion for GNU Emacs

;; Copyright (C) 2008, 2009, 2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; URL: http://cx4a.org/software/auto-complete
;; Keywords: completion, convenience
;; Version: 1.3.1

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
;;
;; This extension provides a way to complete with popup menu like:
;;
;;     def-!-
;;     +-----------------+
;;     |defun::::::::::::|
;;     |defvar           |
;;     |defmacro         |
;;     |       ...       |
;;     +-----------------+
;;
;; You can complete by typing and selecting menu.
;;
;; Entire documents are located in doc/ directory.
;; Take a look for information.
;;
;; Enjoy!

;;; Code:



(eval-when-compile
  (require 'cl))

(require 'popup)

;;;; Global stuff

(defun ac-error (&optional var)
  "Report an error and disable `auto-complete-mode'."
  (ignore-errors
    (message "auto-complete error: %s" var)
    (auto-complete-mode -1)
    var))



;;;; Customization

(defgroup auto-complete nil
  "Auto completion."
  :group 'completion
  :prefix "ac-")

(defcustom ac-delay 0.1
  "Delay to completions will be available."
  :type 'float
  :group 'auto-complete)

(defcustom ac-auto-show-menu 0.8
  "Non-nil means completion menu will be automatically shown."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil)
                 (float :tag "Timer"))
  :group 'auto-complete)

(defcustom ac-show-menu-immediately-on-auto-complete t
  "Non-nil means menu will be showed immediately on `auto-complete'."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-expand-on-auto-complete t
  "Non-nil means expand whole common part on first time `auto-complete'."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-disable-faces '(font-lock-comment-face font-lock-string-face font-lock-doc-face)
  "Non-nil means disable automatic completion on specified faces."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-stop-flymake-on-completing t
  "Non-nil means disble flymake temporarily on completing."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-use-fuzzy t
  "Non-nil means use fuzzy matching."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-fuzzy-cursor-color "red"
  "Cursor color in fuzzy mode."
  :type 'string
  :group 'auto-complete)

(defcustom ac-use-comphist t
  "Non-nil means use intelligent completion history."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-comphist-threshold 0.7
  "Percentage of ignoring low scored candidates."
  :type 'float
  :group 'auto-complete)

(defcustom ac-comphist-file
  (expand-file-name (concat (if (boundp 'user-emacs-directory)
                                user-emacs-directory
                              "~/.emacs.d/")
                            "/ac-comphist.dat"))
  "Completion history file name."
  :type 'string
  :group 'auto-complete)

(defcustom ac-use-quick-help t
  "Non-nil means use quick help."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-quick-help-delay 1.5
  "Delay to show quick help."
  :type 'float
  :group 'auto-complete)

(defcustom ac-menu-height 10
  "Max height of candidate menu."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-menu-height 'ac-menu-height)

(defcustom ac-quick-help-height 20
  "Max height of quick help."
  :type 'integer
  :group 'auto-complete)

(defcustom ac-quick-help-prefer-x t
  "Prefer X tooltip than overlay popup for displaying quick help."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-candidate-limit nil
  "Limit number of candidates. Non-integer means no limit."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-max 'ac-candidate-limit)

(defcustom ac-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    c-mode cc-mode c++-mode
    java-mode clojure-mode scala-mode
    scheme-mode
    ocaml-mode tuareg-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-compatible-packages-regexp
  "^ac-"
  "Regexp to indicate what packages can work with auto-complete."
  :type 'string
  :group 'auto-complete)

(defcustom ac-trigger-commands
  '(self-insert-command)
  "Trigger commands that specify whether `auto-complete' should start or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-commands-on-completing
  '(delete-backward-char
    backward-delete-char
    backward-delete-char-untabify)
  "Trigger commands that specify whether `auto-complete' should continue or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-key nil
  "Non-nil means `auto-complete' will start by typing this key.
If you specify this TAB, for example, `auto-complete' will start by typing TAB,
and if there is no completions, an original command will be fallbacked."
  :type 'string
  :group 'auto-complete
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and value
                    (fboundp 'ac-set-trigger-key))
           (ac-set-trigger-key value))))

(defcustom ac-auto-start 2
  "Non-nil means completion will be started automatically.
Positive integer means if a length of a word you entered is larger than the value,
completion will be started automatically.
If you specify `nil', never be started automatically."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil)
                 (integer :tag "Require"))
  :group 'auto-complete)

(defcustom ac-ignores nil
  "List of string to ignore completion."
  :type '(repeat string)
  :group 'auto-complete)

(defcustom ac-ignore-case 'smart
  "Non-nil means auto-complete ignores case.
If this value is `smart', auto-complete ignores case only when
a prefix doen't contain any upper case letters."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Smart" smart)
                 (const :tag "No" nil))
  :group 'auto-complete)

(defcustom ac-dwim t
  "Non-nil means `auto-complete' works based on Do What I Mean."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-use-menu-map nil
  "Non-nil means a special keymap `ac-menu-map' on completing menu will be used."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-use-overriding-local-map nil
  "Non-nil means `overriding-local-map' will be used to hack for overriding key events on auto-copletion."
  :type 'boolean
  :group 'auto-complete)

(defface ac-completion-face
  '((t (:foreground "darkgray" :underline t)))
  "Face for inline completion"
  :group 'auto-complete)

(defface ac-candidate-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate."
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:background "steelblue" :foreground "white")))
  "Face for selected candidate."
  :group 'auto-complete)

(defvar auto-complete-mode-hook nil
  "Hook for `auto-complete-mode'.")



;;;; Internal variables

(defvar auto-complete-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar ac-cursor-color nil
  "Old cursor color.")

(defvar ac-inline nil
  "Inline completion instance.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-show-menu nil
  "Flag to show menu on timer tick.")

(defvar ac-last-completion nil
  "Cons of prefix marker and selected item of last completion.")

(defvar ac-quick-help nil
  "Quick help instance")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-buffer nil
  "Buffer where auto-complete is started.")

(defvar ac-point nil
  "Start point of prefix.")

(defvar ac-last-point nil
  "Last point of updating pattern.")

(defvar ac-prefix nil
  "Prefix string.")
(defvaralias 'ac-target 'ac-prefix)

(defvar ac-selected-candidate nil
  "Last selected candidate.")

(defvar ac-common-part nil
  "Common part string of meaningful candidates.
If there is no common part, this will be nil.")

(defvar ac-whole-common-part nil
  "Common part string of whole candidates.
If there is no common part, this will be nil.")

(defvar ac-prefix-overlay nil
  "Overlay for prefix string.")

(defvar ac-timer nil
  "Completion idle timer.")

(defvar ac-show-menu-timer nil
  "Show menu idle timer.")

(defvar ac-quick-help-timer nil
  "Quick help idle timer.")

(defvar ac-triggered nil
  "Flag to update.")

(defvar ac-limit nil
  "Limit number of candidates for each sources.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-candidates-cache nil
  "Candidates cache for individual sources.")

(defvar ac-fuzzy-enable nil
  "Non-nil means fuzzy matching is enabled.")

(defvar ac-dwim-enable nil
  "Non-nil means DWIM completion will be allowed.")

(defvar ac-mode-map (make-sparse-keymap)
  "Auto-complete mode map. It is also used for trigger key command. See also `ac-trigger-key'.")

(defvar ac-completing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map "\r" 'ac-complete)
    (define-key map (kbd "M-TAB") 'auto-complete)
    (define-key map "\C-s" 'ac-isearch)

    (define-key map "\M-n" 'ac-next)
    (define-key map "\M-p" 'ac-previous)
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    (define-key map [f1] 'ac-help)
    (define-key map [M-f1] 'ac-persist-help)
    (define-key map (kbd "C-?") 'ac-help)
    (define-key map (kbd "C-M-?") 'ac-persist-help)

    (define-key map [C-down] 'ac-quick-help-scroll-down)
    (define-key map [C-up] 'ac-quick-help-scroll-up)
    (define-key map "\C-\M-n" 'ac-quick-help-scroll-down)
    (define-key map "\C-\M-p" 'ac-quick-help-scroll-up)

    (dotimes (i 9)
      (let ((symbol (intern (format "ac-complete-%d" (1+ i)))))
        (fset symbol
              `(lambda ()
                 (interactive)
                 (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
                   (ac-complete))))
        (define-key map (read-kbd-macro (format "M-%s" (1+ i))) symbol)))

    map)
  "Keymap for completion.")
(defvaralias 'ac-complete-mode-map 'ac-completing-map)

(defvar ac-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-n" 'ac-next)
    (define-key map "\C-p" 'ac-previous)
    (set-keymap-parent map ac-completing-map)
    map)
  "Keymap for completion on completing menu.")

(defvar ac-current-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ac-completing-map)
    map))

(defvar ac-match-function 'all-completions
  "Default match function.")

(defvar ac-prefix-definitions
  '((symbol . ac-prefix-symbol)
    (file . ac-prefix-file)
    (valid-file . ac-prefix-valid-file)
    (c-dot . ac-prefix-c-dot)
    (c-dot-ref . ac-prefix-c-dot-ref))
  "Prefix definitions for common use.")

(defvar ac-sources '(ac-source-words-in-same-mode-buffers)
  "Sources for completion.")
(make-variable-buffer-local 'ac-sources)

(defvar ac-compiled-sources nil
  "Compiled source of `ac-sources'.")

(defvar ac-current-sources nil
  "Current working sources. This is sublist of `ac-compiled-sources'.")

(defvar ac-omni-completion-sources nil
  "Do not use this anymore.")

(defvar ac-current-prefix-def nil)

(defvar ac-ignoring-prefix-def nil)



;;;; Intelligent completion history

(defvar ac-comphist nil
  "Database of completion history.")

(defsubst ac-comphist-make-tab ()
  (make-hash-table :test 'equal))

(defsubst ac-comphist-tab (db)
  (nth 0 db))

(defsubst ac-comphist-cache (db)
  (nth 1 db))

(defun ac-comphist-make (&optional tab)
  (list (or tab (ac-comphist-make-tab)) (make-hash-table :test 'equal :weakness t)))

(defun ac-comphist-get (db string &optional create)
  (let* ((tab (ac-comphist-tab db))
         (index (gethash string tab)))
    (when (and create (null index))
      (setq index (make-vector (length string) 0))
      (puthash string index tab))
    index))

(defun ac-comphist-add (db string prefix)
  (setq prefix (min prefix (1- (length string))))
  (when (<= 0 prefix)
    (setq string (substring-no-properties string))
    (let ((stat (ac-comphist-get db string t)))
      (incf (aref stat prefix))
      (remhash string (ac-comphist-cache db)))))

(defun ac-comphist-score (db string prefix)
  (setq prefix (min prefix (1- (length string))))
  (if (<= 0 prefix)
      (let ((cache (gethash string (ac-comphist-cache db))))
        (or (and cache (aref cache prefix))
            (let ((stat (ac-comphist-get db string))
                  (score 0.0))
              (when stat
                (loop for p from 0 below (length string)
                      ;; sigmoid function
                      with a = 5
                      with d = (/ 6.0 a)
                      for x = (- d (abs (- prefix p)))
                      for r = (/ 1.0 (1+ (exp (* (- a) x))))
                      do
                      (incf score (* (aref stat p) r))))
              ;; Weight by distance
              (incf score (max 0.0 (- 0.3 (/ (- (length string) prefix) 100.0))))
              (unless cache
                (setq cache (make-vector (length string) nil))
                (puthash string cache (ac-comphist-cache db)))
              (aset cache prefix score)
              score)))
    0.0))

(defun ac-comphist-sort (db collection prefix &optional threshold)
  (let (result
        (n 0)
        (total 0)
        (cur 0))
    (setq result (mapcar (lambda (a)
                           (when (and cur threshold)
                             (if (>= cur (* total threshold))
                                 (setq cur nil)
                               (incf n)
                               (incf cur (cdr a))))
                           (car a))
                         (sort (mapcar (lambda (string)
                                         (let ((score (ac-comphist-score db string prefix)))
                                           (incf total score)
                                           (cons string score)))
                                       collection)
                               (lambda (a b) (< (cdr b) (cdr a))))))
    (if threshold
        (cons n result)
      result)))

(defun ac-comphist-serialize (db)
  (let (alist)
    (maphash (lambda (k v)
               (push (cons k v) alist))
             (ac-comphist-tab db))
    (list alist)))

(defun ac-comphist-deserialize (sexp)
  (condition-case nil
      (ac-comphist-make (let ((tab (ac-comphist-make-tab)))
                          (mapc (lambda (cons)
                                  (puthash (car cons) (cdr cons) tab))
                                (nth 0 sexp))
                          tab))
    (error (message "Invalid comphist db.") nil)))

(defun ac-comphist-init ()
  (ac-comphist-load)
  (add-hook 'kill-emacs-hook 'ac-comphist-save))

(defun ac-comphist-load ()
  (interactive)
  (let ((db (if (file-exists-p ac-comphist-file)
                (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents ac-comphist-file)
                    (goto-char (point-min))
                    (ac-comphist-deserialize (read (current-buffer))))))))
    (setq ac-comphist (or db (ac-comphist-make)))))

(defun ac-comphist-save ()
  (interactive)
  (require 'pp)
  (ignore-errors
    (with-temp-buffer
      (pp (ac-comphist-serialize ac-comphist) (current-buffer))
      (write-region (point-min) (point-max) ac-comphist-file))))



;;;; Auto completion internals

(defun ac-menu-at-wrapper-line-p ()
  "Return non-nil if current line is long and wrapped to next visual line."
  (and (not truncate-lines)
       (eq (line-beginning-position)
           (save-excursion
             (vertical-motion 1)
             (line-beginning-position)))))

(defun ac-prefix-symbol ()
  "Default prefix definition function."
  (require 'thingatpt)
  (car-safe (bounds-of-thing-at-point 'symbol)))
(defalias 'ac-prefix-default 'ac-prefix-symbol)

(defun ac-prefix-file ()
  "File prefix."
  (let ((point (re-search-backward "[\"<>' \t\r\n]" nil t)))
    (if point (1+ point))))

(defun ac-prefix-valid-file ()
  "Existed (or to be existed) file prefix."
  (let* ((line-beg (line-beginning-position))
         (end (point))
         (start (or (let ((point (re-search-backward "[\"<>'= \t\r\n]" line-beg t)))
                      (if point (1+ point)))
                    line-beg))
         (file (buffer-substring start end)))
    (if (and file (or (string-match "^/" file)
                      (and (setq file (and (string-match "^[^/]*/" file)
                                           (match-string 0 file)))
                           (file-directory-p file))))
        start)))

(defun ac-prefix-c-dot ()
  "C-like languages dot(.) prefix."
  (if (re-search-backward "\\.\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-prefix-c-dot-ref ()
  "C-like languages dot(.) and reference(->) prefix."
  (if (re-search-backward "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-define-prefix (name prefix)
  "Define new prefix definition.
You can not use it in source definition like (prefix . `NAME')."
  (push (cons name prefix) ac-prefix-definitions))

(defun ac-match-substring (prefix candidates)
  (loop with regexp = (regexp-quote prefix)
        for candidate in candidates
        if (string-match regexp candidate)
        collect candidate))

(defsubst ac-source-entity (source)
  (if (symbolp source)
      (symbol-value source)
    source))

(defun ac-source-available-p (source)
  (if (and (symbolp source)
           (get source 'available))
      (eq (get source 'available) t)
    (let* ((src (ac-source-entity source))
           (avail-pair (assq 'available src))
           (avail-cond (cdr avail-pair))
           (available (and (if avail-pair
                               (cond
                                ((symbolp avail-cond)
                                 (funcall avail-cond))
                                ((listp avail-cond)
                                 (eval avail-cond)))
                             t)
                           (loop for feature in (assoc-default 'depends src)
                                 unless (require feature nil t) return nil
                                 finally return t))))
      (if (symbolp source)
          (put source 'available (if available t 'no)))
      available)))

(defun ac-compile-sources (sources)
  "Compiled `SOURCES' into expanded sources style."
  (loop for source in sources
        if (ac-source-available-p source)
        do
        (setq source (ac-source-entity source))
        (flet ((add-attribute (name value &optional append) (add-to-list 'source (cons name value) append)))
          ;; prefix
          (let* ((prefix (assoc 'prefix source))
                 (real (assoc-default (cdr prefix) ac-prefix-definitions)))
            (cond
             (real
              (add-attribute 'prefix real))
             ((null prefix)
              (add-attribute 'prefix 'ac-prefix-default))))
          ;; match
          (let ((match (assq 'match source)))
            (cond
             ((eq (cdr match) 'substring)
              (setcdr match 'ac-match-substring)))))
        and collect source))

(defun ac-compiled-sources ()
  (or ac-compiled-sources
      (setq ac-compiled-sources
            (ac-compile-sources ac-sources))))

(defsubst ac-menu-live-p ()
  (popup-live-p ac-menu))

(defun ac-menu-create (point width height)
  (setq ac-menu
        (popup-create point width height
                      :around t
                      :face 'ac-candidate-face
                      :selection-face 'ac-selection-face
                      :symbol t
                      :scroll-bar t
                      :margin-left 1)))

(defun ac-menu-delete ()
  (when ac-menu
    (popup-delete ac-menu)
    (setq ac-menu)))

(defsubst ac-inline-marker ()
  (nth 0 ac-inline))

(defsubst ac-inline-overlay ()
  (nth 1 ac-inline))

(defsubst ac-inline-live-p ()
  (and ac-inline (ac-inline-overlay) t))

(defun ac-inline-show (point string)
  (unless ac-inline
    (setq ac-inline (list (make-marker) nil)))
  (save-excursion
    (let ((overlay (ac-inline-overlay))
          (width 0)
          (string-width (string-width string))
          (length 0)
          (original-string string))
      ;; Calculate string space to show completion
      (goto-char point)
      (let (c)
        (while (and (not (eolp))
                    (< width string-width)
                    (setq c (char-after))
                    (not (eq c ?\t)))   ; special case for tab
        (incf width (char-width c))
        (incf length)
        (forward-char)))

      ;; Show completion
      (goto-char point)
      (cond
       ((= width 0)
        (set-marker (ac-inline-marker) point)
        (let ((buffer-undo-list t))
          (insert " "))
        (setq width 1
              length 1))
       ((<= width string-width)
        ;; No space to show
        ;; Do nothing
        )
       ((> width string-width)
        ;; Need to fill space
        (setq string (concat string (make-string (- width string-width) ? )))))
      (setq string (propertize string 'face 'ac-completion-face))
      (if overlay
          (progn
            (move-overlay overlay point (+ point length))
            (overlay-put overlay 'invisible nil))
        (setq overlay (make-overlay point (+ point length)))
        (setf (nth 1 ac-inline)  overlay)
        (overlay-put overlay 'priority 9999)
        ;; Help prefix-overlay in some cases
        (overlay-put overlay 'keymap ac-current-map))
      (overlay-put overlay 'display (substring string 0 1))
      ;; TODO no width but char
      (overlay-put overlay 'after-string (substring string 1))
      (overlay-put overlay 'string original-string))))

(defun ac-inline-delete ()
  (when (ac-inline-live-p)
    (ac-inline-hide)
    (delete-overlay (ac-inline-overlay))
    (setq ac-inline nil)))

(defun ac-inline-hide ()
  (when (ac-inline-live-p)
    (let ((overlay (ac-inline-overlay))
          (marker (ac-inline-marker))
          (buffer-undo-list t))
      (when overlay
        (when (marker-position marker)
          (save-excursion
            (goto-char marker)
            (delete-char 1)
            (set-marker marker nil)))
        (move-overlay overlay (point-min) (point-min))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'after-string nil)))))

(defun ac-inline-update ()
  (if (and ac-completing ac-prefix (stringp ac-common-part))
      (let ((common-part-length (length ac-common-part))
            (prefix-length (length ac-prefix)))
        (if (> common-part-length prefix-length)
            (progn
              (ac-inline-hide)
              (ac-inline-show (point) (substring ac-common-part prefix-length)))
          (ac-inline-delete)))
    (ac-inline-delete)))

(defun ac-put-prefix-overlay ()
  (unless ac-prefix-overlay
    (let (newline)
      ;; Insert newline to make sure that cursor always on the overlay
      (when (and (eq ac-point (point-max))
                 (eq ac-point (point)))
        (popup-save-buffer-state
          (insert "\n"))
        (setq newline t))
      (setq ac-prefix-overlay (make-overlay ac-point (1+ (point)) nil t t))
      (overlay-put ac-prefix-overlay 'priority 9999)
      (overlay-put ac-prefix-overlay 'keymap (make-sparse-keymap))
      (overlay-put ac-prefix-overlay 'newline newline))))

(defun ac-remove-prefix-overlay ()
  (when ac-prefix-overlay
    (when (overlay-get ac-prefix-overlay 'newline)
      ;; Remove inserted newline
      (popup-save-buffer-state
        (goto-char (point-max))
        (if (eq (char-before) ?\n)
            (delete-char -1))))
    (delete-overlay ac-prefix-overlay)))

(defun ac-activate-completing-map ()
  (if (and ac-show-menu ac-use-menu-map)
      (set-keymap-parent ac-current-map ac-menu-map))
  (when (and ac-use-overriding-local-map
             (null overriding-terminal-local-map))
    (setq overriding-terminal-local-map ac-current-map))
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) ac-current-map)))

(defun ac-deactivate-completing-map ()
  (set-keymap-parent ac-current-map ac-completing-map)
  (when (and ac-use-overriding-local-map
             (eq overriding-terminal-local-map ac-current-map))
    (setq overriding-terminal-local-map nil))
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) nil)))

(defsubst ac-selected-candidate ()
  (if ac-menu
      (popup-selected-item ac-menu)))

(defun ac-prefix (requires ignore-list)
  (loop with current = (point)
        with point
        with prefix-def
        with sources
        for source in (ac-compiled-sources)
        for prefix = (assoc-default 'prefix source)
        for req = (or (assoc-default 'requires source) requires 1)

        if (null prefix-def)
        do
        (unless (member prefix ignore-list)
          (save-excursion
            (setq point (cond
                         ((symbolp prefix)
                          (funcall prefix))
                         ((stringp prefix)
                          (and (re-search-backward (concat prefix "\\=") nil t)
                               (or (match-beginning 1) (match-beginning 0))))
                         ((stringp (car-safe prefix))
                          (let ((regexp (nth 0 prefix))
                                (end (nth 1 prefix))
                                (group (nth 2 prefix)))
                            (and (re-search-backward (concat regexp "\\=") nil t)
                                 (funcall (if end 'match-end 'match-beginning)
                                          (or group 0)))))
                         (t
                          (eval prefix))))
            (if (and point
                     (integerp req)
                     (< (- current point) req))
                (setq point nil))
            (if point
                (setq prefix-def prefix))))
        
        if (equal prefix prefix-def) do (push source sources)

        finally return
        (and point (list prefix-def point (nreverse sources)))))

(defun ac-init ()
  "Initialize current sources to start completion."
  (setq ac-candidates-cache nil)
  (loop for source in ac-current-sources
        for function = (assoc-default 'init source)
        if function do
        (save-excursion
          (cond
           ((functionp function)
            (funcall function))
           (t
            (eval function))))))

(defun ac-candidates-1 (source)
  (let* ((do-cache (assq 'cache source))
         (function (assoc-default 'candidates source))
         (action (assoc-default 'action source))
         (document (assoc-default 'document source))
         (symbol (assoc-default 'symbol source))
         (ac-limit (or (assoc-default 'limit source) ac-limit))
         (face (or (assoc-default 'face source) (assoc-default 'candidate-face source)))
         (selection-face (assoc-default 'selection-face source))
         (cache (and do-cache (assq source ac-candidates-cache)))
         (candidates (cdr cache)))
    (unless cache
      (setq candidates (save-excursion
                         (cond
                          ((functionp function)
                           (funcall function))
                          (t
                           (eval function)))))
      ;; Convert (name value) format candidates into name with text properties.
      (setq candidates (mapcar (lambda (candidate)
                                 (if (consp candidate)
                                     (propertize (car candidate) 'value (cdr candidate))
                                   candidate))
                               candidates))
      (when do-cache
        (push (cons source candidates) ac-candidates-cache)))
    (setq candidates (funcall (or (assoc-default 'match source)
                                  ac-match-function)
                              ac-prefix candidates))
    ;; Remove extra items regarding to ac-limit
    (if (and (integerp ac-limit) (> ac-limit 1) (> (length candidates) ac-limit))
        (setcdr (nthcdr (1- ac-limit) candidates) nil))
    ;; Put candidate properties
    (setq candidates (mapcar (lambda (candidate)
                               (popup-item-propertize candidate
                                                      'action action
                                                      'symbol symbol
                                                      'document document
                                                      'popup-face face
                                                      'selection-face selection-face))
                             candidates))
    candidates))

(defun ac-candidates ()
  "Produce candidates for current sources."
  (loop with completion-ignore-case = (or (eq ac-ignore-case t)
                                          (and (eq ac-ignore-case 'smart)
                                               (let ((case-fold-search nil)) (not (string-match "[[:upper:]]" ac-prefix)))))
        with case-fold-search = completion-ignore-case
        with prefix-len = (length ac-prefix)
        for source in ac-current-sources
        append (ac-candidates-1 source) into candidates
        finally return
        (progn
          (delete-dups candidates)
          (if (and ac-use-comphist ac-comphist)
              (if ac-show-menu
                  (let* ((pair (ac-comphist-sort ac-comphist candidates prefix-len ac-comphist-threshold))
                         (n (car pair))
                         (result (cdr pair))
                         (cons (if (> n 0) (nthcdr (1- n) result)))
                         (cdr (cdr cons)))
                    (if cons (setcdr cons nil))
                    (setq ac-common-part (try-completion ac-prefix result))
                    (setq ac-whole-common-part (try-completion ac-prefix candidates))
                    (if cons (setcdr cons cdr))
                    result)
                (setq candidates (ac-comphist-sort ac-comphist candidates prefix-len))
                (setq ac-common-part (if candidates (popup-x-to-string (car candidates))))
                (setq ac-whole-common-part (try-completion ac-prefix candidates))
                candidates)
            (setq ac-common-part (try-completion ac-prefix candidates))
            (setq ac-whole-common-part ac-common-part)
            candidates))))

(defun ac-update-candidates (cursor scroll-top)
  "Update candidates of menu to `ac-candidates' and redraw it."
  (setf (popup-cursor ac-menu) cursor
        (popup-scroll-top ac-menu) scroll-top)
  (setq ac-dwim-enable (= (length ac-candidates) 1))
  (if ac-candidates
      (progn
        (setq ac-completing t)
        (ac-activate-completing-map))
    (setq ac-completing nil)
    (ac-deactivate-completing-map))
  (ac-inline-update)
  (popup-set-list ac-menu ac-candidates)
  (if (and (not ac-fuzzy-enable)
           (<= (length ac-candidates) 1))
      (popup-hide ac-menu)
    (if ac-show-menu
        (popup-draw ac-menu))))

(defun ac-reposition ()
  "Force to redraw candidate menu with current `ac-candidates'."
  (let ((cursor (popup-cursor ac-menu))
        (scroll-top (popup-scroll-top ac-menu)))
    (ac-menu-delete)
    (ac-menu-create ac-point (popup-preferred-width ac-candidates) (popup-height ac-menu))
    (ac-update-candidates cursor scroll-top)))

(defun ac-cleanup ()
  "Cleanup auto completion."
  (if ac-cursor-color
      (set-cursor-color ac-cursor-color))
  (when (and ac-use-comphist ac-comphist)
    (when (and (null ac-selected-candidate)
               (member ac-prefix ac-candidates))
      ;; Assume candidate is selected by just typing
      (setq ac-selected-candidate ac-prefix)
      (setq ac-last-point ac-point))
    (when ac-selected-candidate
      (ac-comphist-add ac-comphist
                       ac-selected-candidate
                       (if ac-last-point
                           (- ac-last-point ac-point)
                         (length ac-prefix)))))
  (ac-deactivate-completing-map)
  (ac-remove-prefix-overlay)
  (ac-remove-quick-help)
  (ac-inline-delete)
  (ac-menu-delete)
  (ac-cancel-timer)
  (ac-cancel-show-menu-timer)
  (ac-cancel-quick-help-timer)
  (setq ac-cursor-color nil
        ac-inline nil
        ac-show-menu nil
        ac-menu nil
        ac-completing nil
        ac-point nil
        ac-last-point nil
        ac-prefix nil
        ac-prefix-overlay nil
        ac-selected-candidate nil
        ac-common-part nil
        ac-whole-common-part nil
        ac-triggered nil
        ac-limit nil
        ac-candidates nil
        ac-candidates-cache nil
        ac-fuzzy-enable nil
        ac-dwim-enable nil
        ac-compiled-sources nil
        ac-current-sources nil
        ac-current-prefix-def nil
        ac-ignoring-prefix-def nil))

(defsubst ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-expand-string (string &optional remove-undo-boundary)
  "Expand `STRING' into the buffer and update `ac-prefix' to `STRING'.
This function records deletion and insertion sequences by `undo-boundary'.
If `remove-undo-boundary' is non-nil, this function also removes `undo-boundary'
that have been made before in this function."
  (when (not (equal string (buffer-substring ac-point (point))))
    (undo-boundary)
    ;; We can't use primitive-undo since it undoes by
    ;; groups, divided by boundaries.
    ;; We don't want boundary between deletion and insertion.
    ;; So do it manually.
    ;; Delete region silently for undo:
    (if remove-undo-boundary
        (progn
          (let (buffer-undo-list)
            (save-excursion
              (delete-region ac-point (point))))
          (setq buffer-undo-list
                (nthcdr 2 buffer-undo-list)))
      (delete-region ac-point (point)))
    (insert string)
    ;; Sometimes, possible when omni-completion used, (insert) added
    ;; to buffer-undo-list strange record about position changes.
    ;; Delete it here:
    (when (and remove-undo-boundary
               (integerp (cadr buffer-undo-list)))
      (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))
    (undo-boundary)
    (setq ac-selected-candidate string)
    (setq ac-prefix string)))

(defun ac-set-trigger-key (key)
  "Set `ac-trigger-key' to `KEY'. It is recommemded to use this function instead of calling `setq'."
  ;; Remove old mapping
  (when ac-trigger-key
    (define-key ac-mode-map (read-kbd-macro ac-trigger-key) nil))

  ;; Make new mapping
  (setq ac-trigger-key key)
  (when key
    (define-key ac-mode-map (read-kbd-macro key) 'ac-trigger-key-command)))

(defun ac-set-timer ()
  (unless ac-timer
    (setq ac-timer (run-with-idle-timer ac-delay ac-delay 'ac-update-greedy))))

(defun ac-cancel-timer ()
  (when (timerp ac-timer)
    (cancel-timer ac-timer)
    (setq ac-timer nil)))

(defun ac-update (&optional force)
  (when (and auto-complete-mode
             ac-prefix
             (or ac-triggered
                 force)
             (not isearch-mode))
    (ac-put-prefix-overlay)
    (setq ac-candidates (ac-candidates))
    (let ((preferred-width (popup-preferred-width ac-candidates)))
      ;; Reposition if needed
      (when (or (null ac-menu)
                (>= (popup-width ac-menu) preferred-width)
                (<= (popup-width ac-menu) (- preferred-width 10))
                (and (> (popup-direction ac-menu) 0)
                     (ac-menu-at-wrapper-line-p)))
        (ac-inline-hide) ; Hide overlay to calculate correct column
        (ac-menu-delete)
        (ac-menu-create ac-point preferred-width ac-menu-height)))
    (ac-update-candidates 0 0)
    t))

(defun ac-update-greedy (&optional force)
  (let (result)
    (while (when (and (setq result (ac-update force))
                      (null ac-candidates))
             (add-to-list 'ac-ignoring-prefix-def ac-current-prefix-def)
             (ac-start :force-init t)
             ac-current-prefix-def))
    result))

(defun ac-set-show-menu-timer ()
  (when (and (or (integerp ac-auto-show-menu) (floatp ac-auto-show-menu))
             (null ac-show-menu-timer))
    (setq ac-show-menu-timer (run-with-idle-timer ac-auto-show-menu ac-auto-show-menu 'ac-show-menu))))

(defun ac-cancel-show-menu-timer ()
  (when (timerp ac-show-menu-timer)
    (cancel-timer ac-show-menu-timer)
    (setq ac-show-menu-timer nil)))

(defun ac-show-menu ()
  (when (not (eq ac-show-menu t))
    (setq ac-show-menu t)
    (ac-inline-hide)
    (ac-remove-quick-help)
    (ac-update t)))

(defun ac-help (&optional persist)
  (interactive "P")
  (when ac-menu
    (popup-menu-show-help ac-menu persist)))

(defun ac-persist-help ()
  (interactive)
  (ac-help t))

(defun ac-last-help (&optional persist)
  (interactive "P")
  (when ac-last-completion
    (popup-item-show-help (cdr ac-last-completion) persist)))

(defun ac-last-persist-help ()
  (interactive)
  (ac-last-help t))

(defun ac-set-quick-help-timer ()
  (when (and ac-use-quick-help
             (null ac-quick-help-timer))
    (setq ac-quick-help-timer (run-with-idle-timer ac-quick-help-delay ac-quick-help-delay 'ac-quick-help))))

(defun ac-cancel-quick-help-timer ()
  (when (timerp ac-quick-help-timer)
    (cancel-timer ac-quick-help-timer)
    (setq ac-quick-help-timer nil)))

(defun ac-pos-tip-show-quick-help (menu &optional item &rest args)
  (let* ((point (plist-get args :point))
         (around nil)
         (parent-offset (popup-offset menu))
         (doc (popup-menu-documentation menu item)))
    (when (stringp doc)
      (if (popup-hidden-p menu)
          (setq around t)
        (setq point nil))
      (with-no-warnings
        (pos-tip-show doc
                      'popup-tip-face
                      (or point
                          (and menu
                               (popup-child-point menu parent-offset))
                          (point))
                      nil 0
                      popup-tip-max-width
                      nil nil
                      (and (not around) 0))
        (unless (plist-get args :nowait)
          (clear-this-command-keys)
          (unwind-protect
              (push (read-event (plist-get args :prompt)) unread-command-events)
            (pos-tip-hide))
          t)))))

(defun ac-quick-help (&optional force)
  (interactive)
  (when (and (or force (null this-command))
             (ac-menu-live-p)
             (null ac-quick-help))
      (setq ac-quick-help
            (funcall (if (and ac-quick-help-prefer-x
                              (eq window-system 'x)
                              (featurep 'pos-tip))
                         'ac-pos-tip-show-quick-help
                       'popup-menu-show-quick-help)
                     ac-menu nil
                     :point ac-point
                     :height ac-quick-help-height
                     :nowait t))))

(defun ac-remove-quick-help ()
  (when ac-quick-help
    (popup-delete ac-quick-help)
    (setq ac-quick-help nil)))

(defun ac-last-quick-help ()
  (interactive)
  (when (and ac-last-completion
             (eq (marker-buffer (car ac-last-completion))
                 (current-buffer)))
    (let ((doc (popup-item-documentation (cdr ac-last-completion)))
          (point (marker-position (car ac-last-completion))))
      (when (stringp doc)
        (if (and ac-quick-help-prefer-x
                 (eq window-system 'x)
                 (featurep 'pos-tip))
            (with-no-warnings (pos-tip-show doc nil point nil 0))
          (popup-tip doc
                     :point point
                     :around t
                     :scroll-bar t
                     :margin t))))))

(defmacro ac-define-quick-help-command (name arglist &rest body)
  (declare (indent 2))
  `(progn
     (defun ,name ,arglist ,@body)
     (put ',name 'ac-quick-help-command t)))

(ac-define-quick-help-command ac-quick-help-scroll-down ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-down ac-quick-help)))

(ac-define-quick-help-command ac-quick-help-scroll-up ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-up ac-quick-help)))



;;;; Auto completion isearch

(defun ac-isearch-callback (list)
  (setq ac-dwim-enable (eq (length list) 1)))

(defun ac-isearch ()
  (interactive)
  (when (ac-menu-live-p)
    (ac-cancel-show-menu-timer)
    (ac-cancel-quick-help-timer)
    (ac-show-menu)
    (popup-isearch ac-menu :callback 'ac-isearch-callback)))



;;;; Auto completion commands

(defun auto-complete (&optional sources)
  "Start auto-completion at current point."
  (interactive)
  (let ((menu-live (ac-menu-live-p))
        (inline-live (ac-inline-live-p)))
    (ac-abort)
    (let ((ac-sources (or sources ac-sources)))
      (if (or ac-show-menu-immediately-on-auto-complete
              inline-live)
          (setq ac-show-menu t))
      (ac-start))
    (when (ac-update-greedy t)
      ;; TODO Not to cause inline completion to be disrupted.
      (if (ac-inline-live-p)
          (ac-inline-hide))
      ;; Not to expand when it is first time to complete
      (when (and (or (and (not ac-expand-on-auto-complete)
                          (> (length ac-candidates) 1)
                          (not menu-live))
                     (not (let ((ac-common-part ac-whole-common-part))
                            (ac-expand-common))))
                 ac-use-fuzzy
                 (null ac-candidates))
        (ac-fuzzy-complete)))))

(defun ac-fuzzy-complete ()
  "Start fuzzy completion at current point."
  (interactive)
  (when (require 'fuzzy nil)
    (unless (ac-menu-live-p)
      (ac-start))
    (let ((ac-match-function 'fuzzy-all-completions))
      (unless ac-cursor-color
        (setq ac-cursor-color (frame-parameter (selected-frame) 'cursor-color)))
      (if ac-fuzzy-cursor-color
          (set-cursor-color ac-fuzzy-cursor-color))
      (setq ac-show-menu t)
      (setq ac-fuzzy-enable t)
      (setq ac-triggered nil)
      (ac-update t)))
  t)

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (popup-next ac-menu)
    (setq ac-show-menu t)
    (if (eq this-command 'ac-next)
        (setq ac-dwim-enable t))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (popup-previous ac-menu)
    (setq ac-show-menu t)
    (if (eq this-command 'ac-previous)
        (setq ac-dwim-enable t))))

(defun ac-expand ()
  "Try expand, and if expanded twice, select next candidate."
  (interactive)
  (unless (ac-expand-common)
    (let ((string (ac-selected-candidate)))
      (when string
        (when (equal ac-prefix string)
          (ac-next)
          (setq string (ac-selected-candidate)))
        (ac-expand-string string (eq last-command this-command))
        ;; Do reposition if menu at long line
        (if (and (> (popup-direction ac-menu) 0)
                 (ac-menu-at-wrapper-line-p))
            (ac-reposition))
        (setq ac-show-menu t)
        string))))

(defun ac-expand-common ()
  "Try to expand meaningful common part."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (when (and (ac-inline-live-p)
               ac-common-part)
      (ac-inline-hide) 
      (ac-expand-string ac-common-part (eq last-command this-command))
      (setq ac-common-part nil)
      t)))

(defun ac-complete ()
  "Try complete."
  (interactive)
  (let* ((candidate (ac-selected-candidate))
         (action (popup-item-property candidate 'action))
         (fallback nil))
    (when candidate
      (unless (ac-expand-string candidate)
        (setq fallback t))
      ;; Remember to show help later
      (when (and ac-point candidate)
        (unless ac-last-completion
          (setq ac-last-completion (cons (make-marker) nil)))
        (set-marker (car ac-last-completion) ac-point ac-buffer)
        (setcdr ac-last-completion candidate)))
    (ac-abort)
    (cond
     (action
      (funcall action))
     (fallback
      (ac-fallback-command)))
    candidate))

(defun* ac-start (&key
                  requires
                  force-init)
  "Start completion."
  (interactive)
  (if (not auto-complete-mode)
      (message "auto-complete-mode is not enabled")
    (let* ((info (ac-prefix requires ac-ignoring-prefix-def))
           (prefix-def (nth 0 info))
           (point (nth 1 info))
           (sources (nth 2 info))
           prefix
           (init (or force-init (not (eq ac-point point)))))
      (if (or (null point)
              (member (setq prefix (buffer-substring-no-properties point (point)))
                      ac-ignores))
          (prog1 nil
            (ac-abort))
        (unless ac-cursor-color
          (setq ac-cursor-color (frame-parameter (selected-frame) 'cursor-color)))
        (setq ac-show-menu (or ac-show-menu (if (eq ac-auto-show-menu t) t))
              ac-current-sources sources
              ac-buffer (current-buffer)
              ac-point point
              ac-prefix prefix
              ac-limit ac-candidate-limit
              ac-triggered t
              ac-current-prefix-def prefix-def)
        (when (or init (null ac-prefix-overlay))
          (ac-init))
        (ac-set-timer)
        (ac-set-show-menu-timer)
        (ac-set-quick-help-timer)
        (ac-put-prefix-overlay)))))

(defun ac-stop ()
  "Stop completiong."
  (interactive)
  (setq ac-selected-candidate nil)
  (ac-abort))

(defun ac-trigger-key-command (&optional force)
  (interactive "P")
  (if (or force (ac-trigger-command-p last-command))
      (auto-complete)
    (ac-fallback-command 'ac-trigger-key-command)))



;;;; Basic cache facility

(defvar ac-clear-variables-every-minute-timer nil)
(defvar ac-clear-variables-after-save nil)
(defvar ac-clear-variables-every-minute nil)
(defvar ac-minutes-counter 0)

(defun ac-clear-variable-after-save (variable &optional pred)
  (add-to-list 'ac-clear-variables-after-save (cons variable pred)))

(defun ac-clear-variables-after-save ()
  (dolist (pair ac-clear-variables-after-save)
    (if (or (null (cdr pair))
            (funcall (cdr pair)))
        (set (car pair) nil))))

(defun ac-clear-variable-every-minutes (variable minutes)
  (add-to-list 'ac-clear-variables-every-minute (cons variable minutes)))

(defun ac-clear-variable-every-minute (variable)
  (ac-clear-variable-every-minutes variable 1))

(defun ac-clear-variable-every-10-minutes (variable)
  (ac-clear-variable-every-minutes variable 10))

(defun ac-clear-variables-every-minute ()
  (incf ac-minutes-counter)
  (dolist (pair ac-clear-variables-every-minute)
    (if (eq (% ac-minutes-counter (cdr pair)) 0)
        (set (car pair) nil))))



;;;; Auto complete mode

(defun ac-cursor-on-diable-face-p (&optional point)
  (memq (get-text-property (or point (point)) 'face) ac-disable-faces))

(defun ac-trigger-command-p (command)
  "Return non-nil if `COMMAND' is a trigger command."
  (and (symbolp command)
       (or (memq command ac-trigger-commands)
           (string-match "self-insert-command" (symbol-name command))
           (string-match "electric" (symbol-name command)))))

(defun ac-fallback-command (&optional except-command)
  (let* ((auto-complete-mode nil)
         (keys (this-command-keys-vector))
         (command (if keys (key-binding keys))))
    (when (and (commandp command)
               (not (eq command except-command)))
      (setq this-command command)
      (call-interactively command))))

(defun ac-compatible-package-command-p (command)
  "Return non-nil if `COMMAND' is compatible with auto-complete."
  (and (symbolp command)
       (string-match ac-compatible-packages-regexp (symbol-name command))))

(defun ac-handle-pre-command ()
  (condition-case var
      (if (or (setq ac-triggered (and (not ac-fuzzy-enable) ; ignore key storkes in fuzzy mode
                                      (or (eq this-command 'auto-complete) ; special case
                                          (ac-trigger-command-p this-command)
                                          (and ac-completing
                                               (memq this-command ac-trigger-commands-on-completing)))
                                      (not (ac-cursor-on-diable-face-p))))
              (ac-compatible-package-command-p this-command))
          (progn
            (if (or (not (symbolp this-command))
                    (not (get this-command 'ac-quick-help-command)))
                (ac-remove-quick-help))
            ;; Not to cause inline completion to be disrupted.
            (ac-inline-hide))
        (ac-abort))
    (error (ac-error var))))

(defun ac-handle-post-command ()
  (condition-case var
      (when (and ac-triggered
                 (or ac-auto-start
                     ac-completing)
                 (not isearch-mode))
        (setq ac-last-point (point))
        (ac-start :requires (unless ac-completing ac-auto-start))
        (ac-inline-update))
    (error (ac-error var))))

(defun ac-setup ()
  (if ac-trigger-key
      (ac-set-trigger-key ac-trigger-key))
  (if ac-use-comphist
      (ac-comphist-init))
  (unless ac-clear-variables-every-minute-timer
    (setq ac-clear-variables-every-minute-timer (run-with-timer 60 60 'ac-clear-variables-every-minute)))
  (if ac-stop-flymake-on-completing
      (defadvice flymake-on-timer-event (around ac-flymake-stop-advice activate)
        (unless ac-completing
          ad-do-it))
    (ad-disable-advice 'flymake-on-timer-event 'around 'ac-flymake-stop-advice)))

(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :keymap ac-mode-map
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (ac-setup)
        (add-hook 'pre-command-hook 'ac-handle-pre-command nil t)
        (add-hook 'post-command-hook 'ac-handle-post-command nil t)
        (add-hook 'after-save-hook 'ac-clear-variables-after-save nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'pre-command-hook 'ac-handle-pre-command t)
    (remove-hook 'post-command-hook 'ac-handle-post-command t)
    (remove-hook 'after-save-hook 'ac-clear-variables-after-save t)
    (ac-abort)))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

(define-global-minor-mode global-auto-complete-mode
  auto-complete-mode auto-complete-mode-maybe
  :group 'auto-complete)



;;;; Compatibilities with other extensions

(defun ac-flyspell-workaround ()
  "Flyspell uses `sit-for' for delaying its process. Unfortunatelly,
it stops auto completion which is trigger with `run-with-idle-timer'.
This workaround avoid flyspell processes when auto completion is being started."
  (interactive)
  (defadvice flyspell-post-command-hook (around ac-flyspell-workaround activate)
    (unless ac-triggered
      ad-do-it)))



;;;; Standard sources

(defmacro ac-define-source (name source)
  "Source definition macro. It defines a complete command also."
  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "ac-source-%s" name))
       ,source)
     (defun ,(intern (format "ac-complete-%s" name)) ()
       (interactive)
       (auto-complete '(,(intern (format "ac-source-%s" name)))))))

;; Words in buffer source
(defvar ac-word-index nil)

(defun ac-candidate-words-in-buffer (point prefix limit)
  (let ((i 0)
        candidate
        candidates
        (regexp (concat "\\_<" (regexp-quote prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
    (save-excursion
      ;; Search backward
      (goto-char point)
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-backward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      ;; Search backward
      (goto-char (+ point (length prefix)))
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      (nreverse candidates))))

(defun ac-incremental-update-word-index ()
  (unless (local-variable-p 'ac-word-index)
    (make-local-variable 'ac-word-index))
  (if (null ac-word-index)
      (setq ac-word-index (cons nil nil)))
  ;; Mark incomplete
  (if (car ac-word-index)
      (setcar ac-word-index nil))
  (let ((index (cdr ac-word-index))
        (words (ac-candidate-words-in-buffer ac-point ac-prefix (or (and (integerp ac-limit) ac-limit) 10))))
    (dolist (word words)
      (unless (member word index)
        (push word index)
        (setcdr ac-word-index index)))))

(defun ac-update-word-index-1 ()
  (unless (local-variable-p 'ac-word-index)
    (make-local-variable 'ac-word-index))
  (when (and (not (car ac-word-index))
             (< (buffer-size) 1048576))
    ;; Complete index
    (setq ac-word-index
          (cons t
                (split-string (buffer-substring-no-properties (point-min) (point-max))
                              "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)")))))

(defun ac-update-word-index ()
  (dolist (buffer (buffer-list))
    (when (or ac-fuzzy-enable
              (not (eq buffer (current-buffer))))
      (with-current-buffer buffer
        (ac-update-word-index-1)))))

(defun ac-word-candidates (&optional buffer-pred)
  (loop initially (unless ac-fuzzy-enable (ac-incremental-update-word-index))
        for buffer in (buffer-list)
        if (and (or (not (integerp ac-limit)) (< (length candidates) ac-limit))
                (if buffer-pred (funcall buffer-pred buffer) t))
        append (funcall ac-match-function
                        ac-prefix
                        (and (local-variable-p 'ac-word-index buffer)
                             (cdr (buffer-local-value 'ac-word-index buffer))))
        into candidates
        finally return candidates))

(ac-define-source words-in-buffer
  '((candidates . ac-word-candidates)))

(ac-define-source words-in-all-buffer
  '((init . ac-update-word-index)
    (candidates . ac-word-candidates)))

(ac-define-source words-in-same-mode-buffers
  '((init . ac-update-word-index)
    (candidates . (ac-word-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))))))

;; Lisp symbols source
(defvar ac-symbols-cache nil)
(ac-clear-variable-every-10-minutes 'ac-symbols-cache)

(defun ac-symbol-file (symbol type)
  (if (fboundp 'find-lisp-object-file-name)
      (find-lisp-object-file-name symbol type)
    (let ((file-name (with-no-warnings
                       (describe-simplify-lib-file-name
                        (symbol-file symbol type)))))
      (when (equal file-name "loaddefs.el")
        ;; Find the real def site of the preloaded object.
        (let ((location (condition-case nil
                            (if (eq type 'defun)
                                (find-function-search-for-symbol symbol nil
                                                                 "loaddefs.el")
                              (find-variable-noselect symbol file-name))
                          (error nil))))
          (when location
            (with-current-buffer (car location)
              (when (cdr location)
                (goto-char (cdr location)))
              (when (re-search-backward
                     "^;;; Generated autoloads from \\(.*\\)" nil t)
                (setq file-name (match-string 1)))))))
      (if (and (null file-name)
               (or (eq type 'defun)
                   (integerp (get symbol 'variable-documentation))))
          ;; It's a object not defined in Elisp but in C.
          (if (get-buffer " *DOC*")
              (if (eq type 'defun)
                  (help-C-file-name (symbol-function symbol) 'subr)
                (help-C-file-name symbol 'var))
            'C-source)
        file-name))))

(defun ac-symbol-documentation (symbol)
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))
  (ignore-errors
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (prin1 symbol)
        (princ " is ")
        (cond
         ((fboundp symbol)
          (let ((help-xref-following t))
            (describe-function-1 symbol))
          (buffer-string))
         ((boundp symbol)
          (let ((file-name  (ac-symbol-file symbol 'defvar)))
            (princ "a variable")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'variable-documentation t)
                       "Not documented."))
            (buffer-string)))
         ((facep symbol)
          (let ((file-name  (ac-symbol-file symbol 'defface)))
            (princ "a face")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'face-documentation t)
                       "Not documented."))
            (buffer-string)))
         (t
          (let ((doc (documentation-property symbol 'group-documentation t)))
            (when doc
              (princ "a group.\n\n")
              (princ doc)
              (buffer-string)))))))))

(defun ac-symbol-candidates ()
  (or ac-symbols-cache
      (setq ac-symbols-cache
            (loop for x being the symbols
                  if (or (fboundp x)
                         (boundp x)
                         (symbol-plist x))
                  collect (symbol-name x)))))

(ac-define-source symbols
  '((candidates . ac-symbol-candidates)
    (document . ac-symbol-documentation)
    (symbol . "s")
    (cache)))

;; Lisp functions source
(defvar ac-functions-cache nil)
(ac-clear-variable-every-10-minutes 'ac-functions-cache)

(defun ac-function-candidates ()
  (or ac-functions-cache
      (setq ac-functions-cache
            (loop for x being the symbols
                  if (fboundp x)
                  collect (symbol-name x)))))

(ac-define-source functions
  '((candidates . ac-function-candidates)
    (document . ac-symbol-documentation)
    (symbol . "f")
    (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
    (cache)))

;; Lisp variables source
(defvar ac-variables-cache nil)
(ac-clear-variable-every-10-minutes 'ac-variables-cache)

(defun ac-variable-candidates ()
  (or ac-variables-cache
      (setq ac-variables-cache
            (loop for x being the symbols
                  if (boundp x)
                  collect (symbol-name x)))))

(ac-define-source variables
  '((candidates . ac-variable-candidates)
    (document . ac-symbol-documentation)
    (symbol . "v")
    (cache)))

;; Lisp features source
(defvar ac-emacs-lisp-features nil)
(ac-clear-variable-every-10-minutes 'ac-emacs-lisp-features)

(defun ac-emacs-lisp-feature-candidates ()
  (or ac-emacs-lisp-features
      (if (fboundp 'find-library-suffixes)
          (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
            (setq ac-emacs-lisp-features
                  (append (mapcar 'prin1-to-string features)
                          (loop for dir in load-path
                                if (file-directory-p dir)
                                append (loop for file in (directory-files dir)
                                             if (string-match suffix file)
                                             collect (substring file 0 (match-beginning 0))))))))))

(ac-define-source features
  '((depends find-func)
    (candidates . ac-emacs-lisp-feature-candidates)
    (prefix . "require +'\\(\\(?:\\sw\\|\\s_\\)*\\)")
    (requires . 0)))

(defvaralias 'ac-source-emacs-lisp-features 'ac-source-features)

;; Abbrev source
(ac-define-source abbrev
  '((candidates . (mapcar 'popup-x-to-string (append (vconcat local-abbrev-table global-abbrev-table) nil)))
    (action . expand-abbrev)
    (symbol . "a")
    (cache)))

;; Files in current directory source
(ac-define-source files-in-current-dir
  '((candidates . (directory-files default-directory))
    (cache)))

;; Filename source
(defvar ac-filename-cache nil)

(defun ac-filename-candidate ()
  (unless (file-regular-p ac-prefix)
    (ignore-errors
      (loop with dir = (file-name-directory ac-prefix)
            with files = (or (assoc-default dir ac-filename-cache)
                             (let ((files (directory-files dir nil "^[^.]")))
                               (push (cons dir files) ac-filename-cache)
                               files))
            for file in files
            for path = (concat dir file)
            collect (if (file-directory-p path)
                        (concat path "/")
                      path)))))

(ac-define-source filename
  '((init . (setq ac-filename-cache nil))
    (candidates . ac-filename-candidate)
    (prefix . valid-file)
    (requires . 0)
    (action . ac-start)
    (limit . nil)))

;; Dictionary source
(defcustom ac-user-dictionary nil
  "User dictionary"
  :type '(repeat string)
  :group 'auto-complete)

(defcustom ac-user-dictionary-files '("~/.dict")
  "User dictionary files."
  :type '(repeat string)
  :group 'auto-complete)

(defcustom ac-dictionary-directories nil
  "Dictionary directories."
  :type '(repeat string)
  :group 'auto-complete)

(defvar ac-dictionary nil)
(defvar ac-dictionary-cache (make-hash-table :test 'equal))

(defun ac-clear-dictionary-cache ()
  (interactive)
  (clrhash ac-dictionary-cache))

(defun ac-read-file-dictionary (filename)
  (let ((cache (gethash filename ac-dictionary-cache 'none)))
    (if (and cache (not (eq cache 'none)))
        cache
      (let (result)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents filename)
            (setq result (split-string (buffer-string) "\n"))))
        (puthash filename result ac-dictionary-cache)
        result))))

(defun ac-buffer-dictionary ()
  (apply 'append
         (mapcar 'ac-read-file-dictionary
                 (mapcar (lambda (name)
                           (loop for dir in ac-dictionary-directories
                                 for file = (concat dir "/" name)
                                 if (file-exists-p file)
                                 return file))
                         (list (symbol-name major-mode)
                               (ignore-errors
                                 (file-name-extension (buffer-file-name))))))))

(defun ac-dictionary-candidates ()
  (apply 'append `(,ac-user-dictionary
                   ,(ac-buffer-dictionary)
                   ,@(mapcar 'ac-read-file-dictionary
                             ac-user-dictionary-files))))

(ac-define-source dictionary
  '((candidates . ac-dictionary-candidates)
    (symbol . "d")))

(provide 'auto-complete)
;;; auto-complete.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/initialize-bundle ()
  "Initialize YASnippet and load snippets in the bundle.";;; snippets for python-mode
(yas/define-snippets 'python-mode
                     '(("__" "__${init}__" "__...__" nil nil nil nil nil)
                       ("class" "class ${1:ClassName}(${2:object}):\n    $0\n" "class" nil nil nil nil nil)
                       ("class" "class ${1:ClassName}(${2:object}):\n\n    def __init__(self${3:, }$4):\n        super($1,self).__init__($4)\n        $0\n" "class" nil nil nil nil nil)
                       ("coding" "# -*- coding: utf-8 -*-\n" "coding.utf8" nil nil nil nil nil)
                       ("def" "def ${1:name}($2):\n    $0\n" "def" nil nil nil nil nil)
                       ("defm" "def ${1:name}(self, $2):\n    $0\n" "defm" nil nil nil nil nil)
                       ("doc" "\"\"\"$0\n\"\"\"\n" "doc" nil nil nil nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil)
                       ("from" "from $1 import $2\n$0\n" "from ... import ..." nil nil nil nil nil)
                       ("ifmain" "if __name__ == '__main__':\n    $0" "if __name__ == '__main__': ..." nil nil nil nil nil)
                       ("init" "def __init__(self$1):\n    $0\n" "init" nil nil nil nil nil)
                       ("ipdb" "import ipdb; ipdb.set_trace()" "ipdb" nil nil nil nil nil)
                       ("param" ":param $1: $0" "param" "'force-in-comment" nil nil nil nil)
                       ("pdb" "import pdb; pdb.set_trace()\n" "pdb" nil nil nil nil nil)
                       ("prop" "@property\ndef ${1:prop}(self):\n    ${0:pass}\n\n@$1.setter\ndef $1(self, value):\n    pass\n" "decorator: @property def x: ... @x.setter" nil nil "((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))" nil nil)
                       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n   def fset(self, value):\n       self._$1 = value\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil "((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))" nil nil)
                       ("testcase" "class ${1:TestCase}(${2:unittest.TestCase}):\n    $0\n" "unittest.TestCase normal" nil nil nil nil nil)
                       ("testcase" "class $1${2:TestCase}(${3:unittest.TestCase}):\n\n    def setUp(self):\n        ${4:pass} \n	\n    def tearDown(self):\n        ${5:pass}\n\n    $0\n" "unittest.TestCase with setUp tearDown" nil nil nil nil nil)
                       ("try" "try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}" "try.except" nil nil nil nil nil)
                       ("try" "try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nelse:\n	${5:pass}" "try.exceptelse" nil nil nil nil nil)
                       ("try" "try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}}:\n	${4:raise}\nelse:\n	${5:pass}\nfinally:\n	${6:pass}" "try.exceptelsefinally" nil nil nil nil nil)
                       ("try" "try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nfinally:\n	${5:pass}" "try.exceptfinally" nil nil nil nil nil)
                       ("while" "while ${condition}:\n    $0" "while ... : ..." nil nil nil nil nil))
                     '(text-mode))


;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
                     '(("!env" "#!/usr/bin/env ruby\n" "/usr/bin/env ruby" nil nil nil nil nil)
                       ("#" "# => " "# =>" nil nil nil nil nil)
                       (":" ":${key} => ${\"value\"}" ":key => \"value\"" nil nil nil nil nil)
                       ("=b" "=begin rdoc\n  $0\n=end" "=b" nil nil nil nil nil)
                       ("Array" "Array.new(${10}) { |${i}| $0 }" "Array.new(10) { |i| ... }" nil "Arrays" nil nil nil)
                       ("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil nil nil nil nil)
                       ("Dir" "Dir.glob(${1:\"dir/glob/*}\") { |${2:file}| $0 }" "Dir.glob(\"..\") do |file| .. end" nil "Files" nil nil nil)
                       ("Enum" "include Enumerable\n\ndef each(&block)\n  $0\nend" "Enum" nil "Enumerables" nil nil nil)
                       ("File" "File.foreach(${1:\"${2:path/to/file}\"}) { |${3:line}| $0 }" "File" nil "Files" nil nil nil)
                       ("File.1" "File.open(${1:\"${2:path/to/file}\"}${3/(^[rwab+]+$)|.*/(?1:, \")/}${3:w}${3/(^[rwab+]+$)|.*/(?1:\")/}) { |${4:file}| $0 }" "File.1" nil "Files" nil nil nil)
                       ("File.2" "File.read(${1:\"${2:path/to/file}\"})" "File.2" nil "Files" nil nil nil)
                       ("Forw-" "extend Forwardable" "Forw-" nil nil nil nil nil)
                       ("Hash" "Hash.new { |${1:hash}, ${2:key}| ${1:hash}[${2:key}] = $0 }" "Hash" nil "Hashes" nil nil nil)
                       ("Md" "File.open(${1:\"${2:path/to/file}.dump\"}, \"w\") { |${3:file}| Marshal.dump(${4:obj}, ${3:file}) }" "Md" nil nil nil nil nil)
                       ("Ml" "File.open(${1:\"${2:path/to/file}.dump\"}) { |${3:file}| Marshal.load(${3:file}) }" "Ml" nil nil nil nil nil)
                       ("Pn-" "PStore.new(${1:\"${2:file_name.pstore}\"})" "Pn-" nil nil nil nil nil)
                       ("README" "# Snippets for Ruby Mode\n\n## About\n\nComprehensive collection of Ruby snippets for\n[yasnippet](http://code.google.com/p/yasnippet/ \"yasnippet - Google Code\").\nThe collection also contains snippets for major Ruby frameworks like Rails\nand RSpec.\n\nThe Rails snippets were originally borrowed from\n[mknittig/yasnippet-rails](http://github.com/eschulte/yasnippets-rails/tree).\n\nThe RSpec snippets require that RSpec files are edited in a separate\nEmacs mode. I recommend using\n[rspec-mode.el](http://github.com/pezra/rspec-mode.el/tree/master).\nThe RSpec snippets were originally borrowed from\n[gary/yasnippets-rspec](http://github.com/gary/yasnippets-rspec/tree/master).\n\n## Contributors\n\nSee https://github.com/bmaland/yasnippet-ruby-mode/contributors\n\nMuch of the credits should naturally go to the yasnippet-rails and\nyasnippet-rspec authors.\n\nThanks to Jeff Wheeler for his work on the snippet_copier.py script!\n" "README.markdown" nil nil nil nil nil)
                       ("Yd-" "File.open(${1:\"${2:path/to/file}.yaml\"}, \"w\") { |${3:file}| YAML.dump(${4:obj}, ${3:file}) }" "Yd-" nil nil nil nil nil)
                       ("Yl-" "File.open(\"${1:path/to/file.yaml}\") { |${2:file}| YAML.load(${2:file}) }" "YAML.load(file)" nil nil nil nil nil)
                       ("aa" "attr_accessor :$0" "attr_accesor :..." nil nil nil nil nil)
                       ("ae" "assert_equal ${expected}, ${actual}\n" "assert_equal  ... ,  ... " nil "assert" nil nil nil)
                       ("ako" "assert_kind_of ${class}, ${object}\n" "assert_kind_of  ... ,  ... " nil "assert" nil nil nil)
                       ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil "Enumerables" nil nil nil)
                       ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil nil nil nil nil)
                       ("ann" "assert_not_nil ${object}\n" "assert_not_nil  ... " nil "assert" nil nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "Enumerables" nil nil nil)
                       ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil nil nil nil nil)
                       ("ar" "attr_reader :$0" "attr_reader :..." nil nil nil nil nil)
                       ("ars" "assert_response :${success}\n" "assert_response : ... " nil "assert" nil nil nil)
                       ("art" "assert_redirected_to :controller => \"${controller}\"\n" "assert_redirected_to :controller => \" ... \"" nil "assert" nil nil nil)
                       ("as" "assert`snippet_paren.rb`${1:test}, \"${0:Failure message.}\"`snippet_paren.rb end`" "as" nil "Tests" nil nil nil)
                       ("ase" "assert_equal`snippet_paren.rb`${1:expected}, ${0:actual}`snippet_paren.rb end`" "ase" nil "Tests" nil nil nil)
                       ("asid" "assert_in_delta`snippet_paren.rb`${1:expected_float}, ${2:actual_float}, ${0:2 ** -20}`snippet_paren.rb end`" "asid" nil "Tests" nil nil nil)
                       ("asio" "assert_instance_of`snippet_paren.rb`${1:ExpectedClass}, ${0:actual_instance}`snippet_paren.rb end`" "asio" nil "Tests" nil nil nil)
                       ("asko" "assert_kind_of`snippet_paren.rb`${1:ExpectedKind}, ${0:actual_instance}`snippet_paren.rb end`" "asko" nil "Tests" nil nil nil)
                       ("asm" "assert_match`snippet_paren.rb`/${1:expected_pattern}/, ${0:actual_string}`snippet_paren.rb end`" "asm" nil "Tests" nil nil nil)
                       ("asn" "assert_nil`snippet_paren.rb`${0:instance}`snippet_paren.rb end`" "asn" nil "Tests" nil nil nil)
                       ("asne" "assert_not_equal`snippet_paren.rb`${1:unexpected}, ${0:actual}`snippet_paren.rb end`" "asne" nil "Tests" nil nil nil)
                       ("asnm" "assert_no_match`snippet_paren.rb`/${1:unexpected_pattern}/, ${0:actual_string}`snippet_paren.rb end`" "asnm" nil "Tests" nil nil nil)
                       ("asnn" "assert_not_nil`snippet_paren.rb`${0:instance}`snippet_paren.rb end`" "asnn" nil "Tests" nil nil nil)
                       ("asnr" "assert_nothing_raised(${1:Exception}) { $0 }" "asnr" nil "Tests" nil nil nil)
                       ("asns" "assert_not_same`snippet_paren.rb`${1:unexpected}, ${0:actual}`snippet_paren.rb end`" "asns" nil "Tests" nil nil nil)
                       ("asnt" "assert_nothing_thrown { $0 }" "asnt" nil "Tests" nil nil nil)
                       ("aso" "assert_operator`snippet_paren.rb`${1:left}, :${2:operator}, ${0:right}`snippet_paren.rb end`" "aso" nil "Tests" nil nil nil)
                       ("asr" "assert_raise(${1:Exception}) { $0 }" "asr" nil "Tests" nil nil nil)
                       ("asrt" "assert_respond_to`snippet_paren.rb`${1:object}, :${0:method}`snippet_paren.rb end`" "asrt" nil "Tests" nil nil nil)
                       ("ass" "assert_same`snippet_paren.rb`${1:expected}, ${0:actual}`snippet_paren.rb end`" "ass" nil "Tests" nil nil nil)
                       ("ass.1" "assert_send`snippet_paren.rb`[${1:object}, :${2:message}, ${0:args}]`snippet_paren.rb end`" "ass.1" nil "Tests" nil nil nil)
                       ("ast" "assert_throws(:${1:expected}) { $0 }" "ast" nil "Tests" nil nil nil)
                       ("begin" "begin\n  $3\nrescue ${1:Exception} => ${2:e}\n  $0\nend" "begin ... rescue ... end" nil "Blocks" nil nil nil)
                       ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil nil nil nil nil)
                       ("bm-" "TESTS = ${1:10_000}\nBenchmark.bmbm do |results|\n  $0\nend" "bm-" nil nil nil nil nil)
                       ("bt" "belongs_to :${class}\n" "belongs_to : ... " nil "ActiveRecord" nil nil nil)
                       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil "Conditions" nil nil nil)
                       ("cl" "classify { |${e}| $0 }" "classify { |...| ... }" nil nil nil nil nil)
                       ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil "definitions" nil nil nil)
                       ("cla-" "class ${1:ClassName} < DelegateClass(${2:ParentClass})\n  def initialize(${3:args})\n    super(${4:del_obj})\n    \n    $0\n  end\n  \n  \nend" "class .. < DelegateClass .. initialize .. end  (class)" nil "definitions" nil nil nil)
                       ("cla.1" "${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}} = Struct.new(:${2:attr_names}) do\n  def ${3:method_name}\n    $0\n  end\n  \n  \nend" "cla.1" nil "definitions" nil nil nil)
                       ("cla.2" "class ${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}}\n  $0\nend" "cla.2" nil "definitions" nil nil nil)
                       ("cla.3" "class ${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}}\n  def initialize${2/(^.*?\\S.*)|.*/(?1:\\()/}${2:args}${2/(^.*?\\S.*)|.*/(?1:\\))/}\n    $0\n  end\n  \n  \nend" "cla.3" nil "definitions" nil nil nil)
                       ("cla.4" "class ${1:BlankSlate}\n  instance_methods.each { |meth| undef_method(meth) unless meth =~ /\\A__/ }\n  \n  def initialize${2/(^.*?\\S.*)|.*/(?1:\\()/}${2:args}${2/(^.*?\\S.*)|.*/(?1:\\))/}\n    @${3:delegate} = ${4:delegate_object}\n    \n    $0\n  end\n  \n  def method_missing(meth, *args, &block)\n    @${3:delegate}.send(meth, *args, &block)\n  end\n  \n  \nend" "cla.4" nil "definitions" nil nil nil)
                       ("clafn" "split(\"::\").inject(Object) { |par, const| par.const_get(const) }" "class_from_name()" nil nil nil nil nil)
                       ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil "collections" nil nil nil)
                       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n         (or (buffer-file-name)\n             (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend\n" "class ... end" nil nil nil nil nil)
                       ("col" "collect { |${e}| $0 }\n" "collect { |e| ... }" nil "Enumerables" nil nil nil)
                       ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil "Enumerables" nil nil nil)
                       ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil nil nil nil nil)
                       ("deec" "Marshal.load(Marshal.dump(${0:obj_to_copy}))" "deec" nil nil nil nil nil)
                       ("def" "def ${1:method_name}\n  $0\nend" "def ... end" nil "definitions" nil nil nil)
                       ("defd" "def_delegator :${1:@del_obj}, :${2:del_meth}, :${3:new_name}" "defd" nil nil nil nil nil)
                       ("defds" "def_delegators :${1:@del_obj}, :${0:del_methods}" "defds" nil nil nil nil nil)
                       ("defmm" "def method_missing(meth, *args, &blk)\n  $0\nend" "defmm" nil nil nil nil nil)
                       ("defs" "def self.${1:class_method_name}\n  $0\nend" "def.self ... end" nil nil nil nil nil)
                       ("deft" "def test_${1:case_name}\n  $0\nend" "deft" nil nil nil nil nil)
                       ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil nil nil nil nil)
                       ("desc" "describe \"${1:method}\" do\n  it$0\nend" "describe (rspec)" nil nil nil nil nil)
                       ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil "Enumerables" nil nil nil)
                       ("do" "do |${1:variable}|\n  $0\nend" "do |variable| ... end" nil "Blocks" nil nil nil)
                       ("dow" "downto(${1:0}) { ${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}${2:n}${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:| )/}$0 }" "dow" nil "Loops" nil nil nil)
                       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil nil nil nil nil)
                       ("eab" "each_byte { |${1:byte}| $0 }" "eab" nil nil nil nil nil)
                       ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil "Enumerables" nil nil nil)
                       ("eac-" "each_char { |${1:chr}| $0 }" "eac-" nil nil nil nil nil)
                       ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil nil nil nil nil)
                       ("eak" "each_key { |${1:key}| $0 }" "eak" nil nil nil nil nil)
                       ("eal" "each_line$1 { |${2:line}| $0 }" "eal" nil nil nil nil nil)
                       ("eap" "each_pair { |${1:name}, ${2:val}| $0 }" "eap" nil nil nil nil nil)
                       ("eas-" "each_slice(${1:2}) { |${2:group}| $0 }" "eas-" nil "Enumerables" nil nil nil)
                       ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil nil nil nil nil)
                       ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil "Enumerables" nil nil nil)
                       ("fet" "fetch(${1:name}) { ${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}${2:key}${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:| )/}$0 }" "fet" nil nil nil nil nil)
                       ("fil" "fill(${1:range}) { ${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}${2:i}${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:| )/}$0 }" "fil" nil nil nil nil nil)
                       ("filedn" "File.dirname(${1:__FILE__}) + \"/$0\"" "File.dirname(__FILE__)" nil "Files" nil nil nil)
                       ("fin" "find { |${1:e}| $0 }" "find { |e| .. }" nil "Enumerables" nil nil nil)
                       ("fina" "find_all { |${1:e}| $0 }" "fina" nil "Enumerables" nil nil nil)
                       ("fl" "flunk`snippet_paren.rb`\"${0:Failure message.}\"`snippet_paren.rb end`" "fl" nil nil nil nil nil)
                       ("flao" "inject(Array.new) { |${1:arr}, ${2:a}| ${1:arr}.push(*${2:a}) }" "flao" nil "Arrays" nil nil nil)
                       ("flsh" "flash[:${notice}] = \"${Text here...}\"\n" "flash[: ... ] = \" ... \"" nil "Rails" nil nil nil)
                       ("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil "Loops" nil nil nil)
                       ("gre" "grep(${1:/${2:pattern}/}) { |${3:match}| $0 }" "gre" nil "Enumerables" nil nil nil)
                       ("gsu" "gsub(/${1:pattern}/) { ${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}${2:match}${2/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:| )/}$0 }" "gsu" nil nil nil nil nil)
                       ("hm" "has_many :${class}\n" "has_many : ... " nil "ActiveRecord" nil nil nil)
                       ("ho" "has_one :${class}\n" "has_one : ... " nil "ActiveRecord" nil nil nil)
                       ("i" "def initialize(${1:params})\n  ${2:body}\nend\n$0" "def initialize(...) ... end" nil nil nil nil nil)
                       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil "Conditions" nil nil nil)
                       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil "Conditions" nil nil nil)
                       ("inj" "inject${1/.+/(/}${1:init}${1/.+/)/} { |${2:mem}, ${3:var}| $0 }" "inj" nil "Enumerables" nil nil nil)
                       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil "Enumerables" nil nil nil)
                       ("lam" "lambda { ${1/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}${1:args}${1/(^(?<var>\\s*(?:\\*|\\*?[a-z_])[a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:| )/}$0 }" "lam" nil nil nil nil nil)
                       ("logi" "logger.info \"${Text here...}\"\n" "logger.info \" ... \"" nil "general" nil nil nil)
                       ("loo" "loop { $0 }" "loop { .. }" nil "Loops" nil nil nil)
                       ("map" "map { |${1:e}| $0 }" "map" nil "Enumerables" nil nil nil)
                       ("mapwi-" "enum_with_index.map { |${1:e}, ${2:i}| $0 }" "mapwi-" nil "Enumerables" nil nil nil)
                       ("max" "max { |a, b| $0 }" "max" nil "Enumerables" nil nil nil)
                       ("min" "min { |a, b| $0 }" "min" nil "Enumerables" nil nil nil)
                       ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil nil nil nil nil)
                       ("mod" "module ${ModuleName}\n  $0\nend\n" "module ModuleName ... end" nil nil nil nil nil)
                       ("mod.1" "module ${1:ModuleName}\n  module ClassMethods\n    $0\n  end\n  \n  module InstanceMethods\n    \n  end\n  \n  def self.included(receiver)\n    receiver.extend         ClassMethods\n    receiver.send :include, InstanceMethods\n  end\nend\n" "mod.1" nil nil nil nil nil)
                       ("module" "module ${ModuleName}\n  $0\nend\n" "module ModuleName ... end" nil nil nil nil nil)
                       ("nam" "namespace :${1:${TM_FILENAME/\\.\\w+//}} do\n  $0\nend" "nam" nil nil nil nil nil)
                       ("ope" "open(${1:\"${2:path/or/url/or/pipe}\"}${3/(^[rwab+]+$)|.*/(?1:, \")/}${3:w}${3/(^[rwab+]+$)|.*/(?1:\")/}) { |${4:io}| $0 }" "ope" nil nil nil nil nil)
                       ("opt" "opts.on( \"-${1:o}\", \"--${2:long-option-name}\"${3/^\\s*$|(.*\\S.*)/(?1:, )/}${3:String},\n         \"${4:Option description.}\" ) do |${6:opt}|\n  $0\nend" "opt" nil nil nil nil nil)
                       ("optp" "require \"optparse\"\n\noptions = {${1::default => \"args\"}}\n\nARGV.options do |opts|\n  opts.banner = \"Usage:  #{File.basename(\\$PROGRAM_NAME)} [OPTIONS]${2/^\\s*$|(.*\\S.*)/(?1: )/}${2:OTHER_ARGS}\"\n  \n  opts.separator \"\"\n  opts.separator \"Specific Options:\"\n  \n  $0\n  \n  opts.separator \"Common Options:\"\n  \n  opts.on( \"-h\", \"--help\",\n           \"Show this message.\" ) do\n    puts opts\n    exit\n  end\n  \n  begin\n    opts.parse!\n  rescue\n    puts opts\n    exit\n  end\nend\n" "optp" nil nil nil nil nil)
                       ("par" "params[:${id}]\n" "params[: ... ]" nil "Rails" nil nil nil)
                       ("patfh" "File.join(File.dirname(__FILE__), *%w[${1:rel path here}])" "patfh" nil nil nil nil nil)
                       ("pend" "__END__\n" "program end" nil nil nil nil nil)
                       ("r" "attr_reader :" "attr_reader ..." nil nil nil nil nil)
                       ("ra" "render :action => \"${action}\"\n" "render :action => \" ... \"" nil "Rails render" nil nil nil)
                       ("ral" "render :action => \"${action}\", :layout => \"${layoutname}\"\n" "render :action => \" ... \", :layout => \" ... \"" nil "Rails render" nil nil nil)
                       ("ran" "sort_by { rand }" "ran" nil "Enumerables" nil nil nil)
                       ("rb" "#!/usr/bin/ruby -wKU\n" "/usr/bin/ruby -wKU" nil "general" nil nil nil)
                       ("rcea" "render_component :action => \"${index}\"\n" "render_component :action => \" ... \"" nil "Rails render" nil nil nil)
                       ("rcec" "render_component :controller => \"${items}\"\n" "render_component :controller => \" ... \"" nil "Rails render" nil nil nil)
                       ("rceca" "render_component :controller => \"${items}\", :action => \"${index}\"\n" "render_component :controller => \" ... \", :action => \" ... \"" nil "Rails render" nil nil nil)
                       ("rea" "redirect_to :action => \"${index}\"\n" "redirect_to :action => \" ... \"" nil "Rails redirect" nil nil nil)
                       ("reai" "redirect_to :action => \"${show}\", :id => ${@item}\n" "redirect_to :action => \" ... \", :id =>  ... " nil "Rails redirect" nil nil nil)
                       ("rec" "redirect_to :controller => \"${items}\"\n" "redirect_to :controller => \" ... \"" nil "Rails redirect" nil nil nil)
                       ("reca" "redirect_to :controller => \"${items}\", :action => \"${list}\"\n" "redirect_to :controller => \" ... \", :action => \" ... \"" nil "Rails redirect" nil nil nil)
                       ("recai" "redirect_to :controller => \"${items}\", :action => \"${show}\", :id => ${@item}\n" "redirect_to :controller => \" ... \", :action => \" ... \", :id =>  ... " nil "Rails redirect" nil nil nil)
                       ("rej" "reject { |${1:e}| $0 }" "reject { |e| .. }" nil "Enumerables" nil nil nil)
                       ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil "Enumerables" nil nil nil)
                       ("rep" "results.report(\"${1:name}:\") { TESTS.times { $0 } }" "results_report(..) { .. }" nil nil nil nil nil)
                       ("req" "require \"$0\"" "require \"...\"" nil nil nil nil nil)
                       ("res" "respond_to do |format|\n  format.${1:html} $0\nend\n" "respond_to do |format| .." nil "ActionController" nil nil nil)
                       ("reve" "reverse_each { |${1:e}| $0 }" "reve" nil nil nil nil nil)
                       ("rf" "render :file => \"${filepath}\"\n" "render :file => \" ... \"" nil "Rails render" nil nil nil)
                       ("rfu" "render :file => \"${filepath}\", :use_full_path => ${false}\n" "render :file => \" ... \", :use_full_path =>  ... " nil "Rails render" nil nil nil)
                       ("ri" "render :inline => \"${<%= 'hello' %>}\"\n" "render :inline => \" ... \"" nil "Rails render" nil nil nil)
                       ("ril" "render :inline => \"${<%= 'hello' %>}\", :locals => { ${name} => \"${value}\" }\n" "render :inline => \" ... \", :locals => {  ...  => \" ... \" }" nil "Rails render" nil nil nil)
                       ("rit" "render :inline => \"${<%= 'hello' %>}\", :type => :${rxml})\n" "render :inline => \" ... \", :type => : ... )" nil "Rails render" nil nil nil)
                       ("rl" "render :layout => \"${layoutname}\"\n" "render :layout => \" ... \"" nil "Rails render" nil nil nil)
                       ("rn" "render :nothing => ${true}\n" "render :nothing =>  ... " nil "Rails render" nil nil nil)
                       ("rns" "render :nothing => ${true}, :status => ${401}\n" "render :nothing =>  ... , :status =>  ... " nil "Rails render" nil nil nil)
                       ("rp" "render :partial => \"${item}\"\n" "render :partial => \" ... \"" nil "Rails render" nil nil nil)
                       ("rpc" "render :partial => \"${item}\", :collection => ${items}\n" "render :partial => \" ... \", :collection =>  ... " nil "Rails render" nil nil nil)
                       ("rpl" "render :partial => \"${item}\", :locals => { :${name} => \"${value}\"}\n" "render :partial => \" ... \", :locals => { : ...  => \" ... \"}" nil "Rails render" nil nil nil)
                       ("rpo" "render :partial => \"${item}\", :object => ${object}\n" "render :partial => \" ... \", :object =>  ... " nil "Rails render" nil nil nil)
                       ("rps" "render :partial => \"${item}\", :status => ${500}\n" "render :partial => \" ... \", :status =>  ... " nil "Rails render" nil nil nil)
                       ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil nil nil nil nil)
                       ("rt" "render :text => \"${Text here...}\"\n" "render :text => \" ... \"" nil "Rails render" nil nil nil)
                       ("rtl" "render :text => \"${Text here...}\", :layout => \"${layoutname}\"\n" "render :text => \" ... \", :layout => \" ... \"" nil "Rails render" nil nil nil)
                       ("rtlt" "render :text => \"${Text here...}\", :layout => ${true}\n" "render :text => \" ... \", :layout =>  ... " nil "Rails render" nil nil nil)
                       ("rts" "render :text => \"${Text here...}\", :status => ${401}\n" "render :text => \" ... \", :status =>  ... " nil "Rails render" nil nil nil)
                       ("rw" "attr_accessor :" "attr_accessor ..." nil nil nil nil nil)
                       ("sca" "scan(/${1:pattern}/) { |${2:match}| $0 }" "scan(/../) { |match| .. }" nil nil nil nil nil)
                       ("sel" "select { |${e}| $0 }" "select { |e| .. }" nil "Enumerables" nil nil nil)
                       ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil "Enumerables" nil nil nil)
                       ("ses" "session[:${user}]\n" "session[: ... ]" nil "Rails" nil nil nil)
                       ("sinc" "class << self; self end" "singleton_class()" nil nil nil nil nil)
                       ("sor" "sort { |a, b| $0 }" "sort { |a, b| .. }" nil "Enumerables" nil nil nil)
                       ("sorb" "sort_by { |${e}| $0 }" "sort_by { |e| .. }" nil "Enumerables" nil nil nil)
                       ("sr" "should_receive(:${1:method}).and_return(${2:return_val})" "should_receive(:).and_return()" nil nil nil nil nil)
                       ("ste" "step(${1:2}) { |${2:n}| $0 }" "step(2) { |e| .. }" nil nil nil nil nil)
                       ("sub" "sub(/${pattern}/) { |${match}| $0 }" "sub(/../) { |match| .. }" nil nil nil nil nil)
                       ("task" "task :${2:name} do\n  $0\nend" "task :name ... end" nil nil nil nil nil)
                       ("task.1" "desc \"${1:Task description}\"\ntask :${2:name} do\n  $0\nend" "desc ... task :name ... end" nil nil nil nil nil)
                       ("tc" "require \"test/unit\"\n\nrequire \"${1:library_file_name}\"\n\nclass Test${2:${1/([\\w&&[^_]]+)|./\\u$1/g}} < Test::Unit::TestCase\n  def test_${3:case_name}\n    $0\n  end\nend" "tc" nil nil nil nil nil)
                       ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil nil nil nil nil)
                       ("tra" "transaction${1/(^.*?\\S.*)|.*/(?1:\\()/}${1:true}${1/(^.*?\\S.*)|.*/(?1:\\))/} { $0 }" "tra" nil nil nil nil nil)
                       ("ts" "require \"test/unit\"\n\nrequire \"tc_${1:test_case_file}\"\nrequire \"tc_${2:test_case_file}\"\n" "ts" nil nil nil nil nil)
                       ("unif" "ARGF.each_line$1 do |${2:line}|\n  $0\nend" "unix_filter { .. }" nil nil nil nil nil)
                       ("unless" "unless ${1:condition}\n  $0\nend" "unless ... end" nil "Conditions" nil nil nil)
                       ("until" "until ${1:condition}\n  $0\nend" "until ... end" nil nil nil nil nil)
                       ("upt" "upto(${1:1.0/0.0}) { |${2:n}| $0 }" "upto(1.0/0.0) { |n| .. }" nil nil nil nil nil)
                       ("usai" "if ARGV.$1\n  abort \"Usage:  #{\\$PROGRAM_NAME} ${2:ARGS_GO_HERE}\"\nend" "usai" nil nil nil nil nil)
                       ("usau" "unless ARGV.$1\n  abort \"Usage:  #{\\$PROGRAM_NAME} ${2:ARGS_GO_HERE}\"\nend" "usau" nil nil nil nil nil)
                       ("va" "validates_associated :${attr}\n" "validates_associated : ... " nil "ActiveRecord" nil nil nil)
                       ("vc" "validates_confirmation_of :${attr}\n" "validates_confirmation_of : ... " nil "ActiveRecord" nil nil nil)
                       ("ve" "validates_exclusion_of :${attr}\n" "validates_exclusion_of : ... " nil "ActiveRecord" nil nil nil)
                       ("vf" "validates_format_of :${attr}, :with => /${regex}/\n" "validates_format_of : ... , :with => / ... /" nil "ActiveRecord" nil nil nil)
                       ("vi" "validates_inclusion_of :${attr}" "validates_inclusion_of : ..." nil "ActiveRecord" nil nil nil)
                       ("vn" "validates_numericality_of :${attr}\n" "validates_numericality_of : ... " nil "ActiveRecord" nil nil nil)
                       ("vp" "validates_presence_of :${attr}" "validates_presence_of : ..." nil "ActiveRecord" nil nil nil)
                       ("vu" "validates_uniqueness_of :${attr}\n" "validates_uniqueness_of : ... " nil "ActiveRecord" nil nil nil)
                       ("w" "attr_writer :" "attr_writer ..." nil nil nil nil nil)
                       ("when" "when ${1:condition}\n  $0" "when ..." nil "Conditions" nil nil nil)
                       ("while" "while ${condition}\n  $0\nend" "while ... end" nil "Loops" nil nil nil)
                       ("xml-" "REXML::Document.new(File.read(${1:\"${2:path/to/file}\"}))" "xml-" nil nil nil nil nil)
                       ("xpa" "elements.each(\"${1://XPath}\") do |${2:node}|\n  $0\nend" "xpath(..) { .. }" nil nil nil nil nil)
                       ("y" ":yields: $0" ":yields: arguments (rdoc)" nil nil nil nil nil)
                       ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil "Enumerables" nil nil nil)
                       ("{" "{ |${1:variable}| $0 }" "{ |variable| ... }" nil "Blocks" nil nil nil))
                     '(text-mode))


;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
                     '(("aft" "after(${1::each}) do\n  $0\nend" "after" nil "rspec-mode" nil nil nil)
                       ("annot" "any_number_of_times" "any_number_of_times" nil "rspec-mode" nil nil nil)
                       ("anr" "and_return(${1:value})$0" "and_return($value)" nil "rspec-mode" nil nil nil)
                       ("anra" "and_raise(${1:RuntimeError})$0" "and_raise($error)" nil "rspec-mode" nil nil nil)
                       ("anrb" "and_return { $1 }" "and_return with block" nil "rspec-mode" nil nil nil)
                       ("ant" "and_throw(${1:sym})" "and_throw" nil "rspec-mode" nil nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "rspec-mode" nil nil nil)
                       ("atl" "at_least(${1:n}).times" "at_least" nil "rspec-mode" nil nil nil)
                       ("atm" "at_most(${1:n}).times" "at_most" nil "rspec-mode" nil nil nil)
                       ("bef" "before(${1::each}) do\n  $0\nend" "before" nil "rspec-mode" nil nil nil)
                       ("befm" "before(:each) do\n  @${1:model} = ${1:$(replace-regexp-in-string \"_\" \"\" (upcase-initials text))}.new$0\nend\n" "before (rspec)" nil "rspec-mode" nil nil nil)
                       ("bfe" "before(:each) do\n  $0\nend" "before(:each) do ... end" nil "rspec-mode" nil nil nil)
                       ("conn" "controller_name :${1:controller}" "controller_name" nil "rspec-mode" nil nil nil)
                       ("des" "describe '${1:description}' do\n\n  it 'should ${2:description}' do\n    $0\n  end\n\nend" "describe (String)" nil "rspec-mode" nil nil nil)
                       ("desc" "require File.dirname(__FILE__) + '/../spec_helper'\n\ndescribe ${1:controller} do\n\n  $0\n\nend" "describe (Controller)" nil "rspec-mode" nil nil nil)
                       ("desrc.delete" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'DELETE ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): DELETE" nil "rspec-mode" nil nil nil)
                       ("desrc.get" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'GET ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): GET" nil "rspec-mode" nil nil nil)
                       ("desrc.post" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'POST ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): POST" nil "rspec-mode" nil nil nil)
                       ("desrc.put" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'PUT ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): PUT" nil "rspec-mode" nil nil nil)
                       ("dest" "describe ${1:Type} do\n\n  it 'should ${2:description}' do\n    $0\n  end\n\nend" "describe (type)" nil "rspec-mode" nil nil nil)
                       ("dests" "describe ${1:Type}, '${2:description}' do\n\n  it 'should ${3:description}' do\n    $0\n  end\n\nend" "describe (type, string)" nil "rspec-mode" nil nil nil)
                       ("ex" "exactly(${1:n}).times" "exactly" nil "rspec-mode" nil nil nil)
                       ("fm" "${1:var} = flexmock('${2:mock_name}')\n$0" "flexmock mock object w/name" nil "rspec-mode" nil nil nil)
                       ("fmar" "${1:var} = flexmock(:model, ${2:YourModel})\n$0" "flexmock mock object, ActiveRecord" nil "rspec-mode" nil nil nil)
                       ("it" "it \"should ${1:do something}\" do\n  $0\nend" "it \"should do something\" do ... end" nil "rspec-mode" nil nil nil)
                       ("mat" "class ${1:ReverseTo}\n  def initialize($3)\n    @$3 = $3\n  end\n\n  def matches?(actual)\n    @actual = actual\n    # Satisfy expectation here. Return false or raise an error if it's not met.\n    $0@actual.reverse.should == @$3\n    true\n  end\n\n  def failure_message\n    \"expected #{@actual.inspect} to $2 #{@$3.inspect}, but it didn't\"\n  end\n\n  def negative_failure_message\n    \"expected #{@actual.inspect} not to $2 #{@$3.inspect}, but it did\"\n  end\nend\n\ndef ${2:reverse_to}(${3:expected})\n  $1.new($3)\nend" "custom matcher" nil "rspec-mode" nil nil nil)
                       ("mocw.flexmock" "Spec::Runner.configure do |config|\n  config.mock_with :flexmock\nend" "mock_with flexmock" nil "rspec-mode" nil nil nil)
                       ("mocw.mocha" "Spec::Runner.configure do |config|\n  config.mock_with :mocha\nend" "mock_with mocha" nil "rspec-mode" nil nil nil)
                       ("mocw.rr" "Spec::Runner.configure do |config|\n  config.mock_with :rr\nend" "mock_with rr" nil "rspec-mode" nil nil nil)
                       ("on" "once" "once" nil "rspec-mode" nil nil nil)
                       ("resh" "require File.dirname(__FILE__) + '/../spec_helper'" "Require spec_helper" nil "rspec-mode" nil nil nil)
                       ("sce" "Scenario '${1:title}' do\n  Given '${2:given}'\n  When '${3:when}'\n  Then '${4:then}'\nend\n$0" "Scenario" nil "rspec-mode" nil nil nil)
                       ("set" "setup do\n  $1\nend" "setup do ... end" nil "rspec-mode" nil nil nil)
                       ("sh=" "${1:target}.should == ${2:value}\n$0" "should ==" nil "rspec-mode" nil nil nil)
                       ("shb" "${1:target}.should be(${2:result})\n$0" "should be" nil "rspec-mode" nil nil nil)
                       ("shbc" "${1:target}.should be_close(${2:result}, ${3:tolerance})\n$0" "should be_close" nil "rspec-mode" nil nil nil)
                       ("shbio" "${1:target}.should be_instance_of(${2:klass})\n$0" "should be_instance_of" nil "rspec-mode" nil nil nil)
                       ("shbko" "${1:target}.should be_a_kind_of(${2:klass})\n$0" "should be_kind_of" nil "rspec-mode" nil nil nil)
                       ("shbr" "response.should be_redirect\n$0" "should be_redirect" nil "rspec-mode" nil nil nil)
                       ("shbs" "response.should be_success\n$0" "should be_success" nil "rspec-mode" nil nil nil)
                       ("she.eql" "${1:target}.should eql(${2:value})\n$0" "should eql" nil "rspec-mode" nil nil nil)
                       ("she.equal" "${1:target}.should equal(${2:value})\n$0" "should equal" nil "rspec-mode" nil nil nil)
                       ("shh" "${1:target}.should have(${2:num}).${3:things}\n$0" "should have" nil "rspec-mode" nil nil nil)
                       ("shhal" "${1:target}.should have_at_least(${2:num}).${3:things}\n$0" "should have_at_least" nil "rspec-mode" nil nil nil)
                       ("shham" "${1:target}.should have_at_most(${2:num}).${3:things}\n$0" "should have_at_most" nil "rspec-mode" nil nil nil)
                       ("shhr" "${1:target}.should have(${2:x}).records\n$0" "should have_records" nil "rspec-mode" nil nil nil)
                       ("shm.match" "${1:target}.should match(/${2:regex}/)\n$0" "should match" nil "rspec-mode" nil nil nil)
                       ("shn=" "${1:target}.should_not == ${2:value}\n$0" "should_not ==" nil "rspec-mode" nil nil nil)
                       ("shnb" "${1:target}.should_not be(${2:result})\n$0" "should_not be" nil "rspec-mode" nil nil nil)
                       ("shnbc" "${1:target}.should_not be_close(${2:result}, ${3:tolerance})\n$0" "should_not be_close" nil "rspec-mode" nil nil nil)
                       ("shnbio" "${1:target}.should_not be_instance_of(${2:klass})\n$0" "should_not be_instance_of" nil "rspec-mode" nil nil nil)
                       ("shnbko" "${1:target}.should_not be_a_kind_of(${2:klass})\n$0" "should_not be_kind_of" nil "rspec-mode" nil nil nil)
                       ("shnbr" "response.should_not be_redirect\n$0" "should_not be_redirect" nil "rspec-mode" nil nil nil)
                       ("shnbs" "response.should_not be_success\n$0" "should_not be_success" nil "rspec-mode" nil nil nil)
                       ("shne.eql" "${1:target}.should_not eql(${2:value})\n$0" "should_not eql" nil "rspec-mode" nil nil nil)
                       ("shne.equal" "${1:target}.should_not equal(${2:value})\n$0" "should_not equal" nil "rspec-mode" nil nil nil)
                       ("shnm.match" "${1:target}.should_not match(/${2:regex}/)\n$0" "should_not match" nil "rspec-mode" nil nil nil)
                       ("shnp" "${1:target}.should_not ${2:be_}${3:predicate} $0" "should_not predicate" nil "rspec-mode" nil nil nil)
                       ("shnr" "${1:mock}.should_not_receive(:${2:message})$0" "should_not_receive" nil "rspec-mode" nil nil nil)
                       ("shnre" "lambda { ${1: } }.should_not raise_error(${2:error})\n$0" "should_not raise_error" nil "rspec-mode" nil nil nil)
                       ("shnrt" "${1:target}.should_not respond_to(:${2:sym})\n$0" "should_not respond_to" nil "rspec-mode" nil nil nil)
                       ("shns" "${1:target}.should_not satisfy { |obj| ${2: } }\n$0" "should_not satisfy" nil "rspec-mode" nil nil nil)
                       ("shnt" "lambda { ${1: } }.should_not throw_symbol(:${2:symbol})\n$0" "should_not throw" nil "rspec-mode" nil nil nil)
                       ("shp" "${1:target}.should ${2:be_}${3:predicate} $0" "should predicate" nil "rspec-mode" nil nil nil)
                       ("shr" "${1:mock}.should_receive(:${2:message})$0" "should_receive" nil "rspec-mode" nil nil nil)
                       ("shre" "lambda { ${1: } }.should raise_error(${2:error})\n$0" "should raise_error" nil "rspec-mode" nil nil nil)
                       ("shrt" "response.should render_template(\"${0:template}\")" "response.should render_template(\"$template\")" nil "rspec-mode" nil nil nil)
                       ("shrt.redirect" "response.should redirect_to(${1:url})\n$0" "should redirect_to" nil "rspec-mode" nil nil nil)
                       ("shrt.render" "response.should render_template(:${1:template})\n$0" "should render_template" nil "rspec-mode" nil nil nil)
                       ("shrt.respond" "${1:target}.should respond_to(:${2:sym})\n$0" "should respond_to" nil "rspec-mode" nil nil nil)
                       ("shs" "${1:target}.should satisfy { |obj| ${2: } }\n$0" "should satisfy" nil "rspec-mode" nil nil nil)
                       ("sht" "lambda { ${1: } }.should throw_symbol(:${2:symbol})\n$0" "should throw" nil "rspec-mode" nil nil nil)
                       ("sto" "Story '${1:title}', %{\n  As a ${2:role}\n  I want ${3:feature}\n  So that ${4:value}\n} do\nend" "Story" nil "rspec-mode" nil nil nil)
                       ("stub" "${1:target}.stub!(:${2:message})$0" "$target.stub!(:$message)" nil "rspec-mode" nil nil nil)
                       ("tw" "twice" "twice" nil "rspec-mode" nil nil nil)
                       ("wia" "with(${1:args})\n$0" "with args" nil "rspec-mode" nil nil nil))
                     '(text-mode))


;;; snippets for text-mode
(yas/define-snippets 'text-mode
                     '(("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil nil nil nil)
                       ("time" "`(current-time-string)`" "(current time)" nil nil nil nil nil))
                     'nil)


;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
                     '(("do" "do\n{\n    $0\n} while (${1:condition});" "do { ... } while (...)" nil nil nil nil nil)
                       ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})\n{\n    $0\n}" "for (...; ...; ...) { ... }" nil nil nil nil nil)
                       ("if" "if (${1:condition})\n{\n    $0\n}" "if (...) { ... }" nil nil nil nil nil)
                       ("inc" "#include \"$1\"\n" "#include \"...\"" nil nil nil nil nil)
                       ("inc" "#include <$1>\n" "#include <...>" nil nil nil nil nil)
                       ("main" "int main(int argc, char *argv[])\n{\n    $0\n    return 0;\n}\n" "int main(argc, argv) { ... }" nil nil nil nil nil)
                       ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}\n#define $1\n\n$0\n\n#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil nil nil nil)
                       ("struct" "struct ${1:name}\n{\n    $0\n};" "struct ... { ... }" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
                     '(("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil nil nil nil)
                       ("class" "class ${1:Name}\n{\npublic:\n    ${1:$(yas/substr text \"[^: ]*\")}($2);\n    virtual ~${1:$(yas/substr text \"[^: ]*\")}();\n};" "class ... { ... }" nil nil nil nil nil)
                       ("ns" "namespace " "namespace ..." nil nil nil nil nil)
                       ("template" "template <typename ${T}>" "template <typename ...>" nil nil nil nil nil)
                       ("using" "using namespace ${std};\n$0" "using namespace ... " nil nil nil nil nil))
                     '(cc-mode))


;;; snippets for c-mode
(yas/define-snippets 'c-mode
                     '(("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");\n" "FILE *fp = fopen(..., ...);" nil nil nil nil nil)
                       ("printf" "printf (\"${1:%s}\\\\n\"${1:$(if (string-match \"%\" text) \",\" \"\\);\")\n}$2${1:$(if (string-match \"%\" text) \"\\);\" \"\")}" "printf " nil nil nil nil nil))
                     '(cc-mode))


;;; snippets for csharp-mode
(yas/define-snippets 'csharp-mode
                     '(("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n" "private attribute ....;" nil nil nil nil nil)
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n\n/// <summary>\n/// $4\n/// </summary>\n/// <value>$5</value>\npublic $1 $2\n{\n    get {\n        return this.$2;\n    }\n    set {\n        this.$2 = value;\n    }\n}\n" "private attribute ....; public property ... ... { ... }" nil nil nil nil nil)
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 ${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};\n\n/// <summary>\n/// ${3:Description}\n/// </summary>\n/// <value><c>$1</c></value>\npublic ${1:Type} ${2:Name}\n{\n    get {\n        return this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};\n    }\n    set {\n        this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")} = value;\n    }\n}\n" "private _attribute ....; public Property ... ... { ... }" nil nil nil nil nil)
                       ("class" "${5:public} class ${1:Name}\n{\n    #region Ctor & Destructor\n    /// <summary>\n    /// ${3:Standard Constructor}\n    /// </summary>\n    public $1($2)\n    {\n    }\n\n    /// <summary>\n    /// ${4:Default Destructor}\n    /// </summary>    \n    public ~$1()\n    {\n    }\n    #endregion\n}\n" "class ... { ... }" nil nil nil nil nil)
                       ("comment" "/// <summary>\n/// $1\n/// </summary>\n" "/// <summary> ... </summary>" nil nil nil nil nil)
                       ("comment" "/// <param name=\"$1\">$2</param>\n" "/// <param name=\"...\"> ... </param>" nil nil nil nil nil)
                       ("comment" "/// <returns>$1</returns>\n" "/// <param name=\"...\"> ... </param>" nil nil nil nil nil)
                       ("comment" "/// <exception cref=\"$1\">$2</exception>\n" "/// <exception cref=\"...\"> ... </exception>" nil nil nil nil nil)
                       ("method" "/// <summary>\n/// ${5:Description}\n/// </summary>${2:$(if (string= (upcase text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" text \"</c></returns>\"))}\n${1:public} ${2:void} ${3:MethodName}($4)\n{\n$0\n}\n" "public void Method { ... }" nil nil nil nil nil)
                       ("namespace" "namespace $1\n{\n$0\n}\n" "namespace .. { ... }" nil nil nil nil nil)
                       ("prop" "/// <summary>\n/// $5\n/// </summary>\n/// <value>$6</value>\n$1 $2 $3\n{\n    get {\n        return this.$4;\n    }\n    set {\n        this.$4 = value;\n    }\n}\n" "property ... ... { ... }" nil nil nil nil nil)
                       ("region" "#region $1\n$0\n#endregion\n" "#region ... #endregion" nil nil nil nil nil)
                       ("using" "using $1;\n" "using ...;" nil nil nil nil nil)
                       ("using" "using System;\n" "using System;" nil nil nil nil nil)
                       ("using" "using System.$1;\n" "using System....;" nil nil nil nil nil))
                     '(cc-mode))


;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
                     '(("prop" "- (${1:id})${2:foo}\n{\n    return $2;\n}\n\n- (void)set${2:$(capitalize text)}:($1)aValue\n{\n    [$2 autorelease];\n    $2 = [aValue retain];\n}\n$0" "foo { ... } ; setFoo { ... }" nil nil nil nil nil))
                     '(cc-mode))


;;; snippets for css-mode
(yas/define-snippets 'css-mode
                     '(("bg" "background-color: #${1:DDD};" "background-color: ..." nil nil nil nil nil)
                       ("bg" "background-image: url($1);" "background-image: ..." nil nil nil nil nil)
                       ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil nil nil nil)
                       ("cl" "clear: $1;\n" "clear: ..." nil nil nil nil nil)
                       ("disp" "display: block;\n" "display: block" nil nil nil nil nil)
                       ("disp" "display: inline;\n" "display: inline" nil nil nil nil nil)
                       ("disp" "display: none;\n" "display: none" nil nil nil nil nil)
                       ("ff" "font-family: $1;\n" "font-family: ..." nil nil nil nil nil)
                       ("fs" "font-size: ${12px};\n" "font-size: ..." nil nil nil nil nil)
                       ("mar" "margin-bottom: $1;\n" "margin-bottom: ..." nil nil nil nil nil)
                       ("mar" "margin-left: $1;\n" "margin-left: ..." nil nil nil nil nil)
                       ("mar" "margin: $1;\n" "margin: ..." nil nil nil nil nil)
                       ("mar" "margin: ${top} ${right} ${bottom} ${left};\n" "margin top right bottom left" nil nil nil nil nil)
                       ("mar" "margin-right: $1;\n" "margin-right: ..." nil nil nil nil nil)
                       ("mar" "margin-top: $1;\n" "margin-top: ..." nil nil nil nil nil)
                       ("pad" "padding-bottom: $1;\n" "padding-bottom: ..." nil nil nil nil nil)
                       ("pad" "padding-left: $1;\n" "padding-left: ..." nil nil nil nil nil)
                       ("pad" "padding: $1;\n" "padding: ..." nil nil nil nil nil)
                       ("pad" "padding: ${top} ${right} ${bottom} ${left};\n" "padding: top right bottom left" nil nil nil nil nil)
                       ("pad" "padding-right: $1;\n" "padding-right: ..." nil nil nil nil nil)
                       ("pad" "padding-top: $1;\n" "padding-top: ..." nil nil nil nil nil))
                     '(text-mode))


;;; snippets for emacs-lisp-mode
(yas/define-snippets 'emacs-lisp-mode
                     '(("defun" "(defun $1 ()\n  \"thisandthat.\"\n  (interactive)\n  (let (var1)\n    (setq var1 some)\n    $0\n  )\n)" "function template" nil nil nil nil nil)
                       ("dired" ";; idiom for processing a list of files in dired's marked files\n \n;; suppose myProcessFile is your function that takes a file path\n;; and do some processing on the file\n\n(defun dired-myProcessFile ()\n  \"apply myProcessFile function to marked files in dired.\"\n  (interactive)\n  (require 'dired)\n  (mapc 'myProcessFile (dired-get-marked-files))\n)\n\n;; to use it, type M-x dired-myProcessFile\n" "process marked files in dired" nil nil nil nil nil)
                       ("file" "(defun doThisFile (fpath)\n  \"Process the file at path FPATH ...\"\n  (let ()\n    ;; create temp buffer without undo record or font lock. (more efficient)\n    ;; first space in temp buff name is necessary\n    (set-buffer (get-buffer-create \" myTemp\"))\n    (insert-file-contents fpath nil nil nil t)\n\n    ;; process it ...\n    ;; (goto-char 0) ; move to begining of file's content (in case it was open)\n    ;; ... do something here\n    ;; (write-file fpath) ;; write back to the file\n\n    (kill-buffer \" myTemp\")))\n" "a function that process a file" nil nil nil nil nil)
                       ("file" "(defun read-lines (filePath)\n  \"Return a list of lines in FILEPATH.\"\n  (with-temp-buffer\n    (insert-file-contents filePath)\n    (split-string\n     (buffer-string) \"\\n\" t)) )\n\n;; process all lines\n(mapc \n (lambda (aLine) \n   (message aLine) ; do your stuff here\n   )\n (read-lines \"inputFilePath\")\n)" "read lines of a file" nil nil nil nil nil)
                       ("find-replace" "(defun replace-html-chars-region (start end)\n  \"Replace “<” to “&lt;” and other chars in HTML.\nThis works on the current region.\"\n  (interactive \"r\")\n  (save-restriction \n    (narrow-to-region start end)\n    (goto-char (point-min))\n    (while (search-forward \"&\" nil t) (replace-match \"&amp;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \"<\" nil t) (replace-match \"&lt;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \">\" nil t) (replace-match \"&gt;\" nil t))\n    )\n  )\n" "find and replace on region" nil nil nil nil nil)
                       ("grabstring" "(setq $0 (buffer-substring-no-properties myStartPos myEndPos))\n" "grab buffer substring" nil nil nil nil nil)
                       ("grabthing" "(setq $0 (thing-at-point 'symbol))\n" "grab word under cursor" nil nil nil nil nil)
                       ("traverse_dir" ";; apply a function to all files in a dir\n(require 'find-lisp)\n(mapc 'my-process-file (find-lisp-find-files \"~/myweb/\" \"\\\\.html$\"))\n" "traversing a directory" nil nil nil nil nil)
                       ("word-or-region" ";; example of a command that works on current word or text selection\n(defun down-case-word-or-region ()\n  \"Lower case the current word or text selection.\"\n(interactive)\n(let (pos1 pos2 meat)\n  (if (and transient-mark-mode mark-active)\n      (setq pos1 (region-beginning)\n            pos2 (region-end))\n    (setq pos1 (car (bounds-of-thing-at-point 'symbol))\n          pos2 (cdr (bounds-of-thing-at-point 'symbol))))\n\n  ; now, pos1 and pos2 are the starting and ending positions\n  ; of the current word, or current text selection if exists\n\n  ;; put your code here.\n  $0\n  ;; Some example of things you might want to do\n  (downcase-region pos1 pos2) ; example of a func that takes region as args\n  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.\n  (delete-region pos1 pos2) ; get rid of it\n  (insert \"newText\") ; insert your new text\n\n  )\n)\n" "Command that works on region or word" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for erlang-mode
(yas/define-snippets 'erlang-mode
                     '(("after" "after\n    $1 -> $0\n" "after ... ->" nil nil nil nil nil)
                       ("begin" "begin\n    $0\nend\n" "begin ... end" nil nil nil nil nil)
                       ("beh" "-behaviour(${1:gen_server}).\n$0\n" "-behaviour(...)." nil nil nil nil nil)
                       ("case" "case $1 of\n    $0\nend\n" "case ... of ... end" nil nil nil nil nil)
                       ("compile" "-compile([${1:export_all}]).\n$0\n" "-compile(...)." nil nil nil nil nil)
                       ("def" "-define($1,$2).\n$0\n" "-define(...,...)." nil nil nil nil nil)
                       ("exp" "-export([${1:start/0}]).\n$0\n" "-export([])." nil nil nil nil nil)
                       ("fun" "fun ($1) -> $0 end\n" "fun (...) -> ... end" nil nil nil nil nil)
                       ("if" "if\n    $1 -> $2;\n    true -> $0\nend\n" "if ... -> ... ; true -> ... end" nil nil nil nil nil)
                       ("ifdef" "-ifdef($1).\n$0\n-endif.\n" "-ifdef(...). ... -endif." nil nil nil nil nil)
                       ("ifndef" "-ifndef($1).\n$0\n-endif.\n" "-ifndef(...). ... -endif." nil nil nil nil nil)
                       ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).\n$0\n" "-import([])." nil nil nil nil nil)
                       ("inc" "-include(\"$1\").\n$0\n" "-include(\"...\")." nil nil nil nil nil)
                       ("inc" "-include_lib(\"$1\").\n$0\n" "-include_lib(\"...\")." nil nil nil nil nil)
                       ("loop" "${1:loop}($2) ->\n    receive\n	${3:_} ->\n	    $1($2)\n    end.\n$0\n" "loop(...) -> receive _ -> loop(...) end." nil nil nil nil nil)
                       ("mod" "-module(${1:`(file-name-nondirectory\n              (file-name-sans-extension (or (buffer-file-name) (buffer-name))))`}).\n$0\n" "-module()." nil nil nil nil nil)
                       ("rcv" "receive\n    $1 -> $0\nend\n" "receive ... -> ... end" nil nil nil nil nil)
                       ("rcv" "receive\nafter\n    $1 -> $0\nend\n" "receive after ... -> ... end" nil nil nil nil nil)
                       ("rec" "-record($1,{$2}).\n$0\n" "-record(...,{...})." nil nil nil nil nil)
                       ("try" "try $1 of\n    $0\ncatch\nafter\nend\n" "try ... of ... catch after end" nil nil nil nil nil)
                       ("undef" "-undef($1).\n$0\n" "-undef(...)." nil nil nil nil nil))
                     '(text-mode))


;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
                     '(("au" "automatic $0 \n" "automatic" nil nil nil nil nil)
                       ("bd" "block data $0\n" "block data" nil nil nil nil nil)
                       ("c" "continue $0\n" "continue" nil nil nil nil nil)
                       ("ch" "character $0\n" "character" nil nil nil nil nil)
                       ("cx" "complex $0\n" "complex" nil nil nil nil nil)
                       ("dc" "double complex $0\n" "double complex" nil nil nil nil nil)
                       ("do" "do while (${1:condition})\n   $0\nend do\n" "do while (...) end do" nil nil nil nil nil)
                       ("dp" "double precision $0\n" "double precision" nil nil nil nil nil)
                       ("eq" "equivalence $0\n" "equivalence" nil nil nil nil nil)
                       ("ib" "implicit byte $0\n" "implicit byte" nil nil nil nil nil)
                       ("ic" "implicit complex $0\n" "implicit complex" nil nil nil nil nil)
                       ("ich" "implicit character $0\n" "implicit character" nil nil nil nil nil)
                       ("if" "if ( ${1:condition} ) then\n   $0\nend if\n" "if then end if" nil nil nil nil nil)
                       ("ii" "implicit integer $0\n" "implicit integer " nil nil nil nil nil)
                       ("il" "implicit logical $0\n" "implicit logical" nil nil nil nil nil)
                       ("in" "implicit none\n" "implicit none" nil nil nil nil nil)
                       ("inc" "include $0\n" "include" nil nil nil nil nil)
                       ("intr" "intrinsic $0\n" "intrinsic" nil nil nil nil nil)
                       ("ir" "implicit real $0\n" "implicit real" nil nil nil nil nil)
                       ("l" "logical $0\n" "logical" nil nil nil nil nil)
                       ("pa" "parameter $0\n" "parameter" nil nil nil nil nil)
                       ("pr" "program ${1:name}\n  $0\nend program ${1:name}\n" "program ... end program ..." nil nil nil nil nil)
                       ("re" "read (${1:*},${2:*}) $0\n" "read (*,*)" nil nil nil nil nil)
                       ("st" "structure $0\n" "structure" nil nil nil nil nil)
                       ("su" "subroutine $0\n" "subroutine" nil nil nil nil nil)
                       ("wr" "write (${1:*},${2:*}) $0\n" "write (*,*)" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for html-mode
(yas/define-snippets 'html-mode
                     '(("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil)
                       ("br" "<br />" "<br />" nil nil nil nil nil)
                       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil)
                       ("code" "<code class=\"$1\">\n  $0\n</code>" "<code class=\"...\">...</code>" nil nil nil nil nil)
                       ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil "list" nil nil nil)
                       ("div" "<div${1: id=\"${2:some_id}\"}${3: class=\"${4:some_class}\"}>$0</div> " "<div...>...</div>" nil nil nil nil nil)
                       ("div" "<div class=\"$1\">\n  $0\n</div>" "<div class=\"...\">...</div>" nil nil nil nil nil)
                       ("div" "<div id=\"$1\">\n  $0\n</div>" "<div id=\"...\">...</div>" nil nil nil nil nil)
                       ("div" "<div id=\"$1\" class=\"$2\">\n  $0\n</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil nil nil nil)
                       ("dl" "<dl>\n    $0\n</dl>\n" "<dl> ... </dl>" nil "list" nil nil nil)
                       ("dl" "<dl id=\"$1\">\n    $0\n</dl>" "<dl> ... </dl>" nil "list" nil nil nil)
                       ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta" nil nil nil)
                       ("dov" "a mirror up here $3\n\n\n<dov ${1:id=\"${2:some_id and here comes another nested field: ${3:nested_shit}}\"}>\n    $0\n</dov>\n<dov $1>\n    actually some other shit and $3\n</dov>\n" "<dov...>...</dov>" nil nil nil nil nil)
                       ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil "list" nil nil nil)
                       ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil nil nil)
                       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header" nil nil nil)
                       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header" nil nil nil)
                       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header" nil nil nil)
                       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header" nil nil nil)
                       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header" nil nil nil)
                       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header" nil nil nil)
                       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil)
                       ("hr" "<hr />\n" "<hr />" nil nil nil nil nil)
                       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil)
                       ("html" "<html>\n  $0\n</html>\n" "<html>...</html>" nil nil nil nil nil)
                       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil nil nil)
                       ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil nil nil nil)
                       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil)
                       ("li" "<li>$1</li>" "<li>...</li>" nil "list" nil nil nil)
                       ("li" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil "list" nil nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil)
                       ("link" "<!--[if IE]>\n<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />\n<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil nil nil)
                       ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta" nil nil nil)
                       ("meta" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil "meta" nil nil nil)
                       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil "list" nil nil nil)
                       ("ol" "<ol class=\"$1\">\n  $0\n</ol>" "<ol class=\"...\">...</ol>" nil "list" nil nil nil)
                       ("ol" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil "list" nil nil nil)
                       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil)
                       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil)
                       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil)
                       ("script" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script> " nil nil nil nil nil)
                       ("script" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil nil nil nil)
                       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil)
                       ("span" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil nil nil nil)
                       ("span" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil nil nil)
                       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil)
                       ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">\n  $0\n</table>" "<table ...>...</table>" nil "table" nil nil nil)
                       ("td" "<td$1>$2</td>" "<td>...</td>" nil "table" nil nil nil)
                       ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil nil nil)
                       ("th" "<th$1>$2</th>" "<th>...</th>" nil "table" nil nil nil)
                       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil)
                       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil "table" nil nil nil)
                       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil "list" nil nil nil)
                       ("ul" "<ul class=\"$1\">\n  $0\n</ul>" "<ul class=\"...\">...</ul>" nil "list" nil nil nil)
                       ("ul" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil "list" nil nil nil))
                     '(text-mode))


;;; snippets for latex-mode
(yas/define-snippets 'latex-mode
                     '(("begin" "\n\\begin{${1:environment}}\n$0\n\\end{$1}\n" "\\begin{environment} ... \\end{environment}" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
                     '(("+" "+ ${1:Text}\n+$0\n" "Unordered List" nil nil nil nil nil)
                       ("-" "- ${1:Text}\n-$0\n" "Unordered List" nil nil nil nil nil)
                       ("_" "_${1:Text}_ $0\n" "Emphasis" nil nil nil nil nil)
                       ("__" "**${1:Text}** $0\n" "Strong" nil nil nil nil nil)
                       ("`" "\\`${1:Code}\\` $0\n" "Inline Code" nil nil nil nil nil)
                       ("h1" "# ${1:Header 1} #\n\n$0\n" "Header 1 (#)" nil nil nil nil nil)
                       ("h1" "${1:Header 1}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0\n" "Header 1 (=)" nil nil nil nil nil)
                       ("h2" "## ${1:Header 1} ##\n\n$0\n" "Header 2 (##)" nil nil nil nil nil)
                       ("h2" "${1:Header 2}\n${1:$(make-string (string-width text) ?\\-)}\n\n$0\n" "Header 2 (-)" nil nil nil nil nil)
                       ("h3" "### ${1:Header 3} ###\n\n$0\n" "Header 3" nil nil nil nil nil)
                       ("h4" "#### ${1:Header 4} ####\n\n$0\n" "Header 4" nil nil nil nil nil)
                       ("h5" "##### ${1:Header 5} #####\n\n$0\n" "Header 5" nil nil nil nil nil)
                       ("h6" "###### ${1:Header 6} ######\n\n$0\n" "Header 6" nil nil nil nil nil)
                       ("hr" "\n----------\n\n$0\n" "Horizontal Rule (-)" nil nil nil nil nil)
                       ("hr" "\n*******\n\n$0\n" "Horizontal Rule (*)" nil nil nil nil nil)
                       ("img" "![${1:Alt Text}](${2:URL} $3) $0\n" "Image" nil nil nil nil nil)
                       ("link" "[${1:Link Text}](${2:URL} $3) $0\n" "Link" nil nil nil nil nil)
                       ("ol" "${1:1}. ${2:Text}\n${1:$(number-to-string (1+ (string-to-number text)))}. $0\n" "Ordered List" nil nil nil nil nil)
                       ("rimg" "![${1:Alt Text}][$2] $0\n" "Referenced Image" nil nil nil nil nil)
                       ("rlb" "[${1:Reference}]: ${2:URL} $3\n$0\n" "Reference Label" nil nil nil nil nil)
                       ("rlink" "[${1:Link Text}][$2] $0\n" "Reference Link" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for nxml-mode
(yas/define-snippets 'nxml-mode
                     '(("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil)
                       ("br" "<br />" "<br />" nil nil nil nil nil)
                       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil)
                       ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta" nil nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta" nil nil nil)
                       ("form" "<form method=\"$1\" action=\"$2\">\n  $0\n</form>" "<form method=\"...\" action=\"...\"></form>" nil nil nil nil nil)
                       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header" nil nil nil)
                       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header" nil nil nil)
                       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header" nil nil nil)
                       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header" nil nil nil)
                       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header" nil nil nil)
                       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header" nil nil nil)
                       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil)
                       ("hr" "<hr />\n" "<hr />" nil nil nil nil nil)
                       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil)
                       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil nil nil)
                       ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil nil nil nil)
                       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil)
                       ("li" "<li>$1</li>" "<li>...</li>" nil nil nil nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta" nil nil nil)
                       ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil nil nil nil)
                       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil nil nil)
                       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil)
                       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil)
                       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil)
                       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil)
                       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil)
                       ("table" "<table>\n  $0\n</table>" "<table>...</table>" nil nil nil nil nil)
                       ("tag" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil nil nil nil)
                       ("tag" "<${1:tag}>\n  $2\n</$1>$0" "<tag> \\n...\\n</tag>" nil nil nil nil nil)
                       ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil nil nil)
                       ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil nil nil)
                       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil)
                       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil nil nil)
                       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
                     '(("eval" "eval {\n    ${1:# do something risky...}\n};\nif (\\$@) {\n    ${2:# handle failure...}\n}" "eval { ... } if ($@) { ... }" nil nil nil nil nil)
                       ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {\n    ${3:# body...}\n}" "for (...) { ... }" nil nil nil nil nil)
                       ("fore" "foreach my \\$${1:x} (@${2:array}) {\n    ${3:# body...}\n}" "foreach ... { ... }" nil nil nil nil nil)
                       ("if" "if ($1) {\n    $0\n}" "if (...) { ... }" nil nil nil nil nil)
                       ("ife" "if ($1) {\n    $2\n} else {\n    $3\n}" "if (...) { ... } else { ... }" nil nil nil nil nil)
                       ("ifee" "if ($1) {\n	${2:# body...}\n} elsif ($3) {\n	${4:# elsif...}\n} else {\n	${5:# else...}\n}" "if, elsif, else ..." nil nil nil nil nil)
                       ("sub" "sub ${1:function_name} {\n    $0\n}" "sub ... { ... }" nil nil nil nil nil)
                       ("unless" "unless ($1) {\n    $0\n}" "unless (...) { ... }" nil nil nil nil nil)
                       ("while" "while ($1) {\n    $0\n}" "while (...) { ... }" nil nil nil nil nil)
                       ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil nil nil nil)
                       ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil nil nil nil)
                       ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil nil nil nil)
                       ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil nil nil nil))
                     '(text-mode))


;;; snippets for python-mode
(yas/define-snippets 'python-mode
                     '(("__" "__${init}__" "__...__" nil nil nil nil nil)
                       ("class" "class ${1:ClassName}(${2:object}):\n    \"\"\"$3\n    \"\"\"\n\n    def __init__(self, $4):\n        \"\"\"$5\n        ${4:$\n        (let* ((indent\n                (concat \"\\n\" (make-string (current-column) 32)))\n               (args\n                (mapconcat\n                 '(lambda (x)\n                    (if (not (string= (nth 0 x) \"\"))\n                        (concat \"- \" (char-to-string 96) (nth 0 x)\n                                (char-to-string 96) \":\")))\n                 (mapcar\n                  '(lambda (x)\n                     (mapcar\n                      (lambda (x)\n                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))\n                  (mapcar '(lambda (x) (split-string x \"=\"))\n                          (split-string text \",\")))\n                 indent)))\n          (if (string= args \"\")\n              (make-string 3 34)\n            (mapconcat\n             'identity\n             (list \"\" \"Arguments:\" args (make-string 3 34))\n             indent)))\n        }\n        ${4:$\n        (mapconcat\n         '(lambda (x)\n            (if (not (string= (nth 0 x) \"\"))\n                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n         (mapcar\n          '(lambda (x)\n             (mapcar\n              '(lambda (x)\n                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n              x))\n          (mapcar '(lambda (x) (split-string x \"=\"))\n                  (split-string text \",\")))\n         (concat \"\\n\" (make-string (current-column) 32)))\n        }\n        $0\n" "class" nil nil nil nil nil)
                       ("def" "def ${1:name}($2):\n    \"\"\"$3\n    ${2:$\n      (let* \n        ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0\n" "def" nil nil nil nil nil)
                       ("defm" "def ${1:name}(self, $2):\n    \"\"\"$3\n    ${2:$\n    (let* ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0\n" "defm" nil nil nil nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil)
                       ("ifmain" "if __name__ == '__main__':\n    $0" "if __name__ == '__main__': ..." nil nil nil nil nil)
                       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n   def fset(self, value):\n       self._$1 = value\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil nil nil nil)
                       ("propg" "def _get_${1:foo}(self):\n    return self._$1\n\n$1 = property(_get_$1)\n\n$0\n" "_get_foo ... foo=property(...)" nil nil nil nil nil)
                       ("propsg" "def _set_${1:foo}(self, value):\n    self._$1 = value\n\ndef _get_$1(self):\n    return self._$1\n\n$1 = property(_get_$1, _set_$1)\n\n$0\n" "_get_foo ... _set_foo ... foo=property(...)" nil nil nil nil nil)
                       ("while" "while ${condition}:\n    $0" "while ... : ..." nil nil nil nil nil))
                     '(text-mode))


;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
                     '(("chap" "${1:Chapter}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Chapter title" nil nil nil nil nil)
                       ("sec" "${1:Section}\n${1:$(make-string (string-width text) ?\\-)}\n\n$0" "Section title" nil nil nil nil nil)
                       ("tit" "${1:$(make-string (string-width text) ?\\=)}\n${1:Title}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Document title" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
                     '(("=b" "=begin rdoc\n  $0\n=end" "=begin rdoc ... =end" nil "general" nil nil nil)
                       ("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil "definitions" nil nil nil)
                       ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil "collections" nil nil nil)
                       ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil "definitions" nil nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "collections" nil nil nil)
                       ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil "general" nil nil nil)
                       ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil "general" nil nil nil)
                       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil "general" nil nil nil)
                       ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil "definitions" nil nil nil)
                       ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil "collections" nil nil nil)
                       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n				 (or (buffer-file-name)\n				     (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend\n" "class ... end" nil "definitions" nil nil nil)
                       ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil "collections" nil nil nil)
                       ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil "general" nil nil nil)
                       ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil "collections" nil nil nil)
                       ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil "collections" nil nil nil)
                       ("dow" "downto(${0}) { |${n}|\n  $0\n}" "downto(...) { |n| ... }" nil "control structure" nil nil nil)
                       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil "collections" nil nil nil)
                       ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil "collections" nil nil nil)
                       ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil "collections" nil nil nil)
                       ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil "collections" nil nil nil)
                       ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil "collections" nil nil nil)
                       ("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil "control structure" nil nil nil)
                       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil "control structure" nil nil nil)
                       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil "control structure" nil nil nil)
                       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil "collections" nil nil nil)
                       ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil "definitions" nil nil nil)
                       ("r" "attr_reader :" "attr_reader ..." nil "definitions" nil nil nil)
                       ("rb" "#!/usr/bin/ruby -wKU\n" "/usr/bin/ruby -wKU" nil "general" nil nil nil)
                       ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil "collections" nil nil nil)
                       ("req" "require \"$0\"" "require \"...\"" nil "general" nil nil nil)
                       ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil "general" nil nil nil)
                       ("rw" "attr_accessor :" "attr_accessor ..." nil "definitions" nil nil nil)
                       ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil "collections" nil nil nil)
                       ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil "control structure" nil nil nil)
                       ("until" "until ${condition}\n  $0\nend" "until ... end" nil "control structure" nil nil nil)
                       ("upt" "upto(${n}) { |${i}|\n  $0\n}" "upto(...) { |n| ... }" nil "control structure" nil nil nil)
                       ("w" "attr_writer :" "attr_writer ..." nil "definitions" nil nil nil)
                       ("when" "when ${condition}\n  $0\nend" "when ... end" nil "control structure" nil nil nil)
                       ("while" "while ${condition}\n  $0\nend" "while ... end" nil "control structure" nil nil nil)
                       ("y" ":yields: $0" ":yields: arguments (rdoc)" nil "general" nil nil nil)
                       ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil "collections" nil nil nil))
                     '(text-mode))


;;; snippets for scala-mode
(yas/define-snippets 'scala-mode
                     '(("act" "def act = {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "def act = { ..}" nil nil nil nil nil)
                       ("act" "def act(${1:arg}: ${2:type}) = {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "def act(arg: T) = { ..}" nil nil nil nil nil)
                       ("actor" "val a = actor {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "val a = actor { ..}" nil nil nil nil nil)
                       ("ano" "($1) => ${2:body} $0" "(args) => ..." nil nil nil nil nil)
                       ("app" "object ${1:name} extends Application {\n  $0\n}" "object name extends Application" nil nil nil nil nil)
                       ("arr" "Array[${1:value}](${2:args}) $0" "Array[T](..)" nil nil nil nil nil)
                       ("arr" "val ${1:arr} = Array[${2:value}](${3:args}) $0" "val a = Array[T](..)" nil nil nil nil nil)
                       ("asof" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil nil nil nil)
                       ("ass" "assert(${1:x} === ${2:y}) $0" "assert(x === y)" nil nil nil nil nil)
                       ("ass" "assert(true) $0" "assert(true)" nil nil nil nil nil)
                       ("at" "@author ${1:name} $0" "@author name" nil nil nil nil nil)
                       ("at" "@param ${1:name} ${2:description} $0" "@param name description" nil nil nil nil nil)
                       ("at" "@return ${1:description} $0" "@return description" nil nil nil nil nil)
                       ("at" "@version ${1:0.1} $0" "@version number" nil nil nil nil nil)
                       ("bang" "${1:actor} ! ${2:message} $0" "actor ! message" nil nil nil nil nil)
                       ("case" "case ${1:pattern} => $0" "case pattern => " nil nil nil nil nil)
                       ("case" "case _ => $0" "case _ => " nil nil nil nil nil)
                       ("cast" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil nil nil nil)
                       ("cc" "case class ${1:name}(${2:arg}: ${3:type}) $0" "case class T(arg: A)" nil nil nil nil nil)
                       ("cl" "class ${1:name} {\n  $0\n}" "class T { .. }" nil nil nil nil nil)
                       ("cl" "abstract class ${1:name} {\n  $0\n}" "abstract class T { .. }" nil nil nil nil nil)
                       ("cl" "abstract class ${1:name}(${2:args}) {\n  $0\n}" "abstract class T(args) { .. }" nil nil nil nil nil)
                       ("cl" "class ${1:name}(${2:args}) {\n  $0\n}" "class T(args) { .. }" nil nil nil nil nil)
                       ("clof" "classOf[${1:type}] $0" "classOf[T] " nil nil nil nil nil)
                       ("co" "case object ${1:name} $0" "case object T" nil nil nil nil nil)
                       ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil nil nil nil)
                       ("cons" "${1:element1} :: Nil $0\n" "element1 :: Nil" nil nil nil nil nil)
                       ("def" "def ${1:name}(${2:args}) = $0" "def f(arg: T) = ..." nil nil nil nil nil)
                       ("def" "def ${1:name}(${2:args}) = {\n  $0\n}" "def f(arg: T) = {...}" nil nil nil nil nil)
                       ("def" "def ${1:name}(${2:args}): ${3:Unit} = $0" "def f(arg: T): R = ..." nil nil nil nil nil)
                       ("def" "def ${1:name}(${2:args}): ${3:Unit} = {\n  $0\n}" "def f(arg: T): R = {...}" nil nil nil nil nil)
                       ("def" "def ${1:name} = {\n  $0\n}" "def f = {...}" nil nil nil nil nil)
                       ("def" "def ${1:name}: ${2:Unit} = $0" "def f: R = ..." nil nil nil nil nil)
                       ("def" "def ${1:name}: ${3:Unit} = {\n  $0\n}" "def f: R = {...}" nil nil nil nil nil)
                       ("def" "def ${1:name} = $0" "def f = ..." nil nil nil nil nil)
                       ("doc" "/** \n * `(scala-mode-find-clstrtobj-name-doc)`\n * ${1:description}\n * $0  \n */" "/** cls/trt/obj name */" nil nil nil nil nil)
                       ("doc" "/** \n * `(scala-mode-def-and-args-doc)`\n */ " "/** method name */" nil nil nil nil nil)
                       ("doc" "/**\n * `(scala-mode-file-doc)`\n * $0\n * @author ${1:name}\n * @version ${2:0.1} \n */" "/** file name */" nil nil nil nil nil)
                       ("doc" "/*                     __                                               *\\\n**     ________ ___   / /  ___     Scala $3                               **\n**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")` , LAMP/EPFL             **\n**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **\n** /____/\\___/_/ |_/____/_/ | |                                         **\n**                          |/                                          **\n\\*                                                                      */\n/** \n * $0\n * @author ${1:name} \n * @version ${2:0.1}\n * $Id$\n */" "/** scala file */" nil nil nil nil nil)
                       ("doc" "/*                     __                                               *\\\n**     ________ ___   / /  ___     Scala API                            **\n**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")`, LAMP/EPFL             **\n**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **\n** /____/\\___/_/ |_/____/_/ | |                                         **\n**                          |/                                          **\n\\*                                                                      */\n/** \n * $0\n * @author ${1:name} \n * @version ${2:0.1}\n * $Id$\n */" "/** scala api file */" nil nil nil nil nil)
                       ("doc" "/**\n * ${1:description}\n * $0\n */" "/** ... */" nil nil nil nil nil)
                       ("expect" "expect(${1:reply}) {\n  $0\n}" "expect(value) { ..}" nil nil nil nil nil)
                       ("ext" "extends $0" "extends T" nil nil nil nil nil)
                       ("for" "${1:x} <- ${2:xs}" "x <- xs" nil nil nil nil nil)
                       ("for" "for (${1:x} <- ${2:xs} if ${3:guard}) {\n  $0\n}" "for (x <- xs if guard) { ... }" nil nil nil nil nil)
                       ("for" "for (${1:x} <- ${2:xs}) {\n  $0\n}" "for (x <- xs) { ... }" nil nil nil nil nil)
                       ("for" "for {\n  ${1:x} <- ${2:xs}\n  ${3:x} <- ${4:xs}\n} {\n  yield $0\n}" "for {x <- xs \\ y <- ys} { yield }" nil nil nil nil nil)
                       ("foreach" "foreach(${1:x} => ${2:body}) $0" "foreach(x => ..)" nil nil nil nil nil)
                       ("hmap" "new HashMap[${1:key}, ${2:value}] $0" "new HashMap[K, V]" nil nil nil nil nil)
                       ("hmap" "val ${1:m} = new HashMap[${2:key}, ${3:value}] $0" "val m = new HashMap[K, V]" nil nil nil nil nil)
                       ("hset" "new HashSet[${1:key}] $0\n" "new HashSet[K]" nil nil nil nil nil)
                       ("hset" "val ${1:m} = new HashSet[${2:key}] $0" "val m = new HashSet[K]" nil nil nil nil nil)
                       ("if" "if (${1:condition}) {\n  $0\n}" "if (cond) { .. }" nil nil nil nil nil)
                       ("if" "if (${1:condition}) {\n  $2\n} else {\n  $0\n}" "if (cond) { .. } else { .. }" nil nil nil nil nil)
                       ("imp" "import $0" "import .." nil nil nil nil nil)
                       ("intercept" "intercept(classOf[${1:Exception]}) {\n  $0\n}" "intercept(classOf[T]) { ..}" nil nil nil nil nil)
                       ("isof" "isInstanceOf[${1:type}] $0" "isInstanceOf[T] " nil nil nil nil nil)
                       ("ls" "List(${1:args}, ${2:args}) $0" "List(..)" nil nil nil nil nil)
                       ("ls" "val ${1:l} = List(${2:args}, ${3:args}) $0" "val l = List(..)" nil nil nil nil nil)
                       ("main" "def main(args: Array[String]) = {\n  $0\n}" "def main(args: Array[String]) = { ... }" nil nil nil nil nil)
                       ("map" "map(${1:x} => ${2:body}) $0" "map(x => ..)" nil nil nil nil nil)
                       ("map" "Map(${1:key} -> ${2:value}) $0" "Map(key -> value)" nil nil nil nil nil)
                       ("match" "${1:cc} match {\n  case ${2:pattern} => $0\n}" "cc match { .. }" nil nil nil nil nil)
                       ("match" "${1:option} match {\n  case Full(res) => $0\n\n  case Empty => \n\n  case Failure(msg, _, _) => \n\n}" "can match { case Full(res) => .. }" nil nil nil nil nil)
                       ("match" "${1:option} match {\n  case None => $0\n  case Some(res) => \n\n}" "option match { case None => .. }" nil nil nil nil nil)
                       ("mix" "trait ${1:name} {\n  $0\n}" "trait T { .. }" nil nil nil nil nil)
                       ("ob" "object ${1:name} extends ${2:type} $0" "object name extends T" nil nil nil nil nil)
                       ("pac" "package $0" "package .." nil nil nil nil nil)
                       ("pr" "println(${1:obj}) $0" "println(..)" nil nil nil nil nil)
                       ("pr" "print(${1:obj}) $0" "print(..)" nil nil nil nil nil)
                       ("pr" "println(\"${1:msg}\") $0" "println(\"..\")" nil nil nil nil nil)
                       ("pr" "println(\"${1:obj}: \" + ${1:obj}) $0" "println(\"obj: \" + obj)" nil nil nil nil nil)
                       ("pri" "private $0" "private" nil nil nil nil nil)
                       ("pri" "private[${1:this}] $0" "private[this]" nil nil nil nil nil)
                       ("pro" "protected $0" "protected" nil nil nil nil nil)
                       ("pro" "protected[${1:this}] $0" "protected[this]" nil nil nil nil nil)
                       ("suite" "import org.scalatest._\n\nclass ${1:name} extends Suite {\n  $0\n}" "class T extends Suite { .. }" nil nil nil nil nil)
                       ("test" "//@Test\ndef test${1:name} = {\n  $0\n}" "@Test def testX = ..." nil nil nil nil nil)
                       ("throw" "throw new ${1:Exception}(${2:msg}) $0" "throw new Exception" nil nil nil nil nil)
                       ("tr" "trait ${1:name} {\n  $0\n}" "trait T { .. }" nil nil nil nil nil)
                       ("tr" "trait ${1:name} extends ${2:class} {\n  $0\n}" "trait T extends C { .. }" nil nil nil nil nil)
                       ("tr" "trait ${1:name} extends ${2:class} with ${3:trait} {\n  $0\n}" "trait T1 extends C with T2 { .. }" nil nil nil nil nil)
                       ("tr" "trait ${1:name} with ${2:trait} {\n  $0\n}" "trait T1 with T2 { .. }" nil nil nil nil nil)
                       ("try" "try {\n  $0\n} catch {\n  case ${1:e}: ${2:Exception} => \n    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}\n}" "try { .. } catch { case e => ..}" nil nil nil nil nil)
                       ("try" "try {\n  $0\n} catch {\n  case ${1:e}: ${2:Exception} => \n    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}\n} finally {\n\n}" "try { .. } catch { case e => ..} finally { ..}" nil nil nil nil nil)
                       ("try" "try {\n\n} finally {\n  $0\n}" "try { .. } finally { .. }" nil nil nil nil nil)
                       ("tup" "${1:element1} -> ${2:element2} $0" "element1 -> element2" nil nil nil nil nil)
                       ("tup" "(${1:element1}, ${2:element2}) $0" "(element1, element2)" nil nil nil nil nil)
                       ("val" "val ${1:name} = ${2:obj} $0" "val name = .." nil nil nil nil nil)
                       ("val" "val ${1:name} = new ${2:obj} $0" "val name = new .." nil nil nil nil nil)
                       ("val" "val ${1:name}: ${2:T} = ${3:obj} $0\n" "val name: T = .." nil nil nil nil nil)
                       ("var" "var ${1:name} = ${2:obj} $0\n" "var name = .." nil nil nil nil nil)
                       ("var" "var ${1:name} = new ${2:obj} $0\n" "var name = new .." nil nil nil nil nil)
                       ("var" "var ${1:name}: ${2:T} = ${3:obj} $0\n" "var name: T = .." nil nil nil nil nil)
                       ("whi" "while (${1:condition}) {\n  $0\n}" "while(cond) { .. }" nil nil nil nil nil)
                       ("with" "with $0" "with T" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for snippet-mode
(yas/define-snippets 'snippet-mode
                     '(("$f" "\\${${1:${2:n}:}$3${4:\\$(${5:lisp-fn})}\\}$0" "${ ...  } field" nil nil nil nil nil)
                       ("$m" "\\${${2:n}:${4:\\$(${5:reflection-fn})}\\}$0" "${n:$(...)} mirror" nil nil nil nil nil)
                       ("vars" "# name : $1${2:\n# key : ${3:expand-key}}${4:\n# group : ${5:group}} \n# contributor : $6\n# --\n$0" "Snippet header" nil nil nil nil nil))
                     '(text-mode))


;;; snippets for sql-mode
(yas/define-snippets 'sql-mode
                     '(("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}\n" ", ColumnName ColumnType NOT NULL..." nil nil nil nil nil)
                       ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] PRIMARY KEY ..." nil nil nil nil nil)
                       ("constraint" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] FOREIGN KEY ..." nil nil nil nil nil)
                       ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] \n(\n		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}\n$0\n	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) \n)\nGO\n" "create table ..." nil nil nil nil nil)
                       ("create" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] \n(\n		$3		$4		= ${5:NULL}		${6:OUTPUT}\n)\nAS\nBEGIN\n$0\nEND\nGO\n" "create procedure ..." nil nil nil nil nil)
                       ("references" "REFERENCES ${1:TableName}([${2:ColumnName}])\n" "REFERENCES ..." nil nil nil nil nil))
                     '(text-mode))


(yas/global-mode 1)
)

(yas/initialize-bundle)
;;;###autoload(require 'yasnippet-bundle)
(set-default 'yas/dont-activate
             #'(lambda nil
                 (and
                  (or yas/root-directory
                      (featurep 'yas-custom))
                  (null
                   (yas/get-snippet-tables)))))
(provide 'yas-custom)
;;; yas-custom.el ends here
