;;; dracula-theme.el --- Dracula Theme

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: film42
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/dracula/emacs

;;; Commentary:

;; A dark color theme available for a number of editors.

;;; Code:
(deftheme dracula)


;;;; Configuration options:

(defgroup dracula nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula)

(defcustom dracula-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula)

(defcustom dracula-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula)

(defvar dracula-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Dracula theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/dracula/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (dracula-bg      "#282a36" "unspecified-bg" "unspecified-bg") ; official background
                (dracula-fg      "#f8f8f2" "#ffffff" "brightwhite") ; official foreground
                (dracula-current "#44475a" "#303030" "brightblack") ; official current-line/selection
                (dracula-comment "#6272a4" "#5f5faf" "blue")        ; official comment
                (dracula-cyan    "#8be9fd" "#87d7ff" "brightcyan")  ; official cyan
                (dracula-green   "#50fa7b" "#5fff87" "green")       ; official green
                (dracula-orange  "#ffb86c" "#ffaf5f" "brightred")   ; official orange
                (dracula-pink    "#ff79c6" "#ff87d7" "magenta")     ; official pink
                (dracula-purple  "#bd93f9" "#af87ff" "brightmagenta") ; official purple
                (dracula-red     "#ff5555" "#ff8787" "red")         ; official red
                (dracula-yellow  "#f1fa8c" "#ffff87" "yellow")      ; official yellow
                ;; Other colors
                (bg2             "#373844" "#121212" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default
               (cursor :background ,fg3)
               (default :background ,dracula-bg :foreground ,dracula-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,dracula-bg :foreground ,fg4)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,dracula-current :extend t)
               (info-quoted-name :foreground ,dracula-orange)
               (info-string :foreground ,dracula-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,dracula-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,dracula-bg)
               (line-number :slant italic :foreground ,bg4 :background ,dracula-bg)
               (match :background ,dracula-yellow :foreground ,dracula-bg)
               (minibuffer-prompt
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-fg)
                    (list :weight 'bold :foreground dracula-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (shadow :foreground ,dracula-comment)
               (trailing-whitespace :foreground "unspecified-fg" :background ,dracula-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,dracula-green)
               (warning :foreground ,dracula-orange)
               (error :foreground ,dracula-red)
               (header-line :background ,dracula-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-orange)
               (font-lock-comment-face :inherit shadow)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,dracula-cyan)
               (font-lock-doc-face :foreground ,dracula-comment)
               (font-lock-function-name-face :foreground ,dracula-green :weight bold)
               (font-lock-keyword-face :foreground ,dracula-pink :weight bold)
               (font-lock-negation-char-face :foreground ,dracula-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-orange)
               (font-lock-reference-face :foreground ,dracula-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-purple)
               (font-lock-string-face :foreground ,dracula-yellow)
               (font-lock-type-face :foreground ,dracula-purple)
               (font-lock-variable-name-face :foreground ,dracula-fg :weight bold)
               (font-lock-warning-face :foreground ,dracula-orange :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,dracula-pink)
               ;; company
               (company-echo-common :foreground ,dracula-bg :background ,dracula-fg)
               (company-preview :background ,dracula-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,dracula-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,dracula-green)
               (company-scrollbar-bg :background ,dracula-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,dracula-fg :background ,dracula-current)
               (company-tooltip-search :foreground ,dracula-green
                                       :underline t)
               (company-tooltip-search-selection :background ,dracula-green
                                                 :foreground ,dracula-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,dracula-bg)
               (company-tooltip-common :foreground ,dracula-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,dracula-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,dracula-green)
               (completions-first-difference :foreground ,dracula-pink :weight bold)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-orange :background ,dracula-orange)
               (diff-hl-delete :foreground ,dracula-red :background ,dracula-red)
               (diff-hl-insert :foreground ,dracula-green :background ,dracula-green)
               ;; dired
               (dired-directory :foreground ,dracula-green :weight normal)
               (dired-flagged :foreground ,dracula-pink)
               (dired-header :foreground ,fg3 :background ,dracula-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-fg :weight bold)
               (dired-marked :foreground ,dracula-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,dracula-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,dracula-fg)
               (diredp-deletion-file-name :foreground ,dracula-pink :background ,dracula-current)
               (diredp-deletion :foreground ,dracula-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,dracula-orange)
               (diredp-file-name :foreground ,dracula-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,dracula-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,dracula-current)
               (diredp-ignored-file-name :foreground ,dracula-fg)
               (diredp-mode-line-flagged :foreground ,dracula-orange)
               (diredp-mode-line-marked :foreground ,dracula-orange)
               (diredp-no-priv :foreground ,dracula-fg)
               (diredp-number :foreground ,dracula-cyan)
               (diredp-other-priv :foreground ,dracula-orange)
               (diredp-rare-priv :foreground ,dracula-orange)
               (diredp-read-priv :foreground ,dracula-purple)
               (diredp-write-priv :foreground ,dracula-pink)
               (diredp-exec-priv :foreground ,dracula-yellow)
               (diredp-symlink :foreground ,dracula-orange)
               (diredp-link-priv :foreground ,dracula-orange)
               (diredp-autofile-name :foreground ,dracula-yellow)
               (diredp-tagged-autofile-name :foreground ,dracula-yellow)
               ;; elfeed
               (elfeed-search-date-face :foreground ,dracula-comment)
               (elfeed-search-title-face :foreground ,dracula-fg)
               (elfeed-search-unread-title-face :foreground ,dracula-pink :weight bold)
               (elfeed-search-feed-face :foreground ,dracula-fg :weight bold)
               (elfeed-search-tag-face :foreground ,dracula-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,dracula-pink)
               (elfeed-search-filter-face :foreground ,dracula-green :weight bold)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,dracula-red)
               (elfeed-log-warn-level-face :foreground ,dracula-orange)
               (elfeed-log-info-level-face :foreground ,dracula-cyan)
               (elfeed-log-debug-level-face :foreground ,dracula-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,dracula-pink
                                       ,@(when dracula-enlarge-headings
                                           (list :height dracula-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,dracula-purple
                                       ,@(when dracula-enlarge-headings
                                           (list :height dracula-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,dracula-green
                                       ,@(when dracula-enlarge-headings
                                           (list :height dracula-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,dracula-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-yellow)
               (enh-ruby-op-face :foreground ,dracula-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-purple)
               (font-latex-italic-face :foreground ,dracula-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-fg)
               (font-latex-string-face :foreground ,dracula-yellow)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,dracula-pink
                                      ,@(when dracula-enlarge-headings
                                          (list :height dracula-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,dracula-purple
                                      ,@(when dracula-enlarge-headings
                                          (list :height dracula-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,dracula-green
                                      ,@(when dracula-enlarge-headings
                                          (list :height dracula-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,dracula-yellow)
               (gemini-quote-face :foreground ,dracula-purple)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,dracula-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-purple)
               (gnus-header-from :foreground ,dracula-fg)
               (gnus-header-name :foreground ,dracula-green)
               (gnus-header-subject :foreground ,dracula-pink :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,dracula-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pink)
               (haskell-constructor-face :foreground ,dracula-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,dracula-purple)
               (helm-buffer-not-saved :foreground ,dracula-purple :background ,dracula-bg)
               (helm-buffer-process :foreground ,dracula-orange :background ,dracula-bg)
               (helm-buffer-saved-out :foreground ,dracula-fg :background ,dracula-bg)
               (helm-buffer-size :foreground ,dracula-fg :background ,dracula-bg)
               (helm-candidate-number :foreground ,dracula-bg :background ,dracula-fg)
               (helm-ff-directory :foreground ,dracula-green :background ,dracula-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,dracula-green :background ,dracula-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,dracula-bg :weight normal)
               (helm-ff-file :foreground ,dracula-fg :background ,dracula-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,dracula-pink :background ,dracula-bg :weight bold)
               (helm-ff-prefix :foreground ,dracula-bg :background ,dracula-pink :weight normal)
               (helm-ff-symlink :foreground ,dracula-pink :background ,dracula-bg :weight bold)
               (helm-grep-cmd-line :foreground ,dracula-fg :background ,dracula-bg)
               (helm-grep-file :foreground ,dracula-fg :background ,dracula-bg)
               (helm-grep-finish :foreground ,fg2 :background ,dracula-bg)
               (helm-grep-lineno :foreground ,dracula-fg :background ,dracula-bg)
               (helm-grep-match :foreground "unspecified-fg" :background "unspecified-bg" :inherit helm-match)
               (helm-grep-running :foreground ,dracula-green :background ,dracula-bg)
               (helm-header :foreground ,fg2 :background ,dracula-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,dracula-green :background ,dracula-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,dracula-purple :background ,dracula-bg)
               (helm-source-go-package-godoc-description :foreground ,dracula-yellow)
               (helm-source-header :foreground ,dracula-pink :background ,dracula-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,dracula-orange :background ,dracula-bg)
               (helm-time-zone-home :foreground ,dracula-purple :background ,dracula-bg)
               (helm-visible-mark :foreground ,dracula-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,dracula-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,dracula-fg)
               (icicle-search-current-input :foreground ,dracula-pink)
               (icicle-search-context-level-8 :foreground ,dracula-orange)
               (icicle-search-context-level-7 :foreground ,dracula-orange)
               (icicle-search-context-level-6 :foreground ,dracula-orange)
               (icicle-search-context-level-5 :foreground ,dracula-orange)
               (icicle-search-context-level-4 :foreground ,dracula-orange)
               (icicle-search-context-level-3 :foreground ,dracula-orange)
               (icicle-search-context-level-2 :foreground ,dracula-orange)
               (icicle-search-context-level-1 :foreground ,dracula-orange)
               (icicle-search-main-regexp-current :foreground ,dracula-fg)
               (icicle-saved-candidate :foreground ,dracula-fg)
               (icicle-proxy-candidate :foreground ,dracula-fg)
               (icicle-mustmatch-completion :foreground ,dracula-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,dracula-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,dracula-orange)
               (icicle-match-highlight-Completions :foreground ,dracula-green)
               (icicle-key-complete-menu-local :foreground ,dracula-fg)
               (icicle-key-complete-menu :foreground ,dracula-fg)
               (icicle-input-completion-fail-lax :foreground ,dracula-pink)
               (icicle-input-completion-fail :foreground ,dracula-pink)
               (icicle-historical-candidate-other :foreground ,dracula-fg)
               (icicle-historical-candidate :foreground ,dracula-fg)
               (icicle-current-candidate-highlight :foreground ,dracula-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,dracula-fg)
               (icicle-complete-input :foreground ,dracula-orange)
               (icicle-common-match-highlight-Completions :foreground ,dracula-purple)
               (icicle-candidate-part :foreground ,dracula-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-green)
                    (list :weight 'bold :foreground dracula-pink)))
               (ido-only-match :foreground ,dracula-orange)
               (ido-subdir :foreground ,dracula-yellow)
               (ido-virtual :foreground ,dracula-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-fg :background ,dracula-pink)
               ;; ivy
               (ivy-current-match
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-green)
                    (list :weight 'bold :foreground dracula-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,dracula-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :background ,dracula-green
                                            :foreground ,dracula-bg)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,dracula-yellow
                                            :foreground ,dracula-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,dracula-pink
                                            :foreground ,dracula-bg)
               (ivy-confirm-face :foreground ,dracula-orange)
               (ivy-match-required-face :foreground ,dracula-red)
               (ivy-subdir :foreground ,dracula-yellow)
               (ivy-remote :foreground ,dracula-pink)
               (ivy-virtual :foreground ,dracula-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-bg :background ,dracula-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,dracula-cyan)
               (jde-java-font-lock-modifier-face :foreground ,dracula-pink)
               (jde-java-font-lock-number-face :foreground ,dracula-fg)
               (jde-java-font-lock-package-face :foreground ,dracula-fg)
               (jde-java-font-lock-private-face :foreground ,dracula-pink)
               (jde-java-font-lock-public-face :foreground ,dracula-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,dracula-purple)
               (js2-function-param :foreground ,dracula-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,dracula-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,dracula-yellow)
               (js2-private-function-call :foreground ,dracula-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,dracula-orange)
               (js3-external-variable-face :foreground ,dracula-fg)
               (js3-function-param-face :foreground ,dracula-pink)
               (js3-instance-member-face :foreground ,dracula-cyan)
               (js3-jsdoc-tag-face :foreground ,dracula-pink)
               (js3-warning-face :underline ,dracula-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,dracula-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,dracula-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,dracula-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,dracula-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,dracula-fg :weight ultra-bold
                                               :box (:line-width -1 :color dracula-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,dracula-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,dracula-bg)
               (lsp-ui-doc-header :foreground ,dracula-bg :background ,dracula-cyan)
               ;; magit
               (magit-branch-local :foreground ,dracula-cyan)
               (magit-branch-remote :foreground ,dracula-green)
               (magit-tag :foreground ,dracula-orange)
               (magit-section-heading :foreground ,dracula-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-orange
                                            :background ,dracula-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,dracula-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-green)
               (magit-diffstat-removed :foreground ,dracula-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,dracula-orange :weight bold)
               (magit-process-ok :foreground ,dracula-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-purple)
               (markdown-code-face :foreground ,dracula-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pink
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-purple
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-green
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-yellow)
               (markdown-header-face-5 :foreground ,dracula-cyan)
               (markdown-header-face-6 :foreground ,dracula-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,dracula-fg)
               (markdown-inline-code-face :foreground ,dracula-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-orange)
               (markdown-table-face :foreground ,dracula-purple)
               ;; message
               (message-header-to :foreground ,dracula-fg :weight bold)
               (message-header-cc :foreground ,dracula-fg :bold bold)
               (message-header-subject :foreground ,dracula-orange)
               (message-header-newsgroups :foreground ,dracula-purple)
               (message-header-other :foreground ,dracula-purple)
               (message-header-name :foreground ,dracula-green)
               (message-header-xheader :foreground ,dracula-cyan)
               (message-separator :foreground ,dracula-cyan :slant italic)
               (message-cited-text :foreground ,dracula-purple)
               (message-cited-text-1 :foreground ,dracula-purple)
               (message-cited-text-2 :foreground ,dracula-orange)
               (message-cited-text-3 :foreground ,dracula-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,dracula-green :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-current
                          :box ,dracula-current :inverse-video nil
                          ,@(if dracula-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-comment :background dracula-bg
                            :box dracula-bg)
                    (list :foreground dracula-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,dracula-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,dracula-purple)
               (mu4e-highlight-face :background ,dracula-bg
                                    :foreground ,dracula-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,dracula-current
                                           :foreground ,dracula-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,dracula-purple)
               (mu4e-cited-1-face :foreground ,dracula-purple)
               (mu4e-cited-2-face :foreground ,dracula-orange)
               (mu4e-cited-3-face :foreground ,dracula-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,dracula-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,dracula-purple)
               (neo-expand-btn-face :foreground ,dracula-fg)
               (neo-file-link-face :foreground ,dracula-cyan)
               (neo-header-face :background ,dracula-bg
                                :foreground ,dracula-fg
                                :weight bold)
               (neo-root-dir-face :foreground ,dracula-purple :weight bold)
               (neo-vc-added-face :foreground ,dracula-orange)
               (neo-vc-conflict-face :foreground ,dracula-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,dracula-orange)
               (neo-vc-ignored-face :foreground ,dracula-comment)
               (neo-vc-missing-face :foreground ,dracula-red)
               (neo-vc-needs-merge-face :foreground ,dracula-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,dracula-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,dracula-green)
               (neo-vc-user-face :foreground ,dracula-purple)
               ;; org
               (org-agenda-date :foreground ,dracula-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-comment)
               (org-agenda-done :foreground ,dracula-green)
               (org-agenda-structure :foreground ,dracula-purple)
               (org-block :foreground ,dracula-orange)
               (org-code :foreground ,dracula-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,dracula-comment)
               (org-document-title :weight bold :foreground ,dracula-orange
                                   ,@(when dracula-enlarge-headings
                                       (list :height dracula-height-doc-title)))
               (org-done :foreground ,dracula-green)
               (org-ellipsis :foreground ,dracula-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,dracula-pink)
               (org-headline-done :foreground ,dracula-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-bg :background ,dracula-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pink
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-purple
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-green
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-yellow)
               (org-level-5 :weight normal :foreground ,dracula-cyan)
               (org-level-6 :weight normal :foreground ,dracula-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,dracula-fg)
               (org-link :foreground ,dracula-cyan :underline t)
               (org-priority :foreground ,dracula-cyan)
               (org-scheduled :foreground ,dracula-green)
               (org-scheduled-previously :foreground ,dracula-yellow)
               (org-scheduled-today :foreground ,dracula-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,dracula-yellow)
               (org-table :foreground ,dracula-purple)
               (org-tag :foreground ,dracula-pink :weight bold :background ,bg2)
               (org-todo :foreground ,dracula-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,dracula-yellow)
               (org-warning :weight bold :foreground ,dracula-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pink)
               (outline-2 :foreground ,dracula-purple)
               (outline-3 :foreground ,dracula-green)
               (outline-4 :foreground ,dracula-yellow)
               (outline-5 :foreground ,dracula-cyan)
               (outline-6 :foreground ,dracula-orange)
               ;; powerline
               (powerline-active1 :background ,dracula-bg :foreground ,dracula-pink)
               (powerline-active2 :background ,dracula-bg :foreground ,dracula-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,dracula-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,dracula-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,dracula-green)
               (rpm-spec-doc-face :foreground ,dracula-pink)
               (rpm-spec-ghost-face :foreground ,dracula-purple)
               (rpm-spec-macro-face :foreground ,dracula-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,dracula-purple)
               (rpm-spec-section-face :foreground ,dracula-yellow)
               (rpm-spec-tag-face :foreground ,dracula-cyan)
               (rpm-spec-var-face :foreground ,dracula-orange)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,dracula-pink)
               (selectrum-secondary-highlight :foreground ,dracula-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,dracula-green)
               (speedbar-file-face :foreground ,dracula-cyan)
               (speedbar-directory-face :foreground ,dracula-purple)
               (speedbar-tag-face :foreground ,dracula-yellow)
               (speedbar-selected-face :foreground ,dracula-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,dracula-bg
                                        :foreground ,dracula-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-purple :background ,dracula-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pink :background ,dracula-bg
                            :box (:line-width 2 :color ,dracula-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,dracula-purple :background ,dracula-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,dracula-pink :background ,dracula-bg
                             :box (:line-width 2 :color ,dracula-bg :style nil))
               (tab-line-tab-inactive :foreground ,dracula-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-red)
               ;; term
               (term :foreground ,dracula-fg :background ,dracula-bg)
               (term-color-black :foreground ,dracula-bg :background ,dracula-comment)
               (term-color-blue :foreground ,dracula-purple :background ,dracula-purple)
               (term-color-cyan :foreground ,dracula-cyan :background ,dracula-cyan)
               (term-color-green :foreground ,dracula-green :background ,dracula-green)
               (term-color-magenta :foreground ,dracula-pink :background ,dracula-pink)
               (term-color-red :foreground ,dracula-red :background ,dracula-red)
               (term-color-white :foreground ,dracula-fg :background ,dracula-fg)
               (term-color-yellow :foreground ,dracula-yellow :background ,dracula-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,dracula-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,dracula-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,dracula-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-green)
               (web-mode-html-tag-face :foreground ,dracula-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pink)
               (web-mode-string-face :foreground ,dracula-yellow)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,dracula-green)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-red :foreground ,dracula-red)
               (whitespace-empty :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-hspace :background ,bg3 :foreground ,dracula-comment)
               (whitespace-indentation :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-line :background ,dracula-bg :foreground ,dracula-pink)
               (whitespace-newline :foreground ,dracula-comment)
               (whitespace-space :background ,dracula-bg :foreground ,dracula-comment)
               (whitespace-space-after-tab :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-space-before-tab :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-tab :background ,bg2 :foreground ,dracula-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula
         (let ((expand-with-func
                (lambda (func spec)
                  (when (and (eq func 'caddr)
                             dracula-use-24-bit-colors-on-256-colors-terms)
                    (setq func 'cadr))
                  (let (reduced-color-list)
                    (eval `(let ,(dolist (col colors reduced-color-list)
                                   (push `(,(car col) ,(funcall func col))
                                         reduced-color-list))
                             (eval `(backquote ,spec)))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ,(funcall expand-with-func 'caddr spec))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'dracula
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'dracula-bg colors))
               ,(funcall get-func (alist-get 'dracula-red colors))
               ,(funcall get-func (alist-get 'dracula-green colors))
               ,(funcall get-func (alist-get 'dracula-yellow colors))
               ,(funcall get-func (alist-get 'dracula-comment colors))
               ,(funcall get-func (alist-get 'dracula-purple colors))
               ,(funcall get-func (alist-get 'dracula-cyan colors))
               ,(funcall get-func (alist-get 'dracula-fg colors))])))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-theme.el ends here
