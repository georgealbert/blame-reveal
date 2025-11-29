;;; blame-reveal.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in the fringe with adaptive color gradients.
;; Color intensity based on commit recency within an intelligent time window.
;; Show commit info above each block with sticky headers.
;; Performance optimized: lazy loading, smart caching, and viewport rendering.
;;
;; Key Features:
;; - Smart time-based commit selection with auto-calculation
;; - Gradient quality control (strict/auto/relaxed)
;; - Lazy loading for large files (configurable threshold)
;; - Incremental commit info loading with caching
;; - Recursive blame navigation with historical context
;; - Sticky headers for long commits
;; - Theme-aware color schemes with HSL customization
;; - Customizable display layouts (line/compact/full/none)
;;
;; Quick Start:
;;   M-x blame-reveal-mode
;;
;; Common Customizations:
;;   (setq blame-reveal-recent-days-limit 'auto)      ; Smart time window
;;   (setq blame-reveal-gradient-quality 'auto)       ; Balanced quality
;;   (setq blame-reveal-display-layout 'compact)      ; Header format
;;   (setq blame-reveal-color-scheme '(:hue 210 ...)) ; Color theme
;;
;; See full documentation: https://github.com/lucius-chen/blame-reveal

;;; Code:

(require 'vc-git)
(require 'cl-lib)


;;;; Fringe Bitmap Definition

(define-fringe-bitmap 'blame-reveal-full
  [#b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111]
  16 8 'center)

(define-fringe-bitmap 'blame-reveal-loading-bright
  [#b11111111
   #b11111111
   #b11111111
   #b01111110
   #b01111110
   #b00111100
   #b00011000
   #b00000000]
  8 8 'center)

(define-fringe-bitmap 'blame-reveal-loading-dim
  [#b11111111
   #b11111111
   #b11111111
   #b01111110
   #b01111110
   #b00111100
   #b00011000
   #b00000000]
  8 8 'center)


;;;; Keymaps

(defvar blame-reveal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'blame-reveal-mode)
    (define-key map (kbd "c") #'blame-reveal-copy-commit-hash)
    (define-key map (kbd "d") #'blame-reveal-show-commit-diff)
    (define-key map (kbd "s") #'blame-reveal-show-commit-details)
    (define-key map (kbd "h") #'blame-reveal-show-file-history)
    (define-key map (kbd "l") #'blame-reveal-show-line-history)
    (define-key map (kbd "b") #'blame-reveal-blame-recursively)
    (define-key map (kbd "p") #'blame-reveal-blame-back)
    (define-key map (kbd "^") #'blame-reveal-blame-back)
    (define-key map (kbd "g") #'blame-reveal-blame-at-revision)
    (define-key map (kbd "r") #'blame-reveal-reset-to-head)
    map)
  "Keymap for blame-reveal-mode.")

(defvar blame-reveal--emulation-alist nil
  "Emulation mode map alist for blame-reveal.")


;;;; Timer Variables

(defvar blame-reveal--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar blame-reveal--scroll-timer nil
  "Timer to debounce scroll updates.")

(defvar blame-reveal--scroll-render-delay 0.3
  "Internal delay before rendering overlays after scrolling stops.
This is an implementation detail and should not be customized by users.")

(defvar blame-reveal-loading-animation-speed 0.08
  "Loading animation update interval in seconds.")

(defvar blame-reveal-loading-animation-frames-per-step 4
  "Number of frames before advancing to next step.")

(defvar blame-reveal--global-loading-animation-timer nil
  "Global timer for loading animation across all buffers.")


;;;; Buffer-Local State Variables

(defvar-local blame-reveal--temp-overlay-timer nil
  "Timer for delayed temp overlay rendering.")

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--header-overlay nil
  "Overlay for the currently displayed header.")

(defvar-local blame-reveal--sticky-header-overlay nil
  "Overlay for sticky header at window top.")

(defvar-local blame-reveal--header-update-timer nil
  "Timer for delayed header update.")

(defvar-local blame-reveal--last-rendered-commit nil
  "Commit hash of the last rendered block (for smooth transition).")

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--loading-animation-overlays nil
  "Overlays for loading animation.")

(defvar-local blame-reveal--loading-animation-step 0
  "Current step of loading animation.")

(defvar-local blame-reveal--loading-animation-frame-counter 0
  "Frame counter for animation step control.")

(defvar-local blame-reveal--loading-animation-sub-step 0
  "Sub-step for smooth interpolation (0.0 to 1.0).")

(defvar-local blame-reveal--color-strategy nil
  "Current color strategy instance.")


;;;; Data Cache Variables

(defvar-local blame-reveal--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local blame-reveal--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defvar-local blame-reveal--blame-data nil
  "Cached blame data for the file.
List of (LINE-NUMBER . COMMIT-HASH) pairs.")

(defvar-local blame-reveal--blame-data-range nil
  "Range of currently loaded blame data: (START-LINE . END-LINE).
nil means entire file is loaded.")

(defvar-local blame-reveal--timestamps nil
  "Cached min/max timestamps for color calculation.")

(defvar-local blame-reveal--recent-commits nil
  "List of recent commit hashes (most recent first) to colorize.")

(defvar-local blame-reveal--auto-days-cache nil
  "Cached auto-calculated days limit.
Format: (COMMIT-COUNT . DAYS).
Cache is invalidated when commit count changes significantly.")


;;;; Loading State Variables

(defvar-local blame-reveal--last-window-start nil
  "Last window start position to detect scrolling.")

(defvar-local blame-reveal--loading nil
  "Flag to indicate if blame data is being loaded.")

(defvar-local blame-reveal--all-commits-loaded nil
  "Flag indicating if all commits info has been loaded.")


;;;; Recursive Blame Variables
;;;; (These variables support the optional blame-reveal-recursive.el extension)

(defvar-local blame-reveal--blame-stack nil
  "Stack for recursive blame history.
Each element is a plist with :revision, :line, :blame-data, :commit-info, etc.")

(defvar-local blame-reveal--current-revision nil
  "Current revision being blamed (nil = HEAD, 'uncommitted = working tree).")

(defvar-local blame-reveal--revision-display nil
  "Display string for current revision (for mode line).")


;;;; Async Loading State Variables

(defvar-local blame-reveal--expansion-process nil
  "Background process for expanding blame data.")

(defvar-local blame-reveal--pending-expansion nil
  "Pending expansion range: (START-LINE . END-LINE).")

(defvar-local blame-reveal--expansion-buffer nil
  "Temporary buffer for expansion output.")

(defvar-local blame-reveal--is-expanding nil
  "Flag indicating if blame data is being expanded.
When t, suppress scroll-triggered rendering to avoid flicker.")

(defvar-local blame-reveal--initial-load-process nil
  "Background process for initial blame data loading.")

(defvar-local blame-reveal--initial-load-buffer nil
  "Temporary buffer for initial load output.")


;;;; Customization Group

(defgroup blame-reveal nil
  "Show git blame in fringe with colors.

Quick start:
  M-x blame-reveal-mode

Common customizations:
  - `blame-reveal-color-scheme': Change color scheme
  - `blame-reveal-recent-commit-count': How many commits to highlight
  - `blame-reveal-display-layout': Header display layout
  - `blame-reveal-use-magit': Use magit for commit details
  - `blame-reveal-lazy-load-threshold': Lazy loading threshold

See all options: M-x customize-group RET blame-reveal"
  :group 'vc
  :prefix "blame-reveal-")


;;;; Display Customization

(defcustom blame-reveal-style 'left-fringe
  "Which fringe to use for blame indicators."
  :type '(choice (const left-fringe)
                 (const right-fringe))
  :group 'blame-reveal)

(defcustom blame-reveal-display-layout 'compact
  "Format for commit message header.
- `none': no header (only fringe indicators, use 's' to view details)
- `line': show only the commit message line
- `compact': show message + metadata (hash, author, date)
- `full': show message + metadata + description

When set to `none', no header overlay is created when cursor moves
between blocks, providing a cleaner visual experience. You can still
view commit details using `blame-reveal-show-commit-details' (s key)."
  :type '(choice (const :tag "No header (fringe only)" none)
                 (const :tag "Message line only" line)
                 (const :tag "Message + metadata" compact)
                 (const :tag "Message + metadata + description" full))
  :group 'blame-reveal)

(defcustom blame-reveal-display-style 'background
  "Style for commit message header.
- 'background: No background color
- 'box: Use box border
- 'inverse: Inverse video"
  :type '(choice (const background)
                 (const box)
                 (const inverse))
  :group 'blame-reveal)

;;;; Typography Customization

(defcustom blame-reveal-header-weight 'bold
  "Font weight for the commit message line in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Extra-bold" extra-bold))
  :group 'blame-reveal)

(defcustom blame-reveal-header-height 1.0
  "Font height for the commit message line in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-weight 'normal
  "Font weight for the metadata line in blame header (hash, author, date)."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-height 0.9
  "Font height for the metadata line in blame header (hash, author, date).
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-description-weight 'normal
  "Font weight for the description lines in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-description-height 0.9
  "Font height for the description lines in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-header-position 'before-block
  "Position of commit message header relative to code block.
- `before-block': Show header above the first line of the block (default)
- `after-first-line': Show header after the first line of the block

When set to `after-first-line':
  - Header appears immediately after the first line of code
  - Maintains fixed distance from the code
  - For `compact' layout: shows as single line (msg · author · hash)
  - For `line' layout: shows only commit message
  - `full' layout is not supported in this mode (falls back to compact)"
  :type '(choice (const :tag "Before block (above first line)" before-block)
                 (const :tag "After first line (inline style)" after-first-line))
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Refresh all blame-reveal buffers when value changes
         (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when blame-reveal-mode
                 ;; Clear and recreate header
                 (when blame-reveal--header-overlay
                   (delete-overlay blame-reveal--header-overlay)
                   (setq blame-reveal--header-overlay nil))
                 ;; Force header update
                 (blame-reveal--update-header-impl))))))
  :group 'blame-reveal)


;;;; Commit Selection Customization

(defcustom blame-reveal-recent-days-limit 'auto
  "Time limit for highlighting recent commits.

Using days (instead of commit count) ensures consistent behavior
across projects with different commit frequencies.

Values:
  'auto  - Automatically calculate based on commit density and
           gradient quality. Adapts to both normal and recursive
           blame modes. (Recommended)

  number - Fixed days (e.g., 30, 90, 180, 365)
           Shows commits within this many days from reference point.
           In recursive blame, reference point is the revision date.

  nil    - No limit, highlight all commits with relative coloring
           (like IntelliJ IDEA)

Examples:
  'auto - Smart calculation, good for most cases
  30    - Last month (for very active files)
  90    - Last quarter
  180   - Last half year
  365   - Last year
  nil   - All commits (relative age coloring)

The actual number of commits within this period varies by file activity:
- Active files: many commits in the time window
- Quiet files: few commits in the time window

This is intentional and provides consistent time-based context
regardless of commit frequency."
  :type '(choice (const :tag "Auto (smart calculation)" auto)
                 (integer :tag "Fixed days")
                 (const :tag "No limit (all commits)" nil))
  :group 'blame-reveal)

(defcustom blame-reveal-gradient-quality 'auto
  "Control trade-off between time coverage and color distinction.

This setting affects how 'auto mode calculates the days limit.
It controls the target number of commits and minimum color difference.

'strict  - Fewer commits (5-10), excellent distinction (3-5% color steps)
           Best for visual clarity, shorter time window.
           Recommended for files with many commits.

'auto    - Balanced (10-20 commits), good distinction (2-3% color steps)
           Good balance between clarity and historical context.
           Recommended for most cases.

'relaxed - More commits (15-30), acceptable distinction (1.5-2% color steps)
           More historical context, colors may be subtle.
           Recommended for files with few commits.

Only used when `blame-reveal-recent-days-limit' is 'auto.
For fixed days limit, this setting is ignored.

Color distinction is measured by the lightness/saturation step between
consecutive commits in the gradient. Smaller steps mean colors are
harder to distinguish visually."
  :type '(choice (const :tag "Strict (best distinction)" strict)
                 (const :tag "Auto (balanced)" auto)
                 (const :tag "Relaxed (more history)" relaxed))
  :group 'blame-reveal)


;;;; Uncommitted Changes Customization

(defcustom blame-reveal-uncommitted-label "Uncommitted changes"
  "Label to show for uncommitted changes."
  :type 'string
  :group 'blame-reveal)

(defcustom blame-reveal-uncommitted-color nil
  "Color for uncommitted changes header and fringe.

If nil, uses automatic color based on theme:
  - Dark theme: #d9a066 (muted orange)
  - Light theme: #e6b380 (light muted orange)

These colors are designed to have similar visual prominence to
committed changes while being clearly distinguishable.

Set to a color string to use a fixed color.

Note: If you find the fringe too prominent, consider setting
`blame-reveal-show-uncommitted-fringe' to nil and using diff-hl
instead for showing uncommitted changes."
  :type '(choice (const :tag "Auto (theme-based muted orange)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

(defcustom blame-reveal-show-uncommitted-fringe nil
  "Whether to show fringe indicators for uncommitted changes.

When nil (default and recommended):
  - Only show header when cursor is on uncommitted lines
  - No fringe indicators for uncommitted changes
  - Works well with diff-hl or git-gutter for showing changes

When t:
  - Show fringe indicators for uncommitted changes
  - Only recommended if you don't use diff-hl/git-gutter
  - May cause visual redundancy with those tools

Tip: Use this package with diff-hl-mode for the best experience:
  - blame-reveal shows git blame for committed changes
  - diff-hl shows uncommitted changes in fringe"
  :type 'boolean
  :group 'blame-reveal)


;;;; Color Customization

(defcustom blame-reveal-recent-commit-color nil
  "Color for recent commits (within top N and time limit).
If nil, uses automatic gradient based on commit rank.
Can be:
- nil: Auto gradient (continuous gradient based on rank)
- Color string: Fixed color like \"#6699cc\" for all recent commits
- Function: Takes timestamp, returns color for custom gradient logic"
  :type '(choice (const :tag "Auto (gradient based on rank)" nil)
                 (color :tag "Fixed color")
                 (function :tag "Function (timestamp -> color)"))
  :group 'blame-reveal)

(defcustom blame-reveal-old-commit-color nil
  "Color for old commits (not in top N or beyond time limit).
If nil, uses automatic gray color based on theme (dark theme: #4a4a4a, light theme: #d0d0d0).
Set to a color string like \"#888888\" to use a fixed color."
  :type '(choice (const :tag "Auto (theme-based gray)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

(defcustom blame-reveal-color-scheme
  '(:hue 210
         :dark-newest 0.70
         :dark-oldest 0.35
         :light-newest 0.45
         :light-oldest 0.75
         :saturation-min 0.25
         :saturation-max 0.60)
  "Color scheme for blame visualization.
All lightness values are between 0.0 and 1.0.

Dark theme: newest commits are brighter (higher lightness) to stand out
Light theme: newest commits are darker (lower lightness) to stand out

Example schemes:

  High contrast:
  '(:hue 210
    :dark-newest 0.75 :dark-oldest 0.30
    :light-newest 0.35 :light-oldest 0.85
    :saturation-min 0.35 :saturation-max 0.70)

  Green:
  '(:hue 120
    :dark-newest 0.70 :dark-oldest 0.35
    :light-newest 0.40 :light-oldest 0.75
    :saturation-min 0.25 :saturation-max 0.60)

  Purple:
  '(:hue 280
    :dark-newest 0.70 :dark-oldest 0.35
    :light-newest 0.45 :light-oldest 0.75
    :saturation-min 0.25 :saturation-max 0.60)

  Subtle:
  '(:hue 210
    :dark-newest 0.60 :dark-oldest 0.40
    :light-newest 0.55 :light-oldest 0.70
    :saturation-min 0.20 :saturation-max 0.45)"
  :type 'plist
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when blame-reveal-mode
                 (blame-reveal--recolor-and-render))))))
  :group 'blame-reveal)


;;;; Performance Customization

(defcustom blame-reveal-render-margin 50
  "Number of lines to render above and below visible window."
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-temp-overlay-delay 0.05
  "Delay in seconds before rendering temp overlays for old commits.
Lower values (e.g., 0.02) make overlays appear faster but may cause lag
when moving cursor quickly. Higher values (e.g., 0.1) reduce lag but
overlays appear with more delay."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-lazy-load-threshold 3000
  "File size threshold (in lines) for lazy loading.
Files larger than this will use lazy loading (only blame visible region).
Smaller files will load completely for better recursive blame experience.

Recommended values:
- 3000-5000: Good balance for most cases
- 1000-2000: If you have slow git repository
- 10000+: If you have fast SSD and want full loading for larger files"
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-async-blame 'auto
  "Whether to use async loading for git blame data.

When enabled, git blame commands run in background processes,
keeping Emacs responsive during loading and scrolling.

Values:
  'auto - Use async for large files (> `blame-reveal-lazy-load-threshold' lines),
          sync for small files (recommended)
  t     - Always use async loading
  nil   - Always use synchronous loading (may block UI for large files)

Async loading provides better UX for large files but requires Emacs 25.1+.
For small files, sync loading is actually faster due to less overhead."
  :type '(choice (const :tag "Auto (async for large files)" auto)
                 (const :tag "Always async" t)
                 (const :tag "Always sync" nil))
  :group 'blame-reveal)


;;;; Integration Customization

(defcustom blame-reveal-use-magit 'auto
  "Whether to use magit for showing commit details.
- 'auto: Use magit if available, otherwise use built-in
- t: Always use magit (error if not available)
- nil: Always use built-in git commands"
  :type '(choice (const :tag "Auto (use magit if available)" auto)
                 (const :tag "Always use magit" t)
                 (const :tag "Always use built-in" nil))
  :group 'blame-reveal)

;;;; Unified Overlay Management System

(defconst blame-reveal--overlay-types
  '(fringe header sticky-header temp-fringe loading)
  "All overlay types used by blame-reveal.")

(defvar-local blame-reveal--overlay-registry nil
  "Unified registry for all overlays.
Hash table: overlay -> plist of metadata
  :type - one of `blame-reveal--overlay-types'
  :commit - commit hash (for fringe/temp-fringe/header)
  :line - line number (for positioning)
  :data - type-specific data")

(defvar-local blame-reveal--overlays-by-type nil
  "Index of overlays by type.
Hash table: type -> list of overlays")

(defvar-local blame-reveal--overlays-by-commit nil
  "Index of overlays by commit hash.
Hash table: commit -> list of overlays")

(defvar-local blame-reveal--overlays-by-line nil
  "Index of overlays by line number.
Hash table: line -> list of overlays")


;;;; Overlay Registry Initialization

(defun blame-reveal--init-overlay-registry ()
  "Initialize overlay registry and indices."
  (setq blame-reveal--overlay-registry (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-type (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-commit (make-hash-table :test 'equal))
  (setq blame-reveal--overlays-by-line (make-hash-table :test 'eql)))


;;;; Core Overlay Operations

(defun blame-reveal--register-overlay (overlay type &optional metadata)
  "Register OVERLAY of TYPE with optional METADATA.
METADATA is a plist that may contain:
  :commit - commit hash
  :line - line number
  :data - type-specific data

Returns the overlay."
  (unless blame-reveal--overlay-registry
    (blame-reveal--init-overlay-registry))
  ;; Store in main registry
  (let ((full-metadata (plist-put (copy-sequence metadata) :type type)))
    (puthash overlay full-metadata blame-reveal--overlay-registry)
    ;; Index by type
    (let ((type-list (gethash type blame-reveal--overlays-by-type)))
      (puthash type (cons overlay type-list) blame-reveal--overlays-by-type))
    ;; Index by commit if present
    (when-let ((commit (plist-get metadata :commit)))
      (let ((commit-list (gethash commit blame-reveal--overlays-by-commit)))
        (puthash commit (cons overlay commit-list) blame-reveal--overlays-by-commit)))
    ;; Index by line if present
    (when-let ((line (plist-get metadata :line)))
      (let ((line-list (gethash line blame-reveal--overlays-by-line)))
        (puthash line (cons overlay line-list) blame-reveal--overlays-by-line)))
    overlay))

(defun blame-reveal--unregister-overlay (overlay)
  "Unregister OVERLAY from all indices."
  (when-let ((metadata (gethash overlay blame-reveal--overlay-registry)))
    (let ((type (plist-get metadata :type))
          (commit (plist-get metadata :commit))
          (line (plist-get metadata :line)))
      ;; Remove from indices...
      (when type
        (puthash type
                 (delq overlay (gethash type blame-reveal--overlays-by-type))
                 blame-reveal--overlays-by-type))
      (when commit
        (puthash commit
                 (delq overlay (gethash commit blame-reveal--overlays-by-commit))
                 blame-reveal--overlays-by-commit))
      (when line
        (puthash line
                 (delq overlay (gethash line blame-reveal--overlays-by-line))
                 blame-reveal--overlays-by-line))
      ;; Remove from main registry
      (remhash overlay blame-reveal--overlay-registry)))
  ;; Delete overlay - use ignore-errors for safety
  (ignore-errors (delete-overlay overlay)))


;;;; Query Functions

(defun blame-reveal--get-overlays-by-type (type)
  "Get all overlays of TYPE."
  (gethash type blame-reveal--overlays-by-type))

(defun blame-reveal--get-overlays-by-commit (commit)
  "Get all overlays for COMMIT."
  (gethash commit blame-reveal--overlays-by-commit))

(defun blame-reveal--get-overlays-by-line (line)
  "Get all overlays at LINE."
  (gethash line blame-reveal--overlays-by-line))

(defun blame-reveal--get-overlay-metadata (overlay)
  "Get metadata for OVERLAY."
  (gethash overlay blame-reveal--overlay-registry))

(defun blame-reveal--get-overlay-type (overlay)
  "Get type of OVERLAY."
  (plist-get (blame-reveal--get-overlay-metadata overlay) :type))


;;;; Batch Operations

(defun blame-reveal--clear-overlays-by-type (type)
  "Clear all overlays of TYPE."
  (dolist (overlay (blame-reveal--get-overlays-by-type type))
    (blame-reveal--unregister-overlay overlay))
  (puthash type nil blame-reveal--overlays-by-type))

(defun blame-reveal--clear-overlays-by-commit (commit)
  "Clear all overlays for COMMIT."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit))
    (blame-reveal--unregister-overlay overlay))
  (puthash commit nil blame-reveal--overlays-by-commit))

(defun blame-reveal--clear-all-overlays ()
  "Clear all blame-reveal overlays."
  (when blame-reveal--overlay-registry
    (maphash (lambda (overlay _metadata)
               (ignore-errors (delete-overlay overlay)))
             blame-reveal--overlay-registry))
  (blame-reveal--init-overlay-registry))


;;;; High-Level Overlay Creation

(defun blame-reveal--create-managed-overlay (start end type &optional metadata)
  "Create and register an overlay from START to END of TYPE with METADATA.
Returns the created and registered overlay."
  (let ((overlay (make-overlay start end)))
    (blame-reveal--register-overlay overlay type metadata)
    overlay))


;;;; Reuse and Update

(defun blame-reveal--find-reusable-overlay (type line)
  "Find a reusable overlay of TYPE at LINE.
Returns overlay if found, nil otherwise."
  (cl-find-if (lambda (ov)
                (and (eq (blame-reveal--get-overlay-type ov) type)
                     (overlay-buffer ov)
                     (= (line-number-at-pos (overlay-start ov)) line)))
              (blame-reveal--get-overlays-by-line line)))

(defun blame-reveal--update-overlay-metadata (overlay metadata)
  "Update OVERLAY's metadata with new METADATA (plist).
Preserves :type field."
  (when-let ((old-metadata (gethash overlay blame-reveal--overlay-registry)))
    (let* ((type (plist-get old-metadata :type))
           (old-commit (plist-get old-metadata :commit))
           (old-line (plist-get old-metadata :line))
           (new-commit (plist-get metadata :commit))
           (new-line (plist-get metadata :line))
           (new-metadata (plist-put (copy-sequence metadata) :type type)))

      ;; Update commit index if changed
      (when (and old-commit (not (equal old-commit new-commit)))
        (puthash old-commit 
                 (delq overlay (gethash old-commit blame-reveal--overlays-by-commit))
                 blame-reveal--overlays-by-commit)
        (when new-commit
          (let ((commit-list (gethash new-commit blame-reveal--overlays-by-commit)))
            (puthash new-commit (cons overlay commit-list)
                     blame-reveal--overlays-by-commit))))

      ;; Update line index if changed
      (when (and old-line (not (eql old-line new-line)))
        (puthash old-line
                 (delq overlay (gethash old-line blame-reveal--overlays-by-line))
                 blame-reveal--overlays-by-line)
        (when new-line
          (let ((line-list (gethash new-line blame-reveal--overlays-by-line)))
            (puthash new-line (cons overlay line-list)
                     blame-reveal--overlays-by-line))))

      ;; Update main registry
      (puthash overlay new-metadata blame-reveal--overlay-registry))))


;;;; Statistics and Debugging

(defun blame-reveal--overlay-stats ()
  "Return statistics about current overlays as a plist."
  (let ((stats nil))
    (dolist (type blame-reveal--overlay-types)
      (let ((count (length (blame-reveal--get-overlays-by-type type))))
        (setq stats (plist-put stats type count))))
    (plist-put stats :total (hash-table-count blame-reveal--overlay-registry))
    stats))

(defun blame-reveal--print-overlay-stats ()
  "Print overlay statistics (for debugging)."
  (interactive)
  (let ((stats (blame-reveal--overlay-stats)))
    (message "Overlay stats: %s" 
             (mapconcat (lambda (type)
                          (format "%s=%d" type (plist-get stats type)))
                        (cons :total blame-reveal--overlay-types)
                        ", "))))

;;;; Fringe Overlay Management (New Implementation)

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH.
Uses unified overlay management system."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (fringe-face (blame-reveal--ensure-fringe-face color))
             ;; Try to reuse existing overlay at this line
             (overlay (or (blame-reveal--find-reusable-overlay 'fringe line-number)
                          (blame-reveal--create-managed-overlay 
                           pos pos 'fringe
                           (list :commit commit-hash :line line-number)))))

        ;; Update overlay properties
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-style
                                       'blame-reveal-full
                                       fringe-face)))

        ;; Update metadata if reusing
        (when (overlay-buffer overlay)
          (blame-reveal--update-overlay-metadata 
           overlay 
           (list :commit commit-hash :line line-number)))

        overlay))))

(defun blame-reveal--render-block-fringe (block-start block-length commit-hash color)
  "Render fringe for a specific block (v2 using unified management).
Returns list of created/reused overlays."
  (let ((overlays nil)
        (range (blame-reveal--get-visible-line-range))
        (block-end (+ block-start block-length -1)))
    (let ((render-start (max block-start (car range)))
          (render-end (min block-end (cdr range))))
      (when (<= render-start render-end)
        (dotimes (i (- render-end render-start -1))
          (let* ((line-num (+ render-start i))
                 (fringe-ov (blame-reveal--create-fringe-overlay
                             line-num color commit-hash)))
            (when fringe-ov
              (push fringe-ov overlays))))))
    overlays))

(defun blame-reveal--clear-fringe-overlays ()
  "Clear all fringe overlays."
  (blame-reveal--clear-overlays-by-type 'fringe))

(defun blame-reveal--clear-fringe-overlays-for-commit (commit-hash)
  "Clear fringe overlays only for specific COMMIT-HASH."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit-hash))
    (when (eq (blame-reveal--get-overlay-type overlay) 'fringe)
      (blame-reveal--unregister-overlay overlay))))

(defun blame-reveal--get-fringe-overlay-at-line (line-number)
  "Get fringe overlay at LINE-NUMBER, if any."
  (cl-find-if (lambda (ov)
                (eq (blame-reveal--get-overlay-type ov) 'fringe))
              (blame-reveal--get-overlays-by-line line-number)))

;;;; Rendering Functions (Updated to use v2)

(defun blame-reveal--render-visible-region ()
  "Render git blame fringe for visible region (v2 with unified overlay management).
Reuses existing overlays when possible to minimize visual disruption."
  (when blame-reveal--blame-data
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data
                    start-line
                    end-line))
           (rendered-lines (make-hash-table :test 'eql)))

      ;; Ensure commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))

      ;; Update recent commits based on what's loaded so far
      (blame-reveal--update-recent-commits)

      ;; Render blocks
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))

          ;; Skip uncommitted changes unless explicitly enabled
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            ;; Render permanent fringe for recent commits
            (when (blame-reveal--should-render-commit commit-hash)
              (let ((color (blame-reveal--get-commit-color commit-hash))
                    (block-end (+ block-start block-length -1)))

                ;; Render each line in visible range
                (let ((render-start (max block-start start-line))
                      (render-end (min block-end end-line)))
                  (dotimes (i (- render-end render-start -1))
                    (let ((line-num (+ render-start i)))
                      (blame-reveal--create-fringe-overlay
                       line-num color commit-hash)
                      (puthash line-num t rendered-lines)))))))))

      ;; Clean up overlays outside visible range
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line
                       (not (gethash line rendered-lines))
                       (or (< line start-line) (> line end-line)))
              (blame-reveal--unregister-overlay overlay)))))

      ;; Re-trigger header update
      (blame-reveal--update-header))))

(defun blame-reveal--render-expanded-region (start-line end-line)
  "Render blame fringe for newly expanded region (v2).
Reuses existing overlays when possible to avoid flicker."
  (when blame-reveal--blame-data
    (let* ((blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data
                    start-line
                    end-line))
           (rendered-lines (make-hash-table :test 'eql)))

      ;; Render blocks
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))

          ;; Skip uncommitted changes unless explicitly enabled
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            ;; Render permanent fringe for recent commits
            (when (blame-reveal--should-render-commit commit-hash)
              (let ((color (blame-reveal--get-commit-color commit-hash))
                    (block-end (+ block-start block-length -1)))

                ;; Render each line in block
                (dotimes (i (- block-end block-start -1))
                  (let ((line-num (+ block-start i)))
                    (blame-reveal--create-fringe-overlay
                     line-num color commit-hash)
                    (puthash line-num t rendered-lines))))))))

      ;; Keep existing overlays outside expanded region
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line
                       (not (gethash line rendered-lines))
                       (or (< line start-line) (> line end-line)))
              ;; Keep this overlay (it's outside expanded region)
              nil)))))))

;;;; Temp Overlay Management (New Implementation)

(defun blame-reveal--create-temp-fringe-overlay (line-number color commit-hash)
  "Create temporary fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (fringe-face (blame-reveal--ensure-fringe-face color))
             (overlay (blame-reveal--create-managed-overlay 
                       pos pos 'temp-fringe
                       (list :commit commit-hash :line line-number))))

        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-style
                                       'blame-reveal-full
                                       fringe-face)))
        overlay))))

(defun blame-reveal--render-temp-overlays-for-commit (commit-hash color)
  "Render temporary overlays for COMMIT-HASH with COLOR.
Only renders in visible range."
  (let* ((range (blame-reveal--get-visible-line-range))
         (start-line (car range))
         (end-line (cdr range))
         (visible-blocks (blame-reveal--find-block-boundaries
                          blame-reveal--blame-data
                          start-line
                          end-line)))
    ;; Render temp overlays for matching blocks in visible area
    (dolist (block visible-blocks)
      (let ((blk-start (nth 0 block))
            (blk-commit (nth 1 block))
            (blk-length (nth 2 block)))
        (when (equal blk-commit commit-hash)
          (dotimes (i blk-length)
            (let ((line-num (+ blk-start i)))
              (when (and (>= line-num start-line)
                         (<= line-num end-line))
                (blame-reveal--create-temp-fringe-overlay
                 line-num color commit-hash)))))))))

(defun blame-reveal--clear-temp-overlays ()
  "Clear all temporary fringe overlays."
  (blame-reveal--clear-overlays-by-type 'temp-fringe))

(defun blame-reveal--clear-temp-overlays-for-commit (commit-hash)
  "Clear temporary overlays only for specific COMMIT-HASH."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit-hash))
    (when (eq (blame-reveal--get-overlay-type overlay) 'temp-fringe)
      (blame-reveal--unregister-overlay overlay))))

(defun blame-reveal--temp-overlay-renderer (buf hash col)
  "Render temporary overlays for old commit HASH with color COL in buffer BUF (v2)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (equal blame-reveal--current-block-commit hash)
        (blame-reveal--render-temp-overlays-for-commit hash col)))))

;;;; Event Handlers (Updated)

(defun blame-reveal--update-header-impl ()
  "Implementation of header update (v2 with unified overlay management)."
  (when blame-reveal--blame-data
    ;; First, ensure visible commits have their info loaded
    (blame-reveal--ensure-visible-commits-loaded)

    (if-let ((current-block (blame-reveal--get-current-block)))
        ;; Has block: render header and overlays
        (let* ((commit-hash (car current-block))
               (block-start (cdr current-block)))

          ;; If entered a new block
          (unless (equal commit-hash blame-reveal--current-block-commit)
            (setq blame-reveal--current-block-commit commit-hash))

          ;; Only render the current block when stopped
          (pcase-let ((`(,color ,is-uncommitted ,is-old-commit ,hide-header-fringe)
                       (blame-reveal--get-commit-display-info commit-hash)))

            ;; Cancel any pending temp overlay rendering
            (when blame-reveal--temp-overlay-timer
              (cancel-timer blame-reveal--temp-overlay-timer)
              (setq blame-reveal--temp-overlay-timer nil))

            ;; Clear previous header (will be replaced by header overlay system later)
            (when blame-reveal--header-overlay
              (delete-overlay blame-reveal--header-overlay)
              (setq blame-reveal--header-overlay nil))

            ;; Create new header only if layout is not 'none
            (unless (eq blame-reveal-display-layout 'none)
              (setq blame-reveal--header-overlay
                    (blame-reveal--create-header-overlay
                     block-start commit-hash color hide-header-fringe)))

            ;; Show temp fringe overlay for old commits or uncommitted changes
            (when (or is-old-commit
                      (and is-uncommitted blame-reveal-show-uncommitted-fringe))
              (setq blame-reveal--temp-overlay-timer
                    (run-with-idle-timer
                     blame-reveal-temp-overlay-delay nil
                     #'blame-reveal--temp-overlay-renderer
                     (current-buffer) commit-hash color)))
            (setq blame-reveal--last-rendered-commit commit-hash)))

      ;; No current block - completely left all blocks - CLEANUP!
      (when blame-reveal--temp-overlay-timer
        (cancel-timer blame-reveal--temp-overlay-timer)
        (setq blame-reveal--temp-overlay-timer nil))
      (when blame-reveal--header-overlay
        (delete-overlay blame-reveal--header-overlay)
        (setq blame-reveal--header-overlay nil))
      (blame-reveal--clear-temp-overlays)
      (setq blame-reveal--current-block-commit nil)
      (setq blame-reveal--last-rendered-commit nil))

    ;; Only update sticky header if layout is not 'none
    (unless (eq blame-reveal-display-layout 'none)
      (blame-reveal--update-sticky-header))))

;;;; Updated Clear Functions

(defun blame-reveal--clear-overlays ()
  "Remove all blame-reveal overlays (v2 using unified system)."
  ;; Clear all managed overlays
  (blame-reveal--clear-all-overlays)
  ;; Clear legacy header overlay (will be migrated later)
  (when blame-reveal--header-overlay
    (delete-overlay blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil))
  ;; Clear sticky header (will be migrated later)
  (blame-reveal--clear-sticky-header)
  (setq blame-reveal--current-block-commit nil))


;;;; Helper Functions

(defun blame-reveal--should-use-async-p ()
  "Determine if async loading should be used based on configuration."
  (pcase blame-reveal-async-blame
    ('auto (blame-reveal--should-lazy-load-p))  ; Use async for large files
    ('t t)                                       ; Always async
    (_ nil)))                                    ; Always sync

(defun blame-reveal--build-blame-command-args (start-line end-line relative-file)
  "Build git blame command arguments.
START-LINE and END-LINE specify optional line range.
RELATIVE-FILE is the file path relative to git root."
  (let ((args (list "blame" "--porcelain")))
    ;; Add line range if provided
    (when (and start-line end-line)
      (setq args (append args (list "-L" (format "%d,%d" start-line end-line)))))
    ;; Add revision if in recursive blame mode
    (when (and (boundp 'blame-reveal--current-revision)
               blame-reveal--current-revision
               (not (eq blame-reveal--current-revision 'uncommitted)))
      (setq args (append args (list blame-reveal--current-revision))))
    ;; Add file at the end
    (append args (list relative-file))))

(defun blame-reveal--cleanup-async-state (process-var buffer-var)
  "Cleanup async process and buffer.
PROCESS-VAR is the symbol holding the process.
BUFFER-VAR is the symbol holding the buffer."
  (when-let ((proc (symbol-value process-var)))
    (when (process-live-p proc)
      (delete-process proc))
    (set process-var nil))
  (when-let ((buf (symbol-value buffer-var)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (set buffer-var nil)))

(defun blame-reveal--make-async-sentinel (source-buffer temp-buffer success-handler
                                                        process-var buffer-var
                                                        &optional error-handler)
  "Create a process sentinel for async blame loading.
SOURCE-BUFFER is the buffer where blame is displayed.
TEMP-BUFFER is the temporary buffer receiving git output.
SUCCESS-HANDLER is called on success with TEMP-BUFFER.
PROCESS-VAR and BUFFER-VAR are symbols to cleanup on failure.
ERROR-HANDLER is optional function called on error in source buffer."
  `(lambda (proc event)
     (cond
      ((string-match-p "finished" event)
       (when (buffer-live-p ,source-buffer)
         (with-current-buffer ,source-buffer
           (funcall ,success-handler ,temp-buffer))))

      ;; 任何非正常结束都当作错误处理
      (t
       (message "Async blame process ended: %s" event)
       (when (and (buffer-live-p ,temp-buffer)
                  (> (buffer-size ,temp-buffer) 0))
         (with-current-buffer ,temp-buffer
           (message "Git output: %s" (buffer-substring-no-properties
                                      (point-min)
                                      (min (point-max) 500)))))
       (when (buffer-live-p ,temp-buffer)
         (kill-buffer ,temp-buffer))
       (when (buffer-live-p ,source-buffer)
         (with-current-buffer ,source-buffer
           (set ',process-var nil)
           (set ',buffer-var nil)
           (setq blame-reveal--loading nil)
           (blame-reveal--stop-loading-animation)
           ;; Call optional error handler
           ,(when error-handler
              `(funcall ,error-handler))))))))

(defun blame-reveal--start-async-blame (process-var buffer-var buffer-name
                                                    process-name start-line end-line
                                                    success-handler
                                                    &optional error-handler)
  "Start async git blame process.
PROCESS-VAR: Symbol for storing the process.
BUFFER-VAR: Symbol for storing the temp buffer.
BUFFER-NAME: Name for the temp buffer.
PROCESS-NAME: Name for the process.
START-LINE, END-LINE: Optional line range.
SUCCESS-HANDLER: Function called on success with temp buffer.
ERROR-HANDLER: Optional function called on error in source buffer.

Note: Must be called with the source buffer as current-buffer."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file)))
         (source-buffer (current-buffer)))

    (unless git-root
      (setq blame-reveal--loading nil)
      (message "File is not in a git repository")
      (error "File is not in a git repository"))

    ;; Cancel existing process
    (blame-reveal--cleanup-async-state process-var buffer-var)

    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (temp-buffer (generate-new-buffer buffer-name))
           (process-environment (cons "GIT_PAGER=cat"
                                      (cons "PAGER=cat"
                                            process-environment))))

      (set buffer-var temp-buffer)

      (let ((args (blame-reveal--build-blame-command-args
                   start-line end-line relative-file)))

        (set process-var
             (make-process
              :name process-name
              :buffer temp-buffer
              :command (cons "git" args)
              :sentinel (blame-reveal--make-async-sentinel
                         source-buffer temp-buffer success-handler
                         process-var buffer-var error-handler)
              :noquery t))))))

(defun blame-reveal--get-header-faces (color)
  "Get face definitions for header, metadata, and description with COLOR."
  (let ((header-weight blame-reveal-header-weight)
        (header-height blame-reveal-header-height)
        (metadata-weight blame-reveal-metadata-weight)
        (metadata-height blame-reveal-metadata-height)
        (description-weight blame-reveal-description-weight)
        (description-height blame-reveal-description-height)
        (bg (face-background 'default)))
    (pcase blame-reveal-display-style
      ('background
       (list :header (list :foreground color :weight header-weight
                           :height header-height :background bg)
             :metadata (list :foreground color :weight metadata-weight
                             :height metadata-height :background bg)
             :description (list :foreground color :weight description-weight
                                :height description-height :background bg)))
      ('box
       (let ((box-style (list :line-width 1 :color color)))
         (list :header (list :foreground color :weight header-weight
                             :height header-height :box box-style :background bg)
               :metadata (list :foreground color :weight metadata-weight
                               :height metadata-height :box box-style :background bg)
               :description (list :foreground color :weight description-weight
                                  :height description-height :box box-style :background bg))))
      ('inverse
       (list :header (list :background color :weight header-weight :height header-height)
             :metadata (list :background color :weight metadata-weight :height metadata-height)
             :description (list :background color :weight description-weight
                                :height description-height)))
      (_
       (list :header (list :foreground color :weight header-weight
                           :height header-height :background bg)
             :metadata (list :foreground color :weight metadata-weight
                             :height metadata-height :background bg)
             :description (list :foreground color :weight description-weight
                                :height description-height :background bg))))))


;;;; Loading Animation Functions

(defun blame-reveal--get-loading-gradient-color (intensity)
  "Get loading animation color with INTENSITY (0.0 to 1.0)."
  (let* ((is-dark (blame-reveal-color--is-dark-theme-p))
         (base-color (if is-dark "#6c9ef8" "#4a7dc0"))
         (rgb (color-name-to-rgb base-color))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb)))
    (format "#%02x%02x%02x"
            (round (* 255 (* r intensity)))
            (round (* 255 (* g intensity)))
            (round (* 255 (* b intensity))))))

(defun blame-reveal--get-fringe-background ()
  "Get the appropriate fringe background color."
  (or (face-background 'fringe nil t)
      (face-background 'default nil t)
      "unspecified"))

(defun blame-reveal--ensure-fringe-face-with-bg (color)
  "Ensure a face exists for COLOR with proper fringe background."
  (let* ((face-name (intern (format "blame-reveal-fringe-%s" color)))
         (bg-color (blame-reveal--get-fringe-background)))
    (unless (facep face-name)
      (make-face face-name)
      (set-face-attribute face-name nil
                          :foreground color
                          :background bg-color))
    ;; 动态更新背景色(主题切换时)
    (when (facep face-name)
      (set-face-attribute face-name nil :background bg-color))
    face-name))

(defun blame-reveal--create-loading-animation ()
  "Create loading animation overlays with smooth interpolated trail effect."
  ;; 找到显示当前 buffer 的窗口
  (let ((win (get-buffer-window (current-buffer))))
    (if (not win)
        nil  ; 如果 buffer 不可见，不创建动画
      (let* ((win-height (window-body-height win))
             (center-line (/ win-height 2))
             (num-indicators 6)
             (half-num (/ num-indicators 2))
             (start-line (max 1 (- center-line half-num)))
             (overlays nil)
             ;; 当前的精确位置(包含子步进)
             (current-pos (+ blame-reveal--loading-animation-step
                             blame-reveal--loading-animation-sub-step)))

        (save-excursion
          (goto-char (window-start win))
          (forward-line (1- start-line))

          (dotimes (i num-indicators)
            (unless (eobp)
              (let* ((pos (line-beginning-position))
                     (ov (make-overlay pos pos))
                     ;; 计算精确的浮点距离
                     (raw-offset (- i current-pos))
                     ;; 标准化 offset 实现循环衔接
                     (offset (cond
                              ((> raw-offset (/ num-indicators 2.0))
                               (- raw-offset num-indicators))
                              ((< raw-offset (- (/ num-indicators 2.0)))
                               (+ raw-offset num-indicators))
                              (t raw-offset)))
                     ;; 使用平滑的距离衰减函数
                     (distance (abs offset))
                     ;; 指数衰减 + 平滑插值
                     (intensity (cond
                                 ;; 核心亮点区域(距离 < 0.5)
                                 ((< distance 0.5) 1.0)
                                 ;; 平滑衰减区域
                                 ((<= distance 3.5)
                                  (max 0.1 (* (- 1.0 (* distance 0.25))
                                              (- 1.0 (* distance 0.15)))))
                                 ;; 背景暗淡
                                 (t 0.05)))
                     ;; 根据亮度选择 bitmap
                     (bitmap (cond
                              ((> intensity 0.5) 'blame-reveal-loading-bright)
                              ((> intensity 0.2) 'blame-reveal-loading-bright)
                              (t 'blame-reveal-loading-dim)))
                     (color (blame-reveal--get-loading-gradient-color intensity))
                     (face (blame-reveal--ensure-fringe-face-with-bg color)))

                (overlay-put ov 'blame-reveal-loading t)
                (overlay-put ov 'before-string
                             (propertize "!" 'display
                                         (list blame-reveal-style
                                               bitmap
                                               face)))
                (push ov overlays)
                (forward-line 1)))))

        (nreverse overlays)))))

(defun blame-reveal--update-loading-animation ()
  "Update loading animation with smooth sub-step interpolation."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; 只更新正在加载且可见的 buffer
        (when (and blame-reveal-mode
                   blame-reveal--loading
                   (get-buffer-window buffer))  ; 关键：只处理可见的 buffer
          ;; 清理旧的 overlays
          (dolist (ov blame-reveal--loading-animation-overlays)
            (when (overlay-buffer ov)
              (delete-overlay ov)))

          ;; 更新子步进(0.0 到 1.0 之间平滑变化)
          (setq blame-reveal--loading-animation-frame-counter
                (1+ blame-reveal--loading-animation-frame-counter))

          (setq blame-reveal--loading-animation-sub-step
                (/ (float blame-reveal--loading-animation-frame-counter)
                   blame-reveal-loading-animation-frames-per-step))

          ;; 当子步进达到 1.0 时,前进到下一个整数步
          (when (>= blame-reveal--loading-animation-frame-counter
                    blame-reveal-loading-animation-frames-per-step)
            (setq blame-reveal--loading-animation-frame-counter 0)
            (setq blame-reveal--loading-animation-sub-step 0.0)
            (setq blame-reveal--loading-animation-step
                  (mod (1+ blame-reveal--loading-animation-step) 6)))

          ;; 重新创建 overlays(每帧都更新,形成平滑过渡)
          (setq blame-reveal--loading-animation-overlays
                (blame-reveal--create-loading-animation)))))))

(defun blame-reveal--start-loading-animation ()
  "Start loading animation with smooth interpolation."
  (setq blame-reveal--loading-animation-step 0)
  (setq blame-reveal--loading-animation-frame-counter 0)
  (setq blame-reveal--loading-animation-sub-step 0.0)
  (setq blame-reveal--loading-animation-overlays
        (blame-reveal--create-loading-animation))

  ;; 启动全局定时器（如果还没启动）
  (unless blame-reveal--global-loading-animation-timer
    (setq blame-reveal--global-loading-animation-timer
          (run-with-timer blame-reveal-loading-animation-speed
                          blame-reveal-loading-animation-speed
                          #'blame-reveal--update-loading-animation))))

(defun blame-reveal--stop-loading-animation ()
  "Stop and clear loading animation."
  ;; 清理当前 buffer 的动画 overlays
  (dolist (ov blame-reveal--loading-animation-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq blame-reveal--loading-animation-overlays nil)
  (setq blame-reveal--loading-animation-step 0)
  (setq blame-reveal--loading-animation-frame-counter 0)
  (setq blame-reveal--loading-animation-sub-step 0.0)

  ;; 检查是否所有 buffer 都停止加载了
  ;; 如果是，停止全局定时器
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       (and blame-reveal-mode blame-reveal--loading)))
                   (buffer-list))
    (when blame-reveal--global-loading-animation-timer
      (cancel-timer blame-reveal--global-loading-animation-timer)
      (setq blame-reveal--global-loading-animation-timer nil))))


;;;; Parse Function (Shared by Sync and Async)

(defun blame-reveal--parse-blame-output (output-buffer)
  "Parse git blame porcelain output from OUTPUT-BUFFER.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let ((blame-data nil)
        (current-commit nil))
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ;; Line starting with commit hash
         ((looking-at "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)")
          (setq current-commit (match-string 1))
          (let ((line-number (string-to-number (match-string 3))))
            (push (cons line-number current-commit) blame-data))
          (forward-line 1))
         ;; Tab means actual code line, skip it
         ((looking-at "^\t")
          (forward-line 1))
         ;; Other metadata lines, skip
         (t
          (forward-line 1)))))
    (nreverse blame-data)))


;;;; Git Blame Data Functions

(defun blame-reveal--get-blame-data ()
  "Get git blame data for current buffer (entire file) synchronously.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))
    (if (not (and file git-root (vc-git-registered file)))
        nil  ; Return nil if not in git repo
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (if (zerop (call-process "git" nil t nil "blame" "--porcelain" relative-file))
              (blame-reveal--parse-blame-output (current-buffer))
            nil))))))

(defun blame-reveal--get-blame-data-range (start-line end-line)
  "Get git blame data for specific line range START-LINE to END-LINE synchronously.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))
    (if (not (and file git-root (vc-git-registered file)))
        nil  ; Return nil if not in git repo
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (if (zerop (call-process "git" nil t nil "blame"
                                   "--porcelain"
                                   "-L" (format "%d,%d" start-line end-line)
                                   relative-file))
              (blame-reveal--parse-blame-output (current-buffer))
            nil))))))

(defun blame-reveal--load-blame-data-sync ()
  "Load git blame data synchronously.
Use lazy loading for large files, full loading for small files."
  (blame-reveal--start-loading-animation)
  (condition-case err
      (let* ((file (buffer-file-name))
             (git-root (and file (vc-git-root file))))

        (unless git-root
          (setq blame-reveal--loading nil)
          (blame-reveal--stop-loading-animation)
          (message "File is not in a git repository")
          (cl-return-from blame-reveal--load-blame-data-sync nil))

        (let ((use-lazy (blame-reveal--should-lazy-load-p))
              (blame-data nil))

          (if use-lazy
              ;; Large file: only load visible region
              (let* ((range (blame-reveal--get-visible-line-range))
                     (start-line (car range))
                     (end-line (cdr range)))
                (setq blame-data (blame-reveal--get-blame-data-range start-line end-line))
                (when blame-data
                  (setq blame-reveal--blame-data-range (cons start-line end-line))
                  (message "Git blame loaded (sync, lazy): %d lines (range %d-%d)"
                           (length blame-data) start-line end-line)))

            ;; Small file: load entire file
            (setq blame-data (blame-reveal--get-blame-data))
            (when blame-data
              (setq blame-reveal--blame-data-range nil)  ; nil = full load
              (message "Git blame loaded (sync, full): %d lines" (length blame-data))))

          (if blame-data
              (progn
                (setq blame-reveal--blame-data blame-data)
                (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                (setq blame-reveal--color-map (make-hash-table :test 'equal))
                (setq blame-reveal--timestamps nil)
                (setq blame-reveal--recent-commits nil)
                (setq blame-reveal--all-commits-loaded nil)

                ;; Load commits incrementally
                (blame-reveal--load-commits-incrementally)

                ;; Initial render
                (blame-reveal--render-visible-region))

            (message "No git blame data available"))))

    (error
     ;; 错误时也要清理
     (message "Error loading git blame: %s" (error-message-string err))
     nil))

  ;; 无论成功还是失败，最后都要清理（放在 condition-case 外面）
  (setq blame-reveal--loading nil)
  (blame-reveal--stop-loading-animation))

(defun blame-reveal--expand-blame-data-sync (start-line end-line)
  "Synchronously expand blame data to include START-LINE to END-LINE."
  (setq blame-reveal--loading t)  ;; 关键：设置加载标志
  (blame-reveal--start-loading-animation)
  (condition-case err
      (let ((new-data (blame-reveal--get-blame-data-range start-line end-line)))

        (if (null new-data)
            (message "No new blame data in range %d-%d" start-line end-line)

          ;; Merge with existing data
          (let ((existing-lines (make-hash-table :test 'equal))
                (added-count 0))
            ;; Mark existing lines
            (dolist (entry blame-reveal--blame-data)
              (puthash (car entry) t existing-lines))

            ;; Add new lines
            (dolist (entry new-data)
              (unless (gethash (car entry) existing-lines)
                (push entry blame-reveal--blame-data)
                (setq added-count (1+ added-count))))

            (when (> added-count 0)
              ;; Sort by line number
              (setq blame-reveal--blame-data
                    (sort blame-reveal--blame-data
                          (lambda (a b) (< (car a) (car b)))))

              ;; Update range
              (if blame-reveal--blame-data-range
                  (setq blame-reveal--blame-data-range
                        (cons (min start-line (car blame-reveal--blame-data-range))
                              (max end-line (cdr blame-reveal--blame-data-range))))
                (setq blame-reveal--blame-data-range
                      (cons start-line end-line)))

              ;; Load commit info for visible area
              (let ((visible-commits (blame-reveal--get-visible-commits)))
                (dolist (commit visible-commits)
                  (blame-reveal--ensure-commit-info commit)))

              ;; Update recent commits
              (blame-reveal--update-recent-commits)

              ;; Render expanded region
              (blame-reveal--render-expanded-region start-line end-line)

              ;; Update header if cursor is in new region
              (let ((current-line (line-number-at-pos)))
                (when (and (>= current-line start-line)
                           (<= current-line end-line))
                  (blame-reveal--update-header)))

              (message "Blame expanded (sync): +%d lines (total %d)"
                       added-count (length blame-reveal--blame-data))))))

    (error
     ;; 错误时也要清理
     (message "Error expanding git blame: %s" (error-message-string err))
     nil))

  ;; 无论成功还是失败，最后都要清理（放在 condition-case 外面）
  (setq blame-reveal--loading nil)
  (blame-reveal--stop-loading-animation))

(defun blame-reveal--get-commit-info (commit-hash)
  "Get commit info for COMMIT-HASH.
Returns (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP DESCRIPTION)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "show"
                               "--no-patch"
                               "--format=%h|%an|%ar|%s|%at"
                               commit-hash))
      (goto-char (point-min))
      (when (re-search-forward "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([0-9]+\\)" nil t)
        (let ((short-hash (match-string 1))
              (author (match-string 2))
              (date (match-string 3))
              (summary (match-string 4))
              (timestamp (string-to-number (match-string 5))))
          ;; Get description separately
          (erase-buffer)
          (if (zerop (call-process "git" nil t nil "show"
                                   "--no-patch"
                                   "--format=%b"
                                   commit-hash))
              (list short-hash author date summary timestamp (string-trim (buffer-string)))
            (list short-hash author date summary timestamp "")))))))

(defun blame-reveal--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  ;; Skip uncommitted changes - no need to fetch info
  (unless (or (blame-reveal--is-uncommitted-p commit-hash)
              (gethash commit-hash blame-reveal--commit-info))
    (let ((info (blame-reveal--get-commit-info commit-hash)))
      (when info
        (puthash commit-hash info blame-reveal--commit-info)
        ;; Update timestamp range
        (let ((timestamp (nth 4 info)))
          (when timestamp
            (unless blame-reveal--timestamps
              (setq blame-reveal--timestamps (cons timestamp timestamp)))
            (setcar blame-reveal--timestamps
                    (min (car blame-reveal--timestamps) timestamp))
            (setcdr blame-reveal--timestamps
                    (max (cdr blame-reveal--timestamps) timestamp))))))))


;;;; Asynchronous Loading Functions

(defun blame-reveal--load-blame-data-async ()
  "Asynchronously load initial blame data."
  (let* ((use-lazy (blame-reveal--should-lazy-load-p))
         (range (when use-lazy (blame-reveal--get-visible-line-range)))
         (start-line (when use-lazy (car range)))
         (end-line (when use-lazy (cdr range))))

    ;; Print loading message
    (if use-lazy
        (message "Loading git blame (async, lazy): lines %d-%d..." start-line end-line)
      (message "Loading git blame (async, full)..."))

    (blame-reveal--start-loading-animation)

    ;; Start async process
    (blame-reveal--start-async-blame
     'blame-reveal--initial-load-process
     'blame-reveal--initial-load-buffer
     " *blame-initial-load*"
     "blame-initial-load"
     start-line
     end-line
     (lambda (temp-buffer)
       (blame-reveal--handle-initial-load-complete temp-buffer use-lazy)))))

(defun blame-reveal--handle-initial-load-complete (temp-buffer use-lazy)
  "Handle completion of initial async blame loading from TEMP-BUFFER."
  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (let ((blame-data (blame-reveal--parse-blame-output temp-buffer)))

          (if blame-data
              (progn
                (setq blame-reveal--blame-data blame-data)
                (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                (setq blame-reveal--color-map (make-hash-table :test 'equal))
                (setq blame-reveal--timestamps nil)
                (setq blame-reveal--recent-commits nil)
                (setq blame-reveal--all-commits-loaded nil)

                ;; Set range
                (if use-lazy
                    (let* ((range (blame-reveal--get-visible-line-range))
                           (start-line (car range))
                           (end-line (cdr range)))
                      (setq blame-reveal--blame-data-range (cons start-line end-line))
                      (message "Git blame loaded (async, lazy): %d lines (range %d-%d)"
                               (length blame-data) start-line end-line))
                  (setq blame-reveal--blame-data-range nil)
                  (message "Git blame loaded (async, full): %d lines" (length blame-data)))

                ;; Load commit info incrementally
                (blame-reveal--load-commits-incrementally)

                ;; Initial render
                (blame-reveal--render-visible-region))

            (message "No git blame data available"))))

    ;; Cleanup - 无论成功失败都要清理（unwind-protect 保证一定执行）
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (setq blame-reveal--initial-load-buffer nil)
    (setq blame-reveal--initial-load-process nil)
    (setq blame-reveal--loading nil)
    (blame-reveal--stop-loading-animation)))

(defun blame-reveal--expand-blame-data-async (start-line end-line)
  "Asynchronously expand blame data to include START-LINE to END-LINE."
  ;; Record pending expansion and set flag
  (setq blame-reveal--pending-expansion (cons start-line end-line))
  (setq blame-reveal--is-expanding t)
  (setq blame-reveal--loading t)  ;; 关键：设置加载标志

  (blame-reveal--start-loading-animation)

  (blame-reveal--start-async-blame
   'blame-reveal--expansion-process
   'blame-reveal--expansion-buffer
   " *blame-expansion*"
   "blame-expand"
   start-line
   end-line
   (lambda (temp-buffer)
     (blame-reveal--handle-expansion-complete temp-buffer start-line end-line))
   ;; Error handler: cleanup expansion-specific state
   (lambda ()
     (setq blame-reveal--pending-expansion nil)
     (setq blame-reveal--is-expanding nil)
     (setq blame-reveal--loading nil)  ;; 错误时也要清理
     (blame-reveal--stop-loading-animation))))

(defun blame-reveal--handle-expansion-complete (temp-buffer start-line end-line)
  "Handle completion of async blame expansion from TEMP-BUFFER."
  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (let ((new-data (blame-reveal--parse-blame-output temp-buffer)))

          (if (null new-data)
              (message "No new blame data in range %d-%d" start-line end-line)

            ;; Merge with existing data
            (let ((existing-lines (make-hash-table :test 'equal))
                  (added-count 0)
                  (new-commits (make-hash-table :test 'equal)))
              ;; Mark existing lines
              (dolist (entry blame-reveal--blame-data)
                (puthash (car entry) t existing-lines))

              ;; Add new lines and track new commits
              (dolist (entry new-data)
                (unless (gethash (car entry) existing-lines)
                  (push entry blame-reveal--blame-data)
                  (setq added-count (1+ added-count))
                  ;; Track which commits are new
                  (puthash (cdr entry) t new-commits)))

              (when (> added-count 0)
                ;; Sort by line number
                (setq blame-reveal--blame-data
                      (sort blame-reveal--blame-data
                            (lambda (a b) (< (car a) (car b)))))

                ;; Update range
                (if blame-reveal--blame-data-range
                    (setq blame-reveal--blame-data-range
                          (cons (min start-line (car blame-reveal--blame-data-range))
                                (max end-line (cdr blame-reveal--blame-data-range))))
                  (setq blame-reveal--blame-data-range
                        (cons start-line end-line)))

                ;; Load commit info ONLY for new commits
                (maphash (lambda (commit _)
                           (blame-reveal--ensure-commit-info commit))
                         new-commits)

                ;; Update recent commits
                (blame-reveal--update-recent-commits)

                ;; Clear expanding flag BEFORE rendering
                (setq blame-reveal--is-expanding nil)

                ;; Render only expanded region (no flicker)
                (blame-reveal--render-expanded-region start-line end-line)

                ;; Update header if cursor is in new region
                (let ((current-line (line-number-at-pos)))
                  (when (and (>= current-line start-line)
                             (<= current-line end-line))
                    (blame-reveal--update-header)))

                (message "Blame expanded: +%d lines (total %d)"
                         added-count (length blame-reveal--blame-data)))))))

    ;; Cleanup - 无论成功失败都要清理（unwind-protect 保证一定执行）
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (setq blame-reveal--expansion-buffer nil)
    (setq blame-reveal--expansion-process nil)
    (setq blame-reveal--pending-expansion nil)
    (setq blame-reveal--is-expanding nil)
    (setq blame-reveal--loading nil)
    (blame-reveal--stop-loading-animation)))


;;;; Lazy Loading Functions

(defun blame-reveal--should-lazy-load-p ()
  "Check if current file should use lazy loading based on size."
  (> (line-number-at-pos (point-max))
     blame-reveal-lazy-load-threshold))

(defun blame-reveal--is-range-loaded-p (start-line end-line)
  "Check if range START-LINE to END-LINE is already loaded."
  (if (not blame-reveal--blame-data-range)
      t  ;; Full file loaded
    (let ((current-start (car blame-reveal--blame-data-range))
          (current-end (cdr blame-reveal--blame-data-range)))
      (and (>= start-line current-start)
           (<= end-line current-end)))))

(defun blame-reveal--ensure-range-loaded (start-line end-line)
  "Ensure blame data is loaded for range START-LINE to END-LINE.
Uses sync or async based on configuration."
  (unless (blame-reveal--is-range-loaded-p start-line end-line)
    (when blame-reveal--blame-data-range
      (let ((current-start (car blame-reveal--blame-data-range))
            (current-end (cdr blame-reveal--blame-data-range)))
        (when (or (< start-line current-start)
                  (> end-line current-end))
          ;; 关键：如果已经在加载中，不要重复触发
          (unless blame-reveal--loading
            ;; Check if there's already a pending expansion that covers this range
            (unless (and blame-reveal--pending-expansion
                         (let ((pending-start (car blame-reveal--pending-expansion))
                               (pending-end (cdr blame-reveal--pending-expansion)))
                           (and (<= pending-start start-line)
                                (>= pending-end end-line))))
              ;; Need to load more data
              (let ((new-start (min start-line current-start))
                    (new-end (max end-line current-end)))
                (if (blame-reveal--should-use-async-p)
                    (blame-reveal--expand-blame-data-async new-start new-end)
                  (blame-reveal--expand-blame-data-sync new-start new-end))))))))))

(defun blame-reveal--expand-blame-data (start-line end-line)
  "Expand blame data to include START-LINE to END-LINE."
  (message "Expanding blame data: lines %d-%d..." start-line end-line)
  (let* ((file (buffer-file-name))
         (new-data (blame-reveal--get-blame-data-range start-line end-line)))
    (when new-data
      ;; Merge new data with existing data
      (let ((existing-lines (make-hash-table :test 'equal)))
        ;; Mark existing lines
        (dolist (entry blame-reveal--blame-data)
          (puthash (car entry) t existing-lines))

        ;; Add new lines
        (dolist (entry new-data)
          (unless (gethash (car entry) existing-lines)
            (push entry blame-reveal--blame-data))))

      ;; Sort by line number
      (setq blame-reveal--blame-data
            (sort blame-reveal--blame-data
                  (lambda (a b) (< (car a) (car b)))))

      ;; Update range
      (setq blame-reveal--blame-data-range
            (cons start-line end-line))

      ;; Load commit info for newly visible commits
      (blame-reveal--ensure-visible-commits-loaded)

      (message "Blame data expanded to %d lines" (length blame-reveal--blame-data)))))


;;;; Color Strategy System

(cl-defstruct (blame-reveal-color-strategy
               (:constructor nil))
  name description)

(cl-defgeneric blame-reveal-color-calculate (strategy commit-hash context))
(cl-defgeneric blame-reveal-color-quality (strategy num-commits context))
(cl-defgeneric blame-reveal-color-old (strategy context))
(cl-defgeneric blame-reveal-color-uncommitted (strategy context))

;; Utility functions
(defun blame-reveal-color--is-dark-theme-p ()
  (let* ((bg (or (face-background 'default) "white"))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      (< (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb))) 0.5))))

(defun blame-reveal-color--ease-out-cubic (x)
  (let ((x1 (- 1.0 x)))
    (- 1.0 (* x1 x1 x1))))

(defun blame-reveal-color--hsl-to-hex (h s l)
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1)))))
         (m (- l (/ c 2.0)))
         (rgb (cond ((< h 60) (list c x 0)) ((< h 120) (list x c 0))
                    ((< h 180) (list 0 c x)) ((< h 240) (list 0 x c))
                    ((< h 300) (list x 0 c)) (t (list c 0 x))))
         (r (round (* 255 (+ (nth 0 rgb) m))))
         (g (round (* 255 (+ (nth 1 rgb) m))))
         (b (round (* 255 (+ (nth 2 rgb) m)))))
    (format "#%02x%02x%02x" r g b)))

;; Gradient strategy
(cl-defstruct (blame-reveal-gradient-strategy
               (:include blame-reveal-color-strategy)
               (:constructor blame-reveal-gradient-strategy-create))
  (hue 210) (dark-newest 0.70) (dark-oldest 0.35)
  (light-newest 0.45) (light-oldest 0.75)
  (saturation-min 0.25) (saturation-max 0.60))

(cl-defmethod blame-reveal-color-calculate
  ((strategy blame-reveal-gradient-strategy) _commit-hash context)
  (let* ((rank (plist-get context :rank))
         (total-recent (plist-get context :total-recent))
         (is-dark (plist-get context :is-dark)))
    (when (and rank total-recent)
      (let* ((age-ratio (if (= total-recent 1) 0.0
                          (/ (float rank) (- total-recent 1))))
             (eased-ratio (blame-reveal-color--ease-out-cubic age-ratio))
             (hue (blame-reveal-gradient-strategy-hue strategy))
             (sat-min (blame-reveal-gradient-strategy-saturation-min strategy))
             (sat-max (blame-reveal-gradient-strategy-saturation-max strategy))
             (saturation (+ sat-min (* (- sat-max sat-min) (- 1.0 eased-ratio))))
             (lightness (if is-dark
                            (let ((newest (blame-reveal-gradient-strategy-dark-newest strategy))
                                  (oldest (blame-reveal-gradient-strategy-dark-oldest strategy)))
                              (+ oldest (* (- newest oldest) (- 1.0 eased-ratio))))
                          (let ((newest (blame-reveal-gradient-strategy-light-newest strategy))
                                (oldest (blame-reveal-gradient-strategy-light-oldest strategy)))
                            (- oldest (* (- oldest newest) (- 1.0 eased-ratio)))))))
        (blame-reveal-color--hsl-to-hex hue saturation lightness)))))

(cl-defmethod blame-reveal-color-quality
  ((strategy blame-reveal-gradient-strategy) num-commits context)
  (when (and (> num-commits 0) strategy)
    (let* ((is-dark (plist-get context :is-dark))
           (lightness-range
            (if is-dark
                (- (blame-reveal-gradient-strategy-dark-newest strategy)
                   (blame-reveal-gradient-strategy-dark-oldest strategy))
              (- (blame-reveal-gradient-strategy-light-oldest strategy)
                 (blame-reveal-gradient-strategy-light-newest strategy))))
           (saturation-range
            (- (blame-reveal-gradient-strategy-saturation-max strategy)
               (blame-reveal-gradient-strategy-saturation-min strategy)))
           (lightness-step (/ lightness-range (float (max 1 (1- num-commits)))))
           (saturation-step (/ saturation-range (float (max 1 (1- num-commits)))))
           (combined-step (+ (* 0.7 lightness-step) (* 0.3 saturation-step))))
      (cond ((>= combined-step 0.05) 1.0) ((>= combined-step 0.03) 0.8)
            ((>= combined-step 0.02) 0.5) (t (* combined-step 20))))))

(cl-defmethod blame-reveal-color-old
  ((strategy blame-reveal-gradient-strategy) context)
  (let* ((is-dark (plist-get context :is-dark))
         (hue (blame-reveal-gradient-strategy-hue strategy))
         (saturation (blame-reveal-gradient-strategy-saturation-min strategy))
         (lightness (if is-dark
                        (* (blame-reveal-gradient-strategy-dark-oldest strategy) 0.7)
                      (let ((oldest (blame-reveal-gradient-strategy-light-oldest strategy)))
                        (+ oldest (* (- 1.0 oldest) 0.5))))))
    (blame-reveal-color--hsl-to-hex hue saturation lightness)))

(cl-defmethod blame-reveal-color-uncommitted
  ((_strategy blame-reveal-gradient-strategy) context)
  (if (plist-get context :is-dark) "#d9a066" "#e6b380"))

;; Initialize strategy from color scheme
(defun blame-reveal--init-color-strategy ()
  (setq blame-reveal--color-strategy
        (blame-reveal-gradient-strategy-create
         :hue (plist-get blame-reveal-color-scheme :hue)
         :dark-newest (plist-get blame-reveal-color-scheme :dark-newest)
         :dark-oldest (plist-get blame-reveal-color-scheme :dark-oldest)
         :light-newest (plist-get blame-reveal-color-scheme :light-newest)
         :light-oldest (plist-get blame-reveal-color-scheme :light-oldest)
         :saturation-min (plist-get blame-reveal-color-scheme :saturation-min)
         :saturation-max (plist-get blame-reveal-color-scheme :saturation-max))))

;; Wrapper functions (replacing old ones)
(defun blame-reveal--get-commit-color (commit-hash)
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (if (blame-reveal--is-uncommitted-p commit-hash)
      (blame-reveal--get-uncommitted-color)
    (or (gethash commit-hash blame-reveal--color-map)
        (let* ((info (gethash commit-hash blame-reveal--commit-info))
               (timestamp (and info (nth 4 info)))
               (rank (cl-position commit-hash blame-reveal--recent-commits :test 'equal))
               (context (list :timestamp timestamp :rank rank
                              :total-recent (length blame-reveal--recent-commits)
                              :is-dark (blame-reveal-color--is-dark-theme-p)
                              :min-timestamp (car blame-reveal--timestamps)
                              :max-timestamp (cdr blame-reveal--timestamps)))
               (color (if rank
                          (blame-reveal-color-calculate blame-reveal--color-strategy commit-hash context)
                        (blame-reveal--get-old-commit-color))))
          (when color (puthash commit-hash color blame-reveal--color-map))
          (or color (if (blame-reveal-color--is-dark-theme-p) "#6699cc" "#7799bb"))))))

(defun blame-reveal--get-old-commit-color ()
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (or blame-reveal-old-commit-color
      (blame-reveal-color-old blame-reveal--color-strategy
                              (list :is-dark (blame-reveal-color--is-dark-theme-p)))))

(defun blame-reveal--get-uncommitted-color ()
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (or blame-reveal-uncommitted-color
      (blame-reveal-color-uncommitted blame-reveal--color-strategy
                                      (list :is-dark (blame-reveal-color--is-dark-theme-p)))))

(defun blame-reveal--calculate-gradient-quality (num-commits)
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (blame-reveal-color-quality blame-reveal--color-strategy num-commits
                              (list :is-dark (blame-reveal-color--is-dark-theme-p))))


(defsubst blame-reveal--is-uncommitted-p (commit-hash)
  "Check if COMMIT-HASH represents uncommitted changes."
  (string-match-p "^0+$" commit-hash))

(defun blame-reveal--get-commit-display-info (commit-hash)
  "Get display info for COMMIT-HASH.
Returns (COLOR IS-UNCOMMITTED IS-OLD-COMMIT HIDE-FRINGE).
This is a convenience function for common display logic."
  (blame-reveal--ensure-commit-info commit-hash)
  (let* ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
         (is-old-commit (and (not is-uncommitted)
                             (not (blame-reveal--should-render-commit commit-hash))))
         (color (cond
                 (is-uncommitted (blame-reveal--get-uncommitted-color))
                 (is-old-commit (blame-reveal--get-old-commit-color))
                 (t (blame-reveal--get-commit-color commit-hash))))
         (hide-fringe (and is-uncommitted
                           (not blame-reveal-show-uncommitted-fringe))))
    (list color is-uncommitted is-old-commit hide-fringe)))


;;;; Gradient Quality Functions

(defun blame-reveal--get-quality-thresholds ()
  "Get gradient quality thresholds based on user setting.

Returns a plist with:
  :min-quality - Minimum acceptable quality score (0.0-1.0)
  :max-commits - Maximum commits to include in gradient

These values control the auto-calculation algorithm:
- Higher min-quality = fewer commits, better distinction
- Lower min-quality = more commits, more history coverage"
  (pcase blame-reveal-gradient-quality
    ('strict  '(:min-quality 0.8 :max-commits 10))   ; Excellent distinction
    ('auto    '(:min-quality 0.5 :max-commits 20))   ; Good distinction
    ('relaxed '(:min-quality 0.3 :max-commits 30))   ; Acceptable distinction
    (_ '(:min-quality 0.5 :max-commits 20))))        ; Default to auto

(defun blame-reveal--should-recalculate-p ()
  "Check if auto-calculation should be triggered.

Returns t if recalculation is needed because:
- No cache exists yet
- Commit count changed significantly (>10 commits or >20% change)

This prevents unnecessary recalculation on every scroll,
while ensuring the limit adapts as more commits are loaded."
  (if (not blame-reveal--auto-days-cache)
      t  ; No cache, must calculate
    (let* ((cached-count (car blame-reveal--auto-days-cache))
           (current-count (hash-table-count blame-reveal--commit-info))
           (count-diff (abs (- current-count cached-count)))
           (count-ratio (if (> cached-count 0)
                            (/ (float count-diff) cached-count)
                          1.0)))
      ;; Recalculate if count changed significantly
      (or (> count-diff 10)           ; More than 10 new commits
          (> count-ratio 0.2)))))     ; More than 20% change

(defun blame-reveal--get-reference-time ()
  "Get reference time for age calculation."
  (if (and (boundp 'blame-reveal--current-revision)
           blame-reveal--current-revision
           (not (eq blame-reveal--current-revision 'uncommitted)))
      ;; Recursive: use newest commit in current view
      (or (cl-loop for info being the hash-values of blame-reveal--commit-info
                   when (nth 4 info)
                   maximize (nth 4 info))
          (float-time))
    ;; Normal: use current time
    (float-time)))


;;;; Commit Selection Functions

(defun blame-reveal--is-recent-commit-p (commit-hash)
  "Check if COMMIT-HASH is in the recent commits list.

Recent commits are those that pass the days limit filter
(either auto-calculated or user-specified).

Returns t if commit should be highlighted with gradient colors,
nil if it should be shown in gray (old commit color)."
  (member commit-hash blame-reveal--recent-commits))

(defun blame-reveal--update-recent-commits ()
  "Update list of recent commits based on days limit.

In normal blame mode:
- Uses current time as reference
- 'Recent' means commits within N days from now

In recursive blame mode:
- Uses revision date as reference
- 'Recent' means commits within N days from that revision
- This ensures meaningful gradient when exploring history

The days limit comes from `blame-reveal-recent-days-limit':
- 'auto: Calculated automatically based on commit density
- number: Fixed days limit
- nil: No limit (all commits considered recent)"
  (when blame-reveal--commit-info
    (let* ((commit-timestamps nil)
           (reference-time (blame-reveal--get-reference-time))
           ;; Get effective days limit
           (days-limit (pcase blame-reveal-recent-days-limit
                         ('auto (or (blame-reveal--auto-calculate-days-limit) 90))
                         ('nil nil)
                         (_ blame-reveal-recent-days-limit))))

      ;; Collect all commits with timestamps
      (maphash (lambda (commit info)
                 (when-let ((timestamp (nth 4 info)))
                   (push (cons commit timestamp) commit-timestamps)))
               blame-reveal--commit-info)

      ;; Sort by timestamp (newest first)
      (setq commit-timestamps
            (sort commit-timestamps
                  (lambda (a b) (> (cdr a) (cdr b)))))

      ;; Filter by days if limit is set
      (when days-limit
        (let ((age-limit-seconds (* days-limit 86400)))
          (setq commit-timestamps
                (cl-remove-if
                 (lambda (commit-ts)
                   (> (- reference-time (cdr commit-ts))
                      age-limit-seconds))
                 commit-timestamps))))

      (setq blame-reveal--recent-commits
            (mapcar #'car commit-timestamps)))))

(defun blame-reveal--auto-calculate-days-limit ()
  "Automatically calculate days limit for optimal color gradient.

Algorithm:
1. Collect timestamps from loaded commits
2. Analyze recent commits (sample of 5-15 based on availability)
3. Calculate their time span
4. Extend appropriately to reach target commit count
5. Verify gradient quality (color distinguishability)
6. Adjust range if gradient is too poor or too good
7. Cache result to avoid recalculation on every scroll

In recursive blame mode:
- Uses the revision date as reference point (not current time)
- Calculates based on commits visible at that point in history
- Ensures meaningful gradient when exploring old revisions

Performance:
- First call: ~5-10ms (actual calculation)
- Cached calls: <1ms (instant return)
- Cache invalidated only when commit count changes significantly

Returns:
- Recommended days limit (integer)
- nil if insufficient data (<3 commits)"
  (when (and blame-reveal--commit-info
             (> (hash-table-count blame-reveal--commit-info) 3))

    ;; Check cache: avoid recalculation if commit count unchanged
    (if (and blame-reveal--auto-days-cache
             (not (blame-reveal--should-recalculate-p)))
        ;; Return cached value
        (cdr blame-reveal--auto-days-cache)

      ;; Need to recalculate
      (let* ((timestamps nil)
             (reference-time (blame-reveal--get-reference-time))
             (current-count (hash-table-count blame-reveal--commit-info)))

        ;; Collect all loaded timestamps
        (maphash (lambda (_commit info)
                   (when-let ((ts (nth 4 info)))
                     (push ts timestamps)))
                 blame-reveal--commit-info)

        (when (>= (length timestamps) 3)
          (setq timestamps (sort timestamps #'>))

          (let* ((total-commits (length timestamps))
                 ;; Adaptive sample size based on available commits
                 (sample-size (cond
                               ((>= total-commits 15) 15)  ; Ideal: 15 commits
                               ((>= total-commits 10) 10)  ; Good: 10 commits
                               ((>= total-commits 5) 5)    ; Acceptable: 5 commits
                               (t total-commits)))         ; Use all available

                 (sample (seq-take timestamps sample-size))
                 (span-seconds (when (> (length sample) 1)
                                 (- (car sample) (car (last sample)))))
                 (span-days (when span-seconds
                              (/ span-seconds 86400.0))))

            (when span-days
              (let* (;; Extension factor: smaller samples need more extension
                     (extension-factor
                      (cond
                       ((>= sample-size 15) 1.5)  ; Good sample, extend 50%
                       ((>= sample-size 10) 1.8)  ; Medium sample, extend 80%
                       ((>= sample-size 5) 2.0)   ; Small sample, extend 100%
                       (t 2.5)))                  ; Tiny sample, extend 150%

                     (base-days (* span-days extension-factor))

                     ;; Minimum days to ensure gradient quality
                     (min-days-for-gradient
                      (cond
                       ((< span-days 7) 7)      ; Very active: at least 1 week
                       ((< span-days 30) 14)    ; Moderate: at least 2 weeks
                       (t 30)))                 ; Quiet: at least 1 month

                     (adjusted-days (max base-days min-days-for-gradient))

                     ;; Get quality thresholds from user setting
                     (thresholds (blame-reveal--get-quality-thresholds))
                     (min-quality (plist-get thresholds :min-quality))
                     (max-commits (plist-get thresholds :max-commits))

                     ;; Target commit count range
                     (min-commits 5)
                     (target-commits (min max-commits total-commits))

                     ;; Calculate commits in current range
                     (age-limit-seconds (* adjusted-days 86400))
                     (commits-in-range
                      (length (cl-remove-if
                               (lambda (ts)
                                 (> (- reference-time ts) age-limit-seconds))
                               timestamps))))

                ;; Phase 1: Expand to meet minimum commit count
                (while (and (< commits-in-range min-commits)
                            (< adjusted-days 730)
                            (< commits-in-range total-commits))
                  (setq adjusted-days (* adjusted-days 1.5))
                  (setq age-limit-seconds (* adjusted-days 86400))
                  (setq commits-in-range
                        (length (cl-remove-if
                                 (lambda (ts)
                                   (> (- reference-time ts) age-limit-seconds))
                                 timestamps))))

                ;; Phase 2: Adjust for gradient quality
                (let ((gradient-quality (blame-reveal--calculate-gradient-quality
                                         commits-in-range))
                      (iterations 0))

                  ;; Shrink if quality is poor (too many commits, colors too similar)
                  (while (and gradient-quality
                              (< gradient-quality min-quality)
                              (> commits-in-range min-commits)
                              (> adjusted-days min-days-for-gradient)
                              (< iterations 10))  ; Safety limit
                    (setq adjusted-days (* adjusted-days 0.75))
                    (setq age-limit-seconds (* adjusted-days 86400))
                    (setq commits-in-range
                          (length (cl-remove-if
                                   (lambda (ts)
                                     (> (- reference-time ts) age-limit-seconds))
                                   timestamps)))
                    (setq gradient-quality
                          (blame-reveal--calculate-gradient-quality commits-in-range))
                    (setq iterations (1+ iterations)))

                  ;; Expand if quality is too good (too few commits, wasting color range)
                  (setq iterations 0)
                  (while (and gradient-quality
                              (> gradient-quality 0.9)  ; Excellent but maybe too few
                              (< commits-in-range target-commits)
                              (< adjusted-days 730)
                              (< commits-in-range total-commits)
                              (< iterations 10))
                    (setq adjusted-days (* adjusted-days 1.3))
                    (setq age-limit-seconds (* adjusted-days 86400))
                    (let ((new-count (length (cl-remove-if
                                              (lambda (ts)
                                                (> (- reference-time ts) age-limit-seconds))
                                              timestamps))))
                      ;; Only expand if quality stays acceptable
                      (when (>= (blame-reveal--calculate-gradient-quality new-count)
                                min-quality)
                        (setq commits-in-range new-count))
                      (setq iterations (1+ iterations)))))

                ;; Phase 3: Special case - all commits in short time
                (when (and (<= commits-in-range total-commits)
                           (> adjusted-days (* span-days 3)))
                  ;; Don't extend too far beyond actual commit span
                  (setq adjusted-days (max min-days-for-gradient
                                           (* span-days 2))))

                ;; Final: clamp to reasonable bounds and cache
                (let ((final-days (max 7 (min 730 (ceiling adjusted-days)))))
                  ;; Update cache
                  (setq blame-reveal--auto-days-cache
                        (cons current-count final-days))
                  final-days)))))))))


;;;; Sticky Header Support

(defun blame-reveal--clear-sticky-header ()
  "Clear sticky header overlay."
  (when blame-reveal--sticky-header-overlay
    (delete-overlay blame-reveal--sticky-header-overlay)
    (setq blame-reveal--sticky-header-overlay nil)))

(defun blame-reveal--get-header-line-count ()
  "Get the number of lines the header overlay occupies.
Based on current display layout and position settings."
  (if (eq blame-reveal-header-position 'after-first-line)
      ;; After first line: always single line + fringe
      2
    ;; Before block: original multi-line format
    (pcase blame-reveal-display-layout
      ('line 2)      ; message line + fringe line
      ('compact 3)   ; message + metadata + fringe
      ('full 5)      ; message + metadata + description + fringe
      (_ 3))))

(defun blame-reveal--is-header-visible-p (block-start-line window-start-line)
  "Check if header at BLOCK-START-LINE is visible given WINDOW-START-LINE.

Header visibility depends on position mode:
- before-block: header overlay is at line (block-start - 1), 
                needs that line to be visible
- after-first-line: header overlay is at line block-start,
                    header is visible if block-start is visible"

  (if (eq blame-reveal-header-position 'after-first-line)
      ;; After-first-line mode: header on same line as block start
      (<= window-start-line block-start-line)

    ;; Before-block mode: header overlay at previous line's end
    (cond
     ;; Special case: block at line 1, overlay at point-min
     ((= block-start-line 1)
      (<= window-start-line 1))

     ;; Normal case: overlay at (line-end-position (1- block-start-line))
     ;; Need line (block-start-line - 1) to be visible
     (t
      (<= window-start-line (1- block-start-line))))))

(defun blame-reveal--find-block-for-commit (commit-hash block-start-line)
  "Find block info for COMMIT-HASH starting at BLOCK-START-LINE.
Returns the block from blame-reveal--blame-data, or nil if not found."
  (cl-loop for block in (blame-reveal--find-block-boundaries blame-reveal--blame-data)
           when (and (= (nth 0 block) block-start-line)
                     (equal (nth 1 block) commit-hash))
           return block))

(defun blame-reveal--should-show-sticky-header-p (commit-hash block-start-line current-line)
  "Determine if sticky header should be shown.
Arguments:
  COMMIT-HASH: Hash of the commit block
  BLOCK-START-LINE: Starting line of the block
  CURRENT-LINE: Current cursor line

Returns t if all conditions are met:
  1. Header is scrolled off-screen
  2. Cursor is within THIS SPECIFIC BLOCK (not just any block of same commit)"
  (when-let* ((block-info (blame-reveal--find-block-for-commit
                           commit-hash block-start-line))
              (block-length (nth 2 block-info))
              (block-end-line (+ block-start-line block-length -1))
              (window-start-line (line-number-at-pos (window-start))))

    (let* ((header-visible (blame-reveal--is-header-visible-p
                            block-start-line window-start-line))
           ;; CRITICAL: cursor must be in THIS specific block
           (in-this-block (and (>= current-line block-start-line)
                               (<= current-line block-end-line))))

      ;; Debug: uncomment to see detailed check
      ;; (message "  -> block: %d-%d, win-start: %d, header-visible: %s, in-block: %s, result: %s"
      ;;          block-start-line block-end-line window-start-line
      ;;          header-visible in-this-block
      ;;          (and (not header-visible) in-this-block))

      ;; Show if header is off-screen AND cursor is in THIS block
      (and (not header-visible) in-this-block))))

(defun blame-reveal--create-header-overlay-inline (line-number commit-hash color &optional no-fringe)
  "Create inline header at end of LINE-NUMBER (after-first-line mode).
The overlay is positioned at the end of the first line of the block.
COLOR is used for header text foreground.
If NO-FRINGE is non-nil, don't show fringe indicators."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let ((pos (line-end-position)))
        (let* ((overlay (make-overlay pos pos))
               (header-text (blame-reveal--format-header-text commit-hash t))
               (fringe-face (blame-reveal--ensure-fringe-face color))
               (faces (blame-reveal--get-header-faces color))
               ;; For inline mode, use metadata face for the compact line
               (inline-face (plist-get faces :header)))
          (overlay-put overlay 'blame-reveal t)
          (overlay-put overlay 'blame-reveal-commit commit-hash)
          (overlay-put overlay 'blame-reveal-header t)
          ;; CRITICAL: Use after-string to place content AFTER the line
          (overlay-put overlay 'after-string
                       (concat
                        ;; Two spaces for fixed distance from code
                        "  "
                        ;; Header text with appropriate face
                        (propertize header-text 'face inline-face)
                        ;; Fringe indicator on next line
                        (unless no-fringe
                          (propertize "!" 'display
                                      (list blame-reveal-style
                                            'blame-reveal-full
                                            fringe-face)))))
          overlay)))))

(defun blame-reveal--create-sticky-header-overlay (commit-hash)
  "Create sticky header overlay for COMMIT-HASH at window top.
Adapts format based on `blame-reveal-header-position'.
Returns the created overlay."
  (save-excursion
    (goto-char (window-start))
    (pcase-let* ((pos (line-beginning-position))
                 (`(,color ,_is-uncommitted ,_is-old-commit ,hide-fringe)
                  (blame-reveal--get-commit-display-info commit-hash))
                 (is-inline (eq blame-reveal-header-position 'after-first-line))
                 (header-text (blame-reveal--format-header-text commit-hash is-inline))
                 (overlay (make-overlay pos pos))
                 (fringe-face (blame-reveal--ensure-fringe-face color))
                 (faces (blame-reveal--get-header-faces color))
                 (sticky-indicator (propertize " " 'face `(:foreground ,color))))

      (overlay-put overlay 'blame-reveal-sticky t)

      (if is-inline
          ;; Inline mode: single line format positioned at END of first visible line
          (let ((inline-face (plist-get faces :header))
                (line-end (line-end-position)))
            ;; Move overlay to end of line for inline style
            (move-overlay overlay line-end line-end)
            (overlay-put overlay 'after-string
                         (concat
                          ;; Two spaces for distance from code
                          "  "
                          sticky-indicator
                          ;; Header text
                          (propertize header-text 'face inline-face)
                          ;; Fringe on next line
                          (unless hide-fringe
                            (propertize "!" 'display
                                        (list blame-reveal-style
                                              'blame-reveal-full
                                              fringe-face))))))

        ;; Normal mode: multi-line format at beginning of line
        (let ((header-face (plist-get faces :header))
              (metadata-face (plist-get faces :metadata))
              (header-lines (split-string header-text "\n")))
          (overlay-put overlay 'before-string
                       (concat
                        (unless hide-fringe
                          (propertize "!" 'display
                                      (list blame-reveal-style
                                            'blame-reveal-full
                                            fringe-face)))
                        sticky-indicator
                        (propertize (car header-lines) 'face header-face)
                        "\n"
                        (when (cdr header-lines)
                          (concat
                           (unless hide-fringe
                             (propertize "!" 'display
                                         (list blame-reveal-style
                                               'blame-reveal-full
                                               fringe-face)))
                           sticky-indicator
                           (propertize (cadr header-lines) 'face metadata-face)
                           "\n"))
                        (unless hide-fringe
                          (propertize "!" 'display
                                      (list blame-reveal-style
                                            'blame-reveal-full
                                            fringe-face)))))))
      overlay)))

(defun blame-reveal--update-sticky-header ()
  "Update sticky header display based on cursor position.
Shows sticky header when the block header is scrolled off-screen
and cursor is still within that block."
  (blame-reveal--clear-sticky-header)

  (when-let* ((current-block (blame-reveal--get-current-block))
              (commit-hash (car current-block))
              (block-start-line (cdr current-block))
              (current-line (line-number-at-pos)))

    ;; Debug: uncomment to diagnose issues
    ;; (message "Sticky check: commit=%s block-start=%d current=%d"
    ;;          (substring commit-hash 0 8) block-start-line current-line)

    (when (blame-reveal--should-show-sticky-header-p
           commit-hash block-start-line current-line)
      (setq blame-reveal--sticky-header-overlay
            (blame-reveal--create-sticky-header-overlay commit-hash)))))


;;;; Overlay Management Functions

(defun blame-reveal--find-block-boundaries (blame-data &optional start-line end-line)
  "Find boundaries of same-commit blocks in BLAME-DATA.
If START-LINE and END-LINE are provided, only return blocks in that range.
Returns list of (START-LINE COMMIT-HASH BLOCK-LENGTH)."
  (let ((blocks nil)
        (current-commit nil)
        (block-start nil)
        (block-length 0))
    (dolist (entry blame-data)
      (let ((line (car entry))
            (commit (cdr entry)))
        (if (equal commit current-commit)
            (setq block-length (1+ block-length))
          (when current-commit
            ;; Add block if no range specified or block overlaps with range
            (when (or (not start-line)
                      (and (>= (+ block-start block-length -1) start-line)
                           (<= block-start end-line)))
              (push (list block-start current-commit block-length) blocks)))
          (setq current-commit commit
                block-start line
                block-length 1))))
    (when current-commit
      (when (or (not start-line)
                (and (>= (+ block-start block-length -1) start-line)
                     (<= block-start end-line)))
        (push (list block-start current-commit block-length) blocks)))
    (nreverse blocks)))

(defun blame-reveal--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE)."
  (let* ((window-start (window-start))
         (window-end (window-end nil t))
         (start-line (max 1 (- (line-number-at-pos window-start)
                               blame-reveal-render-margin)))
         (end-line (+ (line-number-at-pos window-end)
                      blame-reveal-render-margin)))
    (cons start-line end-line)))

(defun blame-reveal--ensure-fringe-face (color)
  "Ensure fringe face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-face-%s" color))))
    (unless (facep face-name)
      (custom-declare-face face-name
                           `((t :background ,color :foreground ,color))
                           (format "Face for git blame color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--format-header-text (commit-hash &optional inline-mode)
  "Format header text for COMMIT-HASH based on `blame-reveal-display-layout'.
If INLINE-MODE is non-nil, format for after-first-line position:
  - `compact' becomes single line: msg · author · hash
  - `line' shows only message
  - `full' falls back to compact single line"
  ;; Check if this is an uncommitted change (all zeros)
  (if (blame-reveal--is-uncommitted-p commit-hash)
      (if inline-mode
          (format "%s" blame-reveal-uncommitted-label)
        (format "▸ %s" blame-reveal-uncommitted-label))
    (let ((info (gethash commit-hash blame-reveal--commit-info)))
      (if info
          (let ((short-hash (nth 0 info))
                (author (nth 1 info))
                (date (nth 2 info))
                (summary (nth 3 info))
                (description (nth 5 info)))
            (if inline-mode
                ;; Inline mode: compact single-line formats without ▸ prefix
                (pcase blame-reveal-display-layout
                  ('line
                   ;; Only commit message
                   (format "▸ [%s]" summary))
                  ((or 'compact 'full)
                   ;; Single line: msg · author · hash
                   (format "▸ [%s · %s · %s]" summary author short-hash))
                  (_ (format "%s" summary)))
              ;; Normal mode: original multi-line formats with ▸ prefix
              (pcase blame-reveal-display-layout
                ('line
                 (format "▸ %s" summary))
                ('compact
                 (format "▸ %s\n  %s · %s · %s"
                         summary short-hash author date))
                ('full
                 (let ((desc-trimmed (if description (string-trim description) "")))
                   (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                       (let ((desc-lines (split-string desc-trimmed "\n")))
                         (format "▸ %s\n  %s · %s · %s\n\n%s"
                                 summary short-hash author date
                                 (mapconcat (lambda (line) (concat "  " line))
                                            desc-lines
                                            "\n")))
                     (format "▸ %s\n  %s · %s · %s"
                             summary short-hash author date))))
                (_ (format "▸ %s" summary)))))
        ;; Fallback for missing info
        (if inline-mode
            (format "Commit %s" (substring commit-hash 0 8))
          (format "▸ Commit %s" (substring commit-hash 0 8)))))))

(defun blame-reveal--create-header-overlay (line-number commit-hash color &optional no-fringe)
  "Create header line for commit block based on `blame-reveal-header-position'.
LINE-NUMBER is the first line of the commit block.
COLOR is the fringe/header color.
If NO-FRINGE is non-nil, don't show fringe indicators."
  (if (eq blame-reveal-header-position 'after-first-line)
      ;; Use inline mode (after first line)
      (blame-reveal--create-header-overlay-inline line-number commit-hash color no-fringe)
    ;; Use original mode (before block)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-number))
      (unless (eobp)
        (let ((pos (if (= line-number 1)
                       (point-min)
                     (progn
                       (forward-line -1)
                       (line-end-position)))))
          (let* ((overlay (make-overlay pos pos))
                 (header-text (blame-reveal--format-header-text commit-hash nil))
                 (fringe-face (blame-reveal--ensure-fringe-face color))
                 (faces (blame-reveal--get-header-faces color))
                 (header-face (plist-get faces :header))
                 (metadata-face (plist-get faces :metadata))
                 (description-face (plist-get faces :description))
                 (header-lines (split-string header-text "\n"))
                 (need-leading-newline (not (= line-number 1))))
            (overlay-put overlay 'blame-reveal t)
            (overlay-put overlay 'blame-reveal-commit commit-hash)
            (overlay-put overlay 'blame-reveal-header t)
            (overlay-put overlay 'before-string
                         (concat
                          (when need-leading-newline "\n")
                          (unless no-fringe
                            (propertize "!" 'display
                                        (list blame-reveal-style
                                              'blame-reveal-full
                                              fringe-face)))
                          (propertize (car header-lines) 'face header-face)
                          "\n"
                          (when (cdr header-lines)
                            (let ((remaining-lines (cdr header-lines))
                                  (result ""))
                              (dotimes (i (length remaining-lines))
                                (let* ((line (nth i remaining-lines))
                                       (line-face (if (and (eq blame-reveal-display-layout 'full)
                                                           (> i 0)
                                                           (not (string-empty-p (string-trim line))))
                                                      description-face
                                                    metadata-face)))
                                  (setq result
                                        (concat result
                                                (unless no-fringe
                                                  (propertize "!" 'display
                                                              (list blame-reveal-style
                                                                    'blame-reveal-full
                                                                    fringe-face)))
                                                (propertize line 'face line-face)
                                                "\n"))))
                              result))
                          (unless no-fringe
                            (propertize "!" 'display
                                        (list blame-reveal-style
                                              'blame-reveal-full
                                              fringe-face)))))
            overlay))))))

;;;; Rendering Functions

(defun blame-reveal--should-render-commit (commit-hash)
  "Check if a commit should be rendered in permanent layer.
Only recent commits (top N by recency among loaded commits) are permanently visible."
  (blame-reveal--is-recent-commit-p commit-hash))

;; (defun blame-reveal--recolor-and-render ()
;;   "Recalculate colors and re-render (for theme changes)."
;;   (when blame-reveal--blame-data
;;     (setq blame-reveal--color-map (make-hash-table :test 'equal))
;;     (blame-reveal--render-visible-region)))
(defun blame-reveal--recolor-and-render ()
  "Recalculate colors and re-render (for theme changes)."
  (when blame-reveal--blame-data
    (setq blame-reveal--color-map (make-hash-table :test 'equal))
    (blame-reveal--init-color-strategy)  ;; 添加这行
    (blame-reveal--render-visible-region)))


;;;; Loading Functions

(defsubst blame-reveal--get-visible-commits ()
  "Get list of commit hashes in visible area."
  (when blame-reveal--blame-data
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (visible-blocks (blame-reveal--find-block-boundaries
                            blame-reveal--blame-data
                            start-line
                            end-line)))
      (mapcar #'cadr visible-blocks))))

(defun blame-reveal--ensure-visible-commits-loaded ()
  "Ensure commit info is loaded for all visible commits."
  (when-let ((visible-commits (blame-reveal--get-visible-commits)))
    (dolist (commit visible-commits)
      (blame-reveal--ensure-commit-info commit))
    (blame-reveal--update-recent-commits)))

(defun blame-reveal--load-commits-incrementally ()
  "Load commit info for visible area only (lazy loading)."
  (blame-reveal--ensure-visible-commits-loaded))

(defun blame-reveal--load-blame-data-impl (buffer)
  "Implementation of blame data loading for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((file (buffer-file-name))
             (use-lazy (blame-reveal--should-lazy-load-p))
             (blame-data nil))

        (if use-lazy
            ;; Large file: only load visible region
            (let* ((range (blame-reveal--get-visible-line-range))
                   (start-line (car range))
                   (end-line (cdr range)))
              (setq blame-data (blame-reveal--get-blame-data-range
                                start-line end-line))
              (setq blame-reveal--blame-data-range (cons start-line end-line))
              (when blame-data
                (message "Git blame loaded (lazy): %d lines (range %d-%d)"
                         (length blame-data) start-line end-line)))

          ;; Small file: load entire file
          (setq blame-data (blame-reveal--get-blame-data))
          (setq blame-reveal--blame-data-range nil)  ; nil = full load
          (when blame-data
            (message "Git blame loaded (full): %d lines" (length blame-data))))

        (if blame-data
            (progn
              (setq blame-reveal--blame-data blame-data)
              (setq blame-reveal--commit-info (make-hash-table :test 'equal))
              (setq blame-reveal--color-map (make-hash-table :test 'equal))
              (setq blame-reveal--timestamps nil)
              (setq blame-reveal--recent-commits nil)
              (setq blame-reveal--all-commits-loaded nil)
              (setq blame-reveal--loading nil)

              ;; Load commits incrementally
              (blame-reveal--load-commits-incrementally)

              ;; Initial render
              (blame-reveal--render-visible-region))

          (setq blame-reveal--loading nil)
          (message "No git blame data available"))))))

(defun blame-reveal--load-blame-data ()
  "Load git blame data using sync or async based on configuration.
Use lazy loading for large files, full loading for small files."
  (unless blame-reveal--loading
    (setq blame-reveal--loading t)
    (condition-case err
        (if (blame-reveal--should-use-async-p)
            (blame-reveal--load-blame-data-async)
          (blame-reveal--load-blame-data-sync))
      (error
       (setq blame-reveal--loading nil)
       (message "Failed to load git blame: %s" (error-message-string err))))))

(defun blame-reveal--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  (setq blame-reveal--blame-data nil
        blame-reveal--blame-data-range nil
        blame-reveal--commit-info nil
        blame-reveal--color-map nil
        blame-reveal--timestamps nil
        blame-reveal--recent-commits nil
        blame-reveal--all-commits-loaded nil)
  (blame-reveal--load-blame-data))


;;;; Event Handlers

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line."
  (let ((line-num (line-number-at-pos)))
    ;; First try from overlays (faster)
    (or (cl-loop for ov in (overlays-at (point))
                 for commit = (overlay-get ov 'blame-reveal-commit)
                 when commit
                 return (cl-loop for block in (blame-reveal--find-block-boundaries
                                               blame-reveal--blame-data)
                                 when (and (equal commit (nth 1 block))
                                           (>= line-num (nth 0 block))
                                           (< line-num (+ (nth 0 block) (nth 2 block))))
                                 return (cons commit (nth 0 block))))
        ;; Fallback: search in blame data
        (cl-loop for block in (blame-reveal--find-block-boundaries
                               blame-reveal--blame-data)
                 when (and (>= line-num (nth 0 block))
                           (< line-num (+ (nth 0 block) (nth 2 block))))
                 return (cons (nth 1 block) (nth 0 block))))))

(defun blame-reveal--update-header ()
  "Update header display based on current cursor position.
Delays rendering until cursor movement stops.
Also triggers commit info loading when cursor stops."
  (let ((current-block (blame-reveal--get-current-block)))
    (when current-block
      (let ((commit-hash (car current-block)))

        ;; If a new block is entered, immediately clear the overlay of the previous block.
        (when (and blame-reveal--last-rendered-commit
                   (not (equal blame-reveal--last-rendered-commit commit-hash)))
          (blame-reveal--clear-temp-overlays-for-commit
           blame-reveal--last-rendered-commit)
          (setq blame-reveal--last-rendered-commit nil))

        ;; 更新当前块标记
        (setq blame-reveal--current-block-commit commit-hash))))

  ;; Cancel previous timer
  (when blame-reveal--header-update-timer
    (cancel-timer blame-reveal--header-update-timer)
    (setq blame-reveal--header-update-timer nil))

  ;; Set new timer for rendering
  (setq blame-reveal--header-update-timer
        (run-with-idle-timer
         blame-reveal--scroll-render-delay nil
         #'blame-reveal--update-header-impl)))

(defun blame-reveal--scroll-handler-impl (buf)
  "Implementation of scroll handler for buffer BUF.
Only loads blame data (git blame), does NOT load commit info (git show).
Commit info will be loaded later when cursor stops moving."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when blame-reveal-mode
        ;; If using lazy loading, check if we need to load more blame data
        (when blame-reveal--blame-data-range
          (let* ((range (blame-reveal--get-visible-line-range))
                 (start-line (car range))
                 (end-line (cdr range))
                 (current-start (car blame-reveal--blame-data-range))
                 (current-end (cdr blame-reveal--blame-data-range)))
            ;; Only trigger expansion if scrolled beyond current range
            (when (or (< start-line current-start)
                      (> end-line current-end))
              (blame-reveal--ensure-range-loaded start-line end-line))))

        ;; Only render if not expanding (async mode) or always render (sync mode)
        (unless (and (blame-reveal--should-use-async-p)
                     blame-reveal--is-expanding)
          ;; Render visible region with whatever data we have
          (blame-reveal--render-visible-region)
          ;; Only update sticky header if layout is not 'none
          (unless (eq blame-reveal-display-layout 'none)
            (blame-reveal--update-sticky-header)))))))

(defun blame-reveal--on-scroll ()
  "Handle scroll event with debouncing.
Delays rendering until scrolling stops."
  (let ((current-start (window-start)))
    (unless (equal current-start blame-reveal--last-window-start)
      (setq blame-reveal--last-window-start current-start)

      ;; Cancel previous timer
      (when blame-reveal--scroll-timer
        (cancel-timer blame-reveal--scroll-timer))

      ;; Clear permanent fringe overlays using unified system
      (blame-reveal--clear-overlays-by-type 'fringe)

      (when blame-reveal--header-overlay
        (delete-overlay blame-reveal--header-overlay)
        (setq blame-reveal--header-overlay nil))

      (blame-reveal--clear-sticky-header)

      ;; Set new timer with configured delay
      (setq blame-reveal--scroll-timer
            (run-with-idle-timer
             blame-reveal--scroll-render-delay nil
             #'blame-reveal--scroll-handler-impl
             (current-buffer))))))

(defun blame-reveal--scroll-handler (_win _start)
  "Handle window scroll events."
  (blame-reveal--on-scroll))

(defun blame-reveal--theme-change-handler ()
  "Handle theme change for all blame-reveal buffers.
Recalculates colors and refreshes all displays."
  (let ((current-buf (current-buffer))
        (current-point (point)))
    ;; Re-render fringe overlays with new theme colors in all buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when blame-reveal-mode
          (blame-reveal--recolor-and-render))))

    ;; Refresh header in current buffer if it exists
    (when (buffer-live-p current-buf)
      (with-current-buffer current-buf
        (when (and blame-reveal-mode
                   blame-reveal--current-block-commit)
          (save-excursion
            (goto-char current-point)
            ;; Force header refresh by clearing and re-triggering
            (let ((old-commit blame-reveal--current-block-commit))
              (setq blame-reveal--current-block-commit nil)
              ;; Clear existing header
              (when blame-reveal--header-overlay
                (delete-overlay blame-reveal--header-overlay)
                (setq blame-reveal--header-overlay nil))
              ;; Clear temp overlays
              (blame-reveal--clear-temp-overlays)
              ;; Re-trigger header update
              (blame-reveal--update-header))))))))

(defun blame-reveal--on-theme-change (&rest _)
  "Handle theme change event and refresh all blame displays."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer))
  (setq blame-reveal--theme-change-timer
        (run-with-timer 0.3 nil #'blame-reveal--theme-change-handler)))

(defun blame-reveal--setup-theme-advice ()
  "Setup advice to monitor theme changes."
  (if (boundp 'after-enable-theme-hook)
      ;; Use official hook for Emacs 29+
      (add-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    ;; Use advice for older Emacs versions
    (advice-add 'load-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'enable-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'disable-theme :after #'blame-reveal--on-theme-change)))

(defun blame-reveal--remove-theme-advice ()
  "Remove theme change advice."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer)
    (setq blame-reveal--theme-change-timer nil))

  (if (boundp 'after-enable-theme-hook)
      (remove-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    (advice-remove 'load-theme #'blame-reveal--on-theme-change)
    (advice-remove 'enable-theme #'blame-reveal--on-theme-change)
    (advice-remove 'disable-theme #'blame-reveal--on-theme-change)))


;;;; Mode Line Functions

(defun blame-reveal--mode-line-revision ()
  "Return mode line string showing current revision."
  (when blame-reveal--revision-display
    (format " @%s"
            (propertize blame-reveal--revision-display
                        'face 'font-lock-constant-face))))

(defun blame-reveal--setup-mode-line ()
  "Add revision info to mode line."
  (unless (member '(:eval (blame-reveal--mode-line-revision)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  '((:eval (blame-reveal--mode-line-revision)))))))

(defun blame-reveal--cleanup-mode-line ()
  "Remove revision info from mode line."
  (setq global-mode-string
        (delete '(:eval (blame-reveal--mode-line-revision)) global-mode-string)))


;;;; Magit Integration

(defun blame-reveal--should-use-magit-p ()
  "Check if magit should be used based on configuration."
  (pcase blame-reveal-use-magit
    ('auto (and (fboundp 'magit-show-commit)
                (fboundp 'magit-log-buffer-file)))
    ('t (if (and (fboundp 'magit-show-commit)
                 (fboundp 'magit-log-buffer-file))
            t
          (error "Magit is not available but blame-reveal-use-magit is set to t")))
    (_ nil)))


;;;; Interactive Commands

(defun blame-reveal--revert-commit-diff (commit-hash)
  "Revert function for commit diff buffer showing COMMIT-HASH."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "show" "--color=never" commit-hash)
    (goto-char (point-min))))

(defun blame-reveal--revert-file-log (file)
  "Revert function for git log buffer of FILE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "log"
                  "--color=always"
                  "--follow"
                  "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                  "--" file)
    (goto-char (point-min))
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun blame-reveal--revert-line-log (line-num file)
  "Revert function for git log buffer of LINE-NUM in FILE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "log"
                  "--color=always"
                  "-L" (format "%d,%d:%s" line-num line-num file))
    (goto-char (point-min))
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(defun blame-reveal-show-auto-calculation ()
  "Show how auto days limit is calculated for current file.

Displays diagnostic information including:
- Current mode (normal or recursive blame)
- Reference time used for age calculation
- Sample size and time span
- Calculated days limit
- Number of commits in range
- Gradient quality assessment

Useful for understanding the auto-calculation algorithm
and debugging color gradient issues."
  (interactive)
  (if (not blame-reveal--commit-info)
      (message "No blame data loaded yet")
    (let* ((timestamps nil)
           (reference-time (blame-reveal--get-reference-time))
           (is-recursive (and (boundp 'blame-reveal--current-revision)
                              blame-reveal--current-revision
                              (not (eq blame-reveal--current-revision 'uncommitted)))))

      ;; Collect timestamps
      (maphash (lambda (_commit info)
                 (when-let ((ts (nth 4 info)))
                   (push ts timestamps)))
               blame-reveal--commit-info)

      (setq timestamps (sort timestamps #'>))

      (let* ((sample-size (cond
                           ((>= (length timestamps) 15) 15)
                           ((>= (length timestamps) 10) 10)
                           ((>= (length timestamps) 5) 5)
                           (t (length timestamps))))
             (sample (seq-take timestamps sample-size))
             (span-seconds (when (> (length sample) 1)
                             (- (car sample) (car (last sample)))))
             (span-days (when span-seconds (/ span-seconds 86400.0)))
             (calculated-days (blame-reveal--auto-calculate-days-limit))
             (commits-in-range
              (if calculated-days
                  (length (cl-remove-if
                           (lambda (ts)
                             (> (- reference-time ts) (* calculated-days 86400)))
                           timestamps))
                0))
             (gradient-quality
              (when (> commits-in-range 0)
                (blame-reveal--calculate-gradient-quality commits-in-range)))
             (quality-desc
              (cond
               ((null gradient-quality) "N/A")
               ((>= gradient-quality 0.8) "Excellent")
               ((>= gradient-quality 0.5) "Good")
               ((>= gradient-quality 0.3) "Acceptable")
               (t "Poor")))
             (lightness-range
              (let ((scheme blame-reveal-color-scheme)
                    (is-dark (blame-reveal-color--is-dark-theme-p)))
                (if is-dark
                    (- (plist-get scheme :dark-newest)
                       (plist-get scheme :dark-oldest))
                  (- (plist-get scheme :light-oldest)
                     (plist-get scheme :light-newest)))))
             (per-commit-pct
              (if (> commits-in-range 1)
                  (* (/ lightness-range (1- commits-in-range)) 100)
                0)))

        (message
         (concat
          "Auto calculation for current file:\n"
          "- Mode: %s\n"
          "- Reference time: %s\n"
          "- Total commits loaded: %d\n"
          "- Sample size: %d commits\n"
          "- Sample time span: %.1f days\n"
          "- Calculated limit: %s days\n"
          "- Commits in range: %d\n"
          "- Gradient quality: %s (%.2f%% per commit)\n"
          "- Quality mode: %s")
         (if is-recursive
             (format "Recursive (@%s)" blame-reveal--revision-display)
           "Normal (HEAD)")
         (if is-recursive
             (format-time-string "%Y-%m-%d" reference-time)
           "now")
         (length timestamps)
         sample-size
         (or span-days 0)
         (if calculated-days (format "%d" calculated-days) "N/A")
         commits-in-range
         quality-desc
         per-commit-pct
         blame-reveal-gradient-quality)))))

(defun blame-reveal--cleanup-async-processes ()
  "Cleanup all async processes and buffers."
  (blame-reveal--cleanup-async-state 'blame-reveal--expansion-process
                                     'blame-reveal--expansion-buffer)
  (blame-reveal--cleanup-async-state 'blame-reveal--initial-load-process
                                     'blame-reveal--initial-load-buffer)
  (setq blame-reveal--pending-expansion nil)
  (setq blame-reveal--is-expanding nil))

;;;###autoload
(defun blame-reveal-clear-auto-cache ()
  "Clear auto-calculation cache and force recalculation.

Use this if you want to see the effect of changing
`blame-reveal-gradient-quality' setting, or if you suspect
the cached value is stale.

The cache will be automatically rebuilt on next update."
  (interactive)
  (setq blame-reveal--auto-days-cache nil)
  (when blame-reveal-mode
    (blame-reveal--update-recent-commits)
    (blame-reveal--render-visible-region)
    (message "Auto-calculation cache cleared and recalculated")))

;;;###autoload
(defun blame-reveal-copy-commit-hash ()
  "Copy the commit hash of the current line to kill ring."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit (car current-block)))
      (progn
        (kill-new commit)
        (message "Copied commit hash: %s" (substring commit 0 8)))
    (message "No git blame info at current line")))

;;;###autoload
(defun blame-reveal-show-commit-diff ()
  "Show the diff of the commit at current line.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block)))
      (if (blame-reveal--should-use-magit-p)
          (progn
            (magit-show-commit commit-hash)
            (with-current-buffer (magit-get-mode-buffer 'magit-revision-mode)
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "q") #'quit-window)))
        (let ((buffer-name (format "*Commit Diff: %s*" (substring commit-hash 0 8))))
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (call-process "git" nil t nil "show" "--color=never" commit-hash)
              (goto-char (point-min))
              (diff-mode)
              (view-mode 1)
              (setq-local revert-buffer-function
                          (lambda (&rest _)
                            (blame-reveal--revert-commit-diff commit-hash))))
            (pop-to-buffer (current-buffer)))))
    (message "No commit info at current line")))

;;;###autoload
(defun blame-reveal-show-commit-details ()
  "Show full commit details including description in a separate buffer."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block))
            (info (gethash commit-hash blame-reveal--commit-info)))
      (let* ((short-hash (nth 0 info))
             (author (nth 1 info))
             (date (nth 2 info))
             (summary (nth 3 info))
             (description (nth 5 info))
             (desc-trimmed (if description (string-trim description) ""))
             (buffer-name "*Commit Details*")
             (buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)

            (insert (propertize (format "%s\n" summary)
                                'face '(:height 1.1 :weight bold)))

            (insert (propertize short-hash 'face '(:foreground "#6c9ef8" :weight bold)))
            (insert (propertize " • " 'face '(:foreground "#666")))
            (insert (propertize author 'face '(:foreground "#a0a0a0")))
            (insert (propertize " • " 'face '(:foreground "#666")))
            (insert (propertize date 'face '(:foreground "#a0a0a0")))
            (insert "\n")

            (insert (propertize (make-string 60 ?─) 'face '(:foreground "#444")))
            (insert "\n\n")

            (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                (let ((desc-lines (split-string desc-trimmed "\n")))
                  (dolist (line desc-lines)
                    (insert (propertize (format "%s\n" line)
                                        'face '(:foreground "#d0d0d0")))))
              (insert (propertize "(no description)"
                                  'face '(:foreground "#666" :slant italic))))

            (insert "\n")
            (goto-char (point-min)))
          (special-mode)
          (local-set-key (kbd "q") 'quit-window))
        (pop-to-buffer buffer))
    (message "No commit info at current line")))

;;;###autoload
(defun blame-reveal-show-file-history ()
  "Show the git log history of current file.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (if (blame-reveal--should-use-magit-p)
              (magit-log-buffer-file)
            (let ((buffer-name (format "*Git Log: %s*" (file-name-nondirectory file))))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (call-process "git" nil t nil "log"
                                "--color=always"
                                "--follow"
                                "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                                "--" file)
                  (goto-char (point-min))
                  (require 'ansi-color)
                  (ansi-color-apply-on-region (point-min) (point-max))
                  (special-mode)
                  (setq-local revert-buffer-function
                              (lambda (&rest _)
                                (blame-reveal--revert-file-log file))))
                (pop-to-buffer (current-buffer)))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))

;;;###autoload
(defun blame-reveal-show-line-history ()
  "Show the git log history of current line.
This function always uses built-in git."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (let* ((line-num (line-number-at-pos))
                 (buffer-name (format "*Git Log: %s:%d*"
                                      (file-name-nondirectory file)
                                      line-num)))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (call-process "git" nil t nil "log"
                              "--color=always"
                              "-L" (format "%d,%d:%s" line-num line-num file))
                (goto-char (point-min))
                (require 'ansi-color)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode)
                (setq-local revert-buffer-function
                            (lambda (&rest _)
                              (blame-reveal--revert-line-log line-num file))))
              (pop-to-buffer (current-buffer))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))


;;;; Minor Mode Definition

;;;###autoload
(define-minor-mode blame-reveal-mode
  "Toggle git blame fringe display."
  :lighter " BlameReveal"
  :group 'blame-reveal
  (if blame-reveal-mode
      (progn
        (let ((file (buffer-file-name)))
          (if (not (and file (vc-git-registered file)))
              (progn
                (message "Cannot enable blame-reveal-mode: not a git-tracked file")
                (setq blame-reveal-mode nil))

            (setq blame-reveal--emulation-alist
                  `((blame-reveal-mode . ,blame-reveal-mode-map)))
            (add-to-list 'emulation-mode-map-alists
                         'blame-reveal--emulation-alist)

            ;; Initialize unified overlay management
            (blame-reveal--init-overlay-registry)
            (blame-reveal--init-color-strategy)

            ;; Initialize state
            (setq blame-reveal--auto-days-cache nil)
            (setq blame-reveal--blame-data-range nil)
            (setq blame-reveal--all-commits-loaded nil)

            ;; Load blame data
            (blame-reveal--load-blame-data)

            (add-hook 'after-save-hook #'blame-reveal--full-update nil t)
            (add-hook 'window-scroll-functions #'blame-reveal--scroll-handler nil t)
            (add-hook 'post-command-hook #'blame-reveal--update-header nil t)
            (add-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region nil t)

            (blame-reveal--setup-mode-line)
            (setq blame-reveal--blame-stack nil)
            (setq blame-reveal--current-revision nil)
            (setq blame-reveal--revision-display nil)

            (blame-reveal--setup-theme-advice))))

    ;; Cleanup when disabling mode
    (setq emulation-mode-map-alists
          (delq 'blame-reveal--emulation-alist
                emulation-mode-map-alists))

    (blame-reveal--stop-loading-animation)

    (when blame-reveal--temp-overlay-timer
      (cancel-timer blame-reveal--temp-overlay-timer)
      (setq blame-reveal--temp-overlay-timer nil))

    (when blame-reveal--header-update-timer
      (cancel-timer blame-reveal--header-update-timer)
      (setq blame-reveal--header-update-timer nil))

    ;; Use unified cleanup
    (blame-reveal--clear-all-overlays)

    ;; Clear legacy header overlay (will be migrated in next step)
    (when blame-reveal--header-overlay
      (delete-overlay blame-reveal--header-overlay)
      (setq blame-reveal--header-overlay nil))

    (blame-reveal--clear-sticky-header)

    (remove-hook 'after-save-hook #'blame-reveal--full-update t)
    (remove-hook 'window-scroll-functions #'blame-reveal--scroll-handler t)
    (remove-hook 'post-command-hook #'blame-reveal--update-header t)
    (remove-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region t)

    (when blame-reveal--scroll-timer
      (cancel-timer blame-reveal--scroll-timer)
      (setq blame-reveal--scroll-timer nil))

    (blame-reveal--cleanup-async-processes)

    (setq blame-reveal--loading nil)
    (setq blame-reveal--auto-days-cache nil)
    (setq blame-reveal--blame-data-range nil)
    (setq blame-reveal--all-commits-loaded nil)

    (unless (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (and (not (eq buf (current-buffer)))
                              blame-reveal-mode)))
                     (buffer-list))

      (blame-reveal--cleanup-mode-line)
      (setq blame-reveal--blame-stack nil)
      (setq blame-reveal--current-revision nil)
      (setq blame-reveal--revision-display nil)

      (blame-reveal--remove-theme-advice))))

(provide 'blame-reveal)
;;; blame-reveal.el ends here
