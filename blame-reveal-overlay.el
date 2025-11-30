;;; blame-reveal-overlay.el --- Unified overlay management -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified overlay management system with registry and indexing.
;; Handles fringe, header, sticky-header, temp-fringe, and loading overlays.

;;; Code:

(require 'blame-reveal-core)

;;; Registry Initialization

(defun blame-reveal--init-overlay-registry ()
  "Initialize overlay registry and indices."
  (setq blame-reveal--overlay-registry (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-type (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-commit (make-hash-table :test 'equal))
  (setq blame-reveal--overlays-by-line (make-hash-table :test 'eql)))

;;; Core Overlay Operations

(defun blame-reveal--register-overlay (overlay type &optional metadata)
  "Register OVERLAY of TYPE with optional METADATA.
METADATA is a plist that may contain:
  :commit - commit hash
  :line - line number
  :data - type-specific data
Returns the overlay."
  (unless blame-reveal--overlay-registry
    (blame-reveal--init-overlay-registry))
  (let ((full-metadata (plist-put (copy-sequence metadata) :type type)))
    (puthash overlay full-metadata blame-reveal--overlay-registry)
    (let ((type-list (gethash type blame-reveal--overlays-by-type)))
      (puthash type (cons overlay type-list) blame-reveal--overlays-by-type))
    (when-let ((commit (plist-get metadata :commit)))
      (let ((commit-list (gethash commit blame-reveal--overlays-by-commit)))
        (puthash commit (cons overlay commit-list) blame-reveal--overlays-by-commit)))
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
      (remhash overlay blame-reveal--overlay-registry)))
  (ignore-errors (delete-overlay overlay)))

;;; Query Functions

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

;;; Batch Operations

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

;;; High-Level Overlay Creation

(defun blame-reveal--create-managed-overlay (start end type &optional metadata)
  "Create and register an overlay from START to END of TYPE with METADATA.
Returns the created and registered overlay."
  (let ((overlay (make-overlay start end)))
    (blame-reveal--register-overlay overlay type metadata)
    overlay))

;;; Reuse and Update

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
      (when (and old-commit (not (equal old-commit new-commit)))
        (puthash old-commit 
                 (delq overlay (gethash old-commit blame-reveal--overlays-by-commit))
                 blame-reveal--overlays-by-commit)
        (when new-commit
          (let ((commit-list (gethash new-commit blame-reveal--overlays-by-commit)))
            (puthash new-commit (cons overlay commit-list)
                     blame-reveal--overlays-by-commit))))
      (when (and old-line (not (eql old-line new-line)))
        (puthash old-line
                 (delq overlay (gethash old-line blame-reveal--overlays-by-line))
                 blame-reveal--overlays-by-line)
        (when new-line
          (let ((line-list (gethash new-line blame-reveal--overlays-by-line)))
            (puthash new-line (cons overlay line-list)
                     blame-reveal--overlays-by-line))))
      (puthash overlay new-metadata blame-reveal--overlay-registry))))

;;; Fringe Overlay Management

(defconst blame-reveal--high-contrast-colors
  '(
    ;; "#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FFEAA7" 
    ;; "#DDA0DD" "#98D8C8" "#F7DC6F" "#BB8FCE" "#85C1E9"
    ;; "#F8C471" "#82E0AA" "#F1948A" "#85C1E9" "#D7BDE2"
    ;; "#F9E79F" "#ABEBC6" "#AED6F1" "#FAD7A0" "#E8DAEF"

    ;; ;; 红色 (8)
    ;; "#FF5252" "#FF1744" "#D50000" "#FF8A80" "#FF6D6D" "#FF4081" "#F50057" "#C51162"
    ;; ;; 橙红 (8)  
    ;; "#FF3D00" "#DD2C00" "#FF6E40" "#FF9E80" "#FF7043" "#FF5722" "#E64A19" "#D84315"
    ;; ;; 橙色 (8)
    ;; "#FF9100" "#FF6D00" "#FF9800" "#FFA726" "#FFB74D" "#FFCC80" "#FFAB40" "#FFD180"
    ;; ;; 黄色 (8)
    ;; "#FFD600" "#FFC400" "#FFAB00" "#FF8F00" "#FFE082" "#FFEE58" "#FFF176" "#FFF59D"
    ;; ;; 黄绿 (8)
    ;; "#C6FF00" "#AEEA00" "#64DD17" "#00C853" "#76FF03" "#B2FF59" "#CCFF90" "#F4FF81"
    ;; ;; 绿色 (8)
    ;; "#00E676" "#00C853" "#00BFA5" "#00B8D4" "#4CAF50" "#66BB6A" "#81C784" "#AED581"
    ;; ;; 青蓝 (8)
    ;; "#00BFA5" "#00B8D4" "#00E5FF" "#00B0FF" "#26C6DA" "#4DD0E1" "#80DEEA" "#B2EBF2"
    ;; ;; 蓝色 (8)
    ;; "#2979FF" "#2962FF" "#304FFE" "#3D5AFE" "#2196F3" "#42A5F5" "#64B5F6" "#90CAF9"
    ;; ;; 紫蓝 (8)
    ;; "#6200EA" "#651FFF" "#7C4DFF" "#536DFE" "#5C6BC0" "#7986CB" "#9FA8DA" "#C5CAE9"
    ;; ;; 紫色 (8)
    ;; "#E040FB" "#D500F9" "#AA00FF" "#8E24AA" "#9C27B0" "#AB47BC" "#BA68C8" "#CE93D8"
    ;; ;; 粉紫 (8)
    ;; "#EC407A" "#F06292" "#F48FB1" "#F8BBD0" "#AD1457" "#C2185B" "#D81B60" "#E91E63"
    ;; ;; 棕灰 (8)
    ;; "#795548" "#8D6E63" "#A1887F" "#BCAAA4" "#546E7A" "#607D8B" "#78909C" "#90A4AE"

    ;; "按使用场景组织的深色背景高对比度颜色"
    ;; 错误/警告色 (8个) - 红色系
    "#FF5252" "#FF6B6B" "#FF8A80" "#FFABAB" "#F44336" "#E53935" "#D32F2F" "#C62828"
    ;; 成功/完成色 (8个) - 绿色系  
    "#69F0AE" "#00E676" "#00C853" "#64DD17" "#4CAF50" "#43A047" "#388E3C" "#2E7D32"
    ;; 信息/提示色 (8个) - 蓝色系
    "#448AFF" "#2979FF" "#2962FF" "#304FFE" "#2196F3" "#1E88E5" "#1976D2" "#1565C0"
    ;; 高亮/重点色 (8个) - 黄色系
    "#FFD600" "#FFC400" "#FFAB00" "#FF8F00" "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825"
    ;; 次要信息色 (8个) - 青色系
    "#00E5FF" "#00B8D4" "#00BFA5" "#00BFA5" "#00BCD4" "#00ACC1" "#0097A7" "#00838F"
    ;; 特殊状态色 (8个) - 紫色系
    "#E040FB" "#D500F9" "#AA00FF" "#7C4DFF" "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A"
    ;; 中性标识色 (8个) - 灰色系
    "#FFFFFF" "#EEEEEE" "#BDBDBD" "#9E9E9E" "#90A4AE" "#78909C" "#607D8B" "#546E7A"
    ;; 装饰/边框色 (8个) - 各种鲜艳色
    "#FF80AB" "#FFD740" "#76FF03" "#18FFFF" "#651FFF" "#FF6E40" "#00E676" "#FF4081"
    )
  "预定义的高对比度颜色列表")

(defun blame-reveal--random-from-contrast-pool ()
  "从高对比度颜色池中随机选择"
  (nth (random (length blame-reveal--high-contrast-colors)) blame-reveal--high-contrast-colors))

(defun blame-reveal--ensure-margin-face (color)
  "Ensure left margin face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-margin-face-%s" color)))
        (backcolor (blame-reveal--random-from-contrast-pool)))
    (unless (facep face-name)
      (custom-declare-face face-name
                           ;; commit message不显示背景色
                           ;; `((t :inherit font-lock-comment-face :height ,blame-reveal-margin-height :overline t))
                           `((t :foreground ,backcolor :inherit font-lock-comment-face :height ,blame-reveal-margin-height))
                           ;; commit message显示背景色。注意：在背景透明度低于100%时，在设置相同的背景色时，commit message和fringe中的bitmap颜色会不一致
                           ;; `((t :background ,color :inherit font-lock-comment-face :height ,blame-reveal-margin-height :overline t))
                           ;; `((t :foreground ,color :inherit font-lock-comment-face :height ,blame-reveal-margin-height :overline t))
                           (format "Face for git blame margin color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--ensure-fringe-face (color)
  "Ensure fringe face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-face-%s" color))))
    (unless (facep face-name)
      (custom-declare-face face-name
                           `((t :background ,color :foreground ,color))
                           ;; `((t :background ,color :inherit font-lock-comment-face :height ,blame-reveal-margin-height))
                           ;; `((t :background ,color))
                           (format "Face for git blame color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--format-time-string (time tz)
  "Parse time string by timestamp and timezone."
  (let* ((time-format blame-reveal-margin-time-format)
         (tz-in-second (and (string-search "%z" time-format)
                            (car (last (parse-time-string tz))))))
    (format-time-string time-format
                        (seconds-to-time time)
                        tz-in-second)))

(defun blame-reveal--update-margin-overlay (ov color commit-hash)
  "Update commit message on left margin."
  (let* ((commit-info (gethash commit-hash blame-reveal--commit-info))
         (fringe-face (blame-reveal--ensure-fringe-face color))
         (margin-face (blame-reveal--ensure-margin-face color))
         (author  (nth 1 commit-info))
         (date (blame-reveal--format-time-string (nth 4 commit-info) nil))
         (commit-msg (concat date " " (substring commit-hash 0 6) " " author)))
    (overlay-put
     ov 'before-string
     (concat
      (propertize "o" 'display
                  (list (list 'margin 'left-margin)
                        (propertize (concat commit-msg
                                            (make-string (max 0 (- blame-reveal--margin-width (length commit-msg))) ?\s))
                                    'face margin-face)))
      (propertize "o" 'display
                  (list blame-reveal-style
                        'blame-reveal-full
                        fringe-face))))
    ))

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             ;; (fringe-face (blame-reveal--ensure-fringe-face color))
             (overlay (or (blame-reveal--find-reusable-overlay 'fringe line-number)
                          (blame-reveal--create-managed-overlay 
                           pos pos 'fringe
                           (list :commit commit-hash :line line-number)))))

        ;; (overlay-put overlay 'before-string
        ;;              (propertize "!" 'display
        ;;                          (list blame-reveal-style
        ;;                                'blame-reveal-full
        ;;                                fringe-face)))
        (blame-reveal--update-margin-overlay overlay color commit-hash)
        (when (overlay-buffer overlay)
          (blame-reveal--update-overlay-metadata 
           overlay 
           (list :commit commit-hash :line line-number)))
        overlay))))

(defun blame-reveal--render-block-fringe (block-start block-length commit-hash color)
  "Render fringe for a specific block.
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

;;; Temp Overlay Management

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
  "Render temporary overlays for old commit HASH with color COL in buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (equal blame-reveal--current-block-commit hash)
        (blame-reveal--render-temp-overlays-for-commit hash col)))))

;;; Statistics and Debugging

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

(provide 'blame-reveal-overlay)
;;; blame-reveal-overlay.el ends here
