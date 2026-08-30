;; --------------------------------------------------------------------------------
;; screenshot region 

(defun screenshot-region--body-edges ()
  "Return absolute pixel edges of the Emacs text area: (LEFT TOP RIGHT BOTTOM)."
  (cond
   ;; Modern Emacs (27+): window-body-edges with pixelwise + absolute args
   ((and (fboundp 'window-body-edges)
         (condition-case nil
             (progn (window-body-edges nil t t) t)
           (wrong-number-of-arguments nil)))
    (window-body-edges nil t t))

   ;; Emacs 24.4–26: window-inside-absolute-pixel-edges (no extra args)
   ((fboundp 'window-inside-absolute-pixel-edges)
    (window-inside-absolute-pixel-edges nil))

   ;; Fallback: manual computation from frame position + inside pixel edges
   ((fboundp 'window-inside-pixel-edges)
    (let ((inside (window-inside-pixel-edges nil))
          (fp (frame-position)))
      (list (+ (car fp) (nth 0 inside))
            (+ (cdr fp) (nth 1 inside))
            (+ (car fp) (nth 2 inside))
            (+ (cdr fp) (nth 3 inside)))))

   (t
    (error "Your Emacs lacks required window pixel-edge functions"))))

(defun screenshot-region--copy-rect (x y w h)
  "Capture screen rectangle X, Y, W, H to the Windows clipboard via PowerShell."
  (let* ((ps-cmd
          (format
           (concat
            "Add-Type -AssemblyName System.Windows.Forms; "
            "Add-Type -AssemblyName System.Drawing; "
            "$bmp = New-Object System.Drawing.Bitmap(%d, %d); "
            "$g = [System.Drawing.Graphics]::FromImage($bmp); "
            "$g.CopyFromScreen(%d, %d, 0, 0, $bmp.Size); "
            "[System.Windows.Forms.Clipboard]::SetImage($bmp); "
            "$g.Dispose(); $bmp.Dispose()")
           w h x y))
         (status
          (call-process "powershell.exe" nil nil nil
                        "-NoProfile"
                        "-NonInteractive"
                        "-WindowStyle" "Hidden"
                        "-Command" ps-cmd)))
    (if (eq status 0)
        (message "Region screenshot (%dx%d) copied to clipboard." w h)
      (message "PowerShell screenshot failed with status %s." status))))

(defun screenshot-region-to-clipboard (&optional tight-p)
  "Capture the active region to the Windows clipboard.

By default, mimic the visual region: if the selection crosses lines,
extend the captured rectangle to the text-area border where Emacs paints
the region.

With a prefix argument, e.g. `C-u M-x screenshot-region-to-clipboard',
capture only the tight bounding box of the selected characters."
  (interactive "P")

  (unless (region-active-p)
    (user-error "No active region"))

  (let ((start (region-beginning))
        (end (region-end)))
    (when (= start end)
      (user-error "Region is empty"))

    (let* ((edges (screenshot-region--body-edges))
           (body-left   (nth 0 edges))
           (body-top    (nth 1 edges))
           (body-right  (nth 2 edges))
           (body-bottom (nth 3 edges))
           (body-width  (- body-right body-left))

           (min-x most-positive-fixnum)
           (max-x most-negative-fixnum)
           (min-y most-positive-fixnum)
           (max-y most-negative-fixnum))

      ;; Compute bounding box from displayed positions, line by line.
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let* ((lb (line-beginning-position))
                 (le (line-end-position))
                 (sb (max start lb))
                 (se (min end le))
                 (p1 (posn-at-point sb))
                 (p2 (posn-at-point se)))
            (unless (and p1 p2)
              (user-error "Region is not fully visible on screen"))

            (let ((xy1 (posn-x-y p1))
                  (xy2 (posn-x-y p2)))
              (unless (and xy1 xy2)
                (user-error "Could not get pixel coordinates for region"))

              (setq min-x (min min-x (car xy1) (car xy2))
                    max-x (max max-x (car xy1) (car xy2))
                    min-y (min min-y (cdr xy1) (cdr xy2))
                    max-y (max max-y (cdr xy1) (cdr xy2)))))

          (let ((before (point)))
            (forward-line 1)
            (when (= (point) before)
              (goto-char end)))))

      (when (= max-x most-negative-fixnum)
        (user-error "Could not compute region bounds"))

      ;; If matching Emacs' visual highlight, extend to body edges where
      ;; the region face normally extends.
      (unless tight-p
        ;; If the selection includes a newline, the start line's highlight
        ;; usually extends to the right edge.
        (when (save-excursion
                (goto-char start)
                (< (line-end-position) end))
          (setq max-x body-width))

        ;; If the selection includes the beginning of a line, extend left.
        (when (or (save-excursion (goto-char start) (bolp))
                  (save-excursion
                    (goto-char start)
                    (< (save-excursion (forward-line 1) (point)) end)))
          (setq min-x 0)))

      (let* ((x1 (max body-left (+ body-left min-x)))
             (y1 (max body-top (+ body-top min-y)))
             (x2 (min body-right (+ body-left max-x)))
             (y2 (min body-bottom (+ body-top max-y (frame-char-height))))
             (w (- x2 x1))
             (h (- y2 y1)))

        (when (or (<= w 0) (<= h 0))
          (user-error "No visible region to capture"))

        ;; Important: deactivate, force redisplay, then capture after a
        ;; short timer so the screen no longer shows the active region.
        (deactivate-mark)
        (redisplay t)

        (run-with-timer 0 nil
                        #'screenshot-region--copy-rect
                        x1 y1 w h)

        ))))

;; --------------------------------------------------------------------------------
;; dired file to clipboard

(defun dired-copy-file-to-clipboard ()
  "Copy the file at point in Dired to the Windows clipboard.
This copies the actual file object, allowing you to paste it (Ctrl+V)
into Windows Explorer or other file managers.
Uses PowerShell to interact with the Windows Clipboard API."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (abs-file (expand-file-name file))
         (ps-script (make-temp-file "emacs-clip-" nil ".ps1"))
         ;; Escape single quotes for PowerShell string safety
         (ps-code (format "Add-Type -AssemblyName System.Windows.Forms
$f = New-Object System.Collections.Specialized.StringCollection
$f.Add('%s')
[System.Windows.Forms.Clipboard]::SetFileDropList($f)"
                          (replace-regexp-in-string "'" "''" abs-file))))
    
    ;; Write the PowerShell script to a temporary file
    (with-temp-file ps-script
      (insert ps-code))
    
    (unwind-protect
        (progn
          ;; Execute the PowerShell script silently
          (call-process "powershell.exe" nil nil nil
                        "-NoProfile" "-ExecutionPolicy" "Bypass" "-File" ps-script)
          (message "Copied '%s' to clipboard" (file-name-nondirectory abs-file)))
      ;; Ensure the temporary script is deleted even if an error occurs
      (when (file-exists-p ps-script)
        (delete-file ps-script)))))
