;; Add `eshell' as org src block option. Can run in existion `:session'
(defvar org-babel-default-header-args:eshell '())
(defun org-babel-execute:eshell (body _params)
  "Execute BODY through Eshell and return only the command output."
  (with-temp-buffer
    (eshell-mode)
    (goto-char (point-max))
    (insert body)
    (eshell-send-input)
    (while (get-buffer-process (current-buffer))
      (accept-process-output (get-buffer-process (current-buffer)) 0.01))
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(add-to-list 'org-babel-tangle-lang-exts '("eshell" . "eshell"))
(defalias 'org-babel-expand-body:eshell #'identity)
