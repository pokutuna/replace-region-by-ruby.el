;; replace-region-by-ruby.el

(defvar rrbruby:ruby-command "ruby"
  "command or path to Ruby interpreter")

(defvar rrbruby:region-variable "R"
  "variable name which region assigned as an instance of String in Ruby")

(defvar rrbruby:history nil)

(defun rrbruby:read-string ()
  (read-string "ruby: "
               (or (nth 0 rrbruby:history)
                   (format "print %s." rrbruby:region-variable))
               'rrbruby:history))

(defun rrbruby:escape-for-rubystring (string)
  (let ((replace-pat '(("\\\\" . "\\\\\\\\") ("'" . "\\\\'")))
        (buf-string string))
    (dolist (pat replace-pat buf-string)
      (setq buf-string (replace-regexp-in-string (car pat) (cdr pat) buf-string t)))))

(defun replace-region-by-ruby (start end expr)
  (interactive (list (region-beginning) (region-end) (rrbruby:read-string)))
  (unless (executable-find rrbruby:ruby-command) (error "ruby command not found"))
  (let ((script (make-temp-name (expand-file-name "rrruby" temporary-file-directory)))
        (stderr (get-buffer-create " *rrbruby:ruby-error*"))
        (region (buffer-substring start end))
        (status))
    (write-region (format "%s='%s'; %s" rrbruby:region-variable
                          (rrbruby:escape-for-rubystring region) expr) nil script)
    (with-current-buffer stderr (erase-buffer))
    (setq status (shell-command-on-region
                  start end (format "%s %s" rrbruby:ruby-command script) nil t stderr nil))
    (delete-file script)
    (if (eq status 0)
        (message "rrbruby done!")
      (insert region)
      (with-current-buffer stderr
        (goto-char (point-min))
        (message (buffer-substring (search-forward ":") (point-max))))
      )
    ))

(defalias 'rrbruby (symbol-function 'replace-region-by-ruby))

(provide 'replace-region-by-ruby)
