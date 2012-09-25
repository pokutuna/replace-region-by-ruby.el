;; replace-region-by-ruby.el

(defvar rrruby:ruby-command "ruby"
  "command or path to Ruby interpreter")

(defvar rrruby:region-variable "R"
  "variable name which region assigned as an instance of String in Ruby")

(defvar rrruby:history nil)

(defun rrruby:read-string ()
  (read-string "ruby: "
               (or (nth 0 rrruby:history)
                   (format "print %s." rrruby:region-variable))
               'rrruby:history))

(defun rrruby:escape-for-rubystring (string)
  (let ((replace-pat '(("\\\\" . "\\\\\\\\") ("'" . "\\\\'")))
        (buf-string string))
    (dolist (pat replace-pat buf-string)
      (setq buf-string (replace-regexp-in-string (car pat) (cdr pat) buf-string t)))))

(defun replace-region-by-ruby (start end expr)
  (interactive (list (region-beginning) (region-end) (rrruby:read-string)))
  (unless (executable-find rrruby:ruby-command)
    (error "ruby command not found"))
  (let* ((tempfile (make-temp-name (expand-file-name "rrruby" temporary-file-directory)))
         (temperr (make-temp-name (expand-file-name "rrruby" temporary-file-directory)))
         (region (buffer-substring start end))
         (status))
    (write-region (format "%s='%s'; %s" rrruby:region-variable
                          (rrruby:escape-for-rubystring region) expr)
                  nil tempfile)
    (setq status (call-process-region start end "ruby" t (list t temperr) nil tempfile))
    (cond ((not (eq status 0))
           (insert region)
           (message "error")
           ))
    ))

(provide 'replace-region-by-ruby)
