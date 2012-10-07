;; replace-region-by-ruby.el

(defvar rrbruby:ruby-command "ruby"
  "command or path to Ruby interpreter")

(defvar rrbruby:region-variable "R"
  "variable name which region assigned as an instance of String in Ruby")

(defvar rrbruby:history nil)

(defun rrbruby:read-string ()
  (read-string "ruby: "
               (or (nth 0 rrbruby:history) (format "print %s." rrbruby:region-variable))
               'rrbruby:history))

(defun rrbruby:escape-for-rubystring (string)
  (let ((replace-pat '(("\\\\" . "\\\\\\\\") ("'" . "\\\\'")))
        (buf-string string))
    (dolist (pat replace-pat buf-string)
      (setq buf-string (replace-regexp-in-string (car pat) (cdr pat) buf-string t)))))

(defun rrbruby:magick-comment-encoding ()
  "thanks to http://d.hatena.ne.jp/rubikitch/20080307/magiccomment"
  (let* ((coding-system (symbol-name buffer-file-coding-system))
         (encoding (cond ((string-match "japanese-iso-8bit\\|euc-j" coding-system)
                          "euc-jp")
                         ((string-match "shift.jis\\|sjis\\|cp932" coding-system)
                          "shift_jis")
                         ((string-match "utf-8" coding-system)
                          "utf-8"))))
    (format "# -*- coding: %s -*-" encoding)))

(defun rrbruby:write-script (region expr &optional out)
  (or out (setq out (rrbruby:create-tempfile "rrbruby")))
  (write-region (format "%s\n%s='%s';\n%s"
                        (rrbruby:magick-comment-encoding)
                        rrbruby:region-variable
                        (rrbruby:escape-for-rubystring region) expr) nil out)
  out)

(defun rrbruby:create-tempfile (prefix)
  (make-temp-name (expand-file-name prefix temporary-file-directory)))

(defun rrbruby:exec-script (script onsuccess onerror)
  (let ((coding-system-for-read buffer-file-coding-system) ;; for external process output
        (stderr (rrbruby:create-tempfile "rrberr"))
        (status) (output) (errmsg))
    (with-temp-buffer
      (setq status (call-process rrbruby:ruby-command nil (list t stderr) nil script))
      (setq output (buffer-string)))
    (if (eq status 0)
        (funcall onsuccess output)
      (with-current-buffer (find-file-noselect stderr)
        (setq errmsg (buffer-string))
        (kill-buffer))
      (funcall onerror errmsg))
    (delete-file stderr)))

(defun replace-region-by-ruby (start end expr)
  (interactive (list (region-beginning) (region-end) (rrbruby:read-string)))
  (unless (executable-find rrbruby:ruby-command) (error "ruby command not found"))
  (let* ((region (buffer-substring start end))
         (script (rrbruby:write-script region expr)))
    (rrbruby:exec-script
     script
     (lambda (out) (delete-region start end) (insert out) (message "rrbruby done!"))
     (lambda (err) (message (substring err (string-match ":" err)))))
    (delete-file script)))

(defalias 'rrbruby (symbol-function 'replace-region-by-ruby))

(provide 'replace-region-by-ruby)
