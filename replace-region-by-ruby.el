;; replace-region-by-ruby.el

(defvar rrruby:ruby-command "ruby"
  "command or path to Ruby interpreter")

(defvar rrruby:region-variable "R"
  "variable name which region assigned as an instance of String in Ruby")

(defvar rrruby:history nil)

(defun replace-region-by-ruby (start end expr)
  (interactive (list (region-beginning) (region-end)
                     (read-string "ruby: "
                                  (or (nth 0 rrruby:history)
                                      (format "puts %s." rrruby:region-variable))
                                  'rrruby:history)))
  (unless (executable-find rrruby:ruby-command)
    (error "ruby command not found"))
  (let* ((tempfile (make-temp-name (expand-file-name "rrruby" temporary-file-directory)))
         (region (buffer-substring start end))
         (eos (format "EOS%s" (sha1 tempfile))))
    (write-region (format "%s=<<%s\n%s%s\n%s" rrruby:region-variable eos
                          (if (string= (substring region -1) "\n")
                              region
                            (format "%s\n" region))
                          eos expr)
                  nil tempfile)
    (call-process-region start end "ruby" t t nil tempfile)
    ;; (delete-file tempfile)
    (message "done!")))

(provide 'replace-region-by-ruby)
