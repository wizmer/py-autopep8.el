;;; autopep8-mode.el --- Use autopep8 to beautify a Python buffer

;; Copyright (C) 2013-2015, Friedrich Paetzke <f.paetzke@gmail.com>

;; Author: Benoit Coste <benoit.coste@protonmail.com>
;; URL: https://github.com/wizmer/autopep8-mode
;; Version: 2018.1

;;; Commentary:

;; Provides the `autopep8' command, which uses the external "autopep8"
;; tool to tidy up the current buffer according to Python's PEP8.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'autopep8-enable-on-save)

;; To customize the behaviour of "autopep8" you can set the
;; autopep8-options e.g.

;;   (setq autopep8-options '("--max-line-length=100"))

;;; Code:

(define-minor-mode autopep8-mode
  "Automatic python code formating with autopep8"
  :group 'autopep8
  (if autopep8-mode
		(add-hook 'before-save-hook 'autopep8-buffer nil 'buffer-local)
		(remove-hook 'before-save-hook 'autopep8-buffer 'buffer-local)))


(defcustom autopep8-options nil
  "Options used for autopep8.

Note that `--in-place' is used by default."
  :group 'autopep8
  :type '(repeat (string :tag "option")))


(defun autopep8--call-executable (errbuf file)
  (zerop (apply 'call-process "autopep8" nil errbuf nil
                (append autopep8-options `("--in-place", file)))))


;;;###autoload
(defun autopep8-buffer ()
  "Uses the \"autopep8\" tool to reformat the current buffer."
  (interactive)
  (autopep8-bf--apply-executable-to-buffer "autopep8"
                                              'autopep8--call-executable
                                              nil
                                              "py"))



;; BEGIN GENERATED -----------------
;; !!! This file is generated !!!
;; buftra.el
;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>
;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/buftra.el
;; Version: 0.5

;; This code is initially copied from go-mode.el (copyright the go-mode authors).
;; See LICENSE or https://raw.githubusercontent.com/dominikh/go-mode.el/master/LICENSE


(defun autopep8-bf--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in autopep8-bf--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)
                (pop kill-ring)))
             (t
              (error "invalid rcs patch or internal error in autopep8-bf--apply-rcs-patch")))))))))


(defun autopep8-bf--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun autopep8-bf--apply-executable-to-buffer (executable-name
                                           executable-call
                                           only-on-region
                                           file-extension)
  "Formats the current buffer according to the executable"
	(message (format "Only on region: %s" file-extension))
  (when (not (executable-find executable-name))
    (error (format "%s command not found." executable-name)))
  (let ((tmpfile (make-temp-file executable-name nil (concat "." file-extension)))
        (patchbuf (get-buffer-create (format "*%s patch*" executable-name)))
        (errbuf (get-buffer-create (format "*%s Errors*" executable-name)))
        (coding-system-for-read buffer-file-coding-system)
        (coding-system-for-write buffer-file-coding-system))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (funcall executable-call errbuf tmpfile)
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil
                                        patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message (format "Buffer is already %sed" executable-name)))

          (if only-on-region
              (autopep8-bf--replace-region tmpfile)
            (autopep8-bf--apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
                     executable-name executable-name)))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


;; autopep8-bf.el ends here
;; END GENERATED -------------------


(provide 'autopep8-mode)


;;; autopep8-mode.el ends here
