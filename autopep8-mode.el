;;; autopep8-mode.el --- Use autopep8 to beautify a Python buffer

;; Copyright (C) 2018-2018, Benoit Coste <benoit.coste@protonmail.com>

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

(setq autopep8-config nil)

(define-minor-mode autopep8-mode
  "Automatic python code formating with autopep8"
  :group 'autopep8
  (if autopep8-mode
		(progn
			(add-hook 'before-save-hook 'autopep8-buffer nil 'buffer-local)
			(setq-local autopep8-config (autopep8--locate-any-config-file)))
		(remove-hook 'before-save-hook 'autopep8-buffer 'buffer-local)))


(defcustom autopep8-options nil
  "Options used for autopep8.

Note that `--in-place' is used by default."
  :group 'autopep8
  :type '(repeat (string :tag "option")))

(defcustom autopep8-command "autopep8"
	"The command used to invoke autopep8."
	:group 'autopep8
	:type 'string)

;;;###autoload
(defun autopep8-buffer ()
  "Uses the \"autopep8\" tool to reformat the current buffer."
  (interactive)
  (autopep8--apply-executable-to-buffer autopep8-command
    'autopep8--call-executable
    nil
    "py"))

;;;###autoload
(defun autopep8-region ()
	"Uses the \"autopep8\" tool to reformat the active region."
	(interactive)
	(autopep8--apply-executable-to-buffer autopep8-command
		'autopep8--call-executable
		t
		"py"))

(defun autopep8--locate-any-config-file ()
	"Try to locate a configuration FILENAME in the following ways
	1. By path
  2. In ancestors directory
  3. In the home folder"


	(or
	        (autopep8--locate-config-file-ancestor-directories ".flake8")
		(autopep8--locate-config-file-ancestor-directories ".pep8")
		(autopep8--locate-config-file-ancestor-directories "pep8")
		(autopep8--locate-config-file-ancestor-directories "setup.cfg")
		(autopep8--locate-config-file-ancestor-directories "tox.ini")
))

(defun autopep8--locate-config-file-home (filename)
  "Locate a configuration FILENAME in the home directory.

Return the absolute path, if FILENAME exists in the user's home
directory, or nil otherwise."
  (let ((path (expand-file-name filename "~")))
    (when (file-exists-p path)
      path)))


(defun autopep8--locate-config-file-ancestor-directories (filename)
  "Locate a configuration FILENAME in ancestor directories.

If the current buffer has a file name, search FILENAME in the
directory of the current buffer and all ancestors thereof (see
`locate-dominating-file').  If the file is found, return its
absolute path.  Otherwise return nil."
  (when-let* ((basefile (buffer-file-name))
								(directory (locate-dominating-file basefile filename)))
    (expand-file-name filename directory)))

(defun autopep8--locate-config-file-by-path (filepath)
  "Locate a configuration file by a FILEPATH.

If FILEPATH is a contains a path separator, expand it against the
default directory and return it if it points to an existing file.
Otherwise return nil."
  ;; If the path is just a plain file name, skip it.
  (unless (string= (file-name-nondirectory filepath) filepath)
    (let ((file-name (expand-file-name filepath)))
      (and (file-exists-p file-name) file-name))))

(defun autopep8--call-executable (errbuf file)
	(message (format "%s" autopep8-config))
  (zerop (apply 'call-process autopep8-command nil errbuf nil
					 (append autopep8-options `("--in-place", file)
						 (when autopep8-config `("--global-config", autopep8-config))))))


;; This code is initially copied from go-mode.el (copyright the go-mode authors).
;; See LICENSE or https://raw.githubusercontent.com/dominikh/go-mode.el/master/LICENSE


(defun autopep8--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
         (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in autopep8--apply-rcs-patch"))
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
								(error "invalid rcs patch or internal error in autopep8--apply-rcs-patch")))))))))


(defun autopep8--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun autopep8--apply-executable-to-buffer (executable-name
																							executable-call
																							only-on-region
																							file-extension)
  "Formats the current buffer according to the executable"
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
          (autopep8--replace-region tmpfile)
          (autopep8--apply-rcs-patch patchbuf))

        (kill-buffer errbuf)
        (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
               executable-name executable-name)))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))



(provide 'autopep8-mode)


;;; autopep8-mode.el ends here
