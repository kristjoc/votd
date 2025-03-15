;;; votd.el --- Verse Of The Day in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; TODO

;;; Code:

(require 'url)
(require 'json)

(defgroup votd nil
  "Package that fetches the Bible verse of the day from the BibleGateway"
  :group 'external)

(defcustom votd-bible-version "KJV"
  "The version of the Bible to get from"
  :type 'string
  :group 'votd)

(defun split-with-spaces (str)
  "Split STR preserving original spacing between words."
  (let ((parts nil)
        (start 0)
        (len (length str)))
    (while (< start len)
      (if (string-match "\$$[^ ]+\$$\$$ *\$$" str start)
          (progn
            (push (match-string 1 str) parts)
            (push (match-string 2 str) parts)
            (setq start (match-end 0)))
        (setq start len)))
    (nreverse (if (string-match-p " $" str)
                  parts
                (butlast parts)))))

(defun justify-line (line width)
  "Justify LINE to WIDTH characters."
  (let* ((words (split-string line))
         (word-count (length words)))
    (if (<= word-count 1)
        line  ; Return single words unchanged
      (let* ((total-word-length (apply #'+ (mapcar #'length words)))
             (spaces-needed (- width total-word-length))
             (gaps (1- word-count))
             (base-spaces-per-gap (/ spaces-needed gaps))
             (extra-spaces (% spaces-needed gaps))
             (result ""))
        ;; Distribute spaces as evenly as possible
        (dotimes (i (1- word-count))
          (setq result
                (concat result
                        (nth i words)
                        (make-string
                         (if (< i extra-spaces)
                             (1+ base-spaces-per-gap)
                           base-spaces-per-gap)
                         ?\s))))
        (concat result (car (last words)))))))

(defun format-verse-text (text &optional width)
  "Format verse TEXT as a justified paragraph with optional WIDTH."
  (when text  ; Only process if text is not nil
    (with-temp-buffer
      (let* ((fill-column (or width 70))
             (lines '()))
        ;; First fill the paragraph normally
        (insert text)
        (fill-region (point-min) (point-max))
        ;; Collect lines
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                   (line-end-position)))))
            (unless (string-empty-p line)
              (push line lines)))
          (forward-line 1))
        ;; Justify each line except the last one
        (setq lines (nreverse lines))
        (let* ((justified-lines
                (append
                 (mapcar (lambda (line)
                          (justify-line line fill-column))
                        (butlast lines))
                 (last lines)))
               (result (string-join justified-lines "\n")))
          result)))))

(defun decode-html-entities (text)
  "Decode HTML entities in TEXT."
  (when text
    (let ((decoded text))
      (setq decoded (replace-regexp-in-string "&ldquo;" "\"" decoded))
      (setq decoded (replace-regexp-in-string "&rdquo;" "\"" decoded))
      decoded)))

(defun fetch-daily-bible-verse ()
  "Fetch the daily Bible verse from BibleGateway API."
  (let ((url-request-method "GET")
        (url (concat "https://www.biblegateway.com/votd/get/?format=json&version=" votd-bible-version )))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char url-http-end-of-headers)
      (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
             (json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (json-data (json-read-from-string json-string))
             (votd (gethash "votd" json-data))
             (raw-text (gethash "text" votd))
             (verse-text (decode-html-entities raw-text))
             (clean-verse (replace-regexp-in-string "[\"]" "" verse-text))
             (formatted-verse (format-verse-text clean-verse))
             (verse-reference (gethash "display_ref" votd))
             (fill-width 70))
        (format "%s\n%s" 
                formatted-verse 
                (let ((ref-text verse-reference))
                  (concat (make-string (- fill-width (length ref-text)) ?\s)
                          ref-text)))))))

(defun get-votd ()
  "Get the daily verse for the dashboard footer with error handling."
  (condition-case err
      (fetch-daily-bible-verse)
    (error
     (format "Today's verse could not be fetched: %s" (error-message-string err)))))

(provide 'votd)
;;; votd.el ends here
