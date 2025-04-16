;;; votd.el --- Bible Verse Of The Day -*- lexical-binding: t -*-

;; Copyright (C) 2025 Kristjon Ciko

;; Author: Kristjon Ciko
;; Keywords: convenience comm hypermedia
;; Homepage: https://github.com/kristjoc/votd
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; votd is a simple package that fetches the Bible verse of the
;; day from https://www.biblegateway.com/ and formats it to be used as
;; an emacs-dashboard footer or *scratch* buffer message.

;;; Code:

(require 'url)
(require 'json)

(defgroup votd nil
  "Package that fetches the Bible verse of the day from BibleGateway."
  :group 'external)

(defcustom votd-bible-version "KJV"
  "The Bible version, default KJV."
  :type 'string
  :group 'votd)

(defcustom votd-text-width 80
  "The width the votd body in number of characters, default 80."
  :type 'integer
  :group 'votd)

(defcustom votd-fallback-verse "For God so loved the world,
that he gave his only begotten Son,
that whosoever believeth in him should not perish,
but have everlasting life."
  "The fallback verse to use when the online request fails."
  :type 'string
  :group 'votd)

(defcustom votd-fallback-reference "John 3:16"
  "The reference for the fallback verse."
  :type 'string
  :group 'votd)

(defcustom votd-request-timeout 3
  "The timeout for the URL request in seconds."
  :type 'integer
  :group 'votd)

(defcustom votd-include-ref t
  "If non-nil, include the reference (e.g., 'John 3 (KJV)') in the output."
  :type 'boolean
  :group 'votd)


(defun votd-justify-line (line width)
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

(defun votd-format-verse-text (text &optional width)
  "Format verse TEXT as a justified paragraph with optional WIDTH."
  (when text  ; Only process if text is not nil
    (with-temp-buffer
      (let* ((fill-column (or width votd-text-width))
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
                           (votd-justify-line line fill-column))
                         (butlast lines))
                 (last lines)))
               (result (string-join justified-lines "\n")))
          result)))))

(defun votd-decode-html-entities (text)
  "Decode HTML entities in TEXT."
  (when text
    (let ((entity-map '(("&ldquo;" . "\"")
                        ("&rdquo;" . "\"")
                        ("&#8212;" . "--")
                        ("&#8217;" . "'")
                        ("&#8220;" . "\"")
                        ("&#8221;" . "\"")
                        ("&quot;" . "\"")
                        ("&apos;" . "'")
                        ("&lt;" . "<")
                        ("&gt;" . ">")
                        ("&nbsp;" . " ")
                        ("&amp;" . "&") ; Decode &amp; first
                        ("&#039;" . "'")))) ; Decode numeric entities like &#039; after &amp;
      (dolist (entity entity-map text)
        (setq text (replace-regexp-in-string (car entity) (cdr entity) text))))))

(defun votd-fetch-daily-bible-verse ()
  "Fetch the daily Bible verse from BibleGateway API."
  (let ((url-request-method "GET")
        (url (concat "https://www.biblegateway.com/votd/get/?format=json&version=" votd-bible-version)))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t votd-request-timeout)
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
                   (json-object-type 'hash-table)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (json-data (json-read-from-string json-string))
                   (votd (gethash "votd" json-data))
                   (raw-text (gethash "text" votd))
                   (verse-text (votd-decode-html-entities raw-text))
                   (clean-verse (replace-regexp-in-string "[\"]" "" verse-text))
                   (formatted-verse (votd-format-verse-text clean-verse))
                   (verse-reference (gethash "display_ref" votd))
                   (fill-width votd-text-width))
              (format "%s\n%s"
                      formatted-verse
                      (let ((ref-text verse-reference))
                        (concat (make-string (- fill-width (length ref-text)) ?\s)
                                ref-text))))))
      (error
       (message "%s\n%s"
                (votd-format-verse-text votd-fallback-verse)
                (let ((ref-text votd-fallback-reference))
                  (concat (make-string (- votd-text-width (length ref-text)) ?\s)
                          ref-text)))))))

;;;###autoload
(defun votd-get-verse ()
  "Get the daily verse and handle errors."
  (condition-case err
      (votd-fetch-daily-bible-verse)
    (error
     (message "Today's verse could not be fetched: %s" (error-message-string err)))))

;;;###autoload
(defun votd-get-passage ()
  "Prompt the user for a passage, construct the URL, fetch its content, insert it at the cursor, and format using `fill-paragraph'."
  (interactive)
  (let* ((passage (read-string "Enter the passage (e.g., John 3:16, John 3, John 3-4): "))
         (formatted-passage (replace-regexp-in-string " " "" passage)) ;; Remove spaces
         (url (concat "https://www.biblegateway.com/passage/?search=" formatted-passage "&version=" votd-bible-version)))
    (condition-case error
        (let ((original-buffer (current-buffer))) ; Save the current buffer
          (with-current-buffer (url-retrieve-synchronously url t t votd-request-timeout)
            (goto-char (point-min))
            ;; Extract the description from <meta property="og:description">
            (if (search-forward "<meta property=\"og:description\" content=\"" nil t)
                (let* ((start (point))
                       (end (search-forward "\""))
                       (description (buffer-substring-no-properties start (1- end)))
                       (decoded-description (votd-decode-html-entities description)) ; Decode HTML entities
                       (title nil)) ; Initialize title
                  ;; Optionally extract the title if votd-include-ref is true
                  (when votd-include-ref
                    (goto-char (point-min))
                    (when (search-forward "<meta name=\"twitter:title\" content=\"" nil t)
                      (let ((start (point))
                            (end (search-forward "\"")))
                        (setq title (buffer-substring-no-properties start (1- end))))))
                  ;; Insert into the original buffer
                  (with-current-buffer original-buffer
                    (let ((start-insertion (point))) ; Save the starting point of insertion
                      (insert (if votd-include-ref
                                  (format "%s\n\n%s" (or title "") decoded-description) ; Include title
                                decoded-description)) ; Exclude title
                      (fill-paragraph)
                      ;; Move the cursor to the end of the inserted text
                      (goto-char (point-max)))))
	      (message "Could not find the passage description."))))
      (error
       (message "Error while fetching the passage: %s" (error-message-string error))))))

(provide 'votd)
;;; votd.el ends here
