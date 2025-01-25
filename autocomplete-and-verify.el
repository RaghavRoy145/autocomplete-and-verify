;;; autocomplete-and-verify.el --- Context-aware code generation, bug analysis, and spot-fixing -*- lexical-binding: t; -*-

;; Author: Raghav Roy raghavroy145@gmail.com
;; URL: https://github.com/RaghavRoy145/autocomplete-and-verify
;; Package-Requires: ((emacs "24.3") (json "1.4") (url "1.0") (font-lock "1.0"))
;; Version: 0.5

(require 'json)
(require 'url)
(require 'font-lock)

;;; Variables for Configuration
(defvar autocomplete-and-verify-infer-path "infer"
  "Path to the Infer binary. Assumes `infer` is in the system's PATH.")

(defvar autocomplete-and-verify-comment-marker "@autocomplete:"
  "Marker in the comment that specifies a code generation request.")

(defvar autocomplete-and-verify-llm-url "https://api.openai.com/v1/chat/completions"
  "The URL of the ChatGPT API endpoint for code generation and explanations.")

(defvar autocomplete-and-verify-api-key nil
  "Your OpenAI API key. Must be set before using ChatGPT integration.")

;;; Core Functions

(defun autocomplete-and-verify--api-key-check ()
  "Check if the API key is set. Prompt the user if it isn't."
  (unless autocomplete-and-verify-api-key
    (setq autocomplete-and-verify-api-key
          (read-string "Enter your OpenAI API key: "))
    (message "API key set.")))

(defun autocomplete-and-verify--chatgpt-request (prompt code)
  "Send a PROMPT and CODE to the ChatGPT API and return the response."
  (autocomplete-and-verify--api-key-check)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " autocomplete-and-verify-api-key))))
         (url-request-data
          (json-encode `(("model" . "gpt-4o-mini")
                         ("messages" . [((role . "system")
                                         (content . "You are the best developer with the best background in Formal Reasoning, help with the Prompt and explain the code. Alter the given Code ONLY WHEN REQUIRED. When altering code do not remove any important functionality that may lead to compilation errors, do not provide any additional explanations, do not add comments"))
                                        ((role . "user")
                                         (content . ,(format "Language: %s\nPrompt: %s\nCode:\n%s"
                                                             (autocomplete-and-verify--get-buffer-language)
                                                             prompt
                                                             code)))])
                         ("temperature" . 1)
                         ("max_tokens" . 2024)
                         ("top_p" . 1)))))
    (with-current-buffer (url-retrieve-synchronously autocomplete-and-verify-llm-url t t)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (choices (alist-get 'choices response)))
        (if choices
            (alist-get 'message (aref choices 0))
          (error "ChatGPT API returned an error: %s" (json-encode response)))))))

(defun autocomplete-and-verify--extract-prompt ()
  "Extract the user's intent from the comment marked by `autocomplete-and-verify-comment-marker`."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat autocomplete-and-verify-comment-marker "\\(.*\\)") nil t)
        (match-string 1)
      (error "No `%s` comment found" autocomplete-and-verify-comment-marker))))

(defun autocomplete-and-verify--get-buffer-language ()
  "Detect the programming language of the current buffer."
  (let ((extension (file-name-extension (or (buffer-file-name) ""))))
    (cond
     ((string-equal extension "c") "C")
     ((string-equal extension "java") "Java")
     (t (error "Unsupported language. Only C and Java are supported.")))))

(defun autocomplete-and-verify--run-infer (file)
  "Run Infer on the given FILE and return the path to the report file."
  (let ((infer-dir (make-temp-file "autocomplete-and-verify-" t)))
    (with-temp-buffer
      (call-process autocomplete-and-verify-infer-path nil t nil "run" "--pulse-only" "--"
                    (if (string-equal (autocomplete-and-verify--get-buffer-language) "Java") "javac" "gcc")
                    file)
      (expand-file-name "report.txt" (concat infer-dir "/infer-out")))))

(defun autocomplete-and-verify--extract-code-from-content (content)
  "Extract valid code from CONTENT, stripping Markdown delimiters."
  (when (and (stringp content)
             (string-match "^```[a-z]*\n\\(\\(.\\|\n\\)*?\\)```$" content))
    (match-string 1 content)))


(defun autocomplete-and-verify--parse-infer-report (report-file)
  "Parse the Infer report file to extract issues or handle no violations."
  (if (not (file-exists-p report-file))
      (error "Infer report file not found")
    (with-temp-buffer
      (insert-file-contents report-file)
      (let ((content (buffer-string)))
        (if (string-empty-p content)
            '(:no-violations t)
          (let ((issues (split-string content "\n#")))
            (mapcar (lambda (issue)
                      (if (string-match "\\([^:]+\\):\\([0-9]+\\): error: \\(.*\\)$" issue)
                          (list :file (match-string 1 issue)
                                :line (match-string 2 issue)
                                :description (match-string 3 issue))
                        issue))
                    (remove "" issues))))))))

(defun autocomplete-and-verify--highlight-code (code errors)
  "Highlight CODE causing ERRORS in a separate buffer."
  (let ((buffer (get-buffer-create "*Generated Code with Errors*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert code)
      (goto-char (point-min))
      (dolist (error errors)
        (let ((line (string-to-number (plist-get error :line))))
          (goto-char (point-min))
          (forward-line (1- line))
          (add-face-text-property (line-beginning-position) (line-end-position) '(:foreground "red") t)))
      (pop-to-buffer buffer))))

(defun autocomplete-and-verify--explain-errors (errors code)
  "Explain Infer ERRORS using the LLM by framing the prompt with precondition and postcondition analysis."
  (let* ((prompt (concat "The following errors were detected by a formal analysis tool called Infer. "
                         "Infer identifies potential violations of preconditions and postconditions in the code. "
                         "For each error below, explain:\n"
                         "1. The inferred preconditions and postconditions.\n"
                         "2. How the violations occurred.\n\n"
                         "Errors:\n"
                        (mapconcat (lambda (err)
                                     (format "File: %s, Line: %s, Error: %s"
                                             (plist-get err :file)
                                             (plist-get err :line)
                                             (plist-get err :description)))
                                   errors "\n DO NOT PROVIDE any new code\n"))))
    (autocomplete-and-verify--chatgpt-request prompt code)))

(defun autocomplete-and-verify--generate-fixes (errors code)
  (let* ((prompt (concat "Based on the following errors detected by Infer, suggest code fixes:\n\n"
                         "Errors:\n"  "Generate fixes for ERRORS in CODE."
                        (mapconcat (lambda (err)
                                     (format "File: %s, Line: %s, Error: %s"
                                             (plist-get err :file)
                                             (plist-get err :line)
                                             (plist-get err :description)))
                                   errors "\n")
                        "\nBased on this, suggest fixes for the given Code:\n")))
    (autocomplete-and-verify--chatgpt-request prompt code)))

(defun autocomplete-and-verify--apply-fixes (generated-code errors)
  "Iteratively fix errors in the GENERATED-CODE using Infer and the LLM."
  (let ((temp-file (make-temp-file "autocomplete-and-verify-" nil ".c"))
        (fixed-code generated-code))
    (while t
      ;; Write the fixed code to a temporary file
      (with-temp-file temp-file
        (insert fixed-code))
      ;; Run Infer on the temporary file
      (let ((report-file (autocomplete-and-verify--run-infer temp-file)))
        (let ((parsed-output (autocomplete-and-verify--parse-infer-report report-file)))
          (if (plist-get parsed-output :no-violations)
              (progn
                (message "All violations fixed.")
                (delete-file temp-file)
                (cl-return fixed-code))  ;; Exit the loop and return fixed code
            ;; Generate fixes for the detected issues
            (let ((fixes (autocomplete-and-verify--generate-fixes parsed-output fixed-code)))
              (setq fixed-code fixes))))))))

(defun remove-markdown (text)
  "Removes markdown formatting from TEXT, leaving only plain text."
  (let ((cleaned-text text))
    ;; Remove bold (e.g. **bold** becomes bold)
    (setq cleaned-text (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "\\1" cleaned-text))
    ;; Remove inline code (e.g. `code` becomes code)
    (setq cleaned-text (replace-regexp-in-string "`\\([^`]+\\)`" "\\1" cleaned-text))
    ;; Remove lists (e.g. - item becomes item)
    (setq cleaned-text (replace-regexp-in-string "^\\s-*\\-\\s*" "" cleaned-text))
    ;; Optionally, remove any extra newline characters, if needed
    (setq cleaned-text (replace-regexp-in-string "\n\\s-*\n" "\n" cleaned-text))
    cleaned-text))

;;; Interactive Commands

(defun autocomplete-and-verify-run ()
  "Main function to run Copilot Infer package workflow."
  (interactive)
  ;; Ensure Infer is installed
  (autocomplete-and-verify--install-infer)

  ;; Extract the user's prompt and context
  (let* ((prompt (autocomplete-and-verify--extract-prompt))
         (context (buffer-substring-no-properties (point-min) (point-max)))

         ;; Generate code using ChatGPT
         (response (autocomplete-and-verify--chatgpt-request prompt context))
         (content (alist-get 'content response)) ;; Extract `content`

         ;; Extract valid code from `content`
         (generated-code (autocomplete-and-verify--extract-code-from-content content))

         ;; Determine the appropriate file extension based on language
         (extension (if (string-equal (autocomplete-and-verify--get-buffer-language) "Java")
                        ".java" ".c"))
         (temp-file (make-temp-file "autocomplete-and-verify-" nil extension))
         (temp-dir (file-name-directory temp-file))) ;; Temp file directory

    ;; Validate that `generated-code` is not nil
    (if (not generated-code)
        (error "ChatGPT did not return valid code. Response: %S" response))

    ;; Save generated code to a temporary file
    (with-temp-file temp-file
      (insert generated-code))

    ;; Display the generated code in a new buffer (side-by-side)
    (with-current-buffer (get-buffer-create "*Generated Code*")
      (erase-buffer)
      (insert generated-code))
    (let ((window (split-window-right)))
      (set-window-buffer window "*Generated Code*"))

    ;; Run Infer in the temp file's directory
    (let ((default-directory temp-dir)) ;; Set working directory
      (call-process autocomplete-and-verify-infer-path nil "*Infer Debug Output*" nil
                    "run" "--pulse-only" "--"
                    (if (string-equal (autocomplete-and-verify--get-buffer-language) "Java")
                        "javac"
                      "gcc")
                    temp-file))

    ;; Debugging: Check Infer's output
    (with-current-buffer "*Infer Debug Output*"
      (message "Infer output: %s" (buffer-string)))

    ;; Check for the report file
    (let ((report-file (expand-file-name "infer-out/report.txt" temp-dir)))
      (message "Looking for Infer report file at: %s" report-file)
      (if (not (file-exists-p report-file))
          (with-current-buffer (get-buffer-create "*Generated Code*")
            (goto-char (point-max))
            (insert "\n\nERROR: The generated code could not compile. Please check it for issues and generate again."))
        
        ;; Only proceed with parsing if the report file exists
        (let* ((parsed-output (autocomplete-and-verify--parse-infer-report report-file)))

          ;; If no issues are found, inform the user
          (if (plist-get parsed-output :no-violations)
              (message "No issues found in generated code.")

            ;; Otherwise, explain and highlight the detected issues
            (let* ((explanations (autocomplete-and-verify--explain-errors parsed-output generated-code))
                   (explanation-content (alist-get 'content explanations)) ;; Extract content from explanations
                   (cleaned-content (remove-markdown explanation-content))) ;; Clean the markdown

              (message "Issues detected in generated code. Highlighting and explaining errors...")

              ;; Highlight the problematic code
              (autocomplete-and-verify--highlight-code generated-code parsed-output)

              ;; Display explanations in a new buffer (side-by-side)
              (with-current-buffer (get-buffer-create "*Error Explanations*")
                (erase-buffer)
                (insert cleaned-content))  ;; Insert the cleaned content without markdown
              (let ((window (split-window-below)))
                (set-window-buffer window "*Error Explanations*")))))))))

;; Fix errors -> WIP

;; (defun autocomplete-and-verify-fix-errors ()
;;   "Fix errors in the generated code using the LLM iteratively."
;;   (interactive)
;;   (autocomplete-and-verify--install-infer)
  
;;   ;; Apply fixes using the LLM
;;   (let ((fixes-response (autocomplete-and-verify--apply-fixes (buffer-string) nil))
;;         (fixed-code nil))

;;     ;; Extract content from the fixes response (alist)
;;     (setq fixed-code (alist-get 'content fixes-response)) ;; Extract the 'content' key from the alist

;;     ;; Clean the content by removing markdown
;;     (let ((cleaned-fixed-code (remove-markdown fixed-code)))
      
;;       ;; Message indicating the fixed code is ready
;;       (message "Final fixed code is ready.")

;;       ;; Insert the cleaned fixed code into the *Final Fixed Code* buffer
;;       (with-current-buffer (get-buffer-create "*Final Fixed Code*")
;;         (erase-buffer)
;;         (insert cleaned-fixed-code)  ;; Insert the cleaned code
;;         (pop-to-buffer (current-buffer))))))


(defun autocomplete-and-verify--install-infer ()
  "Install Infer tool automatically if not already installed."
  (unless (executable-find autocomplete-and-verify-infer-path)
    (let ((infer-url "https://github.com/facebook/infer/releases/download/v1.1.0/infer-linux64-v1.1.0.tar.xz")
          (install-dir (expand-file-name "~/")))
      (make-directory install-dir t)
      (url-copy-file infer-url (concat install-dir "/infer.tar.xz"))
      (shell-command (format "tar -xf %s -C %s" (concat install-dir "/infer.tar.xz") install-dir))
      (add-to-list 'exec-path (concat install-dir "/infer-linux64-v1.1.0/bin"))
      (message "Infer installed successfully."))))

(provide 'autocomplete-and-verify)
;;; autocomplete-and-verify.el ends here
