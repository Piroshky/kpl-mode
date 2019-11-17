;;
;; Dan Colish & Cory Kolbeck
;; kpl-mode provides syntax highlighting and stuff for the kpl lang
;;

;; Modifications by Andrew Valentine
;; Indent comments
;; fix single lines comments to highlight properly
;; add multiline comment highlighting 
;; the "-" is not treated as a comment when it's a minus

;; TODO
;; - if block gets highlighted as a function if a "(" follows


(defvar type-regexp
  (regexp-opt
   '("anyType" "array" "bool" "char " "class" "double" "enum" "int" "ptr to"
     "record" "endClass" "endRecord" "typeOfNull") 'words)
  "Regex for matching types in KPL")

(defvar keyword-regexp 
  (regexp-opt 
   '("alloc" "arraySize" "asInteger" "asPtrTo" "behavior""break" "by" "case"
     "catch" "code" "const" "continue" "debug" "default" "do" "else" "elseIf"
     "endBehavior" "endCode" "endFor" "endFunction" "endHeader" "endIf"
     "endInterface" "endMethod" "endSwitch" "endTry" "endWhile" "errors"
     "extends" "external" "false" "fields" "for" "free" "function" "header" "if"
     "implements" "infix" "interface" "isInstanceOf" "isKindOf" "messages"
     "method" "methods" "new" "null" "of" "prefix" "renaming" "return" "returns"
     "self" "sizeOf" "super" "superclass" "switch" "throw" "true" "try" "type"
     "until" "uses" "var" "while") 'words)
  "Regex for matching keywords in KPL")

(defvar kpl-mode-font-lock-defaults
	`(
		;;("--.*$" . font-lock-comment-face)
        ("\\(\\<\\S +\\>\\)\\s *(" 1 font-lock-function-name-face)
        ("\\(\\(?:\\w+, \\)*\\w+\\):". 'font-lock-variable-name-face)
        (,keyword-regexp . 'font-lock-keyword-face)
        (,type-regexp . 'font-lock-type-face)
        ))

(defvar kpl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
		(modify-syntax-entry ?/ ". 14" st)
		(modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for kpl-mode")

(defvar block-starter-regexp
  (concat "^[ \t]*" 
	  (regexp-opt '("function" "fields" "class" "while" "for" "interface"
			"until" "try" "behavior" "method" "methods" "record"
			"type" "if" "else" "case" "code" "header"))))


(defun last-meaningful ()
	"Searches back through the buffer for the last non-blank, non-comment"
  (unless (bobp) (forward-line -1))
  (while (and (not (bobp)) (or (looking-at "^[ \t]*\\(--.*\\)?$"))) 
    (forward-line -1)))

(defun last-nonblank ()
	"Searches back through the buffer for the last non-blank"
  (unless (bobp) (forward-line -1))
  (while (and (not (bobp)) (or (looking-at "^[ \t]*$"))) 
    (forward-line -1)))

(defun var-indent ()
	"Indent variable and field declarations by 2 spaces"
  (last-meaningful)
  (if (looking-at "^[ \t]*\\(var\\|fields\\)")
      (+ (current-indentation) 2)
    (current-indentation)))

(defun assign-indent ()
	"Indent variable assignment by 2 spaces"
  (last-meaningful)
	(if (looking-at "^[ \t]*\\const") 
      (+ (current-indentation) 2) 
			(default-indent)))
;; I don't know why current-indent
;; doens't work for assignment lines
;; but you have to add tab-width again

(defun default-indent ()
	"assumes (last-meaningful) has already been called
   decides indentation based on whether it's a block-starter"
	(if (= 0 (count-lines 1 (point)))
			0
		(progn
			(if (looking-at "^[ \t]*[a-zA-Z_]+:.*") (setq delta (- delta 2)))
			(+ delta
				 (if (looking-at block-starter-regexp) 
						 (+ (current-indentation) tab-width )
					 (current-indentation))))))


(defun search-indent (target terminator)
	"Search for the parent of a line, and return its indentation"
  (unless (and (stringp target) (stringp terminator)) (current-indentation))
  (let ((stack 1) ;; stack tracks any sub-blocks we may encounter
	(targ-regex (concat "^[ \t]*" target "\\([^a-zA-Z]\\|$\\)")) 
	(term-regex (concat "^[ \t]*" terminator)))
    (while (/= stack 0) ;; Search backward until the stack is empty
      (forward-line -1)
      (cond
       ((looking-at targ-regex)(setq stack (1- stack)))
       ((looking-at term-regex)(setq stack (1+ stack))))))
  (current-indentation))

(defun get-indent ()
    (save-excursion
      (beginning-of-line)
      (let ((delta 0)) 
	(cond 

	 ;; Indent comments starting with 4 "-" to 0
	 ;; other comments get indented the same way statements do
	 ((looking-at "^[ \t]*----.*$") 0)
	 
	 ;; I found this annoying, YMMV - c.k.
	 ;; Get rid of any whitespace on blank lines
	 ;; ((looking-at "^[ \t]*$") 0) 

	 ;; Indent case statements
	 ((looking-at "^[ \t]*case .*:") (search-indent "switch" "endSwitch")) ;; TODO - fix regex
	 ;; Indent variable declarations
	 ((looking-at "^[ \t]*[a-zA-Z0-9_]+\\:\\.*") (var-indent))

	 ;; Indent variable assignment
	 ((looking-at "^[ \t]*[a-zA-Z0-9_]+\\ *=\\.*") (assign-indent))
	 
	 ;; Indent block terminators by finding the start of block
	 ((looking-at "^[ \t]*\\(end\\([a-zA-Z]*\\)\\)") (search-indent (downcase (match-string 2)) (match-string 1)))	      
	 ;; Indent else by finding matching if
	 ((looking-at "^[ \t]*else") (search-indent "if" "endIf"))	      
	 ;; Indent everything else relative to the last meaningful line
	 (t 
	  (progn
			(last-meaningful)
			(default-indent)))))))

(defun kpl-indent-line ()
  "Indent current line as KPL code."
  (interactive)
  (indent-line-to (max 0 (get-indent))))


(define-derived-mode kpl-mode prog-mode "KPL"
	"Major mode for editing kpl files."

;; Why was this in here? It makes everything break	
;;    (kill-all-local-variables)
    (set-syntax-table kpl-mode-syntax-table)p
    (make-local-variable 'comment-start)
    (setq comment-start "--")
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip "--+[ \t]*")
    (make-local-variable 'comment-column)
    (make-local-variable 'parse-sexp-ignore-comments)
    (setq parse-sexp-ignore-comments t)
    (set (make-local-variable 'indent-line-function) 'kpl-indent-line)
    (set (make-local-variable 'font-lock-defaults) '((kpl-mode-font-lock-defaults))))

(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
