;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
					 '(("fori" "for (${1:int }${2:i} = ${3:0}; $2 < ${4:SOMETHING}; $2++) {\n    $0\n}" "for i" nil nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/fori" nil nil)
					   ("fn" "${1:void} `(when (boundp 'yas-cpp-class) (format \"%s::\" yas-cpp-class))`${2:func}($3)\n{\n	$0\n}\n" "cpp-function" nil nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/fn" nil nil)
					   ("dn" "`(format \"%s::~%s\" yas-cpp-class yas-cpp-class)`($1)\n{\n	$0\n}\n" "destructor"
						(boundp 'yas-cpp-class)
						nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/destructor" nil nil)
					   ("dl" "delete ${1:pointer};\n$0\n" "delete" nil nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/delete" nil nil)
					   ("cn" "`(format \"%s::%s\" yas-cpp-class yas-cpp-class)`($1)\n{\n	$0\n}" "constructor"
						(boundp 'yas-cpp-class)
						nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/constructor" nil nil)
					   ("c:" "`(format \"%s\" yas-cpp-class)`::$0\n" "class-colon" nil nil nil "c:/Users/Jon/.emacs.d/snippets/c++-mode/class-colon" nil nil)))


;;; Do not edit! File generated at Thu Feb 22 13:20:05 2024
