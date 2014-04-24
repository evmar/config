;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("func" "func ${1:fun}(${2:args}) {\n$0\n}" "func" nil nil nil nil nil nil)
                       ("ife" "if ${1:err != nil} {\n   ${2:return err}\n}\n$0" "ife" nil nil nil nil nil nil)
                       ("map" "map[${1:string}] ${2:int} {\n  ${3:\"X\": 100,}\n  $0\n}" "map" nil nil nil nil nil nil)
                       ("pr" "fmt.Printf(\"${1:fmt}\\n\"${2:,str})\n$0" "printf" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Apr 24 16:35:37 2014