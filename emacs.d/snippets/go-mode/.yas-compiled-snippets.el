;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("func" "func ${1:fun}(${2:args}) {\n$0\n}" "func" nil nil nil nil nil nil)
                       ("ife" "if ${1:err != nil} {\n   ${2:return err}\n}\n$0\n" "ife" nil nil nil nil nil nil)
                       ("imp" "import ${1:package}\n$0" "import" nil nil nil nil nil nil)
                       ("main" "func main() {\n   ${1:fmt.Printf(\"Hello, 世界\\n\")}\n   $0\n}\n" "main" nil nil nil nil nil nil)
                       ("map" "map[${1:string}] ${2:int} {\n  ${3:\"X\": 100,}\n  $0\n}" "map" nil nil nil nil nil nil)
                       ("pr" "fmt.Printf(\"${1:fmt}\\n\"${2:,str})\n$0" "printf" nil nil nil nil nil nil)
                       ("switch" "switch {\n    case ${1:cond}:\n         $0\n}" "switch" nil nil nil nil nil nil)
                       ("var" "var ${1:ok} ${2:bool}\n$0" "var" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun Mar 23 11:30:19 2014
