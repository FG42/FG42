;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("resource" "{\n    \"name\": \"$1\",\n    \"fields\": [\n        $0\n    ],\n    \"no_filter\": ${2:$$(yas-choose-value '(\"true\" \"false\"))},\n    \"title-field\": \"$3\"\n}\n" "resource"
                        (featurep 'flymake-json)
                        nil nil "/home/amirreza/.fg42/lib/snippets/js-mode/resource" nil nil)
                       ("in" "{\n    \"name\": \"$1\",\n    \"type\": \"in\",\n    \"bulk\": ${2:$$(yas-choose-value '(\"true\" \"false\"))},\n    \"to\": [\n        $0\n    ]\n},\n" "in"
                        (featurep 'flymake-json)
                        nil nil "/home/amirreza/.fg42/lib/snippets/js-mode/in" nil nil)
                       ("hm" "{\n    \"name\": \"$1\",\n    \"type\": \"has_many\",\n    \"bulk\": ${2:$$(yas-choose-value '(\"true\" \"false\"))},\n    \"to\": \"${1:$(pluralize-string yas-text)}\"\n},\n        $0" "has_many"
                        (featurep 'flymake-json)
                        nil nil "/home/amirreza/.fg42/lib/snippets/js-mode/has_many" nil nil)
                       ("ff" "{\n    \"name\": \"$1\",\n    \"type\": \"${2:$$(yas-choose-value '(\"string\" \"integer\" \"datetime\" \"float\" \"image\" \"in\" \"belongs_to\" \"has_many\"))}\",\n    \"bulk\": ${3:$$(yas-choose-value '(\"true\" \"false\"))}\n},\n        $0" "field"
                        (featurep 'flymake-json)
                        nil nil "/home/amirreza/.fg42/lib/snippets/js-mode/field" nil nil)
                       ("bt" "{\n    \"name\": \"$1\",\n    \"type\": \"belongs_to\",\n    \"bulk\": ${2:$$(yas-choose-value '(\"true\" \"false\"))},\n    \"to\": \"${1:$(pluralize-string yas-text)}\"\n},\n        $0" "belongs_to"
                        (featurep 'flymake-json)
                        nil nil "/home/amirreza/.fg42/lib/snippets/js-mode/belongs_to" nil nil)))


;;; Do not edit! File generated at Sun Feb 23 22:37:27 2020
