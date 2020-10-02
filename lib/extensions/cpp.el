;;; CppExtention --- Enable C++ support in FG42
;;; Commentary:
;;; Code:
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/cpp/init)

;; dependencies
(depends-on 'cmake-mode)


(extension cpp
           :version 0.0.1
           :on-initialize extensions/cpp-initialize
           :docs "lib/extensions/cpp/readme.org")
(provide 'extensions/cpp)
;;; cpp.el ends here
