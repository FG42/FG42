;; --------------------------------------------------------------------
;; Groups
;; --------------------------------------------------------------------
(defgroup fg42-user-preferences nil
  "User preferences group. this group contain user specified options for
FG42"
  :group 'fg42
  :tag '"User Preferences"
)

;; --------------------------------------------------------------------
;; Custom Variables
;; --------------------------------------------------------------------
(defcustom developer-name nil
  "FG42 use this option as author name in project if the value be non-nil"
  :group 'fg42-user-preferences
  :type 'string
  :tag '"Developer full name"
)

(defcustom developer-email nil
  "FG42 use this option as author email in project if the value be non-nil"
  :group 'fg42-user-preferences
  :type 'string
  :tag '"Developer Email"
)

(defcustom fg42-workspace "~/src/"
  "FG42 use this option as default path for new project."
  :group 'fg42-user-preferences
  :type 'string
  :tag '"Workspace"
  )

(provide 'extensions/editor/custom)
