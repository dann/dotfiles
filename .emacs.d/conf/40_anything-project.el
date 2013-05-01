(require 'anything-project)
(define-key global-map (kbd "C-c b") 'anything-project)

;; ignore blib for perl projects
(ap:add-project :name 'perl :look-for '("Makefile.PL" "Build.PL") :exclude-regexp '("/blib"))

;; xcode project
(ap:add-project :name 'iphone :look-for '("Info.plist" "src") :exclude-regexp '("\\(/build\\|\\.xcodeproj\\)"))

;; git project
(ap:add-project :name 'git :look-for '(".git"))

