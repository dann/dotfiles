(add-hook 'cperl-mode-hook
          '(lambda ()
             (require 'perl-completion)
             (perl-completion-mode t)))
