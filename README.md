
# Table of Contents

1.  [Installation](#orgd5b246c)
2.  [Usage](#org4c0bef7)

This package provides support for the [ddate](https://github.com/bo0ts/ddate) command.


<a id="orgd5b246c"></a>

# Installation

This package is now available on [MELPA](https://melpa.org/), so you can install it by adding MELPA to your list of
packages and then using `use-package` like this:

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    
    (use-package ddate
      :ensure t)

You can also install this package directly from this repository via `use-package` and
[`quelpa-use-package`](https://github.com/quelpa/quelpa-use-package). 

    (use-package ddate
      :ensure-system-package ddate
      :quelpa (ddate :fetcher git :url "https://git.sr.ht/~earneson/emacs-ddate"))


<a id="org4c0bef7"></a>

# Usage

Use the `ddate` and `ddate-pretty` functions to do fun things.

I like to use it with [dashboard](https://github.com/emacs-dashboard/emacs-dashboard) like this:

    (defun ela/dashboard-insert-ddate (list-size)
      "Insert the Discordian date into the dashboard."
      (let ((ddate-string (ddate-pretty)))
        (dashboard-center-line ddate-string)
        (insert ddate-string)))
    
    (use-package dashboard
      :init (dashboard-setup-startup-hook)
      :config
      ;; Add the ddate item provider to the list.
      (add-to-list 'dashboard-item-generators
                   '(ddate . ela/dashboard-insert-ddate))
    
      ;; Set up your items with ddate at the top.
      (setq dashboard-items '((ddate)
                              (recents   . 5)
                              (bookmarks . 5)
                              (registers . 5))))

