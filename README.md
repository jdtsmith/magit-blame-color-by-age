# magit-blame-color-by-age
Color `magit-blame` headers by their age.  Not on any package archive; install using `:vc`.  Enable like:

```elisp
(use-package magit-blame-color-by-age
  :vc (:url "https://github.com/jdtsmith/magit-blame-color-by-age")
  :after magit
  :config (magit-blame-color-by-age-mode))
```
  
<img width="635" alt="image" src="https://github.com/user-attachments/assets/695e6906-80ab-4e8b-a975-2734b2a6edd5" />
