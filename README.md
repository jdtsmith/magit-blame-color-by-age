# magit-blame-color-by-age

A small package to color-code `magit-blame` headers by their age.

<img width="635" alt="image" src="https://github.com/user-attachments/assets/695e6906-80ab-4e8b-a975-2734b2a6edd5" />

or, with full header line coloring (see `magit-blame-color-by-age-full-heading`):

<img width="631" alt="image" src="https://github.com/user-attachments/assets/7c5a0338-4251-465f-a550-8f4cea53525e" />

> [!NOTE]
> Large files take some time to fully git-blame.  Magit's "quick start" shows visible blame headers quickly; these will be overridden and re-colored as the full blame completes.

## Config 

Not on any package archive; install using `:vc`.  Enable like:

```elisp
(use-package magit-blame-color-by-age
  :vc (:url "https://github.com/jdtsmith/magit-blame-color-by-age")
  :after magit
  :config (magit-blame-color-by-age-mode))
```
  

