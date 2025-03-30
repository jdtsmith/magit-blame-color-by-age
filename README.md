# magit-blame-color-by-age

A small package to color-code `magit-blame` headers and the fringe by age.

## Screenshots

### Headings with age first, and fringe coloring
<img width="589" alt="image" src="https://github.com/user-attachments/assets/475b992b-9431-47d4-a528-e9417126d6c3" />

### Final default `magit-blame` style, with fringe coloring
<img width="514" alt="image" src="https://github.com/user-attachments/assets/8ba3154e-1425-4db1-b810-2088e980df09" />

### Full heading color
<img width="558" alt="image" src="https://github.com/user-attachments/assets/90120a3b-e4c9-4ad7-9450-1bcf7f76425b" />

> [!NOTE]
> Large files take some time to fully git-blame.  Magit's "quick start" shows visible blame headers quickly; these will be re-colored as the full blame completes.

## Config 

Not on any package archive; install using `:vc`.  Enable like:

```elisp
(use-package magit-blame-color-by-age
  :vc (:url "https://github.com/jdtsmith/magit-blame-color-by-age")
  :hook magit-blame-mode
  ;; if you'd like date first on heading lines:
  ;; :config (setf (alist-get 'heading-format (alist-get 'headings magit-blame-styles)) "%C %-20a %s\n")
  ;; For full heading coloring
  ;; :custom (magit-blame-color-by-age-full-heading t)
)
```
