# magit-blame-color-by-age

A small package to color-code `magit-blame` headers and the fringe by age.

## Screenshots

### Headings with age first, and fringe coloring
<img width="589" alt="image" src="https://github.com/user-attachments/assets/475b992b-9431-47d4-a528-e9417126d6c3" />

### Final default `magit-blame` style, with fringe coloring
<img width="514" alt="image" src="https://github.com/user-attachments/assets/8ba3154e-1425-4db1-b810-2088e980df09" />

### Full heading color
<img width="558" alt="image" src="https://github.com/user-attachments/assets/90120a3b-e4c9-4ad7-9450-1bcf7f76425b" />

> [!NOTE] Large files with deep commit histories can take significant
> time to fully git-blame.  Magit's "quick start" shows visible blame
> headers near point quickly; these will be re-colored after the full
> blame completes and the full extent of the commit history becomes
> clear.

## Config 
Not on any package archive; install using `:vc`.  Enable like:

```elisp
(use-package magit-blame-color-by-age
  :vc (:url "https://github.com/jdtsmith/magit-blame-color-by-age")
  :hook magit-blame-mode
  ;; :config
  ;; if you'd like date first on heading lines:
  ;; (setf (alist-get 'heading-format (alist-get 'headings magit-blame-styles)) "%C %-20a %s\n")
  ;; If you don't want the horizontal separator lines between chunks when there's no header. 
  ;; (setcdr (assoc 'show-lines (alist-get 'lines  magit-blame-styles)) nil)
  ;; If you want to start with 'lines style
  ;; (let* ((mbs magit-blame-styles) (l (assoc 'lines mbs)))
  ;;   (setq magit-blame-styles (append l (remove l mbs))))
  ;; For full heading coloring
  ;; :custom (magit-blame-color-by-age-full-heading t)
)
```

## Usage

Just `magit-blame` as normal (`C-c g b` in a git-aware file buffer).  Cycle through styles with `c`.

## New

- v0.2: Made blame hunk coloring proceed via font-lock, to eliminate overhead in large buffers.
