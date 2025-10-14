;;; ef-oreoredark-theme.el ---  -*- lexical-binding:t -*-

(eval-and-compile
  (require 'ef-themes)

  ;;;###theme-autoload
  (deftheme ef-oreoredark
    :background-mode 'dark
    :kind 'color-scheme
    :family 'ef)
  
  (defconst ef-oreoredark-palette
    '(;;; Basic values

      (bg-main     "#282a36")
      (fg-main     "#A9B7C6")
      (bg-dim      "#282a36")
      (fg-dim      "#A9B7C6")
      (bg-alt      "#3b393e")
      (fg-alt      "#b0a0cf")

      (bg-active   "#5b595e")
      (bg-inactive "#2a272c")

;;; Basic hues for foreground values
      (red           "#e66533")
      (red-warmer   "#e65a33")  ; やや朱赤寄り
      (red-cooler   "#e67333")  ; ややオレンジ寄り
      (red-faint    "#f09b70")

      ;; GREEN (Eucalyptus)
      (green           "#16b673")
      (green-warmer   "#009456") ; 黄緑寄り
      (green-cooler   "#49e9a6") ; 青緑寄り
      (green-faint    "#8ca6a6")

      ;; YELLOW (Galliano)
      (yellow           "#d5971a")
      (yellow-warmer   "#d58a1a") ; 橙より
      (yellow-cooler   "#d5a61a") ; 山吹色寄り
      (yellow-faint    "#e4b781")

      ;; BLUE (Eastern Blue)
      (blue           "#16a3b6")
      (blue-warmer   "#169ab6")  ; やや緑寄り
      (blue-cooler   "#168cb6")  ; やや紫寄り
      (blue-faint    "#53bfd0")

      ;; MAGENTA (Pale Violet Red)
      (magenta           "#df769b") 
      (magenta-warmer   "#df5f8a") ; 赤み強め
      (magenta-cooler   "#df89ac") ; 紫寄り
      (magenta-faint    "#eda4bb")

      ;; CYAN (Turquoise)
      (cyan           "#49d6e9")
      (cyan-warmer   "#49c5e9")  ; 水色寄り
      (cyan-cooler   "#49e9f5")  ; 冷たい青
      (cyan-faint    "#7ee9f5")

;;; Basic hues for background values

      (bg-red-intense     "#a02f50")
      (bg-green-intense   "#30682f")
      (bg-yellow-intense  "#8f665f")
      (bg-blue-intense    "#4f509f")
      (bg-magenta-intense "#885997")
      (bg-cyan-intense    "#0280b9")

      (bg-red-subtle      "#6f202a")
      (bg-green-subtle    "#2a532f")
      (bg-yellow-subtle   "#62432a")
      (bg-blue-subtle     "#3a3e73")
      (bg-magenta-subtle  "#66345a")
      (bg-cyan-subtle     "#334d69")

;;; Diffs

      (bg-added          "#304a4f")
      (bg-added-faint    "#16383f")
      (bg-added-refine   "#2f6767")
      (fg-added          "#a0d0f0")

      (bg-changed        "#51512f")
      (bg-changed-faint  "#40332f")
      (bg-changed-refine "#64651f")
      (fg-changed        "#dada90")

      (bg-removed        "#5a3142")
      (bg-removed-faint  "#4a2034")
      (bg-removed-refine "#782a4a")
      (fg-removed        "#f0bfcf")

;;; Graphs

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#0fed00")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#f1e00a")
      (bg-graph-yellow-1  "#b08940")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special hues

      (bg-mode-line       "#cce7ff")
      (fg-mode-line       "#fedeff")
      (bg-completion      "#503240")
      (bg-hover           "#957856")
      (bg-hover-secondary "#665f7a")
      (bg-hl-line         "#412f4f")
      (bg-paren           "#885566")
      (bg-err             "#501a2d") ; check with err
      (bg-warning         "#4e3930") ; check with warning
      (bg-info            "#0f3f4f") ; check with info

      (border        "#635850")
      (cursor        "#f3c09a")
      (fg-intense    "#ffffff")

      (modeline-err     "#ffbfbf")
      (modeline-warning "#dfd443")
      (modeline-info    "#7fefff")

      (underline-err     "#c04f9f")
      (underline-warning "#c0b000")
      (underline-info    "#229fb2")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#6f6600")

;;; Mappings

;;;; General mappings

      (bg-fringe unspecified)
      (fg-fringe unspecified)

      (err red-warmer)
      (warning yellow)
      (info green)

      (link cyan-warmer)
      (link-alt green-warmer)
      (name yellow)
      (keybind red-warmer)
      (identifier magenta-faint)
      (prompt yellow)

      (bg-region "#169fb1")
      (fg-region unspecified)

;;;; Code mappings

      (builtin red-cooler)
      (comment green-faint)
      (constant red-warmer)
      (fnname yellow)
      (keyword magenta)
      (preprocessor cyan-warmer)
      (docstring yellow-faint)
      (string green-warmer)
      (type green-cooler)
      (variable yellow-cooler)
      (rx-escape blue) ; compare with `string'
      (rx-construct yellow-warmer)

;;;; Accent mappings

      (accent-0 red)
      (accent-1 green-cooler)
      (accent-2 yellow)
      (accent-3 magenta-warmer)

;;;; Date mappings

      (date-common cyan-cooler)
      (date-deadline red)
      (date-deadline-subtle red-faint)
      (date-event fg-alt)
      (date-holiday red-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-scheduled-subtle yellow-faint)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code green-cooler)
      (prose-done green)
      (prose-macro yellow)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-table-formula info)
      (prose-tag yellow-faint)
      (prose-todo red-warmer)
      (prose-verbatim red)

;;;; Mail mappings

      (mail-cite-0 red)
      (mail-cite-1 green-cooler)
      (mail-cite-2 yellow)
      (mail-cite-3 cyan-warmer)
      (mail-part red-faint)
      (mail-recipient yellow)
      (mail-subject red-warmer)
      (mail-other green-warmer)

;;;; Search mappings

      (bg-search-match bg-warning)
      (bg-search-current bg-yellow-intense)
      (bg-search-lazy bg-blue-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-magenta-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-cyan-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Terminal mappings

      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "gray35")
      (fg-term-black-bright    "gray35")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-warmer)
      (fg-term-green-bright    green-warmer)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-cooler)
      (fg-term-yellow-bright   yellow-cooler)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-warmer)
      (fg-term-blue-bright     blue-warmer)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "gray65")
      (fg-term-white           "gray65")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Rainbow mappings

      (rainbow-0 magenta-warmer)
      (rainbow-1 red)
      (rainbow-2 green-warmer)
      (rainbow-3 yellow)
      (rainbow-4 cyan)
      (rainbow-5 yellow-cooler)
      (rainbow-6 magenta-cooler)
      (rainbow-7 red-cooler)
      (rainbow-8 green-cooler)
      ))

  (defcustom ef-oreoredark-palette-overrides nil
    "Overrides for `ef-oreoredark-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Ef themes,
refer to `ef-themes-common-palette-overrides'.

To preview the palette entries, use `ef-themes-preview-colors' or
`ef-themes-preview-colors-current' (read the documentation for
further details)."
    :group 'ef-themes
    :package-version '(ef-themes . "1.0.0")
    :type '(repeat (list symbol (choice symbol string)))
    :link '(info-link "(ef-themes) Palette overrides"))

  (ef-themes-theme ef-oreoredark ef-oreoredark-palette ef-oreoredark-palette-overrides)

  (provide-theme 'ef-oreoredark))

;;; ef-oreoredark-theme.el ends here
