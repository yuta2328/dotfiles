;;; ef-oreore-theme.el ---  -*- lexical-binding:t -*-

(eval-and-compile
  (require 'ef-themes)

  ;;;###theme-autoload
  (deftheme ef-oreore
    :background-mode 'light
    :kind 'color-scheme
    :family 'ef)
  
  (defconst ef-oreore-palette
    '(;;; Basic values
      (bg-main     "#f4f6f6")
      (fg-main     "#101010")
      (bg-dim      "#eaefef")
      (fg-dim      "#66657f")
      (bg-alt      "#d7dbdb")
      (fg-alt      "#204f9a")

      (bg-active   "#b5b8b8")
      (bg-inactive "#f7f9f9")

;;; Basic hues for foreground values
      (red           "#e66533")
      (red-warmer   "#e65a33")  ; やや朱赤寄り
      (red-cooler   "#e67333")  ; ややオレンジ寄り
      (red-faint    "#f09b70")

      ;; GREEN (Eucalyptus)
      (green           "#16b673")
      (green-warmer   "#009456") ; 黄緑寄り
      (green-cooler   "#1CE28C") ; 青緑寄り
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
      (magenta          "#df769b")
      (magenta-warmer   "#df5f8a") ; 赤み強め
      (magenta-cooler   "#df89ac") ; 紫寄り
      (magenta-faint    "#eda4bb")

      ;; CYAN (Turquoise)
      (cyan           "#49d6e9")
      (cyan-warmer   "#49c5e9")  ; 水色寄り
      (cyan-cooler   "#49e9f5")  ; 冷たい青
      (cyan-faint    "#7ee9f5")

;;; Basic hues for background values

      (bg-red-intense     "#ff8f88")
      (bg-green-intense   "#8adf90")
      (bg-yellow-intense  "#fac200")
      (bg-blue-intense    "#cbcfff")
      (bg-magenta-intense "#df8fff")
      (bg-cyan-intense    "#88c8ff")

      (bg-red-subtle      "#ffcbdf")
      (bg-green-subtle    "#b3f6d0")
      (bg-yellow-subtle   "#f0f48f")
      (bg-blue-subtle     "#baeeff")
      (bg-magenta-subtle  "#f0ddff")
      (bg-cyan-subtle     "#c2eff2")

;;; Diffs

      (bg-added          "#c9ffea")
      (bg-added-faint    "#d7fff5")
      (bg-added-refine   "#b3efdf")
      (fg-added          "#004840")

      (bg-changed        "#f4e8bd")
      (bg-changed-faint  "#f9efcb")
      (bg-changed-refine "#efd299")
      (fg-changed        "#553d00")

      (bg-removed        "#ffd6e0")
      (bg-removed-faint  "#ffe9e6")
      (bg-removed-refine "#f5bfc8")
      (fg-removed        "#8f1313")

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#45c050")
      (bg-graph-green-1   "#75ef30")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7f90ff")
      (bg-graph-blue-1    "#a6c0ff")
      (bg-graph-magenta-0 "#e07fff")
      (bg-graph-magenta-1 "#fad0ff")
      (bg-graph-cyan-0    "#70d3f0")
      (bg-graph-cyan-1    "#afefff")

;;; Special hues

      (bg-mode-line       "#cce7ff")
      (fg-mode-line       "#051524")
      (bg-completion      "#cceff5")
      (bg-hover           "#eab5ff")
      (bg-hover-secondary "#aae0bf")
      (bg-hl-line         "#dff6e4")
      (bg-paren           "#cab0ef")
      (bg-err             "#ffdfda") ; check with err
      (bg-warning         "#ffe9bf") ; check with warning
      (bg-info            "#ccefcf") ; check with info

      (border        "#b0b7c0")
      (cursor        "#0055bb")
      (fg-intense    "#000000")

      (modeline-err     "#a00000")
      (modeline-warning "#7f1090")
      (modeline-info    "#0000af")

      (underline-err     "#ef0f1f")
      (underline-warning "#bf5f00")
      (underline-info    "#02af52")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

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

      (bg-region "#ccebf3")
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

      (rainbow-0 yellow)
      (rainbow-1 red)
      (rainbow-2 green-warmer)
      (rainbow-3 magenta-warmer)
      (rainbow-4 cyan)
      (rainbow-5 yellow-cooler)
      (rainbow-6 magenta-cooler)
      (rainbow-7 red-cooler)
      (rainbow-8 green-cooler)
      ))

  (defcustom ef-oreore-palette-overrides nil
    "Overrides for `ef-oreore-palette'.

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

  (ef-themes-theme ef-oreore ef-oreore-palette ef-oreore-palette-overrides)

  (provide-theme 'ef-oreore))

;;; ef-oreore-theme.el ends here
