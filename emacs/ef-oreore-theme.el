;;; ef-oreore-theme.el --- Custom ef-themes inspired theme  -*- lexical-binding:t -*-

(require 'ef-themes)

(defconst ef-oreore-palette-partial
  '((cursor      "#0055bb")
    (bg-main     "#f4f6f6")
    (bg-dim      "#eaefef")
    (bg-alt      "#d7dbdb")
    (fg-main     "#101010")
    (fg-dim      "#66657f")
    (fg-alt      "#204f9a")
    (bg-active   "#b5b8b8")
    (bg-inactive "#f7f9f9")
    (border        "#b0b7c0")

    (red           "#e66533")
    (red-warmer   "#e65a33")
    (red-cooler   "#e67333")
    (red-faint    "#f09b70")
    (green           "#16b673")
    (green-warmer   "#009456")
    (green-cooler   "#1CE28C")
    (green-faint    "#8ca6a6")
    (yellow           "#d5971a")
    (yellow-warmer   "#d58a1a")
    (yellow-cooler   "#d5a61a")
    (yellow-faint    "#e4b781")
    (blue           "#16a3b6")
    (blue-warmer   "#169ab6")
    (blue-cooler   "#168cb6")
    (blue-faint    "#53bfd0")
    (magenta          "#df769b")
    (magenta-warmer   "#df5f8a")
    (magenta-cooler   "#df89ac")
    (magenta-faint    "#eda4bb")
    (cyan           "#49d6e9")
    (cyan-warmer   "#49c5e9")
    (cyan-cooler   "#49e9f5")
    (cyan-faint    "#7ee9f5")

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

    (bg-mode-line-active "#0099ad")
    (fg-mode-line-active "#fedeff")
    (bg-completion       "#cceff5")
    (bg-hover            "#eab5ff")
    (bg-hover-secondary  "#aae0bf")
    (bg-hl-line          "#dff6e4")
    (bg-paren-match      "#cab0ef")
    (bg-err              "#ffdfda")
    (bg-warning          "#ffe9bf")
    (bg-info             "#ccefcf")
    (bg-region           "#ccebf3")))

(defconst ef-oreore-palette-mappings-partial
  '((err red-warmer)
    (warning yellow)
    (info green)

    (fg-link cyan-warmer)
    (fg-link-visited green-warmer)
    (name yellow)
    (keybind red-warmer)
    (identifier magenta-faint)
    (fg-prompt yellow)

    (builtin red-cooler)
    (comment green-faint)
    (constant red-warmer)
    (fnname yellow)
    (fnname-call yellow-faint)
    (keyword magenta)
    (preprocessor cyan-warmer)
    (docstring yellow-faint)
    (string green-warmer)
    (type green-cooler)
    (variable yellow-cooler)
    (variable yellow-faint)
    (rx-backslash blue) ; compare with `string'
    (rx-construct yellow-warmer)

    (accent-0 red)
    (accent-1 green-cooler)
    (accent-2 yellow)
    (accent-3 magenta-warmer)

    (date-common cyan-cooler)
    (date-deadline red-warmer)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday cyan)
    (date-weekend red-faint)

    (fg-prose-code green-cooler)
    (prose-done green)
    (fg-prose-macro yellow)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag yellow-faint)
    (prose-todo red-warmer)
    (fg-prose-verbatim red)

    (mail-cite-0 red)
    (mail-cite-1 green-cooler)
    (mail-cite-2 yellow)
    (mail-cite-3 cyan-warmer)
    (mail-part red-faint)
    (mail-recipient yellow)
    (mail-subject red-warmer)
    (mail-other green-cooler)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    (rainbow-0 magenta)
    (rainbow-1 cyan-cooler)
    (rainbow-2 magenta-cooler)
    (rainbow-3 yellow)
    (rainbow-4 blue-warmer)
    (rainbow-5 magenta-warmer)
    (rainbow-6 green-cooler)
    (rainbow-7 yellow-cooler)
    (rainbow-8 cyan-warmer)))

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

(defconst ef-oreore-palette
  (modus-themes-generate-palette
   ef-oreore-palette-partial
   nil
   nil
   (append ef-oreore-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'ef-oreore
 'ef-themes
 "Legible dark purple-black theme with magenta, purple, fawn, teal colors."
 'light
 'ef-oreore-palette
 nil
 'ef-oreore-palette-overrides
 'ef-themes-custom-faces)

(provide-theme 'ef-oreore)

;;; ef-oreore-theme.el ends here

