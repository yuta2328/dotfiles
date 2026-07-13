;;; ef-oreoredark-theme.el --- Custom ef-themes inspired dark theme  -*- lexical-binding:t -*-

(require 'ef-themes)

(defconst ef-oreoredark-palette-partial
  '((cursor      "#0055bb")
    (bg-main     "#2b2b2b")
    (bg-dim      "#282a36")
    (bg-alt      "#3b393e")
    (fg-main     "#A9B7C6")
    (fg-dim      "#A9B7C6")
    (fg-alt      "#b0a0cf")
    (bg-active   "#5b595e")
    (bg-inactive "#2a272c")
    (border      "#635850")

    (red           "#e66533")
    (red-warmer   "#e65a33")
    (red-cooler   "#e67333")
    (red-faint    "#f09b70")
    (green           "#16b673")
    (green-warmer   "#009456")
    (green-cooler   "#49e9a6")
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

    (bg-mode-line-active "#001B33")
    (fg-mode-line-active "#fedeff")
    (bg-completion       "#503240")
    (bg-hover            "#957856")
    (bg-hover-secondary  "#665f7a")
    (bg-hl-line          "#412f4f")
    (bg-paren-match      "#885566")
    (bg-err              "#501a2d")
    (bg-warning          "#4e3930")
    (bg-info             "#0f3f4f")
    (bg-region           "#0B4E55")))

(defconst ef-oreoredark-palette-mappings-partial
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
    (rx-backslash blue)
    (rx-construct yellow-warmer)

    (accent-0 red)
    (accent-1 green-cooler)
    (accent-2 yellow)
    (accent-3 magenta-warmer)

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
    (mail-other green-warmer)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    (rainbow-0 magenta-warmer)
    (rainbow-1 red)
    (rainbow-2 green-warmer)
    (rainbow-3 yellow)
    (rainbow-4 cyan)
    (rainbow-5 yellow-cooler)
    (rainbow-6 magenta-cooler)
    (rainbow-7 red-cooler)
    (rainbow-8 green-cooler)))

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

(defconst ef-oreoredark-palette
  (modus-themes-generate-palette
   ef-oreoredark-palette-partial
   nil
   nil
   (append ef-oreoredark-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'ef-oreoredark
 'ef-themes
 "Legible dark theme with orange, teal, magenta, and cyan colors."
 'dark
 'ef-oreoredark-palette
 nil
 'ef-oreoredark-palette-overrides
 'ef-themes-custom-faces)

;;; ef-oreoredark-theme.el ends here
