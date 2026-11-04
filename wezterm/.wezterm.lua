local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

-- フォント
config.font = wezterm.font_with_fallback {
  'Source Han Code JP',
  'Yu Gothic',
}

config.font_size = 12.0
config.line_height = 1.1

-- カラーテーマ
config.color_scheme = 'Tokyo Night Day'

-- 背景
config.window_background_opacity = 0.80

-- ウィンドウ
config.window_padding = {
  left = 12,
  right = 12,
  top = 10,
  bottom = 10,
}

-- タブバー
config.enable_tab_bar = true
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = false
config.tab_max_width = 32

-- スクロール
config.scrollback_lines = 10000

-- カーソル
config.default_cursor_style = 'SteadyBlock'

-- 初期サイズ
config.initial_cols = 120
config.initial_rows = 32

-- キーバインド
config.keys = {
  -- タブ操作
  {
    key = 't',
    mods = 'CTRL',
    action = act.SpawnCommandInNewTab {
      domain = { DomainName = 'WSL:Ubuntu' },
      cwd = '/home/yuta',
    },
  },
  {
    key = 'w',
    mods = 'CTRL',
    action = act.CloseCurrentTab { confirm = true },
  },
  {
    key = 'Tab',
    mods = 'CTRL',
    action = act.ActivateTabRelative(1),
  },
  {
    key = 'Tab',
    mods = 'CTRL|SHIFT',
    action = act.ActivateTabRelative(-1),
  },

  -- ペイン分割
  {
    key = 'd',
    mods = 'CTRL|SHIFT',
    action = act.SplitHorizontal {
      domain = 'CurrentPaneDomain',
    },
  },
  {
    key = 'e',
    mods = 'CTRL|SHIFT',
    action = act.SplitVertical {
      domain = 'CurrentPaneDomain',
    },
  },

  -- ペイン移動
  {
    key = 'LeftArrow',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Left',
  },
  {
    key = 'RightArrow',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Right',
  },
  {
    key = 'UpArrow',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Up',
  },
  {
    key = 'DownArrow',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Down',
  },

  -- ペイン最大化
  {
    key = 'z',
    mods = 'CTRL|SHIFT',
    action = act.TogglePaneZoomState,
  },

  -- 検索
  {
    key = 'f',
    mods = 'CTRL|SHIFT',
    action = act.Search {
      CaseSensitiveString = '',
    },
  },

  -- コピー・ペースト
  {
    key = 'c',
    mods = 'CTRL|SHIFT',
    action = act.CopyTo 'Clipboard',
  },
  {
    key = 'v',
    mods = 'CTRL|SHIFT',
    action = act.PasteFrom 'Clipboard',
  },
}

-- Alt+1〜9でタブを切り替える
for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'ALT',
    action = act.ActivateTab(i - 1),
  })
end

return config
