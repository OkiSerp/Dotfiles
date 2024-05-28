import dracula.draw

config.load_autoconfig(False)

dracula.draw.blood(c, {
    "spacing": {
        "vertical": 4,
        "horizontal": 0,
    },
})

c.aliases = {
    "w": "session-save",
    "q": "close",
    "qa": "quit",
    "wq": "quit --save",
    "so": "config-source",
    "ad": "adblock-update",
}

c.auto_save.session = False

c.completion.height = "40%"

c.completion.open_categories = [
    'searchengines',
    'quickmarks',
    'bookmarks',
    'history',
    'filesystem',
]

c.completion.scrollbar.width = 0
c.completion.scrollbar.padding = 0

c.confirm_quit = [ "multiple-tabs" ]

c.content.autoplay = False

c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
]; c.content.blocking.enabled = True
c.content.blocking.method = "adblock"

c.content.blocking.hosts.lists = [
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts",
]

c.colors.webpage.preferred_color_scheme = "dark"

c.content.notifications.enabled = False

c.fonts.default_size = "10pt"
c.fonts.default_family = [ "Iosevka" ]

c.hints.chars = 'asdfghjkl;'

c.scrolling.bar = "never"

c.statusbar.padding = {
    "top": 2,
    "bottom": 2,
    "left": 4,
    "right": 4,
}

c.tabs.favicons.show = "never"
c.tabs.indicator.padding = {
    "top": 2,
    "bottom": 2,
    "left": 4,
    "right": 4,
}

c.url.searchengines = { "DEFAULT": "https://duckduckgo.com/?q={}" }
c.url.start_pages = [ "https://start.duckduckgo.com" ]

c.window.hide_decoration = True
c.window.title_format = "{perc}{current_title}{title_sep}Qute Browser"

config.bind("<Alt-K>", "tab-prev")
config.bind("<Alt-J>", "tab-next")

config.bind("<Alt-Shift-K>", "tab-move -")
config.bind("<Alt-Shift-J>", "tab-move +")

config.bind("<Alt-K>", "completion-item-focus --history prev", mode="command")
config.bind("<Alt-J>", "completion-item-focus --history next", mode="command")

config.bind("<Ctrl-K>", "prompt-item-focus prev", mode="prompt")
config.bind("<Ctrl-J>", "prompt-item-focus next", mode="prompt")

config.unbind("<Ctrl-C>", mode="command")
config.bind("<Ctrl-C>", "mode-leave", mode="command")

config.bind("<Ctrl-C>", "mode-leave", mode="insert")
config.bind("<Ctrl-C>", "clear-keychain ;; search ;; fullscreen --leave")
config.bind("<Ctrl-C>", "mode-leave", mode="caret")
config.bind("<Ctrl-C>", "mode-leave", mode="prompt")
config.bind("<Ctrl-C>", "mode-leave", mode="register")
config.bind("<Ctrl-C>", "mode-leave", mode="yesno")
config.bind("<Ctrl-C>", "mode-leave", mode="hint")
