from pathlib import Path

root = Path(".")
org_path = root / ".doom.d/config.org"
el_path = root / ".doom.d/config.el"
init_path = root / ".doom.d/init.el"


def replace_once(path: Path, old: str, new: str) -> None:
    text = path.read_text()
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{path}: expected one match, found {count}: {old!r}")
    path.write_text(text.replace(old, new, 1))


def replace_in_config(old: str, new: str) -> None:
    replace_once(org_path, old, new)
    replace_once(el_path, old, new)


replace_once(
    org_path,
    "       ;;ivy               ; a search engine for love and life\n"
    "       vertico           ; the search engine of the future",
    "       ivy               ; a search engine for love and life\n"
    "       ;;vertico           ; the search engine of the future",
)
replace_once(
    init_path,
    "       ;;ivy               ; a search engine for love and life\n"
    "       vertico           ; the search engine of the future",
    "       ivy               ; a search engine for love and life\n"
    "       ;;vertico           ; the search engine of the future",
)
replace_once(
    org_path,
    "       ;;ibuffer         ; interactive buffer management",
    "       ibuffer           ; interactive buffer management",
)
replace_once(
    init_path,
    "       ;;ibuffer         ; interactive buffer management",
    "       ibuffer           ; interactive buffer management",
)

replace_in_config(
    '''(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Cycle The Theme" "T" #'ivan/cycle-theme))''',
    '''(map! :leader
      :desc "Cycle The Theme"
      "t T" #'ivan/cycle-theme)''',
)

replace_in_config(
    '''(map! :leader
      :desc "Tangle a file"
      "b t" #'org-babel-tangle)''',
    '''(map! :leader
      :desc "Tangle a file"
      "o b t" #'org-babel-tangle)''',
)

replace_in_config(
    '''(map! :leader
      :after org
      :prefix ("b" . "org-babel-fomats")
      :desc "format src" "f" #'format-elisp-src-blocks)''',
    '''(map! :leader
      :after org
      :desc "Format Org Babel source blocks"
      "o b f" #'format-elisp-src-blocks)''',
)

replace_in_config(
    '''(map! :localleader
      :after org
      :map org-mode-map
      :prefix ("a" . "attachments")
      :desc "paste image" "p" #'org-download-clipboard
      :desc "insert image from url" "i" #'org-download-yank)''',
    '''(map! :localleader
      :after org
      :map org-mode-map
      :desc "Paste image" "a p" #'org-download-clipboard
      :desc "Insert image from URL" "a i" #'org-download-yank)''',
)

replace_in_config(
    '''(map! :leader
      :after magit
      :map 'magit-mode-map
      (:prefix-map ("g" . "git")
       :desc "Clone a Repo" "R" #'ar/git-clone-clipboard-url))''',
    '''(map! :leader
      :after magit
      :map 'magit-mode-map
      :desc "Clone a Repo"
      "g R" #'ar/git-clone-clipboard-url)''',
)

replace_in_config(
    '''(map! :leader
      :after magit
      :map 'magit-mode-map
      (:prefix-map ("g" . "git")
                   (:prefix ("c" . "create")
                    :desc "Create new git tag" "t" #'magit-tag-create)))''',
    '''(map! :leader
      :after magit
      :map 'magit-mode-map
      :desc "Create new git tag"
      "g c t" #'magit-tag-create)''',
)

replace_in_config(
    '''(map! :leader
      :after webpaste
      (:prefix-map ("n" . "notes")
                   (:prefix ("p" . "webpaste")
                    :desc "paste region to a paste service" "r" #'webpaste-paste-region
                    :desc "paste entire buffer to paste service" "b" #'webpaste-paste-buffer)))''',
    '''(map! :leader
      :after webpaste
      :desc "Paste region to a paste service" "n p r" #'webpaste-paste-region
      :desc "Paste entire buffer to a paste service" "n p b" #'webpaste-paste-buffer)''',
)

replace_in_config(
    '''  (map! :leader
        (:prefix-map ("n" . "notes")
         (:prefix-map ("v" . "vector")
          :desc "Vector search" "s" #'org-vector-search
          :desc "Search at point" "S" #'org-vector-search-at-point
          :desc "Full re-index" "i" #'org-vector-embed
          :desc "Vector menu" "m" #'org-vector-transient
          :desc "Kill all" "k" #'org-vector-stop-all))))''',
    '''  (map! :leader
        :desc "Vector search" "n v s" #'org-vector-search
        :desc "Search at point" "n v S" #'org-vector-search-at-point
        :desc "Full re-index" "n v i" #'org-vector-embed
        :desc "Vector menu" "n v m" #'org-vector-transient
        :desc "Kill all vector services" "n v k" #'org-vector-stop-all))''',
)

replace_in_config(
    '''(map!
 :leader
 (:prefix ("y" . "AI/LLM")
  :desc "gptel" :n "y" #'gptel
  :desc "gptel" :n "f" #'gptel-add-file
  :desc "gptel" :n "a" #'gptel-add
  :desc "gptel abort" :n "q" #'gptel-abort
  :desc "gptel Menu" :n "Y" #'gptel-menu
  :desc "gptel copilot" :n "i" #'gptel-complete
  :desc "gptel Send" :n "s" #'gptel-send
  :desc "gptel Topic" :n "t" #'gptel-set-topic
  :desc "Desktop Assistant" :n "d" #'+mcp/desktop-assistant
  (:prefix ("m" . "MCP")
   :desc "Test Filesystem" :n "f" #'+mcp/test-filesystem
   :desc "Test MPRIS" :n "m" #'+mcp/test-mpris)))''',
    '''(map! :leader
      :desc "gptel" :n "y y" #'gptel
      :desc "Add file to gptel" :n "y f" #'gptel-add-file
      :desc "Add to gptel" :n "y a" #'gptel-add
      :desc "Abort gptel" :n "y q" #'gptel-abort
      :desc "gptel menu" :n "y Y" #'gptel-menu
      :desc "gptel complete" :n "y i" #'gptel-complete
      :desc "Send to gptel" :n "y s" #'gptel-send
      :desc "Set gptel topic" :n "y t" #'gptel-set-topic
      :desc "Desktop Assistant" :n "y d" #'+mcp/desktop-assistant
      :desc "Test MCP filesystem" :n "y m f" #'+mcp/test-filesystem
      :desc "Test MCP MPRIS" :n "y m m" #'+mcp/test-mpris)''',
)

replace_in_config(
    '''(map! :leader
      :prefix ("s" . "search")
      :desc "cheat sheat" "c" #'cheat-sh)''',
    '''(map! :leader
      :desc "Cheat sheet"
      "s c" #'cheat-sh)''',
)

for path in (
    root / ".doom.d/keybindings.org",
    root / ".doom.d/autoload/nsa-keybindings.el",
):
    path.unlink(missing_ok=True)

for path in (org_path, el_path):
    text = path.read_text()
    if ":prefix-map" in text:
        raise SystemExit(f"{path}: private :prefix-map remains")
    if ":prefix (" in text:
        raise SystemExit(f"{path}: described :prefix remains")

print("Doom keybinding namespace repair complete")
