% Zara non-secret base configuration managed by Home Manager.
%
% Ownership contract:
%   - This file is declarative dotfiles state and may be replaced by Home Manager.
%   - ~/.config/zarathushtra/config.local.pl is mutable/private operator state.
%   - Keep secrets, machine-private experiments, and temporary overrides in
%     config.local.pl; Zara loads it after this base file so local facts win.

% ----------------------------------------------------------------------
% Search / browser
% ----------------------------------------------------------------------

search_engine("https://search.brave.com/search?q=~w").
app_mapping(browser, ["brave"]).
app_mapping(brave, ["brave"]).
direct_app(brave).

% ----------------------------------------------------------------------
% Emacs / Org-roam
% ----------------------------------------------------------------------

% "open scratch"
app_mapping(scratch,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (switch-to-buffer \"*scratch*\") (goto-char (point-max)))"]).

% Current open-intent parsing consumes the first app token, so both
% "open roam" and "open roam daily" resolve through `roam`.
% Use the canonical Zara dictation command immediately after entering today's
% org-roam daily rather than starting a second microphone/runtime.
app_mapping(roam,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'org-roam-dailies) (org-roam-dailies-goto-today) (start-process \"zara-dictate\" nil \"zara-dictate\"))"]).

app_mapping(emacs, ["emacsclient", "-c", "-a", ""]).
direct_app(emacsclient).

% ----------------------------------------------------------------------
% Desktop apps / media
% ----------------------------------------------------------------------

app_mapping(thunderbird, ["thunderbird"]).
app_mapping(email, ["thunderbird"]).
app_mapping(mail, ["thunderbird"]).
direct_app(thunderbird).

app_mapping(tor, ["torbrowser-launcher"]).
app_mapping(tor_browser, ["torbrowser-launcher"]).
direct_app(torbrowser-launcher).

app_mapping(music, ["feishin"]).
app_mapping(feishin, ["feishin"]).
direct_app(feishin).

% Current open parsing consumes one app token. Point both the common single
% token and canonical alias at YouTube Music.
app_mapping(youtube, ["brave", "--new-window", "https://music.youtube.com/"]).
app_mapping(youtube_music, ["brave", "--new-window", "https://music.youtube.com/"]).

% ----------------------------------------------------------------------
% 4chan /pol/
% ----------------------------------------------------------------------

% Keep a small deterministic alias set until Zara Core owns bounded fuzzy
% matching/edit distance. The '4' and `four` aliases intentionally tolerate
% ASR/tokenization splitting "4 chan" / "four chan" into two tokens.
app_mapping('4chan', ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping('4', ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping(fourchan, ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping(four, ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping(chan, ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping(pol, ["brave", "--new-window", "https://boards.4chan.org/pol/"]).
app_mapping(politically_incorrect, ["brave", "--new-window", "https://boards.4chan.org/pol/"]).

% ----------------------------------------------------------------------
% GitHub / PR compatibility workflow
% ----------------------------------------------------------------------

% Temporary compatibility bridge until Zara Core exposes the requested typed
% GitHub PR query/merge capability directly. `latest prs` opens an Emacs buffer
% populated by gh; it does not fake static PR data.
verb_intent(latest, open, 1).
app_mapping(prs,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(let ((buf (get-buffer-create \"*zara-latest-prs*\"))) (with-current-buffer buf (erase-buffer)) (async-shell-command \"gh search prs --author @me --state open --sort updated --order desc --limit 100\" buf) (pop-to-buffer buf))"]).

% ----------------------------------------------------------------------
% System update
% ----------------------------------------------------------------------

% External todo replacement is intentionally paused. While it is paused,
% "update system" resolves to the bounded helper installed by Home Manager.
verb_intent(update, open, 1).
app_mapping(system, ["zara-system-update"]).

% ----------------------------------------------------------------------
% Magit project aliases
% ----------------------------------------------------------------------

% Current Zara does not yet expose a general known_project/2 user-config fact,
% so `magit <alias>` is the loader-compatible project KB for now.
verb_intent(magit, open, 1).

app_mapping(zara,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/zara\")))"]).
app_mapping(prolog,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/prolog-rlm\")))"]).
app_mapping(agent,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/agentProlog\")))"]).
app_mapping(hackmode,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/hackmode\")))"]).
app_mapping(symbolic,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/symbolic-memory\")))"]).
app_mapping(dotfiles,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/dotfiles\")))"]).
app_mapping(quasar,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/quasar\")))"]).
app_mapping(tek9,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/tek9\")))"]).
app_mapping(plugins,
    ["emacsclient", "-c", "-a", "", "--eval",
     "(progn (require 'magit) (magit-status (expand-file-name \"~/Documents/Projects/zara-plugins\")))"]).

% ----------------------------------------------------------------------
% Voice / dictation
% ----------------------------------------------------------------------

dictation_command(["zara-dictate"]).
wake_word("zara").
wake_word("hey zara").
wake_word("zarathushtra").

% Timers use Zara's native timer semantic path; no todo backend is configured
% here and no second timer store is introduced.
