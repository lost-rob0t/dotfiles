% Zara Android automation configuration tracked in the canonical dotfiles XDG tree.
% This file is imported as data-only Prolog through Zara Android's Git config
% template boundary.  Keep raw package IDs, shell, credentials, and private
% device state out of this file.

automation(youtube_psytrance,
    actions([
        app_search(youtube, "psytrance")
    ])).

automation(revanced_psytrance,
    actions([
        app_search(youtube_revanced, "psytrance")
    ])).

automation(psytrance_both,
    actions([
        app_search(youtube, "psytrance"),
        app_search(youtube_revanced, "psytrance")
    ])).

automation(open_youtube,
    actions([
        open_app(youtube)
    ])).

automation(open_revanced,
    actions([
        open_app(youtube_revanced)
    ])).

% Demonstrates permission acquisition.  Zara stops at the capability boundary
% when Accessibility is unavailable and requires the user to grant it.
automation(accessibility_demo,
    actions([
        ui_click(text("Search")),
        ui_set_text(view_id("com.example:id/query"), "psytrance")
    ])).
