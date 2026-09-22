% Desktop Emacs entry-point ownership and routing.

emacs_default_profile(doom).
emacs_wrapper_path('/.local/bin/emacs').
emacs_wrapper_owner(home_manager, 'nix/home-manager/mods/emacs.nix').
emacs_path_owner(literate_org, 'bash.org', '.bashrc').

% The wrapper only remains the default when ~/.local/bin precedes the Nix
% profile; merely appearing later in PATH is insufficient.
emacs_path_invariant(local_bin_precedes_nix_profile).
emacs_client_socket(doom).
