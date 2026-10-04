% Verified StarIntel operator packaging and runtime ownership.

owner('starintel-admin executable',
      'nix/packages/starintel-admin pinned upstream snapshot').

deployment_path('starintel-admin',
                'Home Manager emacs module home.packages and packages.x86_64-linux.starintel-admin').

runtime_dependency('Doom StarIntel operator UI',
                   executable_on_path('starintel-admin')).

credential_contract('starintel-admin',
                    bearer_token_file('~/.config/starintelinfra/admin-token')).

verification_check('starintel-admin',
                   'checks.x86_64-linux.starintel-admin').
