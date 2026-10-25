# Evaluate the real Home Manager module without building or activating Emacs.
# Run from the repository root; see docs/wiki/emacs.org.
{ flake, emacsModule ? ../nix/home-manager/mods/emacs.nix }:
let
  inherit (flake.inputs.nixpkgs) lib;
  pkgs = flake.inputs.nixpkgs.legacyPackages.x86_64-linux;
  mkHome = extraModules: flake.inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs.inputs = flake.inputs;
    modules = [
      emacsModule
      {
        home.username = "emacs-package-test";
        home.homeDirectory = "/tmp/emacs-package-test";
        home.stateVersion = "25.05";
        emacs.gitEmail = "emacs-package-test@example.invalid";
        emacs.diredXDG.enable = false;
        gptTodos.enable = false;
      }
    ] ++ extraModules;
  };
  base = mkHome [ ];
  extended = mkHome [
    { emacs.extraPackages = [ pkgs.emacsPackages.avy ]; }
    { emacs.extraPackages = [ pkgs.emacsPackages.ace-window ]; }
  ];
  disabled = mkHome [
    {
      emacs.enable = false;
      emacs.extraPackages = [ pkgs.emacsPackages.avy ];
    }
  ];
  paths = home: map (package: package.outPath)
    (home.config.programs.emacs.extraPackages
      (pkgs.emacsPackagesFor home.config.programs.emacs.package));
  extras = map (package: package.outPath) extended.config.emacs.extraPackages;
in
assert lib.assertMsg (base.config.emacs.extraPackages == [ ])
  "emacs.extraPackages must default to an empty list";
assert lib.assertMsg
  (builtins.length extras == 2
    && builtins.elem pkgs.emacsPackages.avy.outPath extras
    && builtins.elem pkgs.emacsPackages.ace-window.outPath extras)
  "emacs.extraPackages must retain contributions from multiple modules";
assert lib.assertMsg (paths extended == paths base ++ extras)
  "emacs.extraPackages must extend the base package list";
assert lib.assertMsg (!disabled.config.programs.emacs.enable)
  "emacs.extraPackages must not enable a disabled Emacs module";
{
  emptyDefault = true;
  mergedOverridesPreserved = true;
  basePackagesPreserved = true;
  disabledModulePreserved = true;
  basePackageCount = builtins.length (paths base);
}
