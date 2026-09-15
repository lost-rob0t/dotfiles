{ lib, pkgs, epkgs }:
let
  buildGitHubPackage =
    {
      pname,
      owner,
      repo,
      rev,
      hash,
      packageRequires ? [ ],
      postInstall ? "",
    }:
    epkgs.trivialBuild {
      inherit pname packageRequires postInstall;
      version = builtins.substring 0 12 rev;
      src = pkgs.fetchFromGitHub {
        inherit owner repo rev hash;
      };
    };
in
{
  app-launcher = buildGitHubPackage {
    pname = "app-launcher";
    owner = "SebastienWae";
    repo = "app-launcher";
    rev = "5493b8ec39c00b4971d260c03e8bbe93b25bb697";
    hash = lib.fakeHash;
  };

  asoc = buildGitHubPackage {
    pname = "asoc";
    owner = "troyp";
    repo = "asoc.el";
    rev = "4a3309a9f250656da6f4a9d34feedf4f5666b17a";
    hash = lib.fakeHash;
  };

  doom-snippets = buildGitHubPackage {
    pname = "doom-snippets";
    owner = "doomemacs";
    repo = "snippets";
    rev = "fd4edaaf0c8476a26994db17d084b36733c635e2";
    hash = lib.fakeHash;
    packageRequires = [ epkgs.yasnippet ];
    postInstall = ''
      find . -mindepth 1 -maxdepth 1 -type d -exec cp -R -- {} "$LISPDIR/" \;
    '';
  };

  evil-quick-diff = buildGitHubPackage {
    pname = "evil-quick-diff";
    owner = "rgrinberg";
    repo = "evil-quick-diff";
    rev = "69c883720b30a892c63bc89f49d4f0e8b8028908";
    hash = "sha256-oGzl1ayW9rIuq0haoiFS7RZsS8NFMdEA7K1BSozgnJU=";
    packageRequires = with epkgs; [ evil magit ];
  };

  libvirt = buildGitHubPackage {
    pname = "libvirt";
    owner = "lost-rob0t";
    repo = "libvirt-el";
    rev = "7453b4737ae6c07dbb74fa1ccd4d65b81ff9fa19";
    hash = lib.fakeHash;
  };

  nose = buildGitHubPackage {
    pname = "nose";
    owner = "emacsattic";
    repo = "nose";
    rev = "f8528297519eba911696c4e68fa88892de9a7b72";
    hash = "sha256-daEi8Kta1oGaDEmUUDDQMahTTPOpvNpDKk22rlr7cB0=";
  };

  opencl-mode = buildGitHubPackage {
    pname = "opencl-mode";
    owner = "salmanebah";
    repo = "opencl-mode";
    rev = "6464abf969d916aba83e393b5206b147ac416da3";
    hash = lib.fakeHash;
  };

  org-recoll = buildGitHubPackage {
    pname = "org-recoll";
    owner = "alraban";
    repo = "org-recoll";
    rev = "1e21fbc70b5e31b746257c12d00acba3dcc1dd5c";
    hash = lib.fakeHash;
    packageRequires = with epkgs; [ dash org ];
  };

  org-timed-alerts = buildGitHubPackage {
    pname = "org-timed-alerts";
    owner = "legalnonsense";
    repo = "org-timed-alerts";
    rev = "ba499f4471800754c75657d92c3150cb0b5deea2";
    hash = lib.fakeHash;
    packageRequires = with epkgs; [
      alert
      dash
      org
      org-ql
      s
      ts
    ];
  };

  "podman.el" = buildGitHubPackage {
    pname = "podman";
    owner = "akirak";
    repo = "podman.el";
    rev = "93f19860badedb0ad1519358fda441940ef688e7";
    hash = lib.fakeHash;
    packageRequires = with epkgs; [
      compat
      s
      transient
    ];
  };

  sly-stepper = buildGitHubPackage {
    pname = "sly-stepper";
    owner = "joaotavora";
    repo = "sly-stepper";
    rev = "da84e3bba8466c2290c2dc7c27d7f4c48c27b39e";
    hash = lib.fakeHash;
    packageRequires = [ epkgs.sly ];
    postInstall = ''
      cp -v -- *.asd *.lisp "$LISPDIR/"
    '';
  };
}
