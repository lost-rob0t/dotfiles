# Expression generated for easy version bumps; update version and hashes
# together from the upstream release assets.
rec {
  pname = "brave";
  version = "1.95.104";

  archives = {
    x86_64-linux = {
      url = "https://github.com/brave/brave-browser/releases/download/v${version}/brave-browser_${version}_amd64.deb";
      sha256 = "e09e3c3e9fc84f75e65048a0be830c7c4907744a402b5790530d2206b276426e";
    };
    aarch64-linux = {
      url = "https://github.com/brave/brave-browser/releases/download/v${version}/brave-browser_${version}_arm64.deb";
      sha256 = "251ee83fa383db6106ae6140df0975689b226d15ceb96cdb4147cb90cd7c25ba";
    };
  };
}
