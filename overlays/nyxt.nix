self: super:

let
  nyxtSrc = fetchurl {
    url = "https://example.com/nyxt.tar.gz"; # replace with the desired source URL
    sha256 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"; # replace with the SHA-256 hash of the source
  };
in
  {
    lispPackages = super.lispPackages.override {
      overrides = {
        nyxt = super.lispPackages.nyxt.override {
          src = nyxtSrc;
        };
      };
    };
  }
