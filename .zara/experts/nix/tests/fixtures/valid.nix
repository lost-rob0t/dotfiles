let
  mkGreeting = name: "hello ${name}";
  base = {
    enabled = true;
  };
in
rec {
  inherit base;
  greeting = mkGreeting "symbolic";
  nested = {
    inherit (base) enabled;
  };
}
