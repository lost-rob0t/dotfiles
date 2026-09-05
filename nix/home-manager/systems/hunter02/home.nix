{ ... }:
{
  imports = [ ../desktop/home.nix ];

  # hunter02 is a full workstation: inherit the current desktop/LLM/MCP/Zara
  # stack and layer the host-specific security tooling on top.
  pentesting = {
    enable = true;
    cracking.enable = false;
  };

  homeManagerUpdater = {
    enable = true;
    hostName = "hunter02";
  };

  # hunter02 is used as a standalone Home Manager host, so Nix GUI programs
  # need access to the host graphics driver tree just like the flake profile.
  nixGl.enable = true;
}
