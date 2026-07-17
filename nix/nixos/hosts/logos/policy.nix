{ config, ... }:

{
  assertions = [
    {
      assertion = config.virtualisation.docker.enable;
      message = "logos must retain Docker support";
    }
    {
      assertion = !config.virtualisation.libvirtd.enable;
      message = "logos must not enable libvirtd";
    }
    {
      assertion = !config.programs.steam.enable;
      message = "logos must not enable Steam";
    }
    {
      assertion = !config.services.sunshine.enable;
      message = "logos must not enable Sunshine";
    }
  ];
}
