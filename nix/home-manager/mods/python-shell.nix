{ lib, pkgs, config, ... }:
{
  pythonFHS = pkgs.buildFHSUserEnv {
    name = "python-fhs";
    targetPkgs = with pkgs; [
      config.dev.pythonPkg
      withPackages (ps: with ps; [ ipython requests aiohttp aiodns  ])

    ];
  };
}
