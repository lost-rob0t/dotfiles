{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["nvidia" "cuda"];

  # Allow unfree packages (required for NVIDIA drivers)
  nixpkgs.config.allowUnfree = true;

  # Enable graphics support (required for CUDA and GPU applications)
  # Note: hardware.graphics.enable was named hardware.opengl.enable until NixOS 24.11
  hardware.graphics.enable = true;

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    # Modesetting is required for Wayland support
    modesetting.enable = true;

    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    open = true;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    # Available options: stable, beta, production, legacy_470, legacy_390, etc.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # CUDA support packages
  environment.systemPackages = with pkgs; [
    cudatoolkit
    cudnn
    cutensor
    # NVIDIA utilities
    nvtop
  ];

  # Set environment variables for CUDA development
  environment.variables = {
    CUDA_PATH = "${pkgs.cudatoolkit}";
    EXTRA_LDFLAGS = "-L/lib -L${config.boot.kernelPackages.nvidiaPackages.stable}/lib";
    EXTRA_CCFLAGS = "-I/usr/include";
  };

  # Add CUDA and NVIDIA libraries to the library path
  environment.sessionVariables = {
    LD_LIBRARY_PATH = "${config.boot.kernelPackages.nvidiaPackages.stable}/lib";
  };
}
