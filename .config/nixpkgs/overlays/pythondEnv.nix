self: super: {
  pythondEnv = super.buildEnv {
    name = "pythondEnv";
    paths = [
      # A Python 3 interpreter with some packages
      (self.python3.withPackages (
        ps: with ps; [
          pyflakes
          pytest
          python-language-server
        ]
      ))

      # Some other packages we'd like as part of this env
      self.mypy
      self.black
      self.pyright
    ];
  };
}
