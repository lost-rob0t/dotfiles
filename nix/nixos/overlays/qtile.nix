(self: super: {
      qtile = super.qtile.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ])  ++ (with self.python311Packages; [
          requests
        ]);
      });
    })
