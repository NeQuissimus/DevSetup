{ 
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in {
    myemacs = import ./emacs.nix { pkgs = self; };
  };
}
