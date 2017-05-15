{ 
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in {
    myemacs = import /home/nequi/dev/ext/DevSetup/emacs.nix { pkgs = self; };
  };
}
