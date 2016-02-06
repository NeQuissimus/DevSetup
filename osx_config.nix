# ~/.nixpkgs/config.nix

 {
   allowUnfree = true;
   packageOverrides = pkgs: rec {
      maven = pkgs.maven.override {
        jdk = pkgs.openjdk8;
      };

      gradleGen = pkgs.gradleGen.override {
        jdk = pkgs.openjdk8;
      };
   };
 }

