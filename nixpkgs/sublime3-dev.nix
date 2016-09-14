with import <nixpkgs> {};

let
  build = "3121";
  libPath = stdenv.lib.makeLibraryPath [glib xorg.libX11 gtk2 cairo pango];
  redirects = [ "/usr/bin/pkexec=/var/setuid-wrappers/pkexec" ];
in let
  # package with just the binaries
  sublime = stdenv.mkDerivation {
    name = "sublimetext3-dev-${build}-bin";

    src = fetchurl {
          name = "sublimetext-${build}.tar.bz2";
          url = "https://download.sublimetext.com/sublime_text_3_build_${build}_x64.tar.bz2";
          sha256 = "0s4rc3zk5c7jd9lmmd7p4c8n9r0d01g879whg3wgkvglr9jwsqhf";
        };

    dontStrip = true;
    dontPatchELF = true;
    buildInputs = [ makeWrapper ];

    buildPhase = ''
      for i in sublime_text plugin_host crash_reporter; do
        patchelf \
          --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
          --set-rpath ${libPath}:${stdenv.cc.cc.lib}/lib${stdenv.lib.optionalString stdenv.is64bit "64"} \
          $i
      done

      # Rewrite pkexec|gksudo argument. Note that we can't delete bytes in binary.
      sed -i -e 's,/bin/cp\x00,cp\x00\x00\x00\x00\x00\x00,g' sublime_text
    '';

    installPhase = ''
      # Correct sublime_text.desktop to exec `sublime' instead of /opt/sublime_text
      sed -e 's,/opt/sublime_text/sublime_text,sublime,' -i sublime_text.desktop

      mkdir -p $out
      cp -prvd * $out/

      wrapProgram $out/sublime_text \
        --set LD_PRELOAD "${libredirect}/lib/libredirect.so" \
        --set NIX_REDIRECTS ${builtins.concatStringsSep ":" redirects}

      # Without this, plugin_host crashes, even though it has the rpath
      wrapProgram $out/plugin_host --prefix LD_PRELOAD : ${stdenv.cc.cc.lib}/lib${stdenv.lib.optionalString stdenv.is64bit "64"}/libgcc_s.so.1:${openssl.out}/lib/libssl.so:${bzip2.out}/lib/libbz2.so
    '';
  };
in stdenv.mkDerivation {
  name = "sublimetext3-dev-${build}";

  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${sublime}/sublime_text $out/bin/sublime
    ln -s ${sublime}/sublime_text $out/bin/sublime3
    mkdir -p $out/share/applications
    ln -s ${sublime}/sublime_text.desktop $out/share/applications/sublime_text.desktop
    ln -s ${sublime}/Icon/256x256/ $out/share/icons
  '';

  meta = with stdenv.lib; {
    description = "Sophisticated text editor for code, markup and prose";
    homepage = https://www.sublimetext.com/;
    maintainers = with maintainers; [ wmertens demin-dmitriy zimbatm ];
    #license = licenses.unfree;
    platforms = platforms.linux;
  };
}
