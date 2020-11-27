{}: self: super:
{
  microcodeIntel = super.microcodeIntel.overrideAttrs (
    x: rec {
      version = "20200616";
      src = super.fetchFromGitHub {
        owner = "intel";
        repo = "Intel-Linux-Processor-Microcode-Data-Files";
        rev = "microcode-${version}";
        sha256 = "13jrs8hwh7dhjjb9kncb8lk199afaxglkh1cfisl6zca1h36g563";
      };
    }
  );
}
