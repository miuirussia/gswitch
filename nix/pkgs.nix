{
  extras = hackage:
    {
      packages = {
        "deriving-aeson" = (((hackage.deriving-aeson)."0.1.2").revisions).default;
        "envparse" = (((hackage.envparse)."0.4.1").revisions).default;
        "servant-flatten" = (((hackage.servant-flatten)."0.2").revisions).default;
        gswitch = ./gswitch.nix;
        };
      };
  resolver = "lts-14.27";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }