{
  extras = hackage:
    {
      packages = {
        "deriving-aeson" = (((hackage.deriving-aeson)."0.1.2").revisions).default;
        gswitch = ./gswitch.nix;
        };
      };
  resolver = "lts-14.27";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }