{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
, projectOverrides ? {}
, withHoogle ? false
}:
with obelisk;
project ./. ({ hackGet, pkgs, ... }:
  let
    beamSrc = hackGet dep/beam;
  in {
  inherit withHoogle;
  android.applicationId = "ca.srid.obelisk-rhyolite.template";
  android.displayName = "Obelisk+Rhyolite Example";
  ios.bundleIdentifier = "ca.srid.obelisk-rhyolite.template";
  ios.bundleName = "Obelisk+Rhyolite Example";

  packages = {
    beam-core = beamSrc + /beam-core;
    beam-postgres = beamSrc + /beam-postgres;
    beam-migrate = beamSrc + /beam-migrate;
  };

  overrides = self: super: {
    beam-postgres = pkgs.haskell.lib.dontCheck super.beam-postgres;  # Requires PG to run tests
  };
} // projectOverrides)
