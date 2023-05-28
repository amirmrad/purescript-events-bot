{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
      utils.url = "github:numtide/flake-utils";
      npmlock2nix =
        { flake = false;
          url = "github:nix-community/npmlock2nix";
        };

    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
         let
           pkgs = nixpkgs.legacyPackages.${system};
           ps-tools = inputs.ps-tools.legacyPackages.${system};
           purs-nix = inputs.purs-nix { inherit system; };
           npmlock2nix = (import inputs.npmlock2nix { inherit pkgs; }).v2;
           mods = npmlock2nix.node_modules {
                    src = ./.; 
                    nodejs = pkgs.nodejs-16_x;
                    sourceOverrides = {
                      fluent-ffmpeg = sourceInfo: drv: drv.overrideAttrs (old: {
                        postPatch = ''
                          echo "module.exports = require('./lib/fluent-ffmpeg')" > index.js;
                          cat index.js
                        '';
                      });
                      # This patch is required due to a renaming issue when
                      # using esbuild
                      # esbuild assumes the moduleRaid is refering to the
                      # recursive definition and changes the name during
                      # bundling. This causes the `eval(var moduleRaid ...)`
                      # call in 'web-whatsapp-js/src/util/Injected.js' to
                      # fail due to different variable names.
                      "@pedroslopez/moduleraid" = sourceInfo: drv: drv.overrideAttrs (old: {
                        postPatch = ''
                          patch moduleraid.js < ${./patches/moduleraid.patch}
                        '';
                      });
                    };
                    PUPPETEER_SKIP_DOWNLOAD = 1; 
                  } + /node_modules;


           ps = purs-nix.purs { 
                  dependencies =
                   [ "aff-promise"
                     "argonaut"
                     "debug"
                     "console"
                     "effect"
                     "foreign"
                     "formatters"
                     "free"
                     "lazy-joe"
                     "node-fs-aff"
                     "now"
                     "numbers"
                     "parsing"
                     "prelude"
                     "run"
                     "variant"
                   ];

                   dir = ./.;
                   srcs = [ "src" ];
                   foreign.Helpers.node_modules = mods;
                   foreign.Client.node_modules = mods;
                };
           app = ps.app {
                 name = "wa-bot";
                 esbuild = {
                   minify = false;
                 };
               };
           lib = ps.bundle {
             esbuild = {
               minify = true;
             };
           };
           etc_fonts =
              let
                fonts = with pkgs; [
                  dejavu_fonts
                  freefont_ttf
                  liberation_ttf
                ];
                cache = pkgs.makeFontsCache { inherit (pkgs) fontconfig; fontDirectories = fonts; };
                config = pkgs.writeTextDir "conf.d/00-nixos-cache.conf" ''<?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    ${builtins.concatStringsSep "\n" (map (font: "<dir>${font}</dir>") fonts)}
                    <cachedir>${cache}</cachedir>
                  </fontconfig>
                '';
              in
              pkgs.buildEnv { name = "etc-fonts"; paths = [ "${pkgs.fontconfig.out}/etc/fonts" config ]; };
           # We use bubblewrap to populate /etc/fonts.
           # We use ungoogled-chromium because chromium some times times out on Hydra.
           #
           # Chromium wrapper code was provided to us by Las Safin (thanks)
           chromium = pkgs.writeShellScriptBin "chromium" ''
             env - ${pkgs.bubblewrap}/bin/bwrap \
               --unshare-all \
               --share-net \
               --ro-bind /nix/store /nix/store \
               --bind /build /build \
               --uid 1000 \
               --gid 1000 \
               --proc /proc \
               --dir /tmp \
               --dev /dev \
               --setenv TMPDIR /tmp \
               --setenv XDG_RUNTIME_DIR /tmp \
               --bind . /data \
               --chdir /data  \
               --ro-bind ${etc_fonts} /etc/fonts \
               -- ${pkgs.ungoogled-chromium}/bin/chromium \
                 --no-sandbox \
                 --disable-setuid-sandbox \
                 --disable-gpu \
                 "$@"
           '';

         in
         { packages = {
             default = lib;
             wa-bot = app;
             bot-image = pkgs.dockerTools.buildImage {
               name = "wa-bot";
               tag = "latest";
               copyToRoot = pkgs.buildEnv {
                name = "image-root";
                paths = [ pkgs.dockerTools.binSh pkgs.dockerTools.usrBinEnv pkgs.fakeNss chromium ];
                pathsToLink = [ "/bin" "/etc" "/var" ];
               };
               runAsRoot = ''
                  #!${pkgs.runtimeShell}
                  mkdir -p /data
               '';
               config = { 
                 Cmd = [ "${app}/bin/wa-bot" ]; 
                 WorkingDir = "/data";
                 Volumes = { "/data" = { }; };
               };
             };
           };

           apps.default = { type = "app"; program = "${app}/bin/wa-bot";};

           devShells.default =
             pkgs.mkShell
               { packages =
                   with pkgs;
                   [ entr
                     nodejs-16_x
                     (ps.command {})
                     ps-tools.for-0_15.purescript-language-server
                     purs-nix.esbuild
                     purs-nix.purescript
                   ];

                 shellHook =
                   ''
                   alias watch="find src | entr -s 'echo bundling; purs-nix bundle'"
                   alias chrome="chromium"
                   '';
               };
         }
      );
}
