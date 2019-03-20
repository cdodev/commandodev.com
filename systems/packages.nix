# This file was generated by Psc-Package2Nix
# You will not want to edit this file.
# To change the contents of this file, first fork Psc-Package2Nix
# And edit the $file_template

{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {

    "aff" = pkgs.stdenv.mkDerivation {
      name = "aff";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-aff.git";
        rev = "v5.1.0";
        sha256 = "00s97s7awa8n7h2p6hkc8kpmlwfvigbbj1phvwx3a90whczfi99d";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "arraybuffer-types" = pkgs.stdenv.mkDerivation {
      name = "arraybuffer-types";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-arraybuffer-types.git";
        rev = "v2.0.0";
        sha256 = "059a8n49yhl46l1b1j2qj4ichzq6dzj29ajkfvw88npzj5w2rshy";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "arrays" = pkgs.stdenv.mkDerivation {
      name = "arrays";
      version = "v5.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-arrays.git";
        rev = "v5.2.0";
        sha256 = "1niqvccjg3kaix8rcpjsvh31j4hf6myiahmbz3my7wjbvx5hcjgc";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "bifunctors" = pkgs.stdenv.mkDerivation {
      name = "bifunctors";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-bifunctors.git";
        rev = "v4.0.0";
        sha256 = "1bdra5fbkraglqrrm484vw8h0wwk48kzkn586v4y7fg106q1q386";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "canvas" = pkgs.stdenv.mkDerivation {
      name = "canvas";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-canvas.git";
        rev = "v4.0.0";
        sha256 = "0m7a523hr6jn8wg3d1jck1s3bar81gq4ar8bq658sbsysh73n1py";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "catenable-lists" = pkgs.stdenv.mkDerivation {
      name = "catenable-lists";
      version = "v5.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-catenable-lists.git";
        rev = "v5.0.1";
        sha256 = "0mbpb8xr9a7a4bvawhki7js5cbv7c0lv0vdwb6r8nmv6b61gzg27";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "colors" = pkgs.stdenv.mkDerivation {
      name = "colors";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/sharkdp/purescript-colors.git";
        rev = "v5.0.0";
        sha256 = "05bkfqllfpkh7nj0nzgd5p387hlpk0x35nam1i6xm3vzma9csj18";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "console" = pkgs.stdenv.mkDerivation {
      name = "console";
      version = "v4.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-console.git";
        rev = "v4.2.0";
        sha256 = "1b2nykdq1dzaqyra5pi8cvvz4dsbbkg57a2c88yi931ynm19nnw6";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "const" = pkgs.stdenv.mkDerivation {
      name = "const";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-const.git";
        rev = "v4.1.0";
        sha256 = "0qbd2aisax52yw6sybdhs7na943cbsd6mylhhgsamrf7hzh6v51p";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "contravariant" = pkgs.stdenv.mkDerivation {
      name = "contravariant";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-contravariant.git";
        rev = "v4.0.0";
        sha256 = "0vvcgfclx236kg4y76nwih787wyqacq8mmx42q64xzl964yrwxkk";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "control" = pkgs.stdenv.mkDerivation {
      name = "control";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-control.git";
        rev = "v4.1.0";
        sha256 = "10703zvsnjm5fc74k6wzjsvpsfyc3jci3jxhm7rxf7ymya9z1z53";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "datetime" = pkgs.stdenv.mkDerivation {
      name = "datetime";
      version = "v4.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-datetime.git";
        rev = "v4.1.1";
        sha256 = "06ghfvbvd5sd0q014qi8j8glk2g2j9f8z8cwq2318kllp92gz07q";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "debug" = pkgs.stdenv.mkDerivation {
      name = "debug";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/garyb/purescript-debug.git";
        rev = "v4.0.0";
        sha256 = "0gwjj80akys0h111i74n429fmny992gx0r4rk1n98gqlqm5cmi21";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "distributive" = pkgs.stdenv.mkDerivation {
      name = "distributive";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-distributive.git";
        rev = "v4.0.0";
        sha256 = "0zbn0yq1vv7fbbf1lncg80irz0vg7wnw9b9wrzxhdzpbkw4jinsl";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "drawing" = pkgs.stdenv.mkDerivation {
      name = "drawing";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/paf31/purescript-drawing.git";
        rev = "v4.0.0";
        sha256 = "1ad1f2c7rnhd25hidkbxxxm2mnc1sr3zsjzwivbynwv1fmyv54nz";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "effect" = pkgs.stdenv.mkDerivation {
      name = "effect";
      version = "v2.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-effect.git";
        rev = "v2.0.1";
        sha256 = "1vqw5wrdxzh1ww6z7271xr4kg4mx0r3k3mwg18dmgmzj11wbn2wh";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "either" = pkgs.stdenv.mkDerivation {
      name = "either";
      version = "v4.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-either.git";
        rev = "v4.1.1";
        sha256 = "12j7vvjly0bgxidxmb2pflx0zy7x425dnvxk2y1pm66n0hbsq7ns";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "enums" = pkgs.stdenv.mkDerivation {
      name = "enums";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-enums.git";
        rev = "v4.0.1";
        sha256 = "0qq0pgmq497nfml0y8xb2qdpga5xqp9sqq4ilv1rpyhg1v7nzb6v";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "exceptions" = pkgs.stdenv.mkDerivation {
      name = "exceptions";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-exceptions.git";
        rev = "v4.0.0";
        sha256 = "17s0rg9k4phivhx9j3l2vqqfdhk61gpj1xfqy8w6zj3rnxj0b2cv";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "exists" = pkgs.stdenv.mkDerivation {
      name = "exists";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-exists.git";
        rev = "v4.0.0";
        sha256 = "0bbdbw3jaqyi8dj2d52zvfgx4vl84d8cr6hp43vy8lfjfcbj0wlk";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "flare" = pkgs.stdenv.mkDerivation {
      name = "flare";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/sharkdp/purescript-flare.git";
        rev = "v5.0.0";
        sha256 = "1qg42p0smz1c0y08w24222pn7ypv0dpl7spb876kqh3a7kx0r7k5";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "foldable-traversable" = pkgs.stdenv.mkDerivation {
      name = "foldable-traversable";
      version = "v4.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foldable-traversable.git";
        rev = "v4.1.1";
        sha256 = "03x89xcmphckzjlp4chc7swrpw347ll5bvr2yp7x9v2jqw2jlyi1";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "foreign" = pkgs.stdenv.mkDerivation {
      name = "foreign";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foreign.git";
        rev = "v5.0.0";
        sha256 = "15mz2s4f8crkd721z4df2aag4s0wil6fs07cpcmi7dpnkn7a4nab";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "foreign-generic" = pkgs.stdenv.mkDerivation {
      name = "foreign-generic";
      version = "v8.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/paf31/purescript-foreign-generic.git";
        rev = "v8.0.0";
        sha256 = "1iwra12xdkg8212z1hn0mw0c3lq0yv8p5p95b50a0l2hgh02nw27";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "foreign-object" = pkgs.stdenv.mkDerivation {
      name = "foreign-object";
      version = "v1.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foreign-object.git";
        rev = "v1.1.0";
        sha256 = "1hfizk13wgn8yxyry9xfilzgf2zjk0yljzljkcpp4ssjj1al907g";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "free" = pkgs.stdenv.mkDerivation {
      name = "free";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-free.git";
        rev = "v5.1.0";
        sha256 = "0f093s5wvbip547fyfxyjqn60z5ay11hs9vzw1h480307vpzq3wm";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "functions" = pkgs.stdenv.mkDerivation {
      name = "functions";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-functions.git";
        rev = "v4.0.0";
        sha256 = "0675k5kxxwdvsjs6a3is8pwm3hmv0vbcza1b8ls10gymmfz3k3hj";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "functors" = pkgs.stdenv.mkDerivation {
      name = "functors";
      version = "v3.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-functors.git";
        rev = "v3.1.1";
        sha256 = "1cnn3zhk6qcyiwbbpvrdqgsbch4qihx2y9d6sv45bvd8kzrbd306";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "gen" = pkgs.stdenv.mkDerivation {
      name = "gen";
      version = "v2.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-gen.git";
        rev = "v2.1.0";
        sha256 = "0ddsfb6a23rahkw9d3ymp2sf6d6vxndj73y61cdv74zrlr2nx74p";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "generics-rep" = pkgs.stdenv.mkDerivation {
      name = "generics-rep";
      version = "v6.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-generics-rep.git";
        rev = "v6.1.1";
        sha256 = "15vchzbcvf6byks90q14lvcwb8hnxqzm2mrlxi7v1f7has4s74kn";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "globals" = pkgs.stdenv.mkDerivation {
      name = "globals";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-globals.git";
        rev = "v4.0.0";
        sha256 = "150mc0kv0cb5fkx0szicwczjr54bglmlyaynj2grf1r4gnjg967s";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "identity" = pkgs.stdenv.mkDerivation {
      name = "identity";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-identity.git";
        rev = "v4.1.0";
        sha256 = "1scdgbfkphfmapw7p9rnsiynpmqzyvnal2glzj450q51f8g1dhld";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "integers" = pkgs.stdenv.mkDerivation {
      name = "integers";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-integers.git";
        rev = "v4.0.0";
        sha256 = "17d4bfpnrmbxlc7hhhrvnli70ydaqyr26zgvc9q52a76zgdcb4cf";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "invariant" = pkgs.stdenv.mkDerivation {
      name = "invariant";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-invariant.git";
        rev = "v4.1.0";
        sha256 = "1fimpbh3yb7clvqxcdf4yf9im64z0v2s9pbspfacgq5b4vshjas9";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "js-date" = pkgs.stdenv.mkDerivation {
      name = "js-date";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-js-date.git";
        rev = "v6.0.0";
        sha256 = "19qyzbr4a1ca8znbd8gcbz0a801f5p1v238ky3408gdx4ba32zjd";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "js-timers" = pkgs.stdenv.mkDerivation {
      name = "js-timers";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-js-timers.git";
        rev = "v4.0.1";
        sha256 = "1a8092sli7zqw1wflibhjza1ww21dxl7x7r602iazzwh2g70v4dg";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "lazy" = pkgs.stdenv.mkDerivation {
      name = "lazy";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lazy.git";
        rev = "v4.0.0";
        sha256 = "156q89l4nvvn14imbhp6xvvm82q7kqh1pyndmldmnkhiqyr84vqv";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "lcg" = pkgs.stdenv.mkDerivation {
      name = "lcg";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lcg.git";
        rev = "v2.0.0";
        sha256 = "1851cq2g84jzjbvbmncbivbhaqzj9zv5ni3gs14k04nmx2brxmvj";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "linear-algebra" = pkgs.stdenv.mkDerivation {
      name = "linear-algebra";
      version = "v0.5.0";
      src = pkgs.fetchgit {
        url = "https://github.com/klangner/purescript-linear-algebra.git";
        rev = "v0.5.0";
        sha256 = "1q2wkh7bqbl3p77ahzxbl03dz5zs3wj8pn2nc2li3xyb7bw7chnm";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "lists" = pkgs.stdenv.mkDerivation {
      name = "lists";
      version = "v5.4.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lists.git";
        rev = "v5.4.0";
        sha256 = "044qr0812wh6a5vz2snm5rrpav3jnykp75acri56qxih1gd3rqla";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "math" = pkgs.stdenv.mkDerivation {
      name = "math";
      version = "v2.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-math.git";
        rev = "v2.1.1";
        sha256 = "1msmy9w7y6fij62sdc55w68gpwkhm6lhgc8qjisjk4sxx1wdg1rr";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "maybe" = pkgs.stdenv.mkDerivation {
      name = "maybe";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-maybe.git";
        rev = "v4.0.1";
        sha256 = "073wa0d51daxdwacfcxg5pf6ch63n4ii55xm1ih87bxrg8mp52mx";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "newtype" = pkgs.stdenv.mkDerivation {
      name = "newtype";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-newtype.git";
        rev = "v3.0.0";
        sha256 = "0qvk9p41miy806b05b4ikbr3if0fcyc35gfrwd2mflcxxp46011c";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "node-buffer" = pkgs.stdenv.mkDerivation {
      name = "node-buffer";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-buffer.git";
        rev = "v5.0.0";
        sha256 = "0ih2y29srdxgn526fw2v1y95hpivjil44vkl93w6nrqsymki36y0";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "node-fs" = pkgs.stdenv.mkDerivation {
      name = "node-fs";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-fs.git";
        rev = "v5.0.0";
        sha256 = "1hkg8j4zkyq71g2bn3vpfqb8x49rdd9k0ayv3zf6l8k80gp3qigx";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "node-path" = pkgs.stdenv.mkDerivation {
      name = "node-path";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-path.git";
        rev = "v3.0.0";
        sha256 = "0j1ni52m62dpcrfakl1ik131i31bkg91yv0p1c40sdw0f59fzf6x";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "node-streams" = pkgs.stdenv.mkDerivation {
      name = "node-streams";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-streams.git";
        rev = "v4.0.0";
        sha256 = "098wdq0rj4nkc470fwmiaars7vxac9n1dh4d82jrji3m77n473da";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "nonempty" = pkgs.stdenv.mkDerivation {
      name = "nonempty";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-nonempty.git";
        rev = "v5.0.0";
        sha256 = "1vz174sg32cqrp52nwb2vz9frrzmdwzzlgl4vc2cm5wlf2anirxj";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "now" = pkgs.stdenv.mkDerivation {
      name = "now";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-now.git";
        rev = "v4.0.0";
        sha256 = "18h5pif2dy4r7w1xg2zw4mvdqlar9xqp3rawkiavmsc946ncf3zs";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "nullable" = pkgs.stdenv.mkDerivation {
      name = "nullable";
      version = "v4.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-nullable.git";
        rev = "v4.1.1";
        sha256 = "14qaxxga8gqlr4pijyvqycdyhjr6qpz3h4aarficw5j75b7x8nyv";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "ordered-collections" = pkgs.stdenv.mkDerivation {
      name = "ordered-collections";
      version = "v1.6.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-ordered-collections.git";
        rev = "v1.6.0";
        sha256 = "1drifyf25g8nclzmq40npm25av8slrr4l4lvlz9f0j07r893ir61";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "orders" = pkgs.stdenv.mkDerivation {
      name = "orders";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-orders.git";
        rev = "v4.0.0";
        sha256 = "13p1sm4dxkmxhld9x5qqg62iiajjb3qpzs66c1r24y5fs4zsfry4";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "p5" = pkgs.stdenv.mkDerivation {
      name = "p5";
      version = "v0.8.0";
      src = pkgs.fetchgit {
        url = "https://github.com/parenparen/purescript-p5.git";
        rev = "v0.8.0";
        sha256 = "19h5dk9jlps3ibhzx0v53ml3r6462m6dlyi9wi0zf9hm0iwcgpwp";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "parallel" = pkgs.stdenv.mkDerivation {
      name = "parallel";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-parallel.git";
        rev = "v4.0.0";
        sha256 = "1d5bnagabw2k8yxywkygwrpblb2ggqh2fhpqfrx2sj1y53x332hg";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "partial" = pkgs.stdenv.mkDerivation {
      name = "partial";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-partial.git";
        rev = "v2.0.0";
        sha256 = "0nw5989ydin2d12b97ch4pdynxkq91xpj7yym5gpd5fpbgy36mdi";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "prelude" = pkgs.stdenv.mkDerivation {
      name = "prelude";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-prelude.git";
        rev = "v4.1.0";
        sha256 = "1pwqhsba4xyywfflma5rfqzqac1vmybwq7p3wkm4wsackvbn34h5";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "profunctor" = pkgs.stdenv.mkDerivation {
      name = "profunctor";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-profunctor.git";
        rev = "v4.0.0";
        sha256 = "1v4kvmhmiwznd4lswp9339h64pgv5zvd3vm1q7gzj70767a3941i";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "profunctor-lenses" = pkgs.stdenv.mkDerivation {
      name = "profunctor-lenses";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-profunctor-lenses.git";
        rev = "v5.0.0";
        sha256 = "10by0cbz68mqbdwhbrzp1lvyyg72lp636lpb4vczq0fxvw3q5v9x";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "proxy" = pkgs.stdenv.mkDerivation {
      name = "proxy";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-proxy.git";
        rev = "v3.0.0";
        sha256 = "0rqf25b1n9p5sgx7gdsxwrfv9rb3sqxgqmqpp5kdm30lfk7snz24";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "psci-support" = pkgs.stdenv.mkDerivation {
      name = "psci-support";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-psci-support.git";
        rev = "v4.0.0";
        sha256 = "0jd773zcklr6hjddqingzmk20a0afpm2r9pczfjbj0d93pbxb4xh";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "quickcheck" = pkgs.stdenv.mkDerivation {
      name = "quickcheck";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-quickcheck.git";
        rev = "v5.0.0";
        sha256 = "18j0qg1xz7vzr7lscg2nn59i2cn3f3cf0pazbi3ka7nf4i1k1b1s";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "random" = pkgs.stdenv.mkDerivation {
      name = "random";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-random.git";
        rev = "v4.0.0";
        sha256 = "0k37v7z529adx6ypxj0xjyfrz45qia6p0vki2wpvi8lik7c698gf";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "record" = pkgs.stdenv.mkDerivation {
      name = "record";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-record.git";
        rev = "v1.0.0";
        sha256 = "1vx6qlcg8x8cij3jsf52gqnd1dvam36pw83x4sad1ddir2s5h0i8";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "refs" = pkgs.stdenv.mkDerivation {
      name = "refs";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-refs.git";
        rev = "v4.1.0";
        sha256 = "08161iy1xbafzblv033v84156azpcqkiw9v9d6gliphrq5fm17gm";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "signal" = pkgs.stdenv.mkDerivation {
      name = "signal";
      version = "v10.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/bodil/purescript-signal.git";
        rev = "v10.1.0";
        sha256 = "03jkc6l01vv28drg1rjx9ymc2bndx9933ckf60bi4ar909kx64v5";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "smolder" = pkgs.stdenv.mkDerivation {
      name = "smolder";
      version = "v11.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/bodil/purescript-smolder.git";
        rev = "v11.0.1";
        sha256 = "0qgzf1dlmhqn4c1dcj6rk1hgzda0xls3lknmrcyvvvdlc39snqk1";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "st" = pkgs.stdenv.mkDerivation {
      name = "st";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-st.git";
        rev = "v4.0.0";
        sha256 = "0m2jkb9dmpbr8s1c20l7sm2q11y5rx8gqfiyspnyhq5apzkknjr0";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "strings" = pkgs.stdenv.mkDerivation {
      name = "strings";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-strings.git";
        rev = "v4.0.1";
        sha256 = "147l3l3fzn7liparhm2y3p8j4ck1lblra5j493p2hy5yy5ndznc8";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "stringutils" = pkgs.stdenv.mkDerivation {
      name = "stringutils";
      version = "v0.0.8";
      src = pkgs.fetchgit {
        url = "https://github.com/menelaos/purescript-stringutils.git";
        rev = "v0.0.8";
        sha256 = "1jvg1cssl6cslvnsp1z9i9rnriz172g9w535wyh74wbm0w4zd79f";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "tailrec" = pkgs.stdenv.mkDerivation {
      name = "tailrec";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-tailrec.git";
        rev = "v4.0.0";
        sha256 = "0z7k80nl8dgv8mc2w8xsl2n0637bd1l8ppxak8kaifgjjwa81hx3";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "transformers" = pkgs.stdenv.mkDerivation {
      name = "transformers";
      version = "v4.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-transformers.git";
        rev = "v4.2.0";
        sha256 = "03qmvl9s7lbvm73dy9ps6qp39pdcm91hb8yakgj7aq8hgpj7b6bg";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "tuples" = pkgs.stdenv.mkDerivation {
      name = "tuples";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-tuples.git";
        rev = "v5.1.0";
        sha256 = "045nsy0r2g51gih0wjhcvhl6gfr8947mlrqwg644ygz72rjm8wq4";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "type-equality" = pkgs.stdenv.mkDerivation {
      name = "type-equality";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-type-equality.git";
        rev = "v3.0.0";
        sha256 = "1b7szyca5s96gaawvgwrw7fa8r7gqsdff7xhz3vvngnylv2scl3w";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "typelevel-prelude" = pkgs.stdenv.mkDerivation {
      name = "typelevel-prelude";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-typelevel-prelude.git";
        rev = "v4.0.0";
        sha256 = "0j59ibdzbs555pjqmh6v4rampg733kvnicpwmyyrp1xrq8vz9a97";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "unfoldable" = pkgs.stdenv.mkDerivation {
      name = "unfoldable";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-unfoldable.git";
        rev = "v4.0.0";
        sha256 = "077vl30j3pxr3zw6cw7wd0vi22j92j8va15r26rn53wzbzcgr41j";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "unsafe-coerce" = pkgs.stdenv.mkDerivation {
      name = "unsafe-coerce";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-unsafe-coerce.git";
        rev = "v4.0.0";
        sha256 = "0k9255mk2mz6xjb11pwkgfcblmmyvr86ig5kr92jwy95xim09zip";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "web-dom" = pkgs.stdenv.mkDerivation {
      name = "web-dom";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-dom.git";
        rev = "v2.0.0";
        sha256 = "17vcby3mldgzbskp1113krdw9a7z5vc83200685s4sfbf7rhc6w6";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };

    "web-events" = pkgs.stdenv.mkDerivation {
      name = "web-events";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-events.git";
        rev = "v2.0.0";
        sha256 = "0z2q9zjjyx3djlkbdhhnhb1aw175rgklb5ylf3rnk9makbfk1rnf";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };
};

in {
  inherit inputs;

  set = "190319";
  source = "https://github.com/cdodev/spacchetti.git";
}
