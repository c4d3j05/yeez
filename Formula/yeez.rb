class Yeez < Formula
  desc "Terminal UI for S3 object CRUD (brick + amazonka)"
  homepage "https://github.com/c4d3j05/yeez"
  license "BSD-3-Clause"

  # ---------------------------------------------------------------------------
  # Prebuilt binary from a GitHub Release (fast path). Apple Silicon only.
  #
  # The `build` CI workflow attaches yeez-macos-arm64 to the release for each
  # v* tag. After a new release, refresh the sha256 with:
  #
  #   VER=0.1.0
  #   curl -fsSL -o /tmp/yeez-arm64 \
  #     "https://github.com/c4d3j05/yeez/releases/download/v$VER/yeez-macos-arm64"
  #   shasum -a 256 /tmp/yeez-arm64
  # ---------------------------------------------------------------------------
  version "0.1.0"

  on_macos do
    on_arm do
      url "https://github.com/c4d3j05/yeez/releases/download/v0.1.0/yeez-macos-arm64"
      sha256 "7aa0f2a044d70942698dc85417f6685c44e2c968a64de937ad846b4bc06f2745"
    end
    on_intel do
      # No prebuilt Intel binary is published. Build from source instead:
      #   brew install --HEAD c4d3j05/tap/yeez
      odie "yeez ships a prebuilt binary for Apple Silicon only; " \
           "install from source with `brew install --HEAD`."
    end
  end

  # ---------------------------------------------------------------------------
  # Build from source: used by `brew install --HEAD` (and the only option on
  # Intel). Compiles the full amazonka tree, so the first build is slow. The
  # ghc/cabal build deps live inside the head block so they are only pulled in
  # for a source build, never for the prebuilt-binary path.
  # ---------------------------------------------------------------------------
  head do
    url "https://github.com/c4d3j05/yeez.git", branch: "main"
    depends_on "cabal-install" => :build
    depends_on "ghc" => :build
  end

  def install
    if build.head?
      system "cabal", "update"
      system "cabal", "build", "exe:yeez", "--jobs"
      bin.install Utils.safe_popen_read("cabal", "list-bin", "exe:yeez").strip => "yeez"
    else
      # The downloaded release asset is the bare executable; Homebrew drops it
      # into the build dir under its original (arch-suffixed) name.
      bin.install Dir["yeez-macos-*"].first => "yeez"
      (bin/"yeez").chmod 0755
    end
  end

  test do
    assert_predicate bin/"yeez", :executable?
  end
end
