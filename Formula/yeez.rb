class Yeez < Formula
  desc "Terminal UI for S3 object CRUD (brick + amazonka)"
  homepage "https://github.com/c4d3j05/yeez"
  license "BSD-3-Clause"

  # ---------------------------------------------------------------------------
  # Prebuilt binary from a GitHub Release (fast path).
  #
  # The `build` CI workflow attaches yeez-macos-arm64 / yeez-macos-x86_64 to
  # the release for each v* tag. Bump the version, then fill in the two
  # sha256 values printed by:
  #
  #   VER=0.1.0
  #   for a in arm64 x86_64; do
  #     curl -fsSL -o /tmp/yeez-$a \
  #       "https://github.com/c4d3j05/yeez/releases/download/v$VER/yeez-macos-$a"
  #     shasum -a 256 /tmp/yeez-$a
  #   done
  # ---------------------------------------------------------------------------
  version "0.1.0"

  on_macos do
    on_arm do
      url "https://github.com/c4d3j05/yeez/releases/download/v0.1.0/yeez-macos-arm64"
      sha256 "REPLACE_WITH_ARM64_SHA256"
    end
    on_intel do
      url "https://github.com/c4d3j05/yeez/releases/download/v0.1.0/yeez-macos-x86_64"
      sha256 "REPLACE_WITH_X86_64_SHA256"
    end
  end

  # ---------------------------------------------------------------------------
  # Build from source: used by `brew install --HEAD` and as a fallback when the
  # release binaries are unavailable. Compiles the full amazonka tree, so the
  # first build is slow.
  # ---------------------------------------------------------------------------
  head "https://github.com/c4d3j05/yeez.git", branch: "main"

  on_head do
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
