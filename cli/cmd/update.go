package cmd

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/spf13/cobra"
	"the0/internal"
	"the0/internal/logger"
)

func NewUpdateCmd(currentVersion string) *cobra.Command {
	var checkOnly bool
	var yes bool

	cmd := &cobra.Command{
		Use:   "update",
		Short: "Update the0 CLI to the latest version",
		Long:  "Check for and install the latest version of the0 CLI from GitHub Releases",
		Run: func(cmd *cobra.Command, args []string) {
			runUpdate(currentVersion, checkOnly, yes)
		},
	}

	cmd.Flags().BoolVar(&checkOnly, "check", false, "Only check for updates, don't install")
	cmd.Flags().BoolVarP(&yes, "yes", "y", false, "Skip confirmation prompt")

	return cmd
}

func runUpdate(currentVersion string, checkOnly bool, yes bool) {
	updater := internal.NewUpdater(currentVersion)

	logger.StartSpinner("Checking for updates")

	release, err := updater.CheckLatestRelease()
	if err != nil {
		logger.StopSpinnerWithError("Failed to check for updates")
		logger.Error("%v", err)
		os.Exit(1)
	}

	latestVersion := internal.ExtractVersionFromTag(release.TagName)

	isNewer, err := internal.CompareVersions(currentVersion, latestVersion)
	if err != nil {
		logger.StopSpinnerWithError("Failed to compare versions")
		logger.Error("%v", err)
		os.Exit(1)
	}

	// Update the cache regardless of outcome
	_ = internal.WriteVersionCache(&internal.VersionCache{
		LastCheck:     time.Now(),
		LatestVersion: latestVersion,
	})

	if !isNewer {
		logger.StopSpinnerWithSuccess("Already up to date")
		logger.Print("  Current version: %s", internal.NormalizeVersion(currentVersion))
		return
	}

	logger.StopSpinner()
	logger.Print("  Update available: %s -> %s", internal.NormalizeVersion(currentVersion), latestVersion)

	if checkOnly {
		logger.Print("  Run 'the0 update' to install")
		return
	}

	// Confirm unless --yes
	if !yes {
		logger.Printf("  Install update? [y/N] ")
		reader := bufio.NewReader(os.Stdin)
		answer, _ := reader.ReadString('\n')
		answer = strings.TrimSpace(strings.ToLower(answer))
		if answer != "y" && answer != "yes" {
			logger.Print("  Update cancelled")
			return
		}
	}

	// Find the platform binary
	binaryName := internal.GetPlatformBinaryName()
	binaryAsset := internal.FindAssetByName(release, binaryName)
	if binaryAsset == nil {
		logger.Error("No binary found for your platform (%s)", binaryName)
		logger.Print("  Available assets:")
		for _, asset := range release.Assets {
			logger.Print("    - %s", asset.Name)
		}
		os.Exit(1)
	}

	// Find checksums file
	checksumAsset := internal.FindAssetByName(release, internal.ChecksumsFileName)

	// Download binary
	logger.StartSpinner("Downloading " + binaryName)

	binaryData, err := updater.DownloadAsset(binaryAsset.BrowserDownloadURL)
	if err != nil {
		logger.StopSpinnerWithError("Download failed")
		logger.Error("%v", err)
		os.Exit(1)
	}

	logger.StopSpinnerWithSuccess(fmt.Sprintf("Downloaded %s (%.1f MB)", binaryName, float64(len(binaryData))/(1024*1024)))

	// Verify checksum if available
	if checksumAsset == nil {
		logger.Print("  Warning: no checksum file found in release, skipping integrity verification")
	} else {
		logger.StartSpinner("Verifying checksum")

		checksumData, err := updater.DownloadAsset(checksumAsset.BrowserDownloadURL)
		if err != nil {
			logger.StopSpinnerWithError("Failed to download checksums")
			logger.Error("%v", err)
			os.Exit(1)
		}

		if err := internal.VerifyChecksum(binaryData, checksumData, binaryName); err != nil {
			logger.StopSpinnerWithError("Checksum verification failed")
			logger.Error("%v", err)
			os.Exit(1)
		}

		logger.StopSpinnerWithSuccess("Checksum verified")
	}

	// Replace binary
	logger.StartSpinner("Installing update")

	if err := internal.ReplaceBinary(binaryData); err != nil {
		logger.StopSpinnerWithError("Installation failed")
		logger.Error("%v", err)
		logger.Print("  You may need to run with elevated permissions")
		os.Exit(1)
	}

	logger.StopSpinnerWithSuccess(fmt.Sprintf("Updated to %s", latestVersion))
}
