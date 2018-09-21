# Fetch licenses for nuget packages

Searches a folder recursively for packages.config files and fetches license urls from the nuget package metadata. Then tries simplistically to parse some common license types from the license text.

## Usage
1. Run paket install
   - `mono .paket/paket.exe install` on mac or linux
   - `.paket/paket.exe install` on windows
1. Run the script providing a root folder
   - `fsharpi ./FetchNugetLicenses.fsx <root-folder>` on mac or linux
   - `fsi FetchNugetLicenses.fsx <root-folder>` on windows
1. Profit! (or non-profit)