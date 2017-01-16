var binstall = require("binstall");
var path = require("path");
var fs = require("fs");
var packageInfo = require(path.join(__dirname, "package.json"));

// Use major.minor.patch from version string - e.g. "1.2.3" from "1.2.3-alpha"
// var binVersion = packageInfo.version.replace(/^(\d+\.\d+\.\d+).*$/, "$1");

// 'arm', 'ia32', or 'x64'.
var arch = process.arch;

// 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
var operatingSystem = process.platform;

var filename = operatingSystem + "-" + arch + ".tar.gz";
var url = "https://dl.bintray.com/stoeffel/elm-reflection/"
  + "0.2.0/" + filename;

var binariesDir = path.join(__dirname, "binaries");
var packageInfo = require(path.join(__dirname, "package.json"));
var binaryExtension = process.platform === "win32" ? ".exe" : "";
var executablePaths = Object.keys(packageInfo.bin).map(function(executable) {
  return path.join(binariesDir, executable + binaryExtension);
});

binstall(url, {path: binariesDir, strip: 1}, {verbose: true, verify: executablePaths})
  .then(function(successMessage) {
    console.log(successMessage);
  }, function(errorMessage) {
    console.error(errorMessage);
    process.exit(1);
  });
