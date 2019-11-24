/**
 * release-postinstall.js
 *
 * XXX: We want to keep this script installable at least with node 4.x.
 *
 * This script is bundled with the `npm` package and executed on release.
 * Since we have a 'fat' NPM package (with all platform binaries bundled),
 * this postinstall script extracts them and puts the current platform's
 * bits in the right place.
 */

var path = require("path");
var cp = require("child_process");
var fs = require("fs");
var os = require("os");
var platform = process.platform;

var packageJson = require("./package.json");
var binariesToCopy = Object.keys(packageJson.bin)
  .map(function(name) {
    return packageJson.bin[name];
  })
  .concat(["esyInstallRelease.js"]);
var foldersToCopy = ["bin", "_export"];

function copyRecursive(srcDir, dstDir) {
  var results = [];
  var list = fs.readdirSync(srcDir);
  var src, dst;
  list.forEach(function(file) {
    src = path.join(srcDir, file);
    dst = path.join(dstDir, file);

    var stat = fs.statSync(src);
    if (stat && stat.isDirectory()) {
      try {
        fs.mkdirSync(dst);
      } catch (e) {
        console.log("directory already exists: " + dst);
        console.error(e);
      }
      results = results.concat(copyRecursive(src, dst));
    } else {
      try {
        fs.writeFileSync(dst, fs.readFileSync(src));
      } catch (e) {
        console.log("could't copy file: " + dst);
        console.error(e);
      }
      results.push(src);
    }
  });
  return results;
}

/**
 * Since os.arch returns node binary's target arch, not
 * the system arch.
 * Credits: https://github.com/feross/arch/blob/af080ff61346315559451715c5393d8e86a6d33c/index.js#L10-L58
 */

function arch() {
  /**
   * The running binary is 64-bit, so the OS is clearly 64-bit.
   */
  if (process.arch === "x64") {
    return "x64";
  }

  /**
   * All recent versions of Mac OS are 64-bit.
   */
  if (process.platform === "darwin") {
    return "x64";
  }

  /**
   * On Windows, the most reliable way to detect a 64-bit OS from within a 32-bit
   * app is based on the presence of a WOW64 file: %SystemRoot%\SysNative.
   * See: https://twitter.com/feross/status/776949077208510464
   */
  if (process.platform === "win32") {
    var useEnv = false;
    try {
      useEnv = !!(
        process.env.SYSTEMROOT && fs.statSync(process.env.SYSTEMROOT)
      );
    } catch (err) {}

    var sysRoot = useEnv ? process.env.SYSTEMROOT : "C:\\Windows";

    // If %SystemRoot%\SysNative exists, we are in a WOW64 FS Redirected application.
    var isWOW64 = false;
    try {
      isWOW64 = !!fs.statSync(path.join(sysRoot, "sysnative"));
    } catch (err) {}

    return isWOW64 ? "x64" : "x86";
  }

  /**
   * On Linux, use the `getconf` command to get the architecture.
   */
  if (process.platform === "linux") {
    var output = cp.execSync("getconf LONG_BIT", { encoding: "utf8" });
    return output === "64\n" ? "x64" : "x86";
  }

  /**
   * If none of the above, assume the architecture is 32-bit.
   */
  return "x86";
}

// implementing it b/c we don't want to depend on fs.copyFileSync which appears
// only in node@8.x
function copyFileSync(sourcePath, destPath) {
  var data;
  try {
    data = fs.readFileSync(sourcePath);
  } catch (e) {
    console.log("Couldn't find " + sourcePath + " trying with .exe");
    data = fs.readFileSync(sourcePath + ".exe");
    sourcePath = sourcePath + ".exe";
  }
  var stat = fs.statSync(sourcePath);
  fs.writeFileSync(destPath, data);
  fs.chmodSync(destPath, stat.mode);
}

var copyPlatformBinaries = platformPath => {
  var platformBuildPath = path.join(__dirname, "platform-" + platformPath);

  foldersToCopy.forEach(folderPath => {
    var sourcePath = path.join(platformBuildPath, folderPath);
    var destPath = path.join(__dirname, folderPath);

    copyRecursive(sourcePath, destPath);
  });

  binariesToCopy.forEach(binaryPath => {
    var sourcePath = path.join(platformBuildPath, binaryPath);
    var destPath = path.join(__dirname, binaryPath);
    if (fs.existsSync(destPath)) {
      fs.unlinkSync(destPath);
    }
    copyFileSync(sourcePath, destPath);
    fs.chmodSync(destPath, 0777);
  });
};

try {
  fs.mkdirSync("_export");
} catch (e) {
  console.log("Could not create _export folder");
}

switch (platform) {
  case "win32":
    if (arch() !== "x64") {
      console.warn("error: x86 is currently not supported on Windows");
      process.exit(1);
    }

    copyPlatformBinaries("windows-x64");
    break;
  case "linux":
  case "darwin":
    copyPlatformBinaries(platform);
    break;
  default:
    console.warn("error: no release built for the " + platform + " platform");
    process.exit(1);
}

require("./esyInstallRelease");
