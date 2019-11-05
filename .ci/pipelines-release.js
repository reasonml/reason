const fs = require("fs");
const path = require("path");

console.log("Creating package.json");

// From the project root pwd
const mainPackageJsonPath =
  fs.existsSync('esy.json') ?
  'esy.json' : 'package.json';

const exists = fs.existsSync(mainPackageJsonPath);
if (!exists) {
  console.error("No package.json or esy.json at " + mainPackageJsonPath);
  process.exit(1);
}
// Now require from this script's location.
const mainPackageJson = require(path.join('..', mainPackageJsonPath));
const bins =
  Array.isArray(mainPackageJson.esy.release.bin) ?
  mainPackageJson.esy.release.bin.reduce(
    (acc, curr) => Object.assign({ [curr]: "bin/" + curr }, acc),
    {}
  ) :
  Object.keys(mainPackageJson.esy.release.bin).reduce(
    (acc, currKey) => Object.assign({ [currKey]: "bin/" + mainPackageJson.esy.release.bin[currKey] }, acc),
    {}
  );

const rewritePrefix =
  mainPackageJson.esy &&
  mainPackageJson.esy.release &&
  mainPackageJson.esy.release.rewritePrefix;

const packageJson = JSON.stringify(
  {
    name: mainPackageJson.name,
    version: mainPackageJson.version,
    license: mainPackageJson.license,
    description: mainPackageJson.description,
    repository: mainPackageJson.repository,
    scripts: {
      postinstall:
        rewritePrefix ?
        "ESY_RELEASE_REWRITE_PREFIX=true node ./postinstall.js" :
        "node ./postinstall.js"
    },
    bin: bins,
    files: [
      "_export/",
      "bin/",
      "postinstall.js",
      "esyInstallRelease.js",
      "platform-linux/",
      "platform-darwin/",
      "platform-windows-x64/"
    ]
  },
  null,
  2
);

fs.writeFileSync(
  path.join(__dirname, "..", "_release", "package.json"),
  packageJson,
  {
    encoding: "utf8"
  }
);

try {
  console.log("Copying LICENSE");
  fs.copyFileSync(
    path.join(__dirname, "..", "LICENSE"),
    path.join(__dirname, "..", "_release", "LICENSE")
  );
} catch (e) {
  console.warn("No LICENSE found");
}

console.log("Copying README.md");
fs.copyFileSync(
  path.join(__dirname, "..", "README.md"),
  path.join(__dirname, "..", "_release", "README.md")
);

console.log("Copying postinstall.js");
fs.copyFileSync(
  path.join(__dirname, "release-postinstall.js"),
  path.join(__dirname, "..", "_release", "postinstall.js")
);

console.log("Creating placeholder files");
const placeholderFile = `:; echo "You need to have postinstall enabled"; exit $?
@ECHO OFF
ECHO You need to have postinstall enabled`;
fs.mkdirSync(path.join(__dirname, "..", "_release", "bin"));

Object.keys(bins).forEach(
  name => {
    if(bins[name]) {
      const binPath = path.join(
        __dirname,
        "..",
        "_release",
        bins[name]
      );
      fs.writeFileSync(binPath, placeholderFile);
      fs.chmodSync(binPath, 0777);
    } else {
      console.log("bins[name] name=" + name + " was empty. Weird.");
      console.log(bins);
    }
  }
);
