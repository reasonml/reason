var fmtPath = atom.config.get('ide-reason.pathToReasonfmt');

if (!fmtPath) {
  console.error('You need to set config ide-reason.pathToReasonfmt');
}

module.exports = function() {
  return {
    // Amount to expand source preview in each direction. If -1, shows no
    // preview
    showSourcePreviews: false,
    // The IDE already shows file paths.
    showFileHeaders: false,
    pathToReasonfmt: fmtPath,
    errorPreviewExpand: 5,
    warningPreviewExpand: 2,
    indentCode: 4,
    indentHuman: 0,
    niceifyModuleAliases: true,
    // Ignored for now - in the mean time we conver terminal ansii back
    // to HTML using `ansi_up`.
    renderHtml: true,
    columns: 90
  };
};
