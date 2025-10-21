  /** raise the same error than if we failed to match */
  (
    if (! Re_pcre.pmatch(~rex=allR, err)) {
      raise(Not_found);
    } else {
      let hintR = {|Syntax error:([\s\S]+)|};
      let hint = get_match_maybe(hintR, err);
      /* assuming on the same row */
      let ((startRow, startColumn), (_, endColumn)) = range;
      File_SyntaxError({
        hint: Helpers.optionMap(String.trim, hint),
        offendingString:
          Helpers.stringSlice(
            ~first=startColumn,
            ~last=endColumn,
            List.nth(cachedContent, startRow),
          ),
      });
    }
  );

