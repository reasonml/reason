Format issue #2669 - Inlined functions look wrongly formatted
  $ refmt ./input.re | tee formatted.re
  let editor =
    Tiptap.Editor.use(
      ~provider,
      ~document=ydoc,
      ~renderCursor=CollaborationCursor.renderCursor,
      ~user=initialUser,
      ~onCreate=({ editor }: Tiptap.createProps) => {
      onCreateEditor(editor)
    });

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

