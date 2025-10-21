let editor =
    Tiptap.Editor.use(
      ~provider,
      ~document=ydoc,
      ~renderCursor=CollaborationCursor.renderCursor,
      ~user=initialUser,
      ~onCreate=({editor}: Tiptap.createProps) => {
         onCreateEditor(editor)
      }
   );

