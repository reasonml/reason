Format extensions in modules

  $ refmt <<EOF
  > [%%toplevelExtension "payload"];
  > module X = {
  >   /* No payload */
  >   [%%someExtension];
  >   [%%someExtension "payload"];
  > };
  > EOF
  [%%toplevelExtension "payload"];
  module X = {
    /* No payload */
    [%%someExtension];
    [%%someExtension "payload"];
  };

