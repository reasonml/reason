include [%form
  type input = {name: string};
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
    },
  }
];

