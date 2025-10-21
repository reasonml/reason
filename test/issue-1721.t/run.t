Format issue #1721 - Inline jsx with function expressions (single arg callback)
  $ refmt ./input.re | tee formatted.re
  <input
    onChange={self.reduce(event => {
      let text =
        ReactDOMRe.domElementToObj(
          ReactEventRe.Form.target(event),
        )##value;
      Search(text);
    })}
    value=self.state.search
  />

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

