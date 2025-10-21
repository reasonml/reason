<input
  onChange=(
    self.reduce(
      (event) => {
        let text = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value;
        Search(text)
      }
    )
  )
  value=self.state.search
/>

