Format issue #2594 - Long refmt run time with stack overflow
  $ refmt ./input.re | tee formatted.re
  module M1 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };
  
  module M6 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };
  
  module M5 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };
  
  module M4 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };
  
  module M3 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };
  
  module M2 = {
    include X({ let r = x => { z: s => s } });
    let f = (m, x) => x;
    let f_z = (m, x) => x;
    let g = { z: s => s };
    let u = s => { z: s => s };
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

