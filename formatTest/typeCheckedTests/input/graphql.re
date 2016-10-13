/* TODO: add types for queries below here */

/**
* - the queries should type check against normal types.
* - but should also result in well-formed strings
* - bummer: we must supply the type of fields
*/

query basic => {
    hero {
        name: string
    },
    droid {
        name: string
    }
};

query nested => {
    nested1 {
        nestedB {
            title: string
        }
    },
    nested2 {
        nestedC {
            id: int
        }
    }
};

query listTest => {
    test {
        listTest: list string
    }
};

query alias => {
    something : foo {
        title: string
    }
};

query optionTest => {
    foo2 {
        x: option string
    }
};

query arguments a b => {
    args (a, b: b, c: 1, d: "else", x: 1) {
        z: string
    }
};

/*
TODO:
- fragments
- inline fragments
*/