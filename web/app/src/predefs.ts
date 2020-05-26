


export class Predef {
    constructor(public readonly name: string, public readonly code: string) {
    }
}


const StackCode = new Predef("Intersection", `
x = rand()
if (x == 3) {
    a = "b"
} else {
    a = "a"
}

if (rand()) {
    b = "b"
} else {
    b = "c"
}

if (a == b) {
    debug(a is "b")
}
`)

export const predefs: Predef[] = [StackCode]
