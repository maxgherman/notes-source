const accounts = [{
    active: true,
    balance: 123000,
    created: new Date('2010-12-14')
}, {
    active: false,
    balance: 0,
    created: new Date('1999-03-17')
}, {
    active: true,
    balance: 200,
    created: new Date('2011-10-01')
}];

const account = accounts[Math.floor(Math.random() * 3)];

const True = (left, _) => left
const False = (_, right) => right
const Boolean = {
    true: True,
    false: False
};

console.log(Boolean[(1 + 1 > 1)](true, false));

const And = (x, y) => x(y, False)  // inclusive or conjunction
const Or = (x, y) => x(True, y)    // exclusive or disjunction

console.log(And(False, True)(true, false))  // false
console.log(And(True, False)(true, false))  // false
console.log(Or(False, True)(true, false))   // true
console.log(Or(True, False)(true, false))   // true


Or(Boolean[1 + 1 > 1], Boolean[1 > 2])(
    () => console.log('left execution branch'),
    () => console.log('right execution branch')
)()

And(Boolean[account.active], True)(
    And(Boolean[account.created.getFullYear() > 2000], True)(
        And(Boolean[account.balance > 100000], True)(
            () => console.log('eligible for discount'),
            () => console.log('not eligible')
        ),
        () => console.log('legacy account')
    ),
    () => console.log('inactive')
)()

const active = (action) => And(Boolean[account.active], True)(
    action, () => console.log('inactive')
)
const year = (action) => And(Boolean[account.created.getFullYear() > 2000], True)(
    action,
    () => console.log('legacy account')
)

const balance = () => And(Boolean[account.balance > 100000], True)(
    () => console.log('eligible for discount'),
    () => console.log('not eligible')
)

active(year(balance()))()
