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

const And = (x, y) => x(y, False)  // inclusive or conjunction
const Or = (x, y) => x(True, y)    // exclusive or disjunction

const either = (_value, _isLeft, _isRight) => ({
    get value() {
        return _value
    },

    get isLeft() {
        return _isLeft;
    },

    get isRight() {
        return _isRight;
    },

    map: undefined,

    chain: undefined,

});

const left = (value) => ({
    ...either(value, true, false),
    map() {
        return this;
    },
    chain() {
        return this;
    }
});

const right = (value) => ({
    ...either(value, false, true),
    map(action) {
        return right(action(value));
    },
    chain(action) {
        return action(value);
    }
});

const result = right(account)
    .chain(item =>
        // we can make more complex logical expressions
        And(Boolean[item.active],
            Boolean[new Date().getDate() > 15])(
                right(item),
                left({ success: false, reason: 'inactive' })
            ))
    .chain(item => And(Boolean[item.created.getFullYear() > 2000], True)(
        right(item),
        left({ success: false, reason: 'legacy account' })
    ))
    .chain(item => And(Boolean[item.balance > 100000], True)(
        right({ success: true, reason: 'eligible for promotion' }),
        left({ success: false, reason: 'not eligible' })
    ));

console.log(result.value)
