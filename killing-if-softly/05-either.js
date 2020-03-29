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

const resultMap = right(account)
    .map(account =>
        account.active ?
            right(account) :
            left({ success: false, reason: 'inactive' })
    )
    .map(item =>
        item.map(value => value.created.getFullYear() > 2000 ?
            right(value) :
            left({ success: false, reason: 'legacy account' })))
    .map(item =>
        item.map(item => item.map(value =>
            value.balance > 100000 ?
                right({ success: true, reason: 'eligible for promotion' }) :
                left({ success: false, reason: 'not eligible' })
        )));

console.log(resultMap.value?.value?.value?.value)

const resultChain = right(account)
    .chain(account =>
        account.active ?
            right(account) :
            left({ success: false, reason: 'inactive' })
    )
    .chain(item => item.created.getFullYear() > 2000 ?
        right(item) :
        left({ success: false, reason: 'legacy account' }))
    .chain(item =>
        item.balance > 100000 ?
            right({ success: true, reason: 'eligible for promotion' }) :
            left({ success: false, reason: 'not eligible' })
    );

console.log(resultChain.value)
