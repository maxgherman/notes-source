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

const eligibleForPromotion = (account) => {
    if (!account.active) {
        return {
            success: false,
            reason: 'inactive'
        };
    }

    if (account.created.getFullYear() <= 2000) {
        return {
            success: false,
            reason: 'legacy account'
        };
    }

    if (account.balance > 100000) {
        return {
            success: true,
            reason: 'eligible for promotion'
        };
    }

    return {
        success: false,
        reason: 'not eligible'
    };
}

console.log(eligibleForPromotion(account))
