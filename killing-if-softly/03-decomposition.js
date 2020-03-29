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

const active = (account) => account.active ?
    null : { success: false, reason: 'inactive' };

const year = (account) => account.created.getFullYear() > 2000 ?
    null : { success: false, reason: 'legacy account' };

const balance = (account) => account.balance > 100000 ?
    { success: true, reason: 'eligible for promotion' } :
    { success: false, reason: 'not eligible' };

const eligibleForPromotion = (account) => {
    let result;
    [active, year, balance]
        .find(action => { result = action(account); return result != null });
    return result;
}

console.log(eligibleForPromotion(account))
