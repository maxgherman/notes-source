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
    if (account.active) {
        if (account.created.getFullYear() > 2000) {
            if (account.balance > 100000) {
                console.log('eligible for promotion');
            } else {
                console.log('not eligible');
            }
        } else {
            console.log('legacy account');
        }
    } else {
        console.log('inactive');
    }
}

eligibleForPromotion(account)
