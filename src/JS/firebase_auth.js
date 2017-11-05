function getUiConfig() {
    return {
        signInSuccessUrl: '/#/', // TODO, ta in parameter för detta
        signInFlow: 'popup',
        signInOptions: [
            {
                provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
                scopes: [
                    'https://www.googleapis.com/auth/plus.login'
                ],
                customParameters: {
                    // Forces account selection even when one account
                    // is available.
                    prompt: 'select_account'
                }
            },
            firebase.auth.EmailAuthProvider.PROVIDER_ID // Other providers don't need to be given as object.
        ],
        // Terms of service url.
        tosUrl: '<your-tos-url>'
    }
}