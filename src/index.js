'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// Initialize Firebase
firebase.initializeApp(require('../firebase.config'));
var database = firebase.database();
var formsRef = database.ref('forms/');
var usersRef = database.ref('users/');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);

// The FirebaseUI Widget.
var ui = null;

// Get all forms from Firebase
app.ports.getAllPublicForms.subscribe(function () {
    formsRef.once('value').then(function (snapshot) {
        var list = getAsList(snapshot.val());
        app.ports.gotAllPublicForms.send(list);
    })
});

// // Get forms from Firebase
app.ports.getForm.subscribe(function (formId) {
    var formRef = database.ref('forms/' + formIf);
    formRef.limitToFirst(1).once('value').then(function (snapshot) {
        var form = getFirstObject(snapshot.val());
        app.ports.gotForm.send(form);
    })
});

// Get user data from Firebase
app.ports.getAllPublicForms.subscribe(function (userId) {
    var userId = firebase.auth().currentUser.uid;
    return firebase.database().ref('/users/' + userId).once('value');
});

// Save user data on Firebase
app.ports.getAllPublicForms.subscribe(function (user) {
    console.log(user);
    firebase.database().ref('users/' + userId).set({
        id: user.userId,
        email: user.email,
        displayName: user.displayName,
        imageUrl : user.imageUrl,
        userData: {
            createdAt: user.createdAt,
            updatedAt: user.updatedAt,
            orgName: user.orgName,
            stripeAccount: user.stripeAccount
        }
    });
});

function getAsList(resultObject) {
    if (resultObject instanceof Array) {
        // TODO, stoppa in index som id i objektet
        return resultObject
    }
    else if (resultObject instanceof Object) {
        return $.map(resultObject, function(value, index) {
            value.id = index;
            return [value];
        });
    }
}

function getFirstObject(resultObject) {
    if (resultObject instanceof Array) {
        for (var i = 0; i< resultObject.length; i++) {
            var resultItem = resultObject[i];
            if (resultItem != undefined) {
                return resultItem;
            }
        }
    }
    else if (resultObject instanceof Object) {
        for(var key in resultObject){
            if (resultObject[key] != undefined) {
                return resultObject[key];
            }
        }
    }
}

app.ports.logOut.subscribe(() => {
    console.log("Loggar ut användaren")
    firebase.auth().signOut()
})


app.ports.startAuthUI.subscribe(() => {
    // console.log("Kör startAuthUI")

    // FirebaseUI config.
        var uiConfig = {
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
        };

    if (ui) {
        // console.log("Kör startAuthUI show");
        ui.reset();
    } else {
        // console.log("Kör startAuthUI skapa nytt")
        // Initialize the FirebaseUI Widget using Firebase.
        ui = new firebaseui.auth.AuthUI(firebase.auth());
    }

    requestAnimationFrame(function(){
        // The start method will wait until the DOM is loaded.
        ui.start('#firebaseui-auth-container', uiConfig);
    });
})


// Port that deletes the firebaseui widget
// app.ports.deleteFBUI.subscribe(() => {
//     if (ui) {
//         $(".firebaseui-container").hide();
//     } else { console.log("No UI to delete.") }
// })


firebase.auth().onAuthStateChanged(function(user) {
    // console.log("onAuthStateChanged user:", user)
    if (user) {
        var boxyUser = {
            "id" : user.uid
            , "email" : user.email
            , "displayName" : user.displayName
            , "imageUrl" : user.photoURL
            // , "createdAt" : "?"
            // , "updatedAt" : "?"
        }
        // console.log("boxy user:", boxyUser);
        // console.log("Firebase user:",user)
    
        app.ports.receiveUser.send(boxyUser)
    } else {
        // User is signed out.
        console.log("User signed out")
        app.ports.userLoggedOut.send(null)
    }
}, function(error) {
    console.log(error);
});
    