'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

require('./firebase_auth.js');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// Initialize Firebase
firebase.initializeApp(require('../firebase.config'));
var db = firebase.firestore();

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);

// The FirebaseUI Widget.
var ui = null;

// Get all forms from Firebase
// app.ports.getAllPublicForms.subscribe(function () {
//     formsRef.once('value').then(function (snapshot) {
//         var list = getAsList(snapshot.val());
//         app.ports.gotAllPublicForms.send(list);
//     })
// });

// // // Get forms from Firebase
// app.ports.getForm.subscribe(function (formId) {
//     var formRef = database.ref('forms/' + formIf);
//     formRef.limitToFirst(1).once('value').then(function (snapshot) {
//         var form = getFirstObject(snapshot.val());
//         app.ports.gotForm.send(form);
//     })
// });

// function getAsList(resultObject) {
//     if (resultObject instanceof Array) {
//         // TODO, stoppa in index som id i objektet
//         return resultObject
//     }
//     else if (resultObject instanceof Object) {
//         return $.map(resultObject, function(value, index) {
//             value.id = index;
//             return [value];
//         });
//     }
// }

// function getFirstObject(resultObject) {
//     if (resultObject instanceof Array) {
//         for (var i = 0; i< resultObject.length; i++) {
//             var resultItem = resultObject[i];
//             if (resultItem != undefined) {
//                 return resultItem;
//             }
//         }
//     }
//     else if (resultObject instanceof Object) {
//         for(var key in resultObject){
//             if (resultObject[key] != undefined) {
//                 return resultObject[key];
//             }
//         }
//     }
// }

// Save user data on Firebase
app.ports.saveUser.subscribe(function (user) {
    // console.log('Sparar användaren: ', user);
    db.collection("users").doc(user.id).set(user)
    .then(function() {
        // console.log("Document successfully written!");
    })
    .catch(function(error) {
        console.error("Error writing document: ", error);
    });
});


// Lyssna på events när användaren loggat in eller ut
firebase.auth().onAuthStateChanged(function(user) {
    user ? handleSignedInUser(user) : handleSignedOutUser();
});




let handleSignedOutUser = function() {
    // User is signed out.
    // console.log("User signed out")
    app.ports.userLoggedOut.send(null)
}


app.ports.logOut.subscribe(() => {
    // console.log("Loggar ut användaren")
    firebase.auth().signOut()
})


app.ports.startAuthUI.subscribe(() => {
    if (ui) {
        // console.log("Kör startAuthUI show");
        ui.reset();
    } else {
        // Initialize the FirebaseUI Widget using Firebase.
        ui = new firebaseui.auth.AuthUI(firebase.auth());
    }

    // Fördröj start för att containern inte hunnit ritas upp
    requestAnimationFrame(function(){
        // The start method will wait until the DOM is loaded.
        ui.start('#firebaseui-auth-container', uiConfig);
    });
})


let handleSignedInUser = function(user) {
    var boxyUser = {
        "id" : user.uid
        , "email" : user.email
        , "displayName" : user.displayName
        , "imageUrl" : user.photoURL
        , "createdAt" : null
        , "updatedAt" : null
        , "orgName" : null
    }

    // Request user from firestore
    var docRef = db.collection("users").doc(user.uid);
    
    docRef.get().then(function(doc) {
        if (doc.exists) {
            // console.log("Document data:", doc.data());
            boxyUser.orgName = doc.data().orgName;
            // TODO Sätt ett nytt datum för updatedAt
            app.ports.userLoggedIn.send(boxyUser);
        } else {
            console.log("Did not find user data");
            // TODO Detta är troligen en nya användare. spara createdAt
            app.ports.userLoggedIn.send(boxyUser);
        }
    }).catch(function(error) {
        console.log("Error getting document:", error);
    });
}


// FirebaseUI config.
let uiConfig =  {
    'callbacks': {
      // Called when the user has been successfully signed in.
      'signInSuccess': function(user, credential, redirectUrl) {
        hideUi();
        // Redirect.
        return true;
      }
    },
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



let hideUi = function() {
    if (ui) {
        // console.log("Kör hideUi");
        ui.reset();
    }
}