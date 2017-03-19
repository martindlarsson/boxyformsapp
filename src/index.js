'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// Initialize Firebase
firebase.initializeApp(require('../firebase.config'));
var database = firebase.database();
var eventsRef = database.ref('events/');
var formRef = database.ref('forms/');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);

// Get events from Firebase
app.ports.getEvents.subscribe(function () {
    // console.log("using ports..");
    eventsRef.once('value').then(function (snapshot) {
        // console.log("Now somethin came from Firebase!");
        app.ports.gotEventList.send(snapshot.val());
    })
});

// Get forms from Firebase
app.ports.getForm.subscribe(function (formId) {
    formRef.orderByChild("formId").equalTo(formId).limitToFirst(1).once('value').then(function (snapshot) {
        var form = getFirstObject(snapshot.val());
        app.ports.gotForm.send(form);
    })
});

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