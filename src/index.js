'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// Initialize Firebase
firebase.initializeApp(require('../firebase.config'));
var database = firebase.database();

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);

// Get events from Firebase
app.ports.getEvents.subscribe(function () {
    // console.log("using ports..");
    var eventsRef = database.ref('events/');
    eventsRef.once('value').then(function (snapshot) {
        // console.log("Now somethin came from Firebase!");
        app.ports.gotEventList.send(snapshot.val());
    })
});

// Get events from Firebase
app.ports.getForm.subscribe(function (formId) {
    // console.log("using ports..");
    var eventsRef = database.ref('forms/');
    eventsRef.orderByChild("formId").equalTo(formId).once('value').then(function (snapshot) {
        // console.log("Now somethin came from Firebase!");
        app.ports.gotForm.send(snapshot.val());
    })
});