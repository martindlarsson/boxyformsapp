'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// Initialize Firebase
firebase.initializeApp(require('../firebase.config'));
var database = firebase.database();
var formRef = database.ref('forms/');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);

// Get all forms from Firebase
app.ports.getAllPublicForms.subscribe(function () {
    formRef.once('value').then(function (snapshot) {
        var list = getAsList(snapshot.val());
        app.ports.gotAllPublicForms.send(list);
    })
});

// // Get forms from Firebase
app.ports.getForm.subscribe(function (formId) {
    formRef.orderByChild("formId").equalTo(formId).limitToFirst(1).once('value').then(function (snapshot) {
        var form = getFirstObject(snapshot.val());
        app.ports.gotForm.send(form);
    })
});

function getAsList(resultObject) {
    if (resultObject instanceof Array) {
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