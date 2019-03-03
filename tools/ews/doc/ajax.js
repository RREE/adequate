/*
 * Copyright 2013, 2014 Simon Wright <simon@pushface.org>
 *
 * This unit is free software; you can redistribute it and/or modify
 * it as you wish. This unit is distributed in the hope that it will
 * be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */


var stateRequest = new OneshotHttpInteraction
  ("state.xml",
   null,
   function (r) {
     var x = r.responseXML;
     var value = x.getElementsByTagName("time-format")[0].firstChild.nodeValue;
     for (var o = document.fTimeFormat.format.options, i = 0;
          i < o.length;
          i++) {
       o[i].selected = (o[i].value == value);
     }
     value = x.getElementsByTagName("forward-light")[0].firstChild.nodeValue;
     for (var o = document.lights.forward, i = 0;
          i < o.length;
          i++) {
       o[i].checked = (o[i].value == value);
     }
     value = x.getElementsByTagName("aft-light")[0].firstChild.nodeValue;
     for (var o = document.lights.aft, i = 0;
          i < o.length;
          i++) {
       o[i].checked = (o[i].value == value);
     }
     var lamps = x.getElementsByTagName("lamp");
     for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
       c[i].checked = lamps[i].firstChild.nodeValue == "true";
     }
   });


var timeRequest = new CyclicHttpInteraction
  ("ajaxTime",
   function (r) {
    document.getElementById("timeDisplay").innerHTML = r.responseText;
   },
   1000);
var lightStateRequest = new CyclicHttpInteraction
  ("lightState.xml",
   function (r) {
     var xml = r.responseXML;
     document.getElementById("forward-light").style.color =
       xml.getElementsByTagName("forward-light")[0].firstChild.nodeValue;
     document.getElementById("aft-light").style.color =
       xml.getElementsByTagName("aft-light")[0].firstChild.nodeValue;
   },
   1000);


var postChange = new OneshotHttpInteraction
  ("ajaxChange",
   null,
   function (r) { });


/**
 * Assign event handlers and begin fetching.
 */
window.onload = function () {

  stateRequest.start();

  timeRequest.start();
  lightStateRequest.start();

  document.fTimeFormat.format.onchange = function () {
    for (var o = document.fTimeFormat.format.options, i = 0;
         i < o.length;
         i++) {
      if (o[i].selected) {
        postChange.start("timeFormat=" +  o[i].value);
        break;
      }
    }
  };

  for (var j = 0; j < document.lights.forward.length; j++) {
  document.lights.forward[j].onclick = new Function(
     "postChange.start('" + "forward-light" + "=" + document.lights.forward[j].value + "');");
};
  for (var j = 0; j < document.lights.aft.length; j++) {
    document.lights.aft[j].onclick = new Function(
       "postChange.start('" + "aft-light" + "=" + document.lights.aft[j].value + "');");
  };

  for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
    c[i].onclick = new Function(
      "postChange.start('lamp=" + i
        + "&value=" + document.lamps.lamp[i].value
        + "&checked=' + document.lamps.lamp[" + i + "].checked);"
    );
  }

};
