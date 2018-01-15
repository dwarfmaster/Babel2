/*
Copyright (c) 2009, 2010 Berend-Jan "SkyLined" Wever <berendjanwever@gmail.com>
Project homepage http://code.google.com/p/asyncxmlhttprequest/
All rights reserved. See COPYRIGHT.txt for details.
*/
function dump_exception(where, e, instance) {
  s = [];
  if (instance) {
    s.push(where + "() threw an exception:");
    s.push(instance.url);
  }
  for (var i in e) {
    try { l = i + "=" + e[i]; } catch(e) { l = i + "=[access denied]"; }
    s.push(l);
  }
  alert(s.join("\r\n"));
}

function AsyncXMLHttpRequest() {
  // An AsyncXMLHttpRequest object works as a wrapper for a real XMLHttpRequest object. The user should be able to use
  // the AsyncXMLHttpRequest as if it is a real XMLHttpRequest. In the current implementation there is one exception
  // to this: reading certain attributes of the real XMLHttpRequest can throw an exception, but reading the same
  // attributes from the AsyncXMLHttpRequest object does not: their value will be "undefined". There may be a way to
  // fix that for certain JavaScript implementations, but not for all.
  this._XMLHttpRequest = null;
  if (typeof(XMLHttpRequest) != "undefined") {
    this._XMLHttpRequest = new XMLHttpRequest();
  } else {
    xmlhttp_objects = ["Msxml2.XMLHTTP.6.0", "Msxml2.XMLHTTP.3.0", "Msxml2.XMLHTTP", "Microsoft.XMLHTTP"];
    for (var i in xmlhttp_objects) {
      try {
        this._XMLHttpRequest = new ActiveXObject(xmlhttp_objects[i]);
      } catch (e) {
        continue;
      }
      break;
    }
  }
  if (this._XMLHttpRequest == null) {
    throw new Error("Cannot instanciate an XMLHttpRequest object");
  }

  // AsyncXMLHttpRequest shares the following attributes with XMLHttpRequest:
  //   readyState, responseBody, responseText, responseXML, status, statusText
  // These work exactly the same in AsyncXMLHttpRequest as in XMLHttpRequest. To make it easy to update them, this
  // function is used internally:
  function AsyncXMLHttpRequest_copy_attributes(instance) {
    properties = ["readyState", "responseBody", "responseText", "responseXML", "status", "statusText"];
    for (var i in properties) {
      var property = properties[i];
      try {
        instance[property] = instance._XMLHttpRequest[property];
      } catch (e) {
        instance[property] = void(0);
      }
    }
  }
  // Make a copy of the default attributes:
  AsyncXMLHttpRequest_copy_attributes(this);
  // In addition to the default attributes, AsyncXMLHttpRequest has these:
  this.timeout = 0;           // Timeout for the request in milliseconds.
  this.timedout = false;      // Indicates if the request has timed out.
  this.duration = 0;          // How long has this object been waiting for a reply?
  // If the request does not complete before the timeout, it is aborted using "abort()", "timedout" is set to true and
  // the "ontimeout" event is fired.
  this.method = void(0);      // The method argument used in open()
  this.url = void(0);         // The url argument used in open()
  this.user = void(0);        // The user argument used in open()
  this.pasword = void(0);     // The password argument used in open()
  this.body = void(0);        // The body argument used in send()
  // These five attributes are set automatically when "open()" and "send()" are called for the users' convenience -
  // they add no extra functionality.

  // AsyncXMLHttpRequest shares the following event with XMLHttpRequest:
  this.onreadystatechange = void(0);
  // However, the AsyncXMLHttpRequest object for which the event fires is passed as the first argument to the event
  // handler. This makes it easy to track which object an event is firing for when you create many parallel requests.

  // In addition to this event, AsyncXMLHttpRequest has these events, which are called when a request is completed:
  this.onload = void(0);      // Called when the server returns success.
  this.onerror = void(0);     // Called when the server returns an error.
  this.ontimeout = void(0);   // Called when the request timed out.
  this.onfinish = void(0);    // Called when the request is closed.
  // Only one of these is called for each request, depending on the outcome; if there is a timeout, the "ontimeout"
  // event is fired but the "onerror" event is not: the "onerror" event is only called when the request does not time
  // out before the server responds with a status code <200 or >299. The AsyncXMLHttpRequest object for which the
  // event fires is passed as the the first argument to the event handler for each of these events.

  // Different browsers can throw different exceptions. All these exceptions are handled to make this object work
  // uniform across browsers:
  // MSIE throws an "access denied" exception when you call open() for an XHR with a cross-origin URL:
  this._msie_access_denied_error = false;
  // MSIE throws an "resource not found" exception when you call send() for an XHR request to certain illegal ports:
  this._msie_access_illegal_port_error = false;
  // Opera throws an "Security violation" exception when you call send() for an XHR with a cross-origin URL:
  this._opera_send_security_violation = false;
  // Firefox throws an "malformed URI" exception when you call open() with a invalid url:
  this._firefox_malformed_uri_error = false;
  // Safari throws an "network error" exception when you call send() with a hostname that cannot be resolved:
  this._safari_network_err = false;

  // Most methods are implemented as wrappers for the XMLHttpRequest object:
  // Wrapper for getAllReponseHeaders()
  this.getAllReponseHeaders = function AsyncXMLHttpRequest_getAllReponseHeaders() {
    this._XMLHttpRequest.getAllReponseHeaders();
  }
  // Wrapper for getReponseHeader(name)
  this.getReponseHeader = function AsyncXMLHttpRequest_getReponseHeader(name) {
    this._XMLHttpRequest.getReponseHeader(name);
  }
  // Wrapper for setRequestHeader(name, value)
  this.setRequestHeader = function AsyncXMLHttpRequest_setRequestHeader(name, value) {
    this._XMLHttpRequest.setRequestHeader(name, value);
  }
  // To make this work uniform across all browsers, some exceptions need to be caught and readyState changes faked.
  // The later can be done with this function:
  function AsyncXMLHttpRequest_fake_readystatechange(instance, newReadyState, do_it_now) {
    // These are the actions that need to be taken:
    function fake_readystatechange() {
      instance.status = 0;
      instance.readyState = newReadyState;
      if (typeof(instance.onreadystatechange) != "undefined") {
        instance.onreadystatechange(instance);
      }
      if (instance.readyState == 4) {
        // readyState changes are always faked if there was an error. If the readyState reaches 4, the request is done
        // and he onerror and onfinish events may need to be fired:
        if (typeof(instance.onerror) != "undefined") {
          instance.onerror(instance);
        }
        if (typeof(instance.onfinish) != "undefined") {
          instance.onfinish(instance);
        }
        delete instance._XMLHttpRequest;
      }
    }
    // The actions may need to be taken now, or later (this prevents recursive event handler loops):
    if (do_it_now) {
      fake_readystatechange();
    } else {
      setTimeout(fake_readystatechange, 1);
    }
  }
  // Wrapper for open(method, url, user, password)
  this.open = function AsyncXMLHttpRequest_open(method, url, user, password) {
    // Make a copy of the arguments for the users' convenience:
    this.method = method;
    this.url = url;
    this.user = user;
    this.pasword = password;
    // Record the start time of the request:
    this._start_time = new Date().valueOf();
    try {
      if (typeof(user) == "undefined") {
        this._XMLHttpRequest.open(method, url, true); // No user/password supplied
      } else {
        this._XMLHttpRequest.open(method, url, true, user, password);
      }
    } catch (e) {
      // Update the attributes of the AsyncXMLHttpRequest object to those of the XMLHttpRequest object:
      AsyncXMLHttpRequest_copy_attributes(this);
      if (typeof(e.number) != "undefined" && e.number == -2147024891) {
        // MSIE does not allow opening a url cross-domain. To make this work uniform across all browsers, the
        // exception is handled and a  readyState change to 1 with a status of 0 is "faked":
        this._msie_access_denied_error = true;
        AsyncXMLHttpRequest_fake_readystatechange(this, 1, true); // change readyState now.
      } else if (typeof(e.name) != "undefined" && e.name == "NS_ERROR_MALFORMED_URI"){
        // Firefox does not allow opening a url that's incorrect. To make this work uniform across all browser, the
        // exception is handled and a readyState change to 1 with a status of 0 is "faked":
        this._firefox_malformed_uri_error = true;
        AsyncXMLHttpRequest_fake_readystatechange(this, 1, true); // change readyState now.
      } else {
        // An unknown exception occured, pass it on:
        dump_exception("open", e, this);
        throw e;
      }
      return;
    }
    // Update the attributes of the AsyncXMLHttpRequest object to those of the XMLHttpRequest object:
    AsyncXMLHttpRequest_copy_attributes(this);
  }
  // Wrapper for send(body)
  this.send = function AsyncXMLHttpRequest_send(body) {
    // Record the start time of the request:
    this._start_time = new Date().valueOf();
    if (this._msie_access_denied_error || this._firefox_malformed_uri_error) {
      // Some urls can cause "open()" to fail in MSIE and Firefox. This would normally have thrown an exception, but
      // it has been caught and handled to make this work uniform across browsers. Now that "send()" had been called,
      // fake a readyState change to 4 with status 0 (error):
      AsyncXMLHttpRequest_fake_readystatechange(this, 4, false); // change readyState later
      return;
    }
    // Make a copy of the arguments for the users' convenience:
    this.body = body;
    try {
      if (typeof body == "undefined") {
        this._XMLHttpRequest.send(null); // no body supplied
      } else {
        this._XMLHttpRequest.send(body);
      }
    } catch (e) {
      if (typeof(e.number) != "undefined" && e.number == -2146697211) {
        // MSIE does not allow opening a url to certain ports. To make this work uniform across all browsers, the
        // exception is handled:
        this._msie_access_illegal_port_error = true;
      } else if (typeof(e.NETWORK_ERR) != "undefined" && e.NETWORK_ERR == 101) {
        // Safari throws an error if the network name cannot be resolved. The readyState has already been changed to 1
        // and 4 and the status is set to 0. There is no need for calling event handlers or anything else:
        this._safari_network_err = true;
      } else if (typeof(e.message) != "undefined" && e.message.match("^Security violation")) {
        // Opera throws an error if the request is cross-origin. To make this work uniform across all browsers the
        // exception is handle and a readyState change to 4 with a status of 0 is "faked" when the readyState changes
        // to 2:
        this._opera_send_security_violation = true;
      } else if (typeof(e.name) != "undefined" && e.name == "NS_ERROR_PORT_ACCESS_NOT_ALLOWED"){
        // Firefox does not allow opening a url for certain ports. To make this work uniform across all browser, the
        // exception is handled and a readyState change to 4 with a status of 0 is "faked":
        AsyncXMLHttpRequest_fake_readystatechange(this, 4, false); // Change readyState later
      } else {
        // An unknown exception occured, pass it on:
        dump_exception("send", e, this);
        throw e;
      }
      return;
    }
    // Update the attributes of the AsyncXMLHttpRequest object to those of the XMLHttpRequest object:
    AsyncXMLHttpRequest_copy_attributes(this);
    // If the "timeout" attribute is set, record the start time and start a timer to detect a timeout:
    if (this.timeout != 0) {
      // Save "this" in a variable, so the timer function can refer to it:
      var instance = this;
      // Start a timer
      this._timer = setTimeout(function AsyncXMLHttpRequest_timer() {
        // This request has timed out: remove the timer, calculate the duration and abort the request:
        delete instance._timer;
        instance.timedout = true;
        instance.duration = new Date().valueOf() - instance._start_time;
        alert(instance._XMLHttpRequest);
        instance.abort();
      }, this.timeout);
    }
  }
  // Wrapper for abort()
  this.abort = function AsyncXMLHttpRequest_abort() {
    this._XMLHttpRequest.abort();
    // Update the attributes of the AsyncXMLHttpRequest object to those of the XMLHttpRequest object:
    AsyncXMLHttpRequest_copy_attributes(this);
  }
  // Save "this" in a variable, so the onreadystate change handler function can refer to it:
  var instance = this;
  // Create an event handler for "readyState" changes of the XMLHttpRequest object. The handler can use "this" to refer
  // to the AsyncXMLHttpRequest and call the appropriate event handler(s) set by the user:
  this._XMLHttpRequest.onreadystatechange = function AsyncXMLHttpRequest_onreadystatechange() {
    // Check if the readyState of the wrapped XHR object is now HIGHER than the value stored in the wrapper. The 
    // wrapper may be faking a higher value because it caught an exception at some point for cross-browser uniformity:
    if (instance.readyState < instance._XMLHttpRequest.readyState) {
      // Copy the new readyState, status and other properties:
      AsyncXMLHttpRequest_copy_attributes(instance);
      if (instance.readyState >= 2) {
        // Send has been called, calculate the duration of the request:
        instance.duration = new Date().valueOf() - instance._start_time;
      }
      // MSIE has readyState 0, no other browser does. So readyState changes to 0 are ignored to make this uniform
      // across browsers:
      if (instance.readyState != 0) {
        // Call the onreadystatechange of the object, if there is one:
        if (typeof(instance.onreadystatechange) != "undefined") {
          instance.onreadystatechange(instance);
        }
      }
      // Opera can throw an exception that we handle to keep the request uniform cross-browsers. If this happened, the
      // readyState will be changed to 2 and never reach 4. So the change to 2 is hiden and a change to 4 is faked:
      if (instance.readyState == 2 && instance._opera_send_security_violation) {
        instance.readyState = 4;
      }
      if (instance.readyState == 4) {
        // When the readyState reaches 4, the request is done and "onload", "onerror" and "ontimeout" events should
        // fire if they are set:
        var ontimeout_called = false;
        if (instance.timeout != 0) {
          // If this request has timed out, call ontimeout, if it exists:
          if (instance.timedout) {
            if (typeof(instance.ontimeout) != "undefined") {
              instance.ontimeout(instance);
            }
            // Prevent calling the "onerror" handler as well:
            ontimeout_called = true;
          } else {
            // The request did not time out, but it has a timer: clear and
            // remove the timer:
            clearTimeout(instance._timer);
            delete instance._timer
          }
        }
        if (!ontimeout_called) {
          // Check if the request succeeded or not and call the right event handler:
          if (instance.status >= 200 && instance.status <= 299) {
            // Call onload if the status is successful and it exists:
            if (typeof(instance.onload) != "undefined") {
              instance.onload(instance);
            }
          } else {
            // Call onerror if the status is unsuccessful and it exists:
            if (typeof(instance.onerror) != "undefined") {
              instance.onerror(instance);
            }
          }
        }
        // This request is finished.
        if (typeof(instance.onfinish) != "undefined") {
          instance.onfinish(instance);
        }
        delete instance._XMLHttpRequest;
      }
    }
  }
}
