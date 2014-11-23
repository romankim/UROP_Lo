    looping = false;
    var toggleFunction = function() {
      if(looping) {
        $(this).val("run");
        clearInterval(intervalVar);
        looping = false;
      }
      else {
        $(this).val("stop");
        intervalVar = setInterval(stepFunction, 250);
        looping = true
      }
     }