window.onscroll = refreshScroll;
window.setInterval(fadeInTheDivs, 30);

var fadingDivs = [];

const util = {
  flatMap: function flatMap (airQuotesMonad, f) {
    var result = [];
    airQuotesMonad.forEach(function (el) {
    result = result.concat(f(el));
    });
    return result;
  },
  filterThenMap: function filterThenMap (arr, p, c) {
    return util.flatMap(arr, function (elem) {
      if (p(elem)) { return [c(elem)]; } else { return []; }
    });
  },
  httpGet: function httpGet(url, callback) {
    util.httpRequest("get", url, callback);
  },
  httpRequest: function httpRequest(type, url, callback) {
    const newRequest = new XMLHttpRequest();
    newRequest.onreadystatechange = function() {
      if (this.readyState == 4 && this.status == 200) {
        callback(this.responseText);
      }
    }
    newRequest.open(type.toUpperCase(), url);
    newRequest.send();
  },
  mapThenFilter: function mapThenFilter (arr, p, c) {
    return util.flatMap(arr, function(elem) {
      if (p(c(elem))) { return [c(elem)]; } else { return []; }
    })
  },
  nullableGet: function nullableGet (nullable, key) {
    if (nullable === null || nullable === undefined) {
      return nullable;
    } else {
      return nullable[key];
    }
  }
}

function fadeInTheDivs() {
  fadingDivs = util.flatMap(fadingDivs, function (elem) {
    if (elem === undefined || elem.style === undefined) { return []; }
    const opacity = parseFloat(elem.style.opacity);
    if (opacity < 1.0) {
      elem.style.opacity = opacity + 0.01;
      return [elem];
    }
  });
}

function refreshScroll() {
  const posts = document.getElementsByClassName("post");
  const lastPost = posts[posts.length - 1];
  if (window.innerHeight > lastPost.getBoundingClientRect().top) {
    const stubs = document.getElementsByClassName("stub");
    if (document.getElementById("thatsallfolks")) {
      return;
    }
    if (stubs.length != 0) {
      const lastStubId = stubs[0].id;
      util.httpGet("/just/" + lastStubId.replace("t", ""), function (entry) {
        var lastStub = document.getElementById(lastStubId);
        lastStub.insertAdjacentHTML('beforebegin', entry);
        lastStub.previousSibling.opacity = 0.01;
        fadingDivs.push(lastStub.previousSibling);
        lastStub.parentNode.removeChild(lastStub);
      });
    } else {
      var offset = posts.length;
      const matches = window.location.toString().match(/from\/(\d+)/)
      if (matches) {
        offset += parseInt(matches[1]) * 2;
      }
      util.httpGet("/next-after/" + offset, function (entries) {
        if (entries == "") {
          const thatsAllFolks = document.createElement("div");
          thatsAllFolks.id = "thatsallfolks";
          thatsAllFolks.hidden = true;
          document.getElementById("main").appendChild(thatsAllFolks);
        } else {
          newStubDiv = document.createElement("div");
          newStubDiv.innerHTML = entries;
          for (var i=0; i<newStubDiv.children.length; i++) { // this bit makes me sad.
            if (!document.getElementById(newStubDiv.children[i].id)) {
              document.getElementById("main").appendChild(newStubDiv.children[i]);
            }
          }
        }
      });
    }
  }
}
