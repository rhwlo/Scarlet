const PAGE_LENGTH = 2;
const PAGE_OFFSET = (function () {
  const pageMatches = window.location.toString().match(/from\/(\d+)/);
  if (!pageMatches) {
    return 0;
  } else {
    return parseInt(pageMatches[1]);
  }})();

function scrolledPast(element) {
  return Bacon.fromBinder(function (sink) {
    $(window).scroll(function (e) {
      if ($(window).scrollTop() + $(window).height() > $(element).position().top) {
        sink($(element));
      }
    });
  });
}

function toResultStream(request) {
  return Bacon.fromPromise($.ajax(request));
}

var lastStubScrolled = scrolledPast("div.post:last")
  .filter(function (el) {
    return (el.next()[0] === undefined || el.nextAll(".stub")[0] === undefined); });

var getMoreStubs = lastStubScrolled
  .filter(function (el) { return (el.nextAll("#thatsallfolks")[0] === undefined); })
  .map(function () { return Math.floor($("div.post").length / PAGE_LENGTH); })
  .filter(function (pageNum) { return ($("page" + pageNum)[0] === undefined)})
  .map(function () {
    return "/next-after/" + (PAGE_OFFSET + Math.floor($("div.post").length / PAGE_LENGTH)); })
  .flatMap(toResultStream)
  .zip(lastStubScrolled);

getMoreStubs.onError(function (err) {
  console.log(err);
});

getMoreStubs
  .onValues(function (newStubs, lastPost) {
    if (newStubs == "" && $("#thatsallfolks")[0] === undefined) {
      lastPost.after("<div id=\"thatsallfolks\" hidden=\"\"></div>");
    } else {
      $.each($.parseHTML(newStubs), function (i, el) {
        if ($("#" + el.id).length == 0) {
          var lastPostOrStub = $("#main > div:last")
          lastPostOrStub.after(el);
        }
      });
    }
  });

var nextStubToPopulate = scrolledPast("div.post:last")
  .filter(function (el) { return el.nextAll(".stub")[0] != undefined })
  .map(function (el) { return $(el.nextAll(".stub")[0]); });

nextStubToPopulate
  .map(function (el) { return el[0].id.replace(/^t/, "/just/"); })
  .flatMap(toResultStream)
  .zip(nextStubToPopulate)
  .onValues(function (newElementAsText, oldStub) {
    var newElement = $.parseHTML(newElementAsText);
    $(newElement)
     .hide()
     .replaceAll(oldStub)
     .fadeIn(1000);
  });
