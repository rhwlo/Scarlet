// The page length (i.e., the number of results to scroll past before trying to load a new page)
//   is set here to its default value of 2. If you’d like to change this, here’s one of the places
//   to do so!
const PAGE_LENGTH = 2;

// `PAGE_OFFSET` determines how many pages we’ve already passed — by default, this is 0, since
//   we tend to start at the beginning. However, if we’ve loaded a page that matches `from/N`,
//   we should use a PAGE_OFFSET of `N`.
const PAGE_OFFSET = (function () {
  const pageMatches = window.location.toString().match(/from\/(\d+)/);
  if (!pageMatches) {
    return 0;
  } else {
    return parseInt(pageMatches[1]);
  }})();

// `scrolledPast(element)` generates an event whenever `element` is scrolled past in the window.
function scrolledPast(element) {
  return Bacon.fromBinder(function (sink) {
    $(window).scroll(function (e) {
      if ($(window).scrollTop() + $(window).height() > $(element).position().top) {
        sink($(element));
      }
    });
  });
}

// `toResultStream` will change our AJAX requests into a result stream for Bacon so that we can
//    more easily interact with them in this framework.
function toResultStream(request) {
  return Bacon.fromPromise($.ajax(request));
}

// `lastStubScrolled` keeps track of whether the final `div.post` element has been scrolled past,
//    and, if it has been, whether there are any `stub` elements remaining after the `div.post`
var lastStubScrolled = scrolledPast("div.post:last")
  .filter(function (el) {
    return (el.next()[0] === undefined || el.nextAll(".stub")[0] === undefined); });

// `getMoreStubs` watches whether more stubs need to be populated on the page (i.e., whether the
//    last stub has been scrolled past) and loads more if need be.
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
    //   should we receive an empty set of `newStubs`, we should add a marker to denote that there
    //   are no more stubs to be loaded, if one doesn’t exist; we’ll call this `#thatsallfolks`.
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

// If we’ve scrolled past the last `div.post` and there _are_ still stubs, we should populate
//    them into real posts.

// First, `nextStubToPopulate` finds that stub, if it exists;
var nextStubToPopulate = scrolledPast("div.post:last")
  .filter(function (el) { return el.nextAll(".stub")[0] != undefined })
  .map(function (el) { return $(el.nextAll(".stub")[0]); });
// and then we fade the content in.
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
